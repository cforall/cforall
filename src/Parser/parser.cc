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
    QUOTED_IDENTIFIER = 360,
    TYPEDIMname = 361,
    TYPEDEFname = 362,
    TYPEGENname = 363,
    TIMEOUT = 364,
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
#define QUOTED_IDENTIFIER 360
#define TYPEDIMname 361
#define TYPEDEFname 362
#define TYPEGENname 363
#define TIMEOUT 364
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

#line 684 "Parser/parser.cc"

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
#define YYLAST   22266

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  297
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1062
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2146

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
    1165,  1166,  1168,  1173,  1180,  1182,  1184,  1194,  1196,  1198,
    1203,  1208,  1211,  1216,  1218,  1220,  1222,  1230,  1231,  1233,
    1237,  1239,  1243,  1245,  1246,  1248,  1250,  1255,  1256,  1260,
    1265,  1266,  1270,  1272,  1277,  1279,  1284,  1286,  1288,  1290,
    1295,  1297,  1299,  1301,  1306,  1308,  1313,  1314,  1336,  1338,
    1340,  1343,  1345,  1348,  1350,  1353,  1355,  1360,  1365,  1367,
    1372,  1377,  1379,  1381,  1383,  1385,  1388,  1390,  1393,  1395,
    1400,  1406,  1409,  1411,  1416,  1422,  1424,  1429,  1435,  1438,
    1440,  1443,  1445,  1450,  1457,  1459,  1464,  1470,  1472,  1477,
    1483,  1486,  1491,  1499,  1501,  1503,  1508,  1510,  1515,  1516,
    1518,  1523,  1525,  1530,  1532,  1534,  1536,  1539,  1543,  1546,
    1550,  1552,  1554,  1556,  1558,  1560,  1562,  1564,  1566,  1568,
    1570,  1575,  1576,  1580,  1586,  1594,  1599,  1600,  1604,  1608,
    1613,  1614,  1620,  1624,  1626,  1628,  1630,  1633,  1635,  1640,
    1642,  1647,  1649,  1651,  1656,  1658,  1664,  1665,  1669,  1670,
    1671,  1672,  1676,  1681,  1682,  1684,  1686,  1688,  1692,  1696,
    1697,  1701,  1703,  1705,  1707,  1709,  1715,  1716,  1722,  1723,
    1727,  1728,  1733,  1735,  1741,  1742,  1744,  1749,  1754,  1765,
    1766,  1770,  1771,  1777,  1778,  1782,  1784,  1788,  1790,  1794,
    1795,  1799,  1800,  1804,  1811,  1812,  1816,  1818,  1833,  1834,
    1835,  1836,  1838,  1842,  1844,  1848,  1855,  1857,  1859,  1864,
    1865,  1867,  1869,  1871,  1903,  1906,  1911,  1913,  1919,  1924,
    1929,  1940,  1946,  1951,  1956,  1961,  1970,  1974,  1981,  1983,
    1984,  1985,  2001,  2003,  2008,  2009,  2010,  2011,  2021,  2022,
    2023,  2032,  2033,  2034,  2038,  2039,  2046,  2055,  2056,  2057,
    2062,  2063,  2072,  2073,  2078,  2079,  2083,  2085,  2087,  2089,
    2091,  2096,  2101,  2102,  2104,  2114,  2115,  2120,  2122,  2124,
    2126,  2128,  2130,  2133,  2135,  2137,  2142,  2144,  2146,  2148,
    2150,  2152,  2154,  2156,  2158,  2160,  2162,  2164,  2166,  2168,
    2170,  2172,  2174,  2176,  2178,  2180,  2182,  2184,  2186,  2188,
    2190,  2192,  2194,  2196,  2201,  2202,  2206,  2213,  2214,  2220,
    2221,  2223,  2225,  2227,  2232,  2234,  2239,  2240,  2242,  2244,
    2249,  2251,  2253,  2255,  2257,  2259,  2264,  2271,  2273,  2275,
    2280,  2288,  2287,  2291,  2299,  2300,  2302,  2304,  2309,  2310,
    2312,  2317,  2318,  2320,  2322,  2327,  2328,  2330,  2335,  2337,
    2339,  2341,  2342,  2344,  2349,  2351,  2353,  2358,  2365,  2369,
    2370,  2375,  2374,  2379,  2378,  2397,  2396,  2408,  2407,  2418,
    2423,  2424,  2429,  2435,  2449,  2450,  2454,  2456,  2458,  2464,
    2466,  2468,  2470,  2472,  2474,  2476,  2478,  2484,  2485,  2490,
    2499,  2501,  2503,  2512,  2514,  2515,  2516,  2518,  2520,  2521,
    2526,  2527,  2528,  2533,  2535,  2538,  2545,  2546,  2547,  2553,
    2558,  2560,  2566,  2567,  2573,  2574,  2578,  2583,  2586,  2585,
    2589,  2592,  2599,  2604,  2603,  2612,  2617,  2622,  2627,  2632,
    2633,  2638,  2640,  2645,  2647,  2649,  2651,  2656,  2657,  2663,
    2664,  2665,  2672,  2673,  2675,  2676,  2677,  2679,  2681,  2688,
    2689,  2691,  2693,  2698,  2699,  2705,  2706,  2708,  2709,  2714,
    2715,  2716,  2718,  2726,  2727,  2729,  2732,  2734,  2738,  2739,
    2740,  2742,  2744,  2749,  2751,  2756,  2758,  2767,  2769,  2774,
    2775,  2776,  2780,  2781,  2782,  2787,  2788,  2793,  2794,  2795,
    2796,  2800,  2801,  2806,  2807,  2808,  2809,  2810,  2824,  2825,
    2830,  2831,  2837,  2839,  2842,  2844,  2846,  2869,  2870,  2876,
    2877,  2883,  2882,  2892,  2891,  2895,  2901,  2907,  2908,  2910,
    2914,  2919,  2921,  2923,  2925,  2931,  2932,  2936,  2937,  2942,
    2944,  2951,  2953,  2954,  2956,  2961,  2963,  2965,  2970,  2972,
    2977,  2982,  2990,  2995,  2997,  3002,  3007,  3008,  3013,  3014,
    3018,  3019,  3020,  3025,  3027,  3033,  3035,  3040,  3042,  3048,
    3049,  3053,  3057,  3061,  3063,  3076,  3078,  3080,  3082,  3084,
    3086,  3088,  3089,  3094,  3097,  3096,  3108,  3107,  3120,  3119,
    3131,  3130,  3142,  3141,  3155,  3161,  3163,  3169,  3170,  3181,
    3188,  3193,  3199,  3202,  3205,  3209,  3215,  3218,  3221,  3226,
    3227,  3228,  3232,  3238,  3239,  3249,  3250,  3254,  3255,  3260,
    3265,  3266,  3272,  3273,  3275,  3280,  3281,  3282,  3283,  3284,
    3286,  3321,  3323,  3328,  3330,  3331,  3333,  3338,  3340,  3342,
    3344,  3349,  3351,  3353,  3355,  3357,  3359,  3361,  3366,  3368,
    3370,  3372,  3381,  3383,  3384,  3389,  3391,  3393,  3395,  3397,
    3402,  3404,  3406,  3408,  3413,  3415,  3417,  3419,  3421,  3423,
    3435,  3436,  3437,  3441,  3443,  3445,  3447,  3449,  3454,  3456,
    3458,  3460,  3465,  3467,  3469,  3471,  3473,  3475,  3490,  3495,
    3500,  3502,  3503,  3505,  3510,  3512,  3514,  3516,  3521,  3523,
    3525,  3527,  3529,  3531,  3533,  3538,  3540,  3542,  3544,  3546,
    3556,  3558,  3560,  3561,  3563,  3568,  3570,  3572,  3577,  3579,
    3581,  3583,  3588,  3590,  3592,  3606,  3608,  3610,  3611,  3613,
    3618,  3620,  3625,  3627,  3629,  3634,  3636,  3641,  3643,  3660,
    3661,  3663,  3668,  3670,  3672,  3674,  3676,  3681,  3682,  3684,
    3686,  3691,  3693,  3695,  3701,  3703,  3706,  3709,  3711,  3715,
    3717,  3719,  3720,  3722,  3724,  3728,  3730,  3735,  3737,  3739,
    3741,  3776,  3777,  3781,  3782,  3784,  3786,  3791,  3793,  3795,
    3797,  3799,  3804,  3805,  3807,  3809,  3814,  3816,  3818,  3824,
    3825,  3827,  3836,  3839,  3841,  3844,  3846,  3848,  3862,  3863,
    3865,  3870,  3872,  3874,  3876,  3878,  3883,  3884,  3886,  3888,
    3893,  3895,  3903,  3904,  3905,  3910,  3911,  3916,  3918,  3920,
    3922,  3924,  3926,  3933,  3935,  3937,  3939,  3941,  3944,  3946,
    3948,  3950,  3952,  3957,  3959,  3961,  3966,  3992,  3993,  3995,
    3999,  4000,  4004,  4006,  4008,  4010,  4012,  4014,  4021,  4023,
    4025,  4027,  4029,  4031,  4036,  4038,  4040,  4047,  4049,  4067,
    4069,  4074,  4075
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
  "invalid_types", "declaration_specifier_nobody", "type_specifier",
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
     405,   125,    40,    64,    41,    46,    91,    93,    44,    58,
     123,    96,    94,    42,    38,    43,    45,    33,   126,    92,
      47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1726)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1061)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     141, 12254,   236,   306, 16947,    39, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,   195,   876,
     213, -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,   148,   374,
   -1726, -1726, -1726, -1726, -1726, -1726,  4263,  4263,   250, 12254,
     287,   311, 15237, -1726,   344, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726,  3585, -1726,   845,   257, -1726, -1726,
   -1726, -1726, -1726, 16795, -1726, -1726,   308,   370,   563,   408,
   -1726,  4263,   370,   370,   370,   362,  3976,   632,   816, 12416,
   -1726, -1726,   479, 16643,  2605, -1726, -1726, -1726,  2711,   682,
    6649, 10671,   777,  2711,  1202,   543, -1726, -1726, -1726, -1726,
     639, -1726, -1726, -1726, -1726,   605, -1726, -1726, -1726, -1726,
   -1726,   635,   646,   639, -1726,   639,   652, -1726, -1726, -1726,
   17813,  4263, -1726, -1726,  4263, -1726, 12254, -1726,   638, 17866,
   -1726, -1726,  4125, 18893, -1726, -1726,   782,   782,   669,  2477,
   -1726, -1726, -1726, -1726,   283, 14379,  3092,   639, -1726, -1726,
   -1726, -1726, -1726, -1726,   713, -1726,   694,   725,   737, -1726,
     780, 21489, -1726, -1726, -1726, -1726, -1726, -1726, -1726, 15704,
    3601,  3585,    55,   752,   758,   778,   787,   807,   826, -1726,
   -1726, 18018, 11074,   773, -1726, 17394, -1726, -1726, -1726, -1726,
     849, -1726, -1726,   848, -1726, 19565,   981, 19713, -1726,   860,
    4263,   646,   866,   871,   881,   883, -1726, -1726, -1726,  2869,
    2652,   886,   962,   131, -1726, -1726,   639,   639,   154,   291,
     188,   154, -1726,   639,   639, -1726,  2916, -1726, -1726,   914,
     924,   782, 14271, -1726, 16795, -1726, -1726,  2711, -1726,  1284,
     543,   926,  1013,   291,  4263,  4263,   563, -1726, 13896, -1726,
     782,   782,   972,  1013,   291,  4263, -1726,  6231, -1726, -1726,
   -1726,   782, -1726, -1726, -1726, -1726,   782, -1726,   961,  3866,
    4263, -1726,  1360,   927, -1726, -1726, -1726, 16494,   646,   322,
   -1726, -1726, 18944, -1726,   962,    45, -1726, 21489, 18893,  3338,
    2916, -1726,   231, -1726, -1726, -1726, 17866,  4263, -1726,   978,
   -1726, -1726, -1726, -1726,  4263,  3188,   368,   220, -1726,  4263,
     694, -1726,   892,   639,   639,   991, 18071,   603, 14862, 14432,
    2711,  2711, -1726,  2711,   782,  2711,   782, -1726, -1726,   639,
   -1726,  1002, -1726, 18223, -1726, -1726, -1726, 18276,   849, -1726,
     502,   735,   256,   589,   543,   994, -1726,  2477,  1010,   694,
    2477,  1773, -1726,  1016,  1049, 21563,  1040,  1050,  1071, 21489,
   21637,  1085, 22098, -1726, -1726, -1726, -1726, -1726, -1726, 21711,
   21711, 15548,  1082,  4414, -1726, -1726, -1726, -1726,   389, -1726,
     443, -1726,   839, -1726, 21489, 21489, -1726,  1038,   734,   918,
     989,   491,  1069,  1092,  1096,  1088,  1140,     9, -1726,   620,
   -1726,  1127, -1726,  1095,  4276, 16016, -1726, -1726,   619,  1127,
   -1726, -1726,   829, -1726, -1726,  3601,  1137,  1145,  1147,  1173,
    1178,  1180, -1726, -1726,   321,  1153, -1726,   868,  1153, -1726,
   -1726, 17813, -1726,  1122,  1179, 16172, -1726, -1726,  3541,  3104,
    1223, 14862,  1229,   565,   877, -1726, -1726, -1726, -1726, -1726,
    4263,  3929, -1726, -1726, -1726, -1726, -1726, -1726,  9445,  3424,
    1082, 19565,  1219,  1236, -1726, -1726,  1239, 19713,   822, -1726,
   -1726, -1726, 19787,  1235, -1726, -1726, -1726, -1726, -1726,  2869,
     832,  1270,  1272,  1274,   861,  1276,  1279,  1290,  2652, -1726,
   -1726,   639,  1285,   563,  1308, -1726, -1726,  1312, -1726, -1726,
     646,  1013, -1726, -1726, -1726,   646, -1726, -1726,  2916, -1726,
   16016, 16016, -1726,   782,  4125,  5357, 15023, -1726, -1726, -1726,
   -1726, -1726,   646,  1013,    45,  1317, -1726, -1726,  2711,  1334,
    1013,   291, -1726,   646,  1013, -1726, 22194, -1726,   782,   782,
   -1726, -1726,  1338,   133,  1353,   543,  1357, -1726, 17108, -1726,
     882, -1726,  1458, 19394, -1726,  4125, 16583, 14271, -1726, 16494,
   21785, -1726, -1726, -1726, -1726, -1726,  3338,   869,  2916, -1726,
   15023,   962, 12254, -1726,  1375, -1726,  1389, -1726, -1726, -1726,
   -1726, -1726,  2477, -1726, -1726,  1466,  3895,  3284, 18276, 11074,
   -1726, 18428, -1726,   782,   782, -1726, -1726,   849, -1726,   678,
    1388,  1528, 21489,   901,  1312,  1371, -1726,   639,   639, -1726,
    1153, -1726, 18071, -1726, -1726, 17555,   782,   782, -1726,  3895,
     639, -1726, 18748, -1726, -1726, 18223, -1726,   283, -1726, -1726,
   -1726,  1405,  4263,   994,  1404,   900, 17866,   905, -1726, -1726,
   -1726, -1726, -1726, -1726,   913, -1726,  1414,  1390, -1726, 15860,
   -1726,  4414, 18481, 18481, -1726, 15860, -1726, 21489, -1726, -1726,
   -1726, -1726, -1726, -1726, 15860, -1726, -1726, 17608, 18481, 18481,
    1095,  1490,  1522,   432,  1589, -1726,   921,  1415,  1121,  1417,
   -1726, 19787, 21489, 19861,  1408, 21489,  1360, 21489,  1360, -1726,
    2571, -1726, -1726, 19935,  1255, 21489, 19935,  1360, -1726, -1726,
   21489, 21489, 21489, 21489, 21489, 21489, 21489, 21489, 21489, 21489,
   21489, 21489, 21489, 21489, 21489, 21489, 21489, 21489, 21489, 20009,
    1395,   780,  3687, 11074, -1726, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726,  1419, 21489, -1726, -1726,   619,
     942, -1726, -1726,   639,   639, -1726, -1726, 16016, -1726,   347,
    1153, -1726,   929,  1153, -1726, -1726, -1726,  1312, -1726, -1726,
    1312, 21859, -1726, -1726, 11074,  1418,  1420,  4052,  1556,  2201,
     360,  1371, -1726,   639,   639,  1371,   363, -1726,   639,   639,
   21489,  4263,  1136,  1150,  1371,    73, 14218, 14218,  4263, -1726,
   -1726, 21489,  1239, -1726, 19565,  1432, -1726,  2142, -1726, -1726,
   -1726, -1726, -1726,   935, -1726, 14218,  1360,  4125,  1360,   939,
    1433,  1436,  1437,   944,  1438,  1439,  1441,   457,  1153, -1726,
   -1726,   474,  1153, -1726, -1726, -1726,  4125,   780, -1726,  1153,
   19089, -1726,   646, 17108, -1726, -1726,   945,  1442,   950,  1444,
   -1726,  1448, -1726,   646, -1726,  1449, -1726,   646,  1013,  1448,
   -1726,   646,  1452,  1454,  1455, -1726, -1726, 17555, -1726,  1463,
   -1726, -1726, -1726,  1360,  4263, 10147,  1538,  1443, 18642, -1726,
    1179, -1726, 14218,   958, -1726, -1726,  1448, -1726, 17866, 16016,
    1450, -1726,  1450, -1726, -1726, -1726,   256,   639,   639, -1726,
   18223, -1726, 11239, 16328, -1726, 17108,  1464,  1470,  1473, -1726,
   11967,   639, -1726,   901, -1726, -1726, -1726, -1726,  1312, -1726,
   -1726, -1726,   782, -1726,  3406, -1726, -1726,   543,   262,  1477,
    1453,  1474,   256, -1726, -1726,  1476,  1479,  1773, 19935, -1726,
    1483,  1472,   257,  1480,  1485,  1486,  1484,  1492, 21489,  1501,
    1502,  1505, 11074, 21489, -1726, -1726,  1635, -1726, -1726, -1726,
   21489, -1726,  1506,  1508, 19639,  1155, -1726, 19935,  1510, -1726,
    1511, -1726, -1726,  2972, -1726, -1726,   967, -1726, -1726, -1726,
   -1726,  2972, -1726, -1726,  1157,   677, -1726, -1726,  1038,  1038,
    1038,   734,   734,   918,   918,   989,   989,   989,   989,   491,
     491,  1069,  1092,  1096,  1088,  1140, 21489,  1167, -1726,  1509,
    2972, -1726, -1726, 19565, -1726, 17108,  1515,  1516,  1517,   942,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,  1312, -1726,
   -1726,  1312, 17108, 17108, -1726, -1726,  4052,   878,  1519,  1520,
    1523,  1524,  2493,  2201, -1726, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,  1525, -1726,
    1371, -1726, -1726, -1726, -1726, -1726, -1726, -1726, -1726,  1526,
    1527, -1726,   563,  2972,  1181,   211, -1726, -1726,  1514, -1726,
   19713, -1726, 21489,   639, 20083, 14218, -1726, -1726, -1726,  1500,
     492,  1153, -1726,   530,  1153, -1726, -1726, -1726, -1726,  1312,
   -1726, -1726, -1726,  1312,   962,  1530,  1312,   275, -1726,  1127,
    1521, -1726, -1726, -1726, -1726, -1726, -1726,  1534, -1726, -1726,
   -1726, 17866,  1448, -1726,   646, -1726, -1726, -1726, -1726, -1726,
   13057,  1532,  1529, -1726,   353, -1726,   431,   383, 10909,  1536,
   15383,  1543,  1546,  2762,  3058,  3000, 20157,  1547, -1726, -1726,
    1548,  1549, -1726, -1726,   646, 21489, 21489,  1676,  1544,   748,
   -1726,  1630,  1550,  1533, -1726, -1726, -1726,  9972, -1726, -1726,
   -1726, -1726, -1726,  1339, -1726, -1726, -1726,  1614, -1726, -1726,
   -1726,  1360, -1726, -1726, 12902, 16795,  1552, -1726,  4263, -1726,
    1535,  1557,  1558, -1726,  1193, -1726, -1726, -1726, -1726,  4125,
   -1726, -1726,  1540,  1541,   968, 17866,   694,   694,  1405,   994,
     994, -1726, -1726,  1082,  1179, 16172, -1726,  1127, -1726, 11404,
   -1726,   545,  1153, -1726,   782, 12088, -1726, -1726,   256,   639,
     639,   283,  4263, -1726, 20231, -1726,   256,  1405,  1562, -1726,
   -1726,   977,   750, 17555, 11074,  1360, -1726,   750, 17661,   750,
   -1726, 21489, 21489, 21489, -1726, -1726, -1726, -1726, 21489, 21489,
    1559, 19565, -1726, -1726,  1563,   771, -1726, -1726, -1726,  3738,
   -1726, -1726,  1197, -1726,   333, -1726, 19935,  1203, -1726, 19787,
   -1726, -1726, 21489,  1545,  1217,  1224,  1239, -1726,   552,  1153,
   -1726, -1726, 17108, 17108, -1726, -1726,  1569,   570,  1153, -1726,
     578,  1796,   639,   639, -1726, -1726, 17108, 17108, -1726,  1567,
   -1726, 15023, 15023,  1571,  1568,  1570,  1575, -1726,  1572, 21489,
   21489,  1228,  1574, -1726, -1726, -1726, -1726, -1726, -1726, -1726,
    1581, 21489, -1726, -1726, -1726,  1312, -1726, -1726, -1726,  1312,
   17108, 17108,   563,   639, -1726, -1726,  1234, 21489, 19238,  1580,
    1584,  1590, -1726, -1726, -1726,  1592, 13212, 13367, 13522, 17866,
   14271, 18481, 18481,  1594, -1726,  1573,  1576,  2130, 13735, -1726,
     434,  4263, -1726, -1726,  4263, -1726, 19935,   430,   497, -1726,
   -1726, -1726, -1726, 21489,  1595,  1664, 10743, 10322, -1726,  1579,
   -1726,  1583, 21489,  1585, 19565,  1591, 21489, 19787, 21489,   856,
   -1726,  1593,    -2, -1726,    64,  1596, -1726, -1726,  1598, -1726,
    1597, -1726,  1599,  1600, 15383,   316, 14057,   639,   452, -1726,
   -1726, -1726,  1603, -1726,  1607, -1726,  1610, -1726,  1608, -1726,
    1609, -1726, -1726, -1726, -1726,  1618,  1612,  1615, 11569,  1620,
    1623,  1624, -1726,  1631, -1726, -1726, -1726,  1312, 21489, 21489,
    1179,  1632, -1726,  1405, -1726,   994,   412,  1453, 19565, -1726,
    1405,  1637, -1726, 17866, -1726,   767,  1639,  1638,   979, -1726,
    1643, -1726, -1726, -1726, -1726, -1726, 19565,  1239, 19787, -1726,
    1670,  2972, -1726,  1670,  1670, -1726,  2972,  3989,  4065, -1726,
   -1726,  1244, -1726, -1726, -1726,  1645,  1640, -1726, -1726, -1726,
    1312, -1726, -1726,  1649,  1650,   639, -1726, -1726, -1726,  1312,
   -1726, -1726, -1726,  1651, -1726, -1726, -1726, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726,  1654, -1726, -1726,
   -1726, -1726,  1656,  1653,   639, -1726, 17108, 17108, -1726, -1726,
   -1726, -1726, 21489, -1726,   275, -1726,  1127, -1726, -1726, -1726,
    1663,  1665, -1726,  1594,  1594,  1594,  3772,   833,  1641,   475,
   -1726,  3772,   482, 16016, -1726, -1726, -1726,  3515, 21489,  3025,
     503, -1726, -1726,    44,  1658,  1658,  4263, -1726, -1726, 17260,
   -1726,   985, -1726, -1726, -1726, -1726,   986,  1667, 15383,  1550,
    1666, 21489,   308,  1668,   362, 13684, 17866, -1726, -1726, -1726,
     655, 15383, 21489,  1080,   606, -1726, 21489, 19411, -1726, -1726,
     544, -1726,  1239, -1726,   992,  1001,  1019, -1726, -1726, -1726,
   -1726,   646,   856,  1669, -1726, -1726, 21489, -1726,  1671,   780,
   10909, -1726, -1726, -1726, -1726, 21489,  1719, -1726,  9797, -1726,
     639, 15023, -1726, -1726, 17866, -1726, -1726, -1726,   256,   256,
   -1726, -1726, -1726,  1674, -1726, 17108, -1726, -1726,  1678, -1726,
    1682,  1691,   994,  1683, -1726, -1726,  1239,  1693, -1726, -1726,
    1694, -1726, -1726, 21489, -1726, 17661, 21489,  1239,  1696,  1248,
   -1726,  1261, -1726,  2972, -1726,  2972, -1726, -1726, -1726, -1726,
   17108,  1697,  1698, -1726, -1726, 17108, 17108,  1700,  1702,  1278,
   14540, 14701, -1726,  1688, -1726, -1726, -1726, -1726,  1703,  1705,
    1282, 21489, -1726, -1726, -1726, -1726, -1726,   575,   833,  1252,
     595, -1726, -1726, -1726, -1726,   639,   639, -1726, -1726, -1726,
     599, -1726,  1021,  3515,   405, -1726,  3025,   639, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, 15383,   448, 20305,  1775,
   15383,  1550, 15184, -1726, -1726, -1726, -1726, 21489, -1726, 20379,
    1785,  1686, 19488, 20453, 15383, 10497,  1550,   600,  1113,  1690,
   21489, -1726,  1714,   493, 15383, -1726, -1726,  1717, -1726, -1726,
    1699,   780,   726,  1716,  1720,  1251,  1801, -1726, -1726, -1726,
   -1726,  4263,  4125,  1405,  1405, -1726, -1726,  1718,  1735, -1726,
   -1726, -1726,  1733,   256,  1744, -1726,  1746, -1726, -1726, -1726,
   -1726,  1753, -1726, -1726, -1726,  1299,  1303, -1726, -1726, -1726,
   -1726, -1726, -1726, -1726, -1726, -1726, -1726,  1747, -1726, -1726,
    1759,  1763, -1726, -1726, -1726, -1726, -1726,  1764,  1765,  1767,
    1252, -1726,   639, -1726, -1726, -1726, -1726, -1726,  1766,  3772,
   -1726,  4977,    92, 11737, -1726, 15278, -1726,   130,  1030, 15383,
    1848,   604,  1756,   426, 15383, 21489,  1771,   600,  1113,  1751,
   21933,  1761,   507,  1855, -1726, 20527, 20601, 21489,  1550,  1754,
   11901, -1726, -1726, -1726, 18695, -1726,  1776,  1755,   157, 15383,
   -1726, 21489, 19935,   585, -1726, -1726, -1726,  1793,  1794,  1800,
   -1726, -1726,   256,  1405, -1726, -1726, -1726, -1726, -1726,  1802,
    1804,  1808, 15023,  1792, -1726, -1726,   584,  1153, -1726, -1726,
     833, -1726, -1726,   286, -1726,   100, -1726, -1726, -1726,  1812,
   12578, -1726, -1726, 15383, -1726,   149, -1726, 15383, 21489,  1813,
   20675, -1726, -1726, 20749, 20823, 21489,  1771,  1550, 20897, 20971,
   15383,  1797,   647,  1809,   650, -1726, -1726,  1818, 12578, 18695,
   -1726,  3834, 18428,  1360,  1822, -1726,  1878,  1832,   727,  1829,
   -1726,  1913, -1726,  1031, 15383,  1838, 15383, 15383, -1726, -1726,
   -1726,  1405,  1842, -1726, -1726, -1726, -1726, -1726, -1726, -1726,
    1312, -1726, 21489, -1726, 21489, -1726, -1726,  1335, 12740, -1726,
   -1726, 15383, -1726, -1726,  1550, -1726, -1726,  1550,  1826,   689,
    1827,   701, -1726, -1726,  1550, -1726,  1550, -1726,  1844, 21045,
   21119, 21193, -1726,  1335, -1726,  1828,  2636,  2416, -1726, -1726,
   -1726,   157,  1851, 21489,  1835,   157,   157, 15383, -1726, -1726,
   21489,  1899,  1903,  1865, -1726, 17108, -1726, -1726, 15278, -1726,
    1335, -1726, -1726,  1864, 21267, 21341, 21415, -1726, -1726,  1550,
   -1726,  1550, -1726,  1550, -1726,  1828, 21489,  1867,  2416,  1863,
     780,  1869, -1726,   791, -1726, -1726,  1055,  1801,   207, -1726,
   -1726, -1726,  6382,  1873, 15278, -1726, -1726,  1550, -1726,  1550,
   -1726,  1550,  1874,  1872, -1726,   646,   780,  1876, -1726,  1852,
     780, -1726, -1726, 15383,  1956,  1879, -1726, -1726, -1726,  9623,
   -1726,   646, -1726, -1726,  1283, 21489, -1726,  1078, -1726, 15383,
   -1726, -1726,   780,  1360,  1883,  1862, -1726, -1726, -1726,  1091,
   -1726, -1726,  1868,  1360, -1726, -1726
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   460,     0,     2,   460,   477,   478,   479,   480,   481,
     482,   483,   484,   485,   466,   468,   467,   469,     0,     0,
       0,   486,   488,   509,   489,   510,   492,   493,   507,   508,
     487,   505,   506,   490,   491,   494,   495,   496,   497,   498,
     499,   500,   501,   502,   503,   504,   511,   512,   815,   514,
     587,   588,   591,   593,   589,   595,     0,     0,     0,   460,
       0,     0,    16,   558,   564,     9,    10,    11,    12,    13,
      14,    15,   773,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   831,   460,   774,   405,     0,   408,   697,   410,
     419,     0,   409,   439,   440,     0,     0,     0,     0,   541,
     462,   464,   470,   460,   472,   475,   526,   513,   444,   519,
     524,   446,   536,   445,   551,   555,   561,   540,   567,   579,
     815,   584,   585,   568,   638,   411,   412,     3,   781,   794,
     465,     0,     0,   815,   853,   815,     2,   870,   871,   872,
     460,     0,  1040,  1041,     0,     1,   460,    16,     0,   460,
     428,   429,     0,   541,   470,   454,   455,   456,   784,     0,
     590,   592,   594,   596,     0,   460,     0,   816,   817,   586,
     515,   690,   691,   689,   750,   745,   735,     0,     0,   782,
       0,     0,   477,   775,   779,   780,   776,   777,   778,   460,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   559,
     562,   460,   460,     0,  1042,   541,   860,   878,  1046,  1039,
    1037,  1044,   404,     0,   164,   703,   163,     0,   413,     0,
       0,     0,     0,     0,     0,     0,   403,   930,   931,     0,
       0,   438,   813,   815,   809,   834,   815,   815,   811,     2,
     815,   810,   891,   815,   815,   888,     0,   534,   535,     0,
       0,   460,   460,     2,   460,   420,   463,   473,   527,     0,
     556,     0,   797,     2,     0,     0,   697,   421,   541,   520,
     537,   552,     0,   797,     2,     0,   476,   521,   528,   529,
     447,   538,   449,   450,   448,   543,   553,   557,     0,   571,
       0,   767,     2,     2,   795,   852,   854,   460,     0,     2,
       2,  1050,   541,  1053,   813,   813,     3,     0,   541,     0,
       0,   431,   815,   811,   810,     2,   460,     0,   771,     0,
     731,   733,   732,   734,     0,     0,   727,     0,   717,     0,
     726,   737,     0,   815,   815,     2,   460,  1061,   461,   460,
     472,   451,   519,   452,   544,   453,   551,   548,   569,   815,
     570,     0,   678,   460,   679,  1015,  1016,   460,   680,   682,
     558,   564,   639,   641,   642,   639,   818,     0,   748,   736,
       0,   822,    21,     0,    20,     0,     0,     0,     0,     0,
       0,     0,    23,    25,     4,     8,     5,     6,     7,     0,
       0,   460,     2,     0,   103,   104,   105,   106,    87,    24,
      88,    42,    86,   107,     0,     0,   122,   124,   128,   131,
     134,   139,   142,   144,   146,   148,   150,   152,   155,     0,
      26,     0,   565,     2,   107,   460,   156,   742,   693,   555,
     695,   741,     0,   692,   696,     0,     0,     0,     0,     0,
       0,     0,   832,   858,   815,   868,   876,   880,   886,     2,
    1048,   460,  1051,     2,   100,   460,     3,   677,     0,  1061,
       0,   461,   519,   544,   551,     3,     3,   659,   663,   673,
     679,   680,     2,   861,   879,  1038,     2,     2,    23,     0,
       2,   703,    24,     0,   701,   704,  1059,     0,     0,   710,
     699,   698,     0,     0,   799,     2,     2,     2,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   837,
     894,   815,     0,   697,     2,   833,   841,   958,   835,   836,
       0,   797,     2,   890,   898,     0,   892,   893,     0,   434,
     460,   460,   525,   461,     0,   541,   460,  1043,  1047,  1045,
     542,   771,     0,   797,   813,     0,   414,   422,   474,     0,
     797,     2,   771,     0,   797,   746,   522,   523,   539,   554,
     560,   563,   558,   564,   582,   583,     0,   747,   460,   687,
       0,   201,   397,   460,     3,     0,   541,   460,   796,   460,
       0,   416,     2,   417,   768,   436,     0,     0,     0,     2,
     460,   813,   460,   771,     0,     2,     0,   730,   729,   728,
     723,   471,     0,   721,   738,   517,     0,     0,   460,   460,
    1017,   461,   457,   458,   459,  1021,  1012,  1013,  1019,     2,
       2,   101,     0,   977,   991,  1061,   973,   815,   815,   982,
     989,   685,   460,   549,   681,   461,   545,   546,   550,     0,
     815,  1027,   461,  1032,  1024,   460,  1029,     0,   648,   640,
     647,  1059,     0,   639,     0,     0,   460,     0,   830,   829,
     825,   827,   828,   826,     0,   820,   823,     0,    22,   460,
      94,     0,   460,   460,    89,   460,    96,     0,    32,    36,
      37,    33,    34,    35,   460,    92,    93,   460,   460,   460,
       2,   103,   104,     0,     0,   182,     0,     0,   585,     0,
    1037,     0,     0,     0,     0,     0,     0,     0,     0,    55,
       0,    61,    62,    66,     0,     0,    66,     0,    90,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   460,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   163,     0,   161,   162,     2,
     942,   694,   939,   815,   815,   947,   566,   460,   859,   815,
     869,   877,   881,   887,     2,   862,   864,   866,     2,   882,
     884,     0,  1049,  1052,   460,     0,     0,     2,   101,   977,
     815,  1061,   912,   815,   815,  1061,   815,   927,   815,   815,
       3,   681,     0,     0,  1061,  1061,   460,   460,     0,     2,
     712,     0,  1059,   709,  1060,     0,   705,     0,     2,   708,
     711,   179,   178,     0,     2,   460,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   815,   846,   850,
     889,   815,   903,   908,   838,   895,     0,     0,   442,   954,
       0,   800,     0,   460,   801,   435,     0,     0,     0,     0,
     433,     2,   802,     0,   418,     2,   771,     0,   797,     2,
     803,     0,     0,     0,     0,   597,   666,   461,     3,     3,
     670,   669,   873,     0,     0,   460,   398,     0,   541,     3,
     100,     3,   460,     0,     3,   772,     2,   725,   460,   460,
     719,   718,   719,   518,   516,   641,   639,   815,   815,  1023,
     460,  1028,   461,   460,  1014,   460,     0,     0,     0,   992,
       0,   815,  1062,   978,   979,   686,   975,   976,   990,  1018,
    1022,  1020,   547,   582,     0,  1026,  1031,   644,   639,     0,
     649,     0,   639,   751,   749,     0,     0,   822,    66,   783,
       0,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   460,     0,   121,   120,     0,   117,   116,    27,
       0,    28,     0,     0,     0,     0,     3,    66,     0,    51,
       0,    52,    59,     0,    58,    70,     0,    67,    68,    71,
      54,     0,    53,    57,     0,     0,    50,   123,   125,   126,
     127,   129,   130,   132,   133,   137,   138,   135,   136,   140,
     141,   143,   145,   147,   149,   151,     0,     0,   407,     0,
       0,    29,     3,   703,   157,   460,     0,     0,     0,   943,
     944,   940,   941,   744,   743,     2,   863,   865,   867,     2,
     883,   885,   460,   460,   968,   967,     2,     0,     0,     0,
       0,     0,   815,   978,   915,   932,     2,   910,   918,   683,
     913,   914,   684,     2,   925,   935,   928,   929,     0,     3,
    1061,   426,     2,  1054,     2,   674,   675,   653,     3,     3,
       3,     3,   697,     0,   155,     0,     3,     3,     0,   706,
       0,   700,     0,   815,     0,   460,     3,   430,   432,     0,
     815,   847,   851,   815,   904,   909,     2,   839,   842,   844,
       2,   896,   899,   901,   813,     0,   955,     3,   959,   960,
       3,   805,     3,   531,   530,   533,   532,     2,   772,   806,
     753,   460,     2,   804,     0,   772,   807,   597,   597,   597,
     460,     0,     0,   688,     0,   401,     0,     0,   460,     0,
       2,     0,     0,     0,     0,     0,   184,     0,   331,   332,
       0,     0,   370,   369,     0,   159,   159,   376,   558,   564,
     198,     0,   185,     0,   209,   186,   187,   460,   203,   188,
     189,   190,   191,     0,   192,   193,   337,     0,   194,   195,
     196,     0,   197,   205,   541,   460,     0,   207,     0,   395,
       0,     0,     0,     3,     0,   785,   772,   760,   761,     0,
       3,   756,     3,     3,     0,   460,   735,   735,  1059,   639,
     639,  1025,  1030,     2,   100,   460,     3,   556,     3,   461,
       3,   815,   985,   988,   460,     3,   974,   980,   639,   815,
     815,     0,     0,   627,     0,   643,   639,  1059,     2,   819,
     821,     0,    95,   460,   460,     0,    99,    97,   460,     0,
     111,     0,     0,     0,   115,   119,   118,   183,     0,     0,
       0,   703,   108,   176,     0,     0,    45,    46,    84,     0,
      84,    84,     0,    72,    74,    48,     0,     0,    44,     0,
      47,   154,     0,     0,     0,     0,  1059,     3,   815,   950,
     953,   945,   460,   460,     3,     3,     0,   815,   921,   924,
     815,     0,   815,   815,   916,   933,   460,   460,  1055,     0,
     676,   460,   460,     0,     0,     0,     0,   415,     3,     0,
       0,     0,     0,   702,   707,     3,   798,   181,   180,     3,
       0,     0,     2,   840,   843,   845,     2,   897,   900,   902,
     460,   460,   697,   815,   966,   965,     0,     0,     0,     0,
       0,     0,     3,   772,   808,     0,   460,   460,   460,   460,
     460,   460,   460,   580,   609,     3,     3,   610,   541,   598,
       0,     0,   855,     2,     0,   399,    66,     0,     0,   322,
     323,   206,   208,     0,     0,     0,   460,   460,   318,     0,
     316,     0,     0,     0,   703,     0,     0,     0,     0,     0,
     160,     0,     0,   377,     0,     0,     3,   213,     0,   204,
       0,   313,     0,     0,     2,     0,   541,   815,     0,   396,
     970,   969,     0,     2,     0,   763,     2,   758,     0,   759,
       0,   739,   720,   724,   722,     0,     0,     0,   460,     0,
       0,     0,     3,     0,     2,   981,   983,   984,     0,     0,
     100,     0,     3,  1059,   633,   639,   649,   649,   703,   650,
    1059,     0,   752,   460,   824,   971,     0,     0,     0,    38,
       0,   112,   114,   113,   110,   109,   703,  1059,     0,    65,
      81,     0,    75,    82,    83,    60,     0,     0,     0,    69,
      56,     0,   153,   406,    30,     0,     0,     2,   946,   948,
     949,     3,     3,     0,     0,   815,     2,   917,   919,   920,
       2,   934,   936,     0,   911,   926,     3,     3,  1056,     3,
     661,   660,   664,  1058,     2,     2,  1057,     0,     3,   812,
     713,   714,     0,     0,   815,   437,   460,   460,     3,     3,
     443,   814,     0,   961,     0,   962,   963,   957,   905,   789,
       2,     0,   791,   580,   580,   580,   610,   616,   585,     0,
     622,   610,     0,   460,   572,   608,   604,     0,     0,     0,
       0,   611,   613,   815,   624,   624,     0,   605,   620,   460,
     402,     0,   326,   327,   324,   325,     0,     0,     2,   223,
       0,     0,   225,   410,   224,   541,   460,   304,   303,   305,
       0,     2,   184,   263,     0,   256,     0,   184,   319,   317,
       0,   311,  1059,   320,     0,     0,     0,   358,   359,   360,
     361,     0,   351,     0,   352,   328,     0,   329,     0,     0,
     460,   214,   202,   315,   314,     0,   349,   368,     0,   400,
     815,   460,   787,   740,   460,     2,     2,   632,   639,   639,
    1033,  1034,  1035,     0,   986,   460,     3,     3,     0,   994,
       0,     0,   639,     0,   646,   645,  1059,     0,   630,     3,
       0,   972,    98,     0,    31,   460,     0,  1059,     0,     0,
      85,     0,    73,     0,    79,     0,    77,    43,   158,   951,
     460,     0,     0,   856,   874,   460,   460,     0,     0,     0,
     460,   460,   716,     0,   423,   425,     3,     3,     0,     0,
       0,     0,   755,   793,   576,   578,   574,     0,     0,  1001,
       0,   617,  1006,   619,   998,   815,   815,   603,   623,   607,
       0,   606,     0,     0,     0,   626,     0,   815,   599,   614,
     625,   615,   621,   668,   672,   671,     2,     0,     0,   244,
       2,   226,   541,   309,   307,   310,   306,     0,   308,     0,
     252,     0,   184,     0,     2,   460,   264,     0,   289,     0,
       0,   312,     0,     0,     2,   335,   362,     0,   353,     2,
       0,     0,     0,     0,   340,     0,   336,   200,   199,   424,
     757,     0,     0,  1059,  1059,  1036,     3,     0,     0,   993,
     995,   631,     0,   639,     0,   629,     2,    49,    41,    39,
      40,     0,    63,   177,    76,     0,     0,     3,   857,   875,
       3,     3,   922,   937,   427,     2,   658,     3,   657,   715,
       0,     0,   848,   906,   956,   964,   601,     0,     0,     0,
    1002,  1003,   815,   602,   999,  1000,   600,   581,     0,     0,
     334,     0,     0,     0,   237,     2,   215,     0,     0,     2,
     246,   261,   272,   266,     2,   184,   301,     0,   276,     0,
       0,   267,   265,   254,   257,     0,     0,   184,   290,     0,
       0,   218,   333,     2,   460,   330,     0,     0,   378,     2,
     338,     0,    66,     0,   350,   762,   764,     0,     0,     0,
     996,   997,   639,  1059,   651,   754,    64,    80,    78,     0,
       0,     0,   460,     0,   849,   907,   815,  1009,  1011,  1004,
       0,   612,   232,   227,   230,     0,   229,   236,   235,     0,
     460,   239,   238,     2,   248,     0,   245,     2,     0,     0,
       0,   253,   258,     0,     0,   184,   302,   277,     0,     0,
       2,     0,   292,   293,   291,   260,   321,     0,   460,   460,
       3,   363,   461,   367,     0,   371,     0,     0,     0,   379,
     380,   221,   341,     0,     2,     0,     2,     2,   635,   637,
     987,  1059,     0,   952,   923,   938,   662,     2,  1005,  1007,
    1008,   618,     0,   234,     0,   233,   217,   240,   460,   391,
     249,     2,   250,   247,   262,   275,   273,   269,   281,   279,
     280,   278,   259,   274,   270,   271,   268,   255,     0,     0,
       0,     0,   220,   240,     3,   356,     0,  1001,   364,   365,
     366,   378,     0,     0,     0,   378,     0,     2,   339,   346,
       0,   343,   345,     0,   636,   460,   228,   231,     2,     3,
     241,   392,   251,     0,     0,     0,     0,   300,   298,   295,
     299,   296,   297,   294,     3,   356,     0,     0,  1002,     0,
       0,     0,   372,     0,   381,   222,     0,   336,     0,   634,
       3,   210,     0,     0,     2,   288,   286,   283,   287,   284,
     285,   282,     0,     0,   357,     0,   384,     0,   382,     0,
     384,   342,   344,     2,     0,     0,   212,   211,   216,     0,
     219,     0,   354,   385,     0,     0,   373,     0,   347,     2,
    1010,   355,     0,     0,     0,     0,   348,   386,   387,     0,
     383,   374,     0,     0,   375,   388
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1726,  6620,  5767, -1726,    -1,   403,  1457,   -26, -1726,  -352,
   -1726,   357, -1726,  -696, -1726,   762,  -857, -1101, -1726,   199,
    6324,  1750, -1726,   471, -1726,  1351,   499,   757,   761,   592,
     760,  1313,  1314,  1320,  1316,  1324, -1726,  -164,  -172,  7946,
     884, -1726,  1644, -1726, -1726,  -664,  7626, -1133,  2208, -1726,
     164, -1726,   887,   -31, -1726, -1726, -1726,   425,    63, -1726,
   -1677, -1671,   288,    40, -1726, -1726, -1726,   297, -1504, -1726,
   -1336, -1726, -1726, -1726, -1726,   -13, -1716,   173, -1726, -1726,
      -9, -1726, -1726, -1726,     7,   451,   453,   120, -1726, -1726,
   -1726, -1726,  -790, -1726,    48,   -20, -1726,   128, -1726,  -148,
   -1726, -1726, -1726,   899,  -821,  -958, -1318, -1726,    29, -1325,
     158,  5980,  -901,  -862, -1726,  -284, -1726,    26, -1726,   -90,
     152,  -233,  -232,  3580,  2350,  -633,    24,    62,   185,   747,
    2953, -1726,  2050, -1726,    66,  3971, -1726,  1989, -1726,    35,
   -1726, -1726,   334,    87,  4391,  2698,   -54,  1843,  -281, -1726,
   -1726, -1726, -1726, -1726,  -260,  5159,  4766, -1726,  -373,   138,
   -1726,  -842,   242, -1726,   174,   733, -1726,   523,   -74, -1726,
   -1726, -1726,  -341,  5637,  -874,  1171,    59,  -575,  -612,   168,
    1154, -1726, -1153,  -151,   -88,  1125,   919,  3099,  -135,  -477,
    -251,  -182,  -460,  1297, -1726,  1625,   473,  1212,  1513, -1726,
   -1726, -1726, -1726,   314,  -168,  -195,  -873, -1726,   -52, -1726,
   -1726, -1100,   463, -1726, -1726, -1726,  2119,  -777,  -356,  -936,
     -19, -1726, -1726, -1726, -1726, -1726, -1726,   160,  -809,   -82,
   -1725,  -193,  8014,   -63,  7029, -1726,  1174, -1726,  4205,   -37,
    -228,  -200,  -181,     1,   -69,   -68,   -62,   -53,    -8,    36,
      57,  -170,   -80,  -122,  -111,   -99,  -710,  -698,  -679,  -670,
    -702,  -117,  -654, -1726, -1726,  -683,  1363,  1364,  1365,   409,
   -1726,   573,  7277, -1726,  -613,  -585,  -545,  -533,  -745, -1726,
   -1556, -1686, -1684, -1676,  -604,  -118,  -288, -1726, -1726,    22,
       3,  -102, -1726,  7890,  1745,  1003,  -470
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1171,   223,   398,   399,    81,    82,   400,   374,   401,
    1478,  1479,   402,   986,   987,   988,  1282,  1283,  1284,  1490,
     424,   404,   405,   406,   693,   694,   407,   408,   409,   410,
     411,   412,   413,   414,   415,   416,   417,   426,  1085,   695,
    1411,   756,   217,   758,   420,   823,  1172,  1173,  1174,  1175,
    1176,  1177,  1178,  2092,  1179,  1180,  1416,  1600,  1934,  1935,
    1864,  1865,  1866,  2059,  2060,  1181,  1614,  1615,  1616,  1768,
    1769,  1182,  1183,  1184,  1185,  1186,  1187,  1424,  1795,  1987,
    1904,  1188,  1189,  1632,  2077,  1633,  1634,  1970,  1190,  1191,
    1192,  1414,  1978,  1979,  1980,  2124,  2139,  2007,  2008,   298,
     299,   885,   886,  1144,    84,    85,    86,    87,    88,    89,
     457,    91,    92,    93,    94,    95,   231,   575,   280,   459,
     428,   460,    98,   308,   100,   101,   154,   339,   340,   105,
     106,   169,   107,   904,   341,   155,   110,   251,   111,   156,
     259,   343,   344,   345,   157,   421,   116,   117,   347,   118,
     566,   874,   872,   873,  1574,   348,   349,   121,   122,  1140,
    1379,  1580,  1581,  1730,  1731,  1380,  1569,  1749,  1582,   123,
     653,  1672,   650,   350,   651,   652,  1245,  1078,   465,   466,
     878,   879,   467,   468,   880,   352,   570,  1196,   430,   431,
     218,   485,   486,   487,   488,   489,   327,  1216,   328,   902,
     900,   600,   329,   368,   330,   331,   432,   125,   175,   176,
     126,  1210,  1211,  1212,  1213,     2,  1127,  1128,   592,  1205,
     127,   318,   319,   261,   272,   549,   128,   221,   129,   232,
    1087,   864,   515,   167,   130,   664,   665,   666,   131,   234,
     235,   236,   237,   313,   133,   134,   135,   136,   137,   138,
     139,   240,   314,   242,   243,   244,   791,   792,   793,   794,
     795,   245,   797,   798,   799,   761,   762,   763,   764,   516,
    1120,  1357,   140,  1680,   625,   626,   627,   628,   629,   630,
    1733,  1734,  1735,  1736,   615,   470,   355,   356,   357,   433,
     209,   142,   143,   144,   359,   815,   631
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   501,   132,    80,   141,   193,   194,   141,   369,   419,
     924,   191,   195,   574,   351,   546,   241,   418,   698,   532,
     995,   812,   200,  1405,   654,   102,  1214,    96,   930,   502,
     207,  1362,  1218,   150,   916,   491,   112,   975,   303,   513,
     179,   704,  1847,   239,  1848,  1058,   263,   354,   503,   365,
     274,  1065,  1849,   857,   859,    80,    80,   633,    80,   504,
     132,   968,   141,   103,  1193,   643,   196,   108,  1247,   646,
     311,  1603,  1603,    80,   917,   337,  1197,  1030,  1602,  1054,
    1903,   501,    80,   102,   206,    96,   918,  1055,   113,  1048,
      80,   521,  1135,  1937,   112,    80,  1636,   238,    80,   452,
     262,  2003,    80,   610,   273,   208,   141,   505,  1049,   502,
     197,   581,   583,  1780,   469,   543,    58,  1050,   506,  1206,
     641,   103,   436,   437,   644,   108,   554,   306,   503,   438,
     507,   198,   266,  1051,  1287,   604,  1936,   207,   270,   504,
      80,  -765,   738,    80,   512,    80,   113,   132,  1638,   141,
     510,    80,   474,    97,   373,   925,   152,   158,    80,    90,
     193,   194,   151,  1294,   529,    80,   499,   195,  1530,  1531,
     102,   208,    96,  1851,   604,  1637,   924,   207,  1492,   944,
     633,   112,  1374,   439,   739,   861,   104,   505,  1942,    80,
      80,   206,  1361,   509,  1079,  1079,   869,   582,   506,  1365,
     514,   207,   916,    58,    80,  1943,   473,   292,   103,   442,
     507,    97,   108,  1079,   482,   551,  1639,    90,   616,    80,
      58,   196,   610,   555,  2011,   582,  1328,   440,    80,    80,
     510,   206,  1081,   113,   561,   204,   145,   896,   567,  1375,
     193,   194,   917,   219,   104,    80,   586,   195,   441,   622,
    1096,  1938,  1251,    80,   918,   206,   699,   255,  2004,  2005,
      58,   267,   848,    80,    80,   197,   647,    80,  1880,   550,
    1434,   830,  1241,   509,    80,   372,   538,  1773,  1376,  1936,
    1079,  1275,  2113,   292,   257,   189,   198,   514,    80,    80,
    1034,    80,   301,  -577,  1929,   889,   294,  1161,    97,   831,
     165,   204,  1058,    58,    90,   816,  -766,  1944,    80,    80,
    1237,   206,   941,  1976,  -797,  1603,    80,  1203,   832,   951,
     911,  1059,  1602,    80,    80,  1062,  2012,  1391,    80,   833,
     633,   104,   112,  1266,  1075,  1076,  2058,   616,   538,  1392,
     522,   993,   796,  1314,   514,  1329,  1301,   159,  1048,   783,
    1847,  1315,  1848,   450,   633,  1363,  1193,   936,   830,   577,
    1849,   633,  2058,   108,  1463,   164,    80,  1049,  1197,    80,
     663,  1903,  1470,  1679,   601,   909,  1050,   834,   602,  1330,
    -393,  1942,   474,   589,   113,   494,   831,   514,   835,  2094,
      63,    64,  1306,    58,    20,  1692,  1694,  1696,   520,   929,
     836,   525,   178,   868,   534,   832,   537,   148,  1374,  1374,
    1374,  -394,   935, -1060,   201,  2039,   833,  1942,   648,    58,
    2002,  1354,   542,   649,   648,   200,   547,  1561,   845,   649,
     436,   437,    58,   553,    80,    58,   473,   438,    76,   180,
    1237,  1885,  1886,  1355,  1387,  1388,   855,   208,   469,    97,
    1603,  -393,   860,   548,  1330,   474,  1497,    80,    80,   173,
     173,   916,   578,   181,   834,  1375,  1375,  1375,   537,    80,
      80,   844,   354,   774,   883,   835,   293,   514,    80,  1771,
     482,  1851,  -394,  1079,  1779,   212,  1456,   836,  1498,   160,
     908,   439,   161,   162,   173,   163,   189,   219,    80,  1035,
     337,   917,   597,   514,  1376,  1376,  1376,    80,   845,   473,
    1132,  1384,  1056,   918,   616,  1063,   620,   436,   437,   620,
    1339,   469,  1929,   257,   438,  1861,  1862,    80,    -3,    58,
    1385,   598,   599,    80,   950,   440,   463,   953,   954,   226,
     955,  1953,  1954,   293,   173,   705,    58,   173,   265,   957,
     706,   844,   959,   960,   961,   697,   441,  1530,  1531,   442,
    1389,   514,   173,  1296,    58,  1522,   220,   288,  -797,   363,
    1861,  1862,    80,   895,    80,  1601,  1617,   204,    14,    15,
      16,    17,    18,  1386,   970,    80,   963,    80,  1244,   473,
     293,    80,  1586,   132,  1950,   141,   633,   964,   965,   707,
    1320,    80,    58,   782,   708,    80,    80,  1592,  1863,  1106,
    1384,  1587,   270,   514,   112,  1603,   102,    58,    96,   728,
     729,   930,  1222,   173,    58,  1501,  1110,   112,  1069,  1649,
     514,   633,  1089,  1738,  1691,   246,   418,    58,    80,  1879,
    1586,   577,    58,  1603,  1342,   108,   257,  1084,   514,   524,
      58,    80,  1739,  1890,   103,   469,    58,   293,   108,  1741,
    1984,  1747,  -930,   730,   731,   970,   113,   173,   173,  -930,
     613,   913,   796,   636,  1594,  1959,  1423,  1221,   173,   113,
    1748,   852,  1346,  1603,   851,  -454,   514,   613,   561,   854,
    1591,   613,   564,   173,  1985,   569,   469,  1454,   288,  -458,
    1742,   620,   970,   863,  1507,    80,   862,    80,   514,    80,
     867,    58,   214,    80,   871,  1019,    80,   870,   469,   469,
     173,  1781,  1516,   215,  1727,   152,   514,   173,   173,  1740,
    1520,    97,   173,  1747,   620,    73,  1997,   469,  1442,   216,
     514,    80,  1952,  1625,    97,  1098,  1763,  1764,  1765,  -628,
      90,    73,  1846,  1852,  1965,   619,  -628,  1747,   894,   620,
    1774,   257,   970,   290,  1114,  1775,    78,   621,  1766,  1996,
     173,   759,  1853,   173,   740,   514,  1856,   104,   741,   622,
    -455,  1948,    78,    79,  1803,  1804,    80,   292,    80,   613,
      14,    15,    16,    17,    18,    14,    15,    16,    17,    18,
      80,  1763,  1764,  1765,   469,   970,   293,    80,   970,   187,
      73,  1487,  -393,   482,   307,  2029,    80,  1240,  2031,  1601,
     548,  1115,  2022,  1766,  1689,    80,    80,    80,  1290,  -786,
     619,  1327,  1767,   697,   620,  1286,  1825,   765,  1826,   697,
     655,    78,    79,   657,   354,    80,   913,   970,   697,    58,
     674,   276,  1291,   776,    58,   277,   779,  2064,   281,   970,
     286,   173,  1681,   247,   248,   367,   249,   697,   325,  2066,
     463,   250,   337,   173,   173,   718,   719,   370,  1446,  1447,
    1897,  2044,    80,    80,   482,  1898,  2045,   189,   141,   371,
      14,    15,    16,    17,    18,  -931,   718,   721,   372,    73,
     189,   141,  -931,  1270,   722,   723,   443,  -691,  1334,   102,
    1271,    96,   444,   524,    14,    15,    16,    17,    18,   619,
     112,  1352,  1489,   620,   463,   472,   718,  1207,  1084,  1286,
      78,   621,   445,    80,   160,  1313,   796,   161,   162,  1913,
     163,   446,   613,   463,  1622,  2109,   663,  1195,   633,    58,
    2110,   108,   199,    64,  1617,    14,    15,    16,    17,    18,
     709,   447,   710,   711,   712,    73,   613,  1627,  1628,  1629,
    1630,  1631,   113,    58,  1080,  1080,   148,   817,   818,   613,
     448,   819,    80,   766,   292,  1728,   442,   767,   514,   514,
      80,   713,   929,  1080,   714,   715,    78,    79,  1568,   716,
     717,   476,  1458,   490,   276,   173,   477,   469,  1676,   905,
     907,  -459,   492,   522,    58,   840,  1121,   514,   495,    80,
     778,   589,   482,   442,   514,   514,  1687,  1129,  1134,   496,
    1056,  1133,   442,    73,   620,  1136,   882,    97,  1991,   497,
     883,   498,   933,    90,   511,    80,   605,   288,   369,   369,
    1209,    80,    80,   619,   943,   173,  1208,   620,   602,   945,
    1080,   512,   257,   602,    78,   621,   530,   946,   560,    64,
     104,   947,  1469,   548,    73,   969,   531,   463,   571,   970,
     418,  1039,    80,   724,   725,   514,   541,   276,   277,  1093,
     637,   292,   286,  1094,   759,   514,   522,   257,   514,  1123,
     514,  1550,   219,   970,  1125,    78,    79,   616,   970,   979,
     589,   981,  1604,   984,   514,   726,   727,   992,   463,  2009,
     996,  1285,  1441,  1480,  1673,  1286,   767,  1435,  1502,   682,
     894,  1474,   552,  1684,   141,  1286,   354,  1685,   593,  1756,
    1757,   263,   274,  1286,   970,  1021,  1783,  2009,   608,   482,
     970,   141,    80,    80,    80,  1784,   640,  1538,  1539,  1094,
    1207,   649,  1381,  1475,   337,   418,   418,   668,   765,   765,
     667,  1532,   102,  1785,    96,  1857,   482,   970,  1037,   767,
     141,  1040,    80,   112,  1945,  2048,   656,  2061,   970,  1286,
      80,   997,   671,    80,    80,   262,   273,    80,   141,   732,
     733,   102,   672,    96,   173,  -456,  1983,   720,    80,  2111,
    1195,   173,   112,   970,   108,    14,    15,    16,    17,    18,
     998,   999,  1000,   673,   266,   682,  1763,  1764,  1765,  1097,
     270,  1099,  2135,   469,   469,   113,  2132,   677,   970,  1195,
     701,    80,   524,   108,   613,  2142,  1108,   636,  1766,  2143,
    1112,  2079,   201,   701,    80,  2083,   734,  1772,   735,  1763,
    1764,  1765,   736,  1080,   113,    14,    15,    16,    17,    18,
     482,   970,   737,    19,    58,  1366,  1367,  1368,    80,   608,
     701,  1766,   742,  1209,   972,   973,  1143,   173,   173,  1208,
    -185,   768,  1377,  1071,  1072,   276,   463,  1585,  1364,   769,
      97,   770,    19,  1724,  1725,  1726,    90,  1073,  1074,   449,
      80,  1390,  1273,  1094,  1288,  1289,    52,    53,    54,    55,
    1005,  1006,  1007,  1008,    58,   970,  1292,   771,  1409,    97,
     354,  1812,   772,   104,   773,    90,    -3,  1239,  -156,  -156,
    1584,    48,    49,    50,    51,    52,    53,    54,    55,   501,
    1073,  1433,   255,   267,  1495,  1496,  1604,   800,   337,   147,
    1500,  1496,   104,  -457,    65,    66,    67,    68,    69,    70,
      71,   990,   141,   551,  1504,  1496,    80,   502,   -17,   257,
      80,  1045,  1488,    80,    73,  1540,  1488,   824,  1381,  1381,
    1381,  1045,  1552,  1570,  1381,   813,   503,   814,   150,   141,
     141,  1697,  1094,   482,  1728,  1823,  1094,   504,   514,  1901,
    1902,   991,  1861,  1862,   548,    78,    79,  1420,  1824,  1496,
     102,   102,   894,   482,   837,    80,   838,   550,   839,  1260,
     841,   112,   112,   842,  1264,  1834,  1835,   847,   765,  1844,
     970,  2132,  2133,   147,   843,  1272,   171,   172,    65,    66,
      67,    68,    69,    70,    71,   505,  1917,  1496,  1606,  1606,
    1918,  1496,   108,   108,   147,   849,   506,   482,   300,    65,
      66,    67,    68,    69,    70,    71,   141,   865,   507,  1493,
    1494,  1001,  1002,   113,   113,   482,  1585,  1003,  1004,  1529,
      80,  1585,  1009,  1010,   866,    80,    80,    80,  -575,   510,
    1532,  1754,  1207,  -121,  -121,  -121,  -121,  -121,  -121,  1344,
    1750,  1750,  1348,  -573,  1743,   830,  1421,   875,  1377,  1377,
    1377,   152,  1566,  1567,  1571,  1674,  1675,   151,   884,  1584,
    1443,  1444,   897,   354,  1584,  -120,  -120,  -120,  -120,  -120,
    -120,   899,   509,   831,   903,   919,   921,   622,    97,    97,
    1480,  1593,  1595,   173,    90,    90,   173,   173,   173,   613,
    1532,   337,   832,   938,   942,    80,   948,   949,   977,   971,
      80,   974,  1018,   833,  1052,  1044,    80,  1045,    80,  1023,
     173,   104,   104,  1091,   547,    80,   173,  1100,   463,  1647,
    1101,  1102,  1103,  1104,   569,  1105,  1124,   482,  1126,  -769,
    1130,   173,    14,    15,    16,    17,    18,   967,  1198,   141,
     482,   548,  1137,  1792,  1138,  1139,   894,  -667,  1231,   674,
    1199,   834,   469,   469,  1232,  1209,  1215,  1233,  1243,  1244,
    1253,  1208,   835,  1249,  1246,   266,  1248,  1252,  1255,  1256,
    1257,   270,  1258,   141,   836,   173,  1259,   482,    14,    15,
      16,    17,    18,  1265,   939,  1261,  1262,   141,  1476,  1263,
    1268,  1971,  1269,  1293,   102,  1333,   845,  1276,  1277,  1298,
    1299,  1300,   483,  1307,  1308,   112,  1341,  1309,  1310,  1358,
    -655,  -654,  1318,  1207,  1353,  -770,  1382,  1383,  1393,  1933,
    1413,   633,    80,   881,    80,  1396,   718,   418,  1397,  1406,
    1407,  1408,  1606,  -690,  1415,  1423,   108,  1509,   970,   844,
    1417,  1427,  1429,  1472,  1430,  1431,  1518,  1437,  1439,  1486,
    1906,  1488,  1503,  1515,  1528,  1533,  1534,   113,  1535,  1536,
    1496,  1541,  1481,  1482,  1483,  1544,  1971,  1557,  1558,  1484,
    1485,  1559,    80,  1562,  1597,    80,  1573,  1386,  1640,  1642,
    1575,    83,  1645,  1576,   149,   482,  1618,  1650,  1652,   482,
    1619,  1653,  1621,   255,   267,  1896,  1655,  1656,  1623,  1657,
    1635,  1532,  1658,   482,  1643,  1659,  1644,  1660,   141,  1585,
    1661,  1662,   463,   482,   173,  1664,   658,   173,  1678,  1669,
     257,  1690,    97,  1682,  1699,  1786,  1698,  1683,    90,   102,
      80,    80,  1686,  1703,  1704,   442,  1209,  1714,   501,    83,
     112,  1712,  1208,  1540,  1722,  1088,  1723,  1578,  1737,  1758,
    1760,  1789,  1584,  1791,   190,   104,   220,   173,   211,  1796,
    2056,  1805,  1933,    83,   469,  1809,   502,  1606,   418,  1810,
     418,   108,  1811,  1813,  1815,  1839,   230,  1822,  1817,   254,
    1869,  1828,  1829,    83,  1832,   503,  1833,  1842,    80,  1843,
    1874,   659,   113,  1875,   482,  1889,   504,  1887,   482,  1893,
    1899,  2081,  1977,   482,  1900,  1910,  1895,   660,  1837,   418,
     661,   662,    65,    66,    67,    68,    69,    70,    71,  2038,
     149,  1161,  1911,  1912,   211,  1914,    83,  1915,   482,   149,
     147,  -656,   310,   316,  1916,    65,    66,    67,    68,    69,
      70,    71,  2104,  1924,   505,   336,  1973,  1925,  1926,  1927,
     547,  1928,   514,  1947,  1949,   506,  -558,    97,  1955,  1958,
    1960,  1966,  1975,    90,   434,  1974,   810,   507,   483,   425,
     190,   190,   482,   141,  1988,  1989,   482,   548,  1311,    75,
    1835,   149,   455,  2134,  1990,   254,  1993,   510,  1994,   482,
     104,   418,  1995,  2006,   102,  2028,  2015,   193,   194,  2032,
      80,   141,    80,   586,   195,   112,  1732,  2030,   881,   230,
     230,  2041,  2042,   482,  2043,   482,   482,  2046,  2047,   173,
    2050,  1973,   102,  2054,  2063,  2065,   310,  2067,   845,   539,
     509,   173,  1606,   112,    83,  2076,   108,  1122,  2080,  2087,
     482,   141,  2082,  2088,   173,  1977,  2089,  2095,   254,  1977,
    1977,  2105,  2106,  2108,  2118,  2120,  2121,   113,  2125,  2126,
    1606,  2129,   102,  2130,   108,    80,    80,  2140,   206,  2141,
     881,   844,  1819,   112,   966,  2144,   482,  1011,  1499,  1012,
    1412,   173,   316,  1014,  2107,   113,  1013,   482,   316,   310,
     310,   539,  1015,  2119,  1419,  1793,   149,  2057,   757,  1230,
    1606,  1891,  1884,  2074,   108,  2114,  1986,    80,  2112,   473,
    2123,   618,  2103,  1787,  2123,  1788,   336,   623,   632,  2034,
    2127,   482,    97,   482,  2084,   113,  2033,  1428,    90,   170,
     283,  1931,   540,   336,  2001,  1572,  2137,   336,  1751,  1242,
    1425,  1090,   482,   820,  1217,   901,  1794,  1800,   482,     3,
      97,  1250,  1026,  1027,  1028,   104,    90,  1721,   482,     0,
       0,     0,    80,   246,     0,     0,     0,  1732,  1732,     0,
       0,   425,    80,     0,     0,     0,     0,     0,     0,     0,
     881,     0,     0,   104,     0,     0,     0,     0,     0,   173,
      97,     0,     0,   173,     0,     0,    90,   881,   881,     0,
     211,     0,     0,     0,     0,   425,     0,   173,   760,  1297,
       0,     0,     0,     0,     0,   190,     0,   173,     0,     0,
       0,     0,     0,   104,     0,     0,  1304,  1305,     0,     0,
     618,   149,     0,     0,   173,   455,     0,     0,     0,   789,
       0,   632,     0,     0,    14,    15,    16,    17,    18,     0,
       0,  1445,     0,     0,     0,     0,     0,     0,   613,     0,
       0,     0,     0,     0,   147,     0,     0,   227,   228,    65,
      66,    67,    68,    69,    70,    71,   147,     0,     0,   230,
    1471,    65,    66,    67,    68,    69,    70,    71,   230,  1732,
       0,     0,    73,     0,     0,     0,     0,     0,   173,  2122,
       0,   483,   173,    58,   810,   434,   434,   173,   310,     0,
     425,   425,  1577,    75,   310,  2131,   336,     0,     0,  1578,
       0,     0,     0,    78,    79,    75,     0,     0,   809,  1505,
       0,     0,   173,   613,     0,   147,     0,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,   310,     0,     0,     0,     0,
       0,     0,     0,    73,     0,  1999,   310,     0,   310,  1732,
     336,     0,    83,     0,     0,     0,   173,     0,     0,     0,
     173,     0,     0,   787,    75,     0,     0,   620,   336,   455,
       0,   632,     0,   173,    78,   788,     0,     0,     0,   623,
       0,     0,  1982,   623,     0,     0,  2040,   931,     0,     0,
    1732,     0,   336,     0,     0,     0,     0,   173,     0,   173,
     173,     0,   632,     0,     0,   336,     0,     0,     0,     0,
       0,   434,     0,     0,     0,     0,   149,     0,     0,     0,
       0,     0,   186,     0,   173,     0,     0,     0,     0,   425,
       0,     0,   149,   149,     0,   425,     0,   881,   881,    14,
      15,    16,    17,    18,   425,     0,     0,   149,   149,   149,
    1281,   881,   881,     0,     0,  1732,  1732,     0,  1281,   256,
     173,     0,     0,     0,     0,     0,  1511,  1512,     0,     0,
     278,   173,   285,     0,   287,     0,  1671,     0,     0,     0,
    1526,  1527,     0,  1677,     0,   881,   881,  1281,     0,     0,
     483,     0,     0,     0,     0,     0,     0,  1732,    58,     0,
    1688,     0,     0,   455,     0,   173,     0,   173,     0,     0,
       0,     0,     0,   256,  1548,  1549,   285,   287,     0,   760,
     760,     0,   434,     0,     0,     0,   173,   425,     0,     0,
     147,     0,   173,   227,   228,    65,    66,    67,    68,    69,
      70,    71,   173,     0,   455,     0,  2138,   789,     0,   789,
    1281,     0,   320,   321,   322,   323,  2145,     0,    73,     0,
       0,     0,     0,     0,     0,   256,   336,   336,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,  2036,    75,
       0,     0,   514,     0,     0,   336,     0,   310,     0,    78,
      79,   147,     0,     0,   171,   172,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,   310,   147,     0,     0,
     227,   228,    65,    66,    67,    68,    69,    70,    71,   182,
       6,     7,     8,     9,    10,    11,    12,    13,   256,     0,
     285,   287,     0,     0,     0,  1782,     0,     0,     0,     0,
       0,     0,     0,   324,     0,   425,     0,     0,     0,     0,
       0,     0,   336,     0,   434,  1311,    75,     0,   149,   425,
       0,   325,   256,     0,     0,     0,     0,     0,   256,     0,
     336,     0,  1225,     0,     0,    14,    15,    16,    17,    18,
     275,   881,   881,   623,     0,   147,     0,     0,     0,  1814,
      65,    66,    67,    68,    69,    70,    71,   982,   256,     0,
    1821,     0,     0,     0,   638,     0,   287,     0,     0,   115,
    1716,  1717,   115,     0,     0,     0,     0,     0,    58,     0,
       0,     0,   455,     0,  1755,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    58,     0,     0,   983,   483,     0,
       0,     0,   681,     0,     0,     0,  1281,     0,     0,     0,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,   147,   115,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   760,
       0,   115,     0,     0,    73,   256,     0,     0,  2036,    75,
     881,     0,   514,     0,     0,     0,   789,   260,     0,    78,
      79,   115,     0,   789,   229,    75,  1907,  1908,     0,     0,
       0,   256,     0,   638,   287,    78,    79,     0,     0,  1806,
       0,     0,     0,     0,     0,   881,     0,     0,   681,     0,
     881,   881,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,   115,   336,     0,   115,     0,     0,
       0,   260,     0,     0,  1827,     0,     0,     0,   256,  1830,
    1831,   483,   332,   115,   364,     0,   147,     0,     0,   171,
     172,    65,    66,    67,    68,    69,    70,    71,   256,     0,
       0,   149,     0,   256,     0,   256,     0,   429,     0,     0,
     149,     0,     0,     0,     0,     0,     0,     0,   425,   115,
     429,     0,     0,   260,     0,     0,   256,     0,   256,   256,
       0,     0,     0,     0,     0,     0,  1992,     0,     0,     0,
       0,     0,     0,     0,     0,   483,   256,   425,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,   256,  1398,
       0,    58,     0,   483,   254,    83,     0,     0,  1281,     0,
     115,     0,   115,  1281,  1281,  1281,     0,     0,     0,   310,
     434,   256,     0,   638,   287,   149,   260,     0,     0,     0,
     931,     0,     0,   147,     0,   455,   227,   228,    65,    66,
      67,    68,    69,    70,    71,   256,   638,   565,    58,     0,
       0,     0,   256,     0,  2053,   115,     0,     0,     0,     0,
     260,    73,     0,     0,   455,     0,   260,     0,   149,     0,
       0,     0,     0,     0,   115,   188,     0,     0,     0,     0,
     147,   229,    75,   227,   228,    65,    66,    67,    68,    69,
      70,    71,    78,    79,   115,     0,   260,   115,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,    73,     0,
       0,   115,   258,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,   279,   282,     0,     0,     0,   309,    75,
       0,   336,   336,     0,     0,     0,   147,     0,     0,    78,
      79,    65,    66,    67,    68,    69,    70,    71,  1278,   429,
       0,     0,  1279,     0,  1280,     0,     0,    58,     0,     0,
       0,     0,     0,     0,   147,     0,   258,   171,   172,    65,
      66,    67,    68,    69,    70,    71,   149,   149,   149,   149,
       0,   149,   149,   429,     0,    75,     0,  1579,   316,   147,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,   425,   425,     0,   115,
    1281,     0,  1281,   429,     0,   174,   177,    73,   258,   260,
       0,     0,   147,  1402,     0,   171,   172,    65,    66,    67,
      68,    69,    70,    71,     0,     0,   254,  1577,    75,     0,
     881,     0,     0,     0,     0,     0,     0,     0,    78,    79,
     222,     0,     0,     0,     0,     0,   147,     0,   455,   360,
     361,    65,    66,    67,    68,    69,    70,    71,   147,  2090,
       0,   227,   228,    65,    66,    67,    68,    69,    70,    71,
       0,   258,     0,   149,     0,   623,     0,   256,   429,   429,
       0,     0,     0,   260,   115,  1400,    73,     0,   256,     0,
     304,     0,     0,   305,     0,     0,     0,    76,     0,     0,
       0,     0,   362,     0,     0,   258,   787,    75,   326,     0,
     620,   258,   256,     0,     0,     0,   115,    78,   788,     0,
       0,   115,     0,   256,   260,   115,     0,   115,     0,     0,
     622,     0,   256,     0,     0,     0,     0,     0,   115,     0,
     115,   258,   147,     0,     0,   171,   172,    65,    66,    67,
      68,    69,    70,    71,   364,     0,   115,   429,     0,   260,
       0,     0,     0,     0,     0,     0,  1579,  1729,   434,   493,
       0,  1579,     0,   425,     0,     0,     0,  1579,     0,  1579,
     115,     0,     0,   260,     0,   683,     0,   565,     0,     0,
     260,     0,     0,   115,     0,   937,     0,     0,   595,     0,
       0,     0,     0,     0,   115,   316,   149,     0,  1395,     0,
       0,     0,     0,   544,   545,     0,     0,   429,     0,     0,
     115,   115,     0,   429,   174,     0,     0,     0,     0,   256,
       0,     0,   429,     0,     0,   115,   115,   115,   147,   174,
     425,   227,   228,    65,    66,    67,    68,    69,    70,    71,
       0,   336,     0,   256,   149,     0,     0,     0,     0,     0,
      58,     0,     0,     0,   258,     0,   591,     0,     0,     0,
       0,     0,     0,   594,   596,     0,     0,     0,   603,     0,
       0,   683,     0,     0,     0,   149,     0,     0,     0,     0,
       0,   429,   147,     0,   906,   227,   228,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
     336,   336,     0,     0,     0,   429,   326,     0,     0,   326,
      73,     0,     0,     0,     0,     0,     0,     0,  1729,  1729,
       0,     0,   429,     0,     0,     0,     0,     0,   258,     0,
     309,    75,     0,  1579,     0,     0,  1579,     0,     0,     0,
       0,    78,    79,     0,   115,   115,     0,     0,     0,   258,
     147,     0,   316,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,   115,     0,   425,     0,     0,   147,   258,
       0,   199,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,   256,     0,     0,     0,  1119,     0,
       0,   115,   310,     0,     0,     0,     0,   222,     0,     0,
       0,     0,     0,     0,   258,     0,  1238,     0,     0,   804,
     805,     0,     0,     0,     0,   260,     0,    75,     0,   256,
     809,    99,     0,   429,   153,   256,   260,    58,   258,     0,
     115,     0,     0,     0,     0,   258,   115,   429,     0,     0,
    1729,     0,     0,     0,     0,     0,     0,     0,   115,  1579,
    1227,   429,     0,   115,    14,    15,    16,    17,    18,   147,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,  1646,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,     0,   149,   147,     0,    73,   171,   172,
      65,    66,    67,    68,    69,    70,    71,    58,     0,     0,
     429,     0,     0,   205,     0,     0,     0,  1577,    75,     0,
       0,     0,   336,    58,     0,     0,     0,     0,    78,    79,
    1729,     0,     0,   268,     0,     0,     0,     0,     0,   147,
     149,     0,     0,   472,    65,    66,    67,    68,    69,    70,
      71,   326,     0,     0,     0,   147,     0,     0,     0,     0,
      65,    66,    67,    68,    69,    70,    71,    73,   149,   149,
     302,  2037,   316,   115,     0,     0,    99,     0,   256,     0,
       0,     0,     0,    73,     0,     0,     0,    74,    75,     0,
     115,   115,     0,     0,     0,   338,     0,     0,    78,    79,
       0,   940,     0,    74,    75,     0,     0,     0,   149,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
     435,     0,     0,     0,     0,     0,   256,     0,     0,     0,
       0,   302,   461,     0,     0,     0,  2037,  2037,     0,     0,
       0,   147,     0,   115,   560,    64,    65,    66,    67,    68,
      69,    70,    71,  1118,     0,     0,  1759,     0,     0,     0,
     508,     0,     0,     0,     0,     0,     0,     0,     0,  1770,
       0,     0,     0,     0,     0,     0,   528,     0,  2037,   115,
     258,   533,   535,     0,   205,     0,     0,     0,   115,     0,
       0,   258,   147,  1020,     0,     0,   429,    65,    66,    67,
      68,    69,    70,    71,  1278,     0,  1798,   556,  1279,     0,
    1280,   558,     0,     0,     0,   258,   559,     0,     0,     0,
       0,     0,     0,     0,     0,   429,   147,   576,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     588,    75,   260,   115,  1491,     0,     0,     0,     0,     0,
    1070,     0,     0,     0,    73,     0,     0,  1082,     0,     0,
       0,     0,     0,   115,     0,     0,   611,     0,     0,   635,
       0,     0,     0,   429,  1577,    75,     0,  1227,     0,     0,
       0,  1578,     0,   642,     0,    78,    79,   642,   147,  1466,
       0,   227,   228,    65,    66,    67,    68,    69,    70,    71,
       0,   115,   429,     0,     0,   256,   115,     0,     0,     0,
       0,     0,     0,     0,  1860,     0,    73,     0,  1870,     0,
     147,     0,   109,   562,   563,    65,    66,    67,    68,    69,
      70,    71,  1883,  1145,     0,     0,  2036,    75,     0,     0,
     514,     0,  1892,     0,     0,     0,     0,    78,    79,   147,
     115,   115,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,   115,   115,     0,     0,     0,   115,
     115,    76,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   302,     0,   147,     0,   611,   171,   172,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,   115,   115,
      76,     0,     0,     0,     0,     0,  1556,     0,     0,     0,
       0,     0,     0,     0,   115,   115,   115,   115,   115,   115,
     115,     0,     0,  1941,   269,     0,   260,  1946,     0,     0,
     147,   476,  1951,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,   147,   429,   429,   256,     0,    65,    66,
      67,    68,    69,    70,    71,  1278,     0,  1981,    73,  1279,
       0,  1280,   256,     0,     0,     0,   461,   109,     0,     0,
       0,     0,     0,     0,   260,     0,     0,     0,   229,    75,
       0,     0,     0,     0,     0,     0,   342,     0,     0,    78,
      79,     0,    75,     0,     0,  1693,   429,   258,   877,     0,
       0,  2010,     0,   535,     0,  2013,   147,   888,     0,   576,
       0,    65,    66,    67,    68,    69,    70,    71,  2027,   147,
     338,   115,    99,   462,    65,    66,    67,    68,    69,    70,
      71,  1278,   258,     0,    73,  1279,     0,  1280,   642,   912,
       0,     0,  2049,     0,  2051,  2052,     0,     0,     0,     0,
     256,     0,     0,   923,  1046,    75,     0,     0,   620,     0,
       0,     0,   611,     0,     0,    78,    79,   932,    75,  2062,
       0,  1695,     0,     0,     0,   642,     0,     0,     0,   147,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,   115,   115,     0,     0,   557,     0,
       0,     0,  1399,  1401,  1403,  2085,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,  2091,     0,   109,     0,
       0,   429,     0,     0,     0,     0,     0,   309,    75,   192,
       0,     0,  1422,     0,     0,     0,     0,   115,    78,    79,
       0,     0,     0,     0,     0,     0,     0,  1145,     0,     0,
    2117,   233,  2091,   260,   115,     0,     0,   612,     0,     0,
     269,  1555,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2128,   256,   461,   612,     0,     0,  2117,   612,     0,
       0,   258,     0,     0,     0,     0,     0,  2136,   429,     0,
    1029,  1467,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,   115,     0,     0,     0,     0,   312,     0,     0,
       0,     0,     0,   115,   912,     0,     0,   147,     0,  1053,
     171,   172,    65,    66,    67,    68,    69,    70,    71,   258,
       0,     0,     0,   115,     0,     0,   461,   461,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,   115,   115,   461,     0,     0,   115,   115,
       0,   744,   745,   746,   747,   748,   749,   750,   751,   752,
     753,   754,     0,     0,     0,   214,   612,     0,   256,     0,
       0,     0,     0,   877,   500,   233,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,   312,   755,     0,     0,     0,     0,     0,     0,     0,
     260,     0,     0,     0,     0,  1194,     0,     0,     0,     0,
       0,     0,   461,   429,     0,     0,     0,     0,   153,     0,
    1588,     0,     0,  1590,     0,     0,     0,     0,     0,     0,
     642,     0,     0,  1229,   271,   877,     0,     0,     0,     0,
    1235,     0,     0,     0,     0,     0,     0,   462,     0,     0,
       0,     0,     0,     0,   587,   312,   381,     0,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,   114,     0,   342,
       0,     0,   338,     0,     0,     0,     0,     0,   269,     0,
     109,     0,     0,     0,     0,     0,   346,     0,   258,     0,
       0,   462,     0,   109,     0,     0,   703,     0,     0,    76,
     392,     0,     0,     0,     0,     0,     0,     0,     0,   612,
     462,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,   464,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   612,     0,   877,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   612,     0,     0,     0,
     115,     0,   877,   877,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   790,     0,   115,   115,     0,     0,
     260,     0,     0,     0,     0,   461,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1752,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   829,     0,   115,     0,     0,     0,
       0,   153,     0,   233,   462,   258,     0,     0,     0,     0,
    1378,     0,     0,     0,     0,     0,     0,   614,  1194,     0,
     271,     0,     0,   312,     0,     0,     0,     0,     0,   312,
       0,     0,     0,     0,   614,     0,     0,     0,   614,     0,
       0,     0,     0,   115,     0,   462,     0,  1194,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     120,     0,     0,     0,     0,  1426,     0,   342,   342,     0,
     312,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   893,     0,   312,     0,     0,   342,     0,     0,     0,
       0,     0,     0,     0,     0,   611,     0,     0,     0,     0,
       0,     0,     0,     0,   533,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   342,   120,     0,     0,     0,     0,
       0,     0,     0,   877,   338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   614,     0,     0,   120,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,   342,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,   284,     0,     0,
       0,   612,   877,   877,   269,     0,   342,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   877,   877,     0,     0,
    1905,   461,   461,     0,     0,     0,   120,     0,     0,     0,
       0,     0,   120,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   258,     0,   464,     0,     0,
     877,   877,     0,   462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1378,  1378,  1378,   153,
     535,     0,     0,     0,     0,   120,     0,     0,     0,   346,
       0,     0,     0,     0,     0,     0,     0,   120,   271,     0,
     114,     0,     0,     0,     0,     0,  1605,  1605,  1932,     0,
       0,   464,     0,   114,     0,     0,     0,     0,     0,     0,
       0,     0,  1047,     0,   790,     0,   342,     0,     0,   614,
     464,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   342,   342,     0,     0,     0,   120,     0,
     120,     0,     0,   614,     0,   120,   375,     0,   338,   376,
       0,   377,   312,   378,     0,     0,   614,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,   312,     0,   153,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,   342,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,   120,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,   877,   877,     0,   391,
       0,     0,    76,   392,   464,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,  1746,
     119,     0,     0,   119,     0,     0,   269,     0,     0,   877,
       0,     0,     0,     0,     0,   464,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1762,     0,     0,     0,
       0,   120,     0,     0,     0,     0,   612,   346,   346,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   346,   120,   119,     0,
    1605,     0,     0,     0,   342,   462,     0,     0,     0,     0,
       0,   338,     0,     0,   153,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   346,   877,     0,     0,     0,     0,
       0,  1047,     0,     0,     0,     0,     0,  1312,   790,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   342,   342,     0,   114,     0,     0,     0,
     877,     0,     0,   346,     0,   877,   877,   342,   342,     0,
     461,   461,   342,   342,     0,     0,   120,   120,     0,   119,
       0,   614,     0,     0,   271,   119,   346,     0,   119,  1850,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   342,   342,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   120,     0,   120,     0,     0,   119,     0,
       0,     0,     0,   464,     0,  1605,     0,     0,   120,     0,
     119,     0,     0,     0,     0,     0,     0,   109,   109,     0,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -461,  -461,     0,  -461,    46,     0,
      47,   119,  -461,   119,   312,     0,   346,     0,   119,   462,
       0,     0,   120,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,   346,   346,   120,     0,     0,   120,   120,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,   120,   120,   120,   119,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,  1972,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   346,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   461,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,     0,  1523,   342,   342,     0,
    1605,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,     0,     0,     0,  1605,  1972,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     342,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,   269,     0,     0,
       0,     0,  1583,     0,   119,     0,   271,     0,  1605,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   109,     0,     0,     0,     0,   614,  2078,     0,     0,
       0,     0,   342,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   877,   342,     0,   124,     0,
       0,   124,     0,     0,   346,   464,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   120,     0,     0,     0,     0,
       0,   342,     0,     0,     0,     0,   342,   342,     0,     0,
       0,   342,   342,     0,     0,     0,     0,     0,     0,   119,
     119,     0,     0,   346,   346,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   346,   346,     0,
       0,     0,   346,   346,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,   119,     0,   119,     0,
     124,   346,   346,     0,     0,     0,   109,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1583,     0,     0,     0,     0,  1583,   124,     0,     0,
       0,     0,  1744,   124,  1583,     0,   124,   114,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,   119,     0,
       0,   119,   119,     0,   119,     0,     0,     0,   124,   464,
       0,     0,     0,   119,     0,     0,   119,   119,   119,     0,
       0,     0,     0,     0,   213,     0,     0,     0,     0,     0,
     224,   225,     0,     0,     0,   612,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,   124,     0,   342,   291,     0,   124,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,   109,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,   346,   346,   109,
     612,     0,     0,   120,     0,     0,     0,     0,  1858,     0,
       0,  1583,     0,   124,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     346,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   271,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   312,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   342,     0,   124,     0,
       0,   114,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   346,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   346,   119,   119,     0,
       0,     0,   124,   203,  1583,     0,     0,     0,     0,     0,
       0,     0,     0,   584,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,   346,     0,     0,     0,     0,   346,   346,     0,     0,
       0,   346,   346,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   203,
       0,     0,   120,   120,   120,   120,   120,   120,   120,     0,
       0,     0,     0,     0,     0,   203,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,   120,     0,     0,   114,   124,   124,   203,
       0,     0,     0,     0,     0,     0,   312,     0,     0,     0,
       0,     0,   458,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,   124,     0,   124,     0,     0,     0,
       0,   785,     0,   786,     0,     0,     0,     0,     0,   124,
       0,     0,   802,   803,   203,     0,     0,     0,     0,   120,
       0,   587,   312,     0,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -460,  -460,
       0,  -460,    46,   312,    47,   614,  -460,     0,     0,     0,
     119,     0,     0,   124,     0,     0,   203,     0,     0,   119,
       0,     0,     0,    58,     0,     0,   124,   119,     0,   124,
     124,     0,   124,   346,     0,     0,   203,     0,     0,     0,
       0,   124,     0,     0,   124,   124,   124,     0,     0,     0,
       0,   114,     0,     0,     0,     0,   119,     0,     0,   120,
       0,   887,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,   114,
     614,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,  2116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
    1394,     0,     0,     0,   124,   203,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,   376,   203,   377,     0,   378,     0,
       0,     0,     0,     0,     0,     0,   346,     0,     0,     0,
       0,   120,     0,  1147,     0,   379,    -2,     0,  1149,  -242,
    -242,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  -336,     0,  1162,  1163,  1164,  1165,  1166,
       0,  1167,     0,   380,   381,     0,   478,     0,   383,  1168,
    1169,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1170,   386,   387,   388,   403,   389,   390,     0,     0,
     203,   203,     0,     0,    73,     0,   458,     0,     0,     0,
       0,     0,   124,     0,     0,   119,   119,   119,   119,   119,
     119,   119,     0,  -242,   391,   124,   124,    76,   392,     0,
       0,   120,   293,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,   119,   119,     0,     0,  -184,
       0,     0,     0,     0,     0,     0,     0,  1068,     0,     0,
     203,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   203,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,   146,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,   203,     0,     0,     0,
       0,     0,     0,     0,     0,  1141,  1142,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1200,  1201,  1202,     0,
       0,  1204,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,   202,     0,   670,
      46,     0,    47,   403,   676,     0,   120,     0,     0,     0,
       0,     0,     0,   685,   686,     0,     0,     0,     0,     0,
       0,    58,     0,   458,     0,     0,     0,     0,   403,   403,
       0,     0,   119,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,  1274,     0,     0,     0,   203,     0,   403,
       0,     0,     0,     0,     0,     0,   297,     0,     0,     0,
       0,     0,     0,     0,   458,   119,     0,     0,   124,     0,
       0,     0,     0,     0,   120,     0,     0,   124,     0,   403,
       0,     0,     0,     0,     0,   124,   458,   458,     0,  1295,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,   458,     0,     0,     0,     0,
       0,     0,     0,   119,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,  1319,     0,     0,     0,
       0,     0,     0,     0,   119,  1323,  1324,  1325,  1326,     0,
       0,     0,   124,  1331,  1332,     0,     0,     0,     0,   297,
       0,     0,     0,  1340,     0,     0,     0,     0,     0,     0,
       0,     0,   458,   536,     0,     0,     0,     0,     0,   203,
       0,     0,     0,   297,  1356,     0,     0,  1359,     0,  1360,
       0,     0,     0,     0,   297,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   568,   572,     0,     0,     0,     0,     0,   579,
     580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   590,     0,     0,     0,     0,
       0,     0,   203,     0,  1418,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   609,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1432,     0,     0,     0,     0,     0,     0,  1436,     0,  1438,
    1440,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1449,     0,  1450,     0,  1451,     0,  1453,     0,     0,
       0,     0,  1461,   124,   124,   124,   124,   124,   124,   124,
       0,     0,   702,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,   124,     0,     0,     0,     0,     0,
       0,     0,     0,   743,   403,   403,   403,   403,   403,   403,
     403,   403,   403,   403,   403,   403,   403,   403,   403,   403,
     403,   403,   403,     0,  1506,     0,     0,     0,     0,   781,
       0,  1513,  1514,   784,     0,   458,     0,   168,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   806,     0,     0,  1537,   807,   808,     0,   119,
     811,     0,  1542,   168,     0,     0,  1543,     0,     0,     0,
     124,     0,     0,     0,     0,   825,   826,   827,   828,     0,
       0,     0,     0,     0,   403,     0,     0,   119,     0,  1560,
       0,     0,     0,     0,   850,     0,     0,     0,     0,     0,
       0,     0,   853,   224,     0,     0,     0,     0,     0,   168,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,     0,   168,     0,     0,   119,     0,     0,
       0,   297,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1641,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   203,   366,     0,     0,     0,
       0,     0,   892,     0,     0,   203,     0,     0,     0,   568,
     124,     0,     0,     0,     0,   898,     0,     0,     0,  1663,
     366,     0,     0,     0,     0,     0,     0,  1668,     0,  1670,
       0,     0,     0,     0,   203,     0,     0,     0,     0,   915,
     920,     0,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,     0,
       0,     0,   168,     0,     0,   168,   168,     0,     0,   168,
       0,     0,   168,   168,     0,     0,     0,   124,  1701,  1702,
       0,     0,   403,     0,     0,     0,     0,   403,     0,     0,
       0,   124,     0,  1707,  1708,     0,  1709,     0,   403,     0,
       0,   458,   458,     0,     0,  1713,     0,     0,     0,     0,
     962,     0,     0,     0,     0,  1718,  1719,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,     0,
     403,   168,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   168,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,  1025,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1042,     0,     0,     0,  1043,     0,
       0,     0,     0,     0,     0,     0,     0,   915,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   203,  1083,
       0,     0,     0,  1807,  1808,     0,     0,     0,  1092,     0,
       0,     0,   353,     0,  1095,     0,  1816,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,   168,     0,     0,     0,     0,   451,   353,
       0,     1,     0,  1840,  1841,  1131,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     517,     0,     0,     0,     0,     0,     1,   517,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,     0,   203,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,     0,  1254,  1909,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,   403,   403,   403,     0,   517,
       0,     0,   403,   403,  1919,     0,     0,  1920,  1921,     0,
       0,     0,     0,     0,  1923,   124,     0,     0,     0,     0,
       0,     0,     0,   353,   624,   366,   403,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   203,     0,     0,   645,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,   403,   403,  1302,   168,   168,     0,  1303,
       0,     0,     0,     0,     0,     0,   915,     0,     0,   168,
       0,     0,     0,     0,     0,     0,  1316,     0,     0,     0,
       0,     0,     0,  1317,     0,     0,     0,     0,     0,     0,
     458,   458,  1321,     0,  1322,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   517,   777,     0,   517,   780,  1350,     0,     0,     0,
    1351,     0,   353,     0,     0,     0,   624,  2035,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   146,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,     0,     0,
       0,   517,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   168,     0,     0,     0,     0,   168,     0,
       0,  2075,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,     0,     0,     0,     0,     0,   168,
       0,     0,   168,   168,     0,   168,  2093,   168,   168,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2102,     0,  1448,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2115,     0,     0,
       0,     0,     0,     0,   517,     0,   168,   353,  1473,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   910,   353,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   624,     0,     0,     0,
     624,     0,   458,     0,     0,     0,     0,   928,     0,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   168,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1546,     0,     0,     0,  1547,     0,     0,   403,
       0,     0,     0,   210,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   264,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1589,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   696,     0,     0,
     353,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   517,   517,     0,   210,
       0,     0,     0,   317,     0,     0,   517,  1038,     0,   517,
    1041,     0,     0,  1651,     0,   358,  1654,     0,     0,     0,
       0,   353,   166,     0,   624,     0,   624,   624,     0,     0,
       0,     0,     0,   624,  1665,     0,     0,     0,     0,   210,
       0,   168,     0,   353,   353,     0,     0,     0,     0,     0,
       0,     0,   471,     0,     0,   475,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,   517,     0,     0,     0,
     517,     0,     0,     0,   517,  1109,     0,     0,   517,  1113,
       0,     0,   168,     0,     0,     0,  1116,  1700,     0,   168,
       0,     0,   168,     0,   289,   427,  1705,     0,     0,     0,
    1706,     0,     0,     0,   210,     0,     0,   295,   456,   296,
       0,     0,     0,     0,  1710,  1711,   856,   858,   264,     0,
       0,   484,     0,   484,     0,     0,     0,     0,     0,   353,
     517,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1654,     0,     0,     0,     0,   403,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   475,     0,
     624,     0,     0,     0,     0,     0,   210,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,   617,     0,   634,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     518,   519,     0,   585,   523,     0,     0,   526,   527,     0,
     168,     0,     0,     0,     0,     0,     0,     0,   168,   168,
       0,     0,     0,     0,     0,  1801,  1802,     0,     0,     0,
       0,   700,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   696,     0,     0,     0,     0,
       0,   696,     0,     0,     0,     0,   517,     0,     0,     0,
     696,     0,     0,     0,     0,   210,     0,     0,     0,     0,
       0,     0,     0,   624,   624,     0,   403,   168,   403,   696,
     624,     0,     0,     0,     0,     0,   168,     0,     0,   168,
       0,   168,   168,     0,     0,   617,     0,   606,   607,     0,
       0,   801,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   639,     0,  1017,     0,   403,     0,     0,
       0,     0,   353,     0,     0,     0,     0,   517,  1345,     0,
     517,  1349,   168,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     403,     0,     0,     0,     0,     0,     0,     0,     0,  1894,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     210,   210,     0,     0,     0,     0,   471,   484,     0,     0,
       0,     0,     0,   484,     0,     0,  1654,     0,   822,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
       0,     0,     0,     0,     0,  1922,   168,     0,   775,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     358,     0,     0,  1940,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   471,
       0,   914,   353,     0,     0,     0,     0,     0,   624,  1457,
    1968,     0,     0,  1969,     0,     0,     0,     0,     0,     0,
       0,     0,   617,     0,     0,   846,   891,     0,     0,     0,
       0,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,     0,   210,     0,     0,     0,
       0,     0,     0,     0,     0,   456,     0,     0,     0,   700,
       0,     0,   700,   700,     0,   700,     0,     0,   922,     0,
       0,     0,     0,   168,   700,   517,  1510,   700,   700,   700,
       0,     0,     0,     0,   517,  1519,     0,   624,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   353,   353,
       0,     0,     0,     0,     0,     0,   168,     0,     0,     0,
       0,     0,   168,     0,     0,     0,     0,  2055,     0,     0,
       0,     0,     0,   956,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,     0,     0,     0,     0,     0,
       0,   926,   927,     0,     0,     0,     0,   822,   976,     0,
       0,   978,     0,   980,   934,     0,     0,   210,     0,   989,
       0,   994,   989,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,   168,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1022,
       0,     0,     0,     0,     0,     0,   471,   471,     0,     0,
       0,     0,  1024,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1033,     0,   471,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   353,     0,   456,     0,     0,
    1022,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   624,     0,     0,     0,     0,  1086,     0,     0,
     484,     0,     0,     0,   168,   168,     0,     0,     0,     0,
       0,     0,   366,     0,     0,     0,   168,  1031,  1032,     0,
       0,     0,   471,  1036,     0,     0,     0,     0,     0,   210,
       0,     0,     0,     0,     0,     0,  1117,     0,     0,     0,
       0,     0,   801,     0,  1057,     0,     0,  1060,  1061,     0,
    1064,     0,  1066,  1067,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   517,   427,     0,     0,     0,     0,
       0,  1107,   358,     0,     0,  1111,     0,     0,  1226,  1228,
     517,     0,     0,     0,     0,     0,   456,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   168,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   989,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1022,     0,
       0,     0,     0,     0,     0,     0,  1267,     0,     0,     0,
       0,  1219,  1220,   989,     0,     0,     0,     0,   353,     0,
       0,     0,     0,     0,     0,  1236,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   471,     0,   353,   353,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   517,   517,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1596,
       0,   517,  1599,  1613,     0,     0,     0,     0,  1620,     0,
     700,     0,  1624,     0,  1626,     0,   484,     0,  1335,     0,
    1338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,  1236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   264,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,  1336,     0,     0,
       0,  1410,  1410,     0,  1343,   617,     0,  1347,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,     0,     0,
       0,     0,     0,     0,     0,   517,     0,     0,     0,     0,
       0,     0,     0,     0,   358,     0,     0,     0,   700,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1452,     0,     0,  1720,     0,
       0,  1462,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   353,
     456,     0,     0,   517,  2000,     0,     0,   517,     0,     0,
       0,   471,   471,     0,     0,     0,     0,   484,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1761,     0,     0,
       0,     0,   989,     0,     0,   822,     0,     0,     0,     0,
       0,     0,  1776,  1778,     0,  1455,     0,     0,   517,     0,
       0,     0,     0,  1464,  1465,     0,   700,   700,   700,     0,
       0,   700,   700,     0,     0,     0,  1599,     0,   475,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1545,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1553,  1554,     0,     0,     0,     0,     0,
       0,     0,  1508,   517,   517,     0,   264,     0,     0,     0,
       0,  1517,     0,     0,  1521,     0,  1524,  1525,     0,     0,
       0,     0,   989,     0,     0,     0,     0,     0,   358,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     484,     0,     0,   822,     0,   517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1551,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1868,     0,     0,     0,     0,     0,
       0,     0,     0,  1871,   976,  1873,     0,     0,  1878,  1882,
       0,  1613,     0,     0,  1666,  1667,  1888,     0,     0,     0,
       0,     0,     0,     0,   484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   484,     0,   822,     0,     0,     0,     0,     0,
       0,  1648,     0,     0,     0,     0,     0,     0,     0,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,   210,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,   264,    46,     0,    47,     0,
       0,     0,     0,     0,     0,     0,  1957,     0,     0,     0,
       0,  1962,  1964,     0,     0,     0,     0,    58,     0,   427,
       0,     0,     0,     0,  1745,     0,     0,     0,     0,  1521,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   358,     0,     0,     0,     0,     0,     0,     0,   678,
       0,     0,   679,   680,     0,     0,     0,     0,  1715,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2014,   700,  2017,     0,     0,  2019,
    2021,     0,  1790,     0,  2024,  2026,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,   471,     0,     0,   -16,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2116,     0,     0,     0,     0,  1818,
       0,     0,  1820,     0,     0,     0,     0,     0,     0,     0,
       0,  1394,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   264,     0,     0,  2069,  2071,  2073,     0,     0,
       0,     0,     0,     0,  1799,     0,     0,  1845,     0,     0,
       0,     0,   375,     0,     0,   376,  2086,   377,     0,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2097,  2099,  2101,     0,  1147,     0,   379,    -2,     0,  1149,
    -243,  -243,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  1161,  -336,     0,  1162,  1163,  1164,  1165,
    1166,     0,  1167,     0,   380,   381,     0,   478,     0,   383,
    1168,  1169,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,  1170,   386,   387,   388,     0,   389,   390,  1854,
    1855,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,  1859,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -243,   391,     0,     0,    76,   392,
       0,     0,     0,   293,   700,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,  1797,     0,
    -184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   471,     0,     0,  1394,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,   989,   376,
       0,   377,     0,   378,     0,     0,     0,     0,     0,   700,
       0,     0,   475,     0,     0,     0,  1930,     0,  1147,     0,
     379,    -2,     0,  1149,     0,     0,  1150,  1151,  1152,  1153,
    1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -336,     0,
    1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,   381,
       0,   478,     0,   383,  1168,  1169,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1170,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1998,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,   293,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,     0,  -184,     4,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1146,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   375,     0,    46,   376,    47,   377,     0,   378,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,  1147,    58,  1148,    -2,     0,  1149,     0,
       0,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  -336,     0,  1162,  1163,  1164,  1165,  1166,
       0,  1167,     0,   380,   381,    61,   478,     0,   383,  1168,
    1169,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1170,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -3,   391,     0,     0,    76,   423,     0,
       0,     0,   293,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
       4,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1146,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   375,     0,    46,   376,
      47,   377,     0,   378,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1147,    58,
    1148,    -2,     0,  1149,     0,     0,  1150,  1151,  1152,  1153,
    1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -336,     0,
    1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,   381,
      61,   478,     0,   383,  1168,  1169,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1170,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   423,     0,     0,     0,   293,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,     0,  -184,     4,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   375,     0,    46,   376,    47,   377,     0,   378,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   379,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   380,   381,    61,   382,     0,   383,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1607,  1608,
    1609,     0,     0,     0,   391,  1610,  1611,    76,   423,     0,
       0,     0,     0,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  1612,
       4,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   375,     0,    46,   376,
      47,   377,     0,   378,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   380,   381,
      61,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1607,  1608,  1609,     0,     0,     0,   391,
    1610,     0,    76,   423,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,     0,  1612,   182,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   247,   248,
       0,   249,    46,     0,    47,     0,   250,     0,     0,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   375,     0,    46,   376,    47,   377,     0,   378,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   379,     0,     0,     0,
       0,     0,     0,     0,     0,  -441,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   380,   381,    61,   382,  -441,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,  1598,    76,   423,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     4,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,    61,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   423,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,   333,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,     0,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,    76,
     453,     0,     0,     0,     0,     0,   393,   454,    79,   394,
     395,   396,   397,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,   333,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,  1223,     0,     0,     0,     0,
       0,   393,  1224,    79,   394,   395,   396,   397,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,   333,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,     0,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,    76,
     392,     0,     0,     0,     0,     0,   393,    78,    79,   394,
     395,   396,   397,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,   333,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   453,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,  1939,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,  1967,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,  1234,    -2,     0,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   376,
       0,   377,     0,   378,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,    58,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,  1459,     0,     0,     0,     0,     0,    73,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
     454,    79,   394,   395,   396,   397,     0,   375,     0,     0,
     376,     0,   377,     0,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,   379,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,    76,   392,     0,     0,     0,     0,     0,
     393,  1460,    79,   394,   395,   396,   397,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,    59,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    61,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,   252,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -461,  -461,     0,  -461,    46,     0,    47,
       0,  -461,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   253,     0,     0,     0,  -788,     0,     0,    78,
      79,     4,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -389,  -389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -389,
       0,     0,     0,    76,    77,     0,     0,     0,     0,     0,
       0,    78,    79,     4,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,     0,     0,     0,     0,  -390,  -390,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -390,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,   252,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -461,
    -461,     0,  -461,    46,     0,    47,     0,  -461,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   253,     0,
    1369,     0,  1370,     0,     0,    78,    79,  1371,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1373,     0,
       0,     0,    76,   952,     0,  1369,     0,  1370,     0,     0,
      78,    79,  1371,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1563,     0,     0,     0,    76,   952,     0,
    1369,     0,  1370,     0,     0,    78,    79,  1371,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1564,     0,
       0,     0,    76,   952,     0,  1369,     0,  1370,     0,     0,
      78,    79,  1371,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1565,     0,     0,     0,    76,   952,     0,
       0,     0,     0,     0,     0,    78,    79,   252,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -461,  -461,     0,  -461,    46,     0,    47,   252,  -461,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -461,  -461,     0,  -461,    46,     0,    47,     0,
    -461,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     253,     0,    63,    64,     0,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   315,     0,     0,     0,     0,     0,     0,    78,    79,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -461,  -461,     0,  -461,    46,     0,    47,
       0,  -461,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   253,     0,     0,     0,  -792,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -461,  -461,     0,  -461,    46,     0,
      47,     0,  -461,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   147,     0,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
      75,     0,    76,   253,     0,     0,     0,     0,     0,     0,
      78,    79,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,  1077,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -652,    76,   335,     0,     0,     0,    63,    64,
       0,    78,    79,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    76,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   333,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,    63,    64,     0,   333,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   334,    76,   335,     0,     0,     0,    63,
      64,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    76,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1836,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   335,     0,     0,     0,
       0,     0,     0,    78,    79,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,  1838,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   335,     0,     0,
       0,     0,     0,     0,    78,    79,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   333,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   315,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     333,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   335,
       0,     0,     0,     0,     0,     0,    78,    79,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,
       0,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,    63,    64,     0,     0,     0,  1394,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
     376,     0,   377,     0,   378,     0,     0,     0,     0,    76,
     253,   183,     0,     0,   184,   185,     0,    78,    79,  1147,
       0,   379,     0,     0,  1149,  1861,  1862,  1150,  1151,  1152,
    1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -336,
       0,  1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,
     381,     0,   478,     0,   383,  1168,  1169,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,  1170,   386,   387,
     388,  1394,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,   375,    76,   392,   376,     0,   377,   293,   378,
     393,    78,    79,   394,   395,   396,   397,     0,     0,     0,
       0,     0,     0,     0,  1147,  -184,   379,     0,     0,  1149,
       0,     0,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  1161,  -336,     0,  1162,  1163,  1164,  1165,
    1166,     0,  1167,     0,   380,   381,     0,   478,     0,   383,
    1168,  1169,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,  1170,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,   293,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,     0,     0,
    -184,    14,    15,    16,    17,    18,    19,   687,    20,   688,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   375,     0,    46,
     376,    47,   377,     0,   378,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   379,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   689,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,    76,   690,     0,     0,     0,   293,     0,
     393,    78,    79,   691,   692,   396,   397,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,     0,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,   422,    76,
     423,     0,     0,     0,     0,     0,   393,    78,    79,   394,
     395,   396,   397,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   375,
       0,    46,   376,    47,   377,     0,   378,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   380,   381,     0,   382,     0,   383,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,     0,
     386,   387,   388,     0,   389,   390,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,    76,   690,     0,     0,     0,
     293,     0,   393,    78,    79,   394,   395,   396,   397,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   375,     0,    46,   376,    47,
     377,     0,   378,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   379,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   380,   381,     0,
     382,     0,   383,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   384,   385,   372,     0,   386,   387,   388,     0,
     389,   390,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,    76,   423,     0,     0,     0,     0,     0,   393,    78,
      79,   394,   395,   396,   397,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   375,     0,    46,   376,    47,   377,     0,   378,   333,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   379,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   380,   381,     0,   382,     0,   383,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   453,     0,
       0,     0,     0,     0,   393,    78,    79,   394,   395,   396,
     397,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   375,     0,    46,
     376,    47,   377,     0,   378,   333,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   379,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,    76,   392,     0,     0,     0,     0,     0,
     393,    78,    79,   394,   395,   396,   397,   573,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   252,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    63,    64,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
       0,     0,     0,     0,     0,    58,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      63,    64,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,    77,
       0,     0,     0,  -790,     0,     0,    78,    79,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
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
       0,   147,     0,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,     0,     0,     0,     0,     0,
      78,    79,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   876,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -665,    76,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1753,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    76,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
     315,     0,     0,     0,     0,     0,     0,    78,    79,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   333,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
      76,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,  1477,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   958,    76,   952,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   952,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   300,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   449,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   335,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     333,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,   333,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   300,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   449,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   315,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   952,     0,     0,
       0,     0,     0,     0,    78,    79,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -461,
    -461,     0,  -461,    46,     0,    47,     0,  -461,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,    63,
      64,     0,   333,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,    76,     0,    46,
       0,    47,    63,    64,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   952,     0,     0,     0,    63,    64,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,    14,    15,    16,    17,
      18,    78,    79,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   315,
       0,    63,    64,     0,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
       0,     0,     0,     0,     0,     0,     0,    78,    79,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,     0,
       0,   393,   454,    79,   394,   395,   396,   397,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   375,     0,    46,
     376,    47,   377,     0,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   379,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,    76,   392,     0,     0,     0,     0,     0,
     393,    78,    79,   394,   395,   396,   397,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
     375,     0,     0,   376,     0,   377,    58,   378,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,     0,   389,   390,   375,     0,     0,
     376,     0,   377,    73,   378,     0,     0,     0,     0,    76,
       0,     0,     0,     0,     0,     0,     0,  1607,  1608,  1609,
       0,   379,     0,   391,  1777,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,  1876,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,   375,     0,     0,   376,     0,   377,
      73,   378,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1607,  1608,  1609,     0,   379,     0,
     391,  1877,     0,    76,   392,     0,     0,     0,     0,     0,
     393,    78,    79,   394,   395,   396,   397,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   478,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,    75,     0,
     479,   480,     0,     0,     0,   481,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  1270,     0,    76,   392,     0,     0,     0,  1271,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,   481,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
     821,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,     0,     0,    76,   392,     0,     0,
       0,   293,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,   985,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,  1016,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  1337,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,  1404,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,     0,     0,    76,   392,     0,     0,
       0,  1468,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,     0,  1867,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  1872,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  1881,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    1961,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  1963,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2016,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2018,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  2020,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    2023,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2025,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2068,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2070,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  2072,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    2096,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2098,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2100,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   669,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   675,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   684,     0,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,     0,     0,
      76,   392,     0,     0,     0,     0,     0,   393,   890,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,     0,
       0,   393,   454,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
    1956,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   678,     0,     0,   679,   680,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   229,     1,     4,     1,    74,    74,     4,   176,   181,
     623,    74,    74,   297,   165,   266,    96,   181,   391,   251,
     716,   481,    76,  1156,   365,     1,   899,     1,   632,   229,
      83,  1131,   906,     4,   619,   217,     1,   701,   140,   232,
      59,   393,  1728,    96,  1728,   790,    99,   165,   229,   166,
     103,   796,  1728,   530,   531,    56,    57,   338,    59,   229,
      59,   694,    59,     1,   885,   353,    74,     1,   942,   357,
     152,  1396,  1397,    74,   619,   165,   885,   760,  1396,   789,
    1796,   309,    83,    59,    83,    59,   619,   789,     1,   787,
      91,   239,   869,     1,    59,    96,    98,    96,    99,   201,
      99,     1,   103,   336,   103,    83,   103,   229,   787,   309,
      74,   304,   305,  1617,   202,   263,    72,   787,   229,   896,
     353,    59,   191,   191,   357,    59,   274,   146,   309,   191,
     229,    74,   103,   787,   991,   330,  1861,   190,   103,   309,
     141,     0,   133,   144,    99,   146,    59,   146,    84,   146,
     230,   152,   205,     1,   180,   625,     4,   118,   159,     1,
     229,   229,     4,  1020,   246,   166,   229,   229,  1321,  1322,
     146,   149,   146,  1729,   369,   177,   789,   230,  1279,   656,
     461,   146,  1140,   191,   175,   541,     1,   309,  1865,   190,
     191,   190,  1128,   230,   806,   807,   552,   152,   309,  1135,
     156,   254,   787,    72,   205,    75,   205,   152,   146,   154,
     309,    59,   146,   825,   215,   268,   152,    59,   336,   220,
      72,   229,   455,   275,    75,   152,  1083,   191,   229,   230,
     310,   230,   807,   146,   288,    83,     0,   593,   290,  1140,
     309,   309,   787,    89,    59,   246,   309,   309,   191,   176,
     825,   159,   948,   254,   787,   254,   391,    99,   158,   159,
      72,   103,   513,   264,   265,   229,    10,   268,  1772,   268,
    1206,   499,    10,   310,   275,   118,   254,  1613,  1140,  2004,
     892,   977,    75,   152,    99,   152,   229,   156,   289,   290,
     767,   292,   140,   160,  1850,   579,   132,    90,   146,   499,
     152,   149,  1047,    72,   146,   487,     0,   177,   309,   310,
     923,   310,   653,   156,   160,  1640,   317,   892,   499,   671,
     608,   791,  1640,   324,   325,   795,   177,  1148,   329,   499,
     611,   146,   297,   966,   804,   805,  2007,   455,   316,  1148,
     152,   714,   459,  1053,   156,   134,  1029,   152,  1046,   451,
    2036,  1053,  2036,   201,   635,  1132,  1177,   645,   586,   297,
    2036,   642,  2033,   297,  1238,   152,   367,  1046,  1177,   370,
     371,  2087,  1246,  1473,   154,   608,  1046,   499,   158,   168,
      89,  2058,   435,   152,   297,   221,   586,   156,   499,  2060,
     107,   108,  1046,    72,    20,  1496,  1497,  1498,   238,   632,
     499,   241,   152,   551,   252,   586,   254,     4,  1366,  1367,
    1368,    89,   645,   151,   157,  1971,   586,  2094,   162,    72,
     134,   146,   262,   167,   162,   479,   268,  1363,   508,   167,
     499,   499,    72,   273,   435,    72,   435,   499,   155,   152,
    1053,  1777,  1778,   168,    61,    62,   528,   425,   536,   297,
    1775,   160,   534,   268,   168,   508,   123,   458,   459,    56,
      57,  1046,   298,   152,   586,  1366,  1367,  1368,   316,   470,
     471,   508,   590,   152,   158,   586,   160,   156,   479,  1612,
     481,  2037,   160,  1095,  1617,   177,  1231,   586,   155,    58,
     607,   499,    61,    62,    91,    64,   152,    89,   499,   152,
     590,  1046,   134,   156,  1366,  1367,  1368,   508,   588,   508,
     866,   158,   152,  1046,   632,   152,   156,   586,   586,   156,
    1095,   609,  2078,   338,   586,    77,    78,   528,   158,    72,
     177,   163,   164,   534,   669,   499,   202,   672,   673,   177,
     675,  1877,  1878,   160,   141,   156,    72,   144,    69,   684,
     161,   588,   687,   688,   689,   391,   499,  1710,  1711,   154,
     177,   156,   159,  1023,    72,  1310,   158,   155,   160,   166,
      77,    78,   573,   592,   575,  1396,  1397,   425,    13,    14,
      15,    16,    17,   152,   158,   586,   154,   588,   176,   588,
     160,   592,   158,   592,   168,   592,   877,   165,   166,   156,
    1070,   602,    72,   451,   161,   606,   607,   177,   160,   152,
     158,   177,   577,   156,   579,  1940,   592,    72,   592,   128,
     129,  1225,   910,   220,    72,  1289,   152,   592,   800,   177,
     156,   912,   814,   158,  1491,     3,   800,    72,   639,  1772,
     158,   579,    72,  1968,   152,   579,   461,   811,   156,   240,
      72,   652,   177,   160,   592,   743,    72,   160,   592,   177,
      75,   158,   160,   172,   173,   158,   579,   264,   265,   167,
     336,   609,   789,   339,   177,   168,    91,   910,   275,   592,
     177,   521,   152,  2008,   520,     3,   156,   353,   742,   525,
    1386,   357,   289,   290,   109,   292,   784,   152,   155,   134,
    1573,   156,   158,   543,   152,   706,   542,   708,   156,   710,
     550,    72,   149,   714,   554,   741,   717,   553,   806,   807,
     317,   177,   152,   160,  1566,   573,   156,   324,   325,  1571,
     152,   579,   329,   158,   156,   132,   152,   825,  1215,   176,
     156,   742,  1875,  1407,   592,   827,   146,   147,   148,   160,
     592,   132,   177,   158,  1887,   152,   167,   158,   590,   156,
     154,   576,   158,   158,   846,   159,   163,   164,   168,  1922,
     367,   152,   177,   370,   154,   156,   177,   592,   158,   176,
       3,   177,   163,   164,  1658,  1659,   787,   152,   789,   455,
      13,    14,    15,    16,    17,    13,    14,    15,    16,    17,
     801,   146,   147,   148,   892,   158,   160,   808,   158,    62,
     132,  1271,   160,   814,   176,   168,   817,   934,   168,  1640,
     635,   847,  1955,   168,  1488,   826,   827,   828,   151,   160,
     152,  1082,   177,   669,   156,   158,  1693,   428,  1695,   675,
     367,   163,   164,   370,   962,   846,   784,   158,   684,    72,
     379,   104,  1016,   444,    72,   108,   447,   168,   111,   158,
     113,   458,  1475,    47,    48,   152,    50,   703,   174,   168,
     536,    55,   962,   470,   471,   404,   405,   152,  1219,  1220,
     154,   154,   883,   884,   885,   159,   159,   152,   885,   152,
      13,    14,    15,    16,    17,   160,   425,   163,   118,   132,
     152,   898,   167,   153,   170,   171,   154,   159,  1090,   885,
     160,   885,   154,   504,    13,    14,    15,    16,    17,   152,
     885,  1114,   151,   156,   590,   152,   455,   898,  1092,   158,
     163,   164,   154,   934,    58,  1052,  1053,    61,    62,  1813,
      64,   154,   608,   609,  1404,   154,   947,   885,  1229,    72,
     159,   885,   107,   108,  1775,    13,    14,    15,    16,    17,
     121,   154,   123,   124,   125,   132,   632,   111,   112,   113,
     114,   115,   885,    72,   806,   807,   573,   155,   156,   645,
     154,   159,   983,   154,   152,   152,   154,   158,   156,   156,
     991,   152,  1225,   825,   155,   156,   163,   164,  1371,   160,
     161,   152,  1234,    22,   257,   602,   158,  1095,  1468,   606,
     607,   134,   152,   152,    72,   154,   852,   156,   152,  1020,
     152,   152,  1023,   154,   156,   156,  1486,   863,   868,   158,
     152,   867,   154,   132,   156,   871,   154,   885,  1912,   158,
     158,   158,   639,   885,   158,  1046,   154,   155,  1216,  1217,
     898,  1052,  1053,   152,   154,   652,   898,   156,   158,   154,
     892,    99,   877,   158,   163,   164,   152,   154,   107,   108,
     885,   158,  1244,   888,   132,   154,   152,   743,   151,   158,
    1244,   152,  1083,   165,   166,   156,   160,   340,   341,   154,
     343,   152,   345,   158,   152,   156,   152,   912,   156,   154,
     156,  1352,    89,   158,   154,   163,   164,  1225,   158,   706,
     152,   708,  1396,   710,   156,   126,   127,   714,   784,  1940,
     717,   154,   154,  1258,  1465,   158,   158,  1209,  1292,   382,
     962,   154,   160,   154,  1131,   158,  1254,   158,   160,   154,
     154,  1194,  1195,   158,   158,   742,   154,  1968,   157,  1150,
     158,  1148,  1153,  1154,  1155,   154,   154,  1329,  1330,   158,
    1131,   167,  1140,  1253,  1254,  1329,  1330,   118,   759,   760,
     154,  1322,  1148,   154,  1148,   154,  1177,   158,   769,   158,
    1177,   772,  1183,  1148,   154,   154,   176,  2008,   158,   158,
    1191,   720,   152,  1194,  1195,  1194,  1195,  1198,  1195,   130,
     131,  1177,   152,  1177,   801,     3,  1902,   169,  1209,   154,
    1148,   808,  1177,   158,  1148,    13,    14,    15,    16,    17,
     721,   722,   723,   152,  1195,   478,   146,   147,   148,   826,
    1195,   828,   154,  1321,  1322,  1148,   158,   152,   158,  1177,
     158,  1242,   833,  1177,   910,   154,   837,   913,   168,   158,
     841,  2041,   157,   158,  1255,  2045,   164,   177,   162,   146,
     147,   148,   174,  1095,  1177,    13,    14,    15,    16,    17,
    1271,   158,   132,    18,    72,  1137,  1138,  1139,  1279,   157,
     158,   168,   155,  1131,   163,   164,   883,   884,   885,  1131,
     177,   154,  1140,   157,   158,   548,   962,  1377,  1134,   154,
    1148,   154,    18,  1563,  1564,  1565,  1148,   157,   158,   156,
    1311,  1147,   157,   158,   157,   158,    61,    62,    63,    64,
     728,   729,   730,   731,    72,   158,   159,   154,  1164,  1177,
    1448,  1672,   154,  1148,   154,  1177,   157,   934,   157,   158,
    1377,    57,    58,    59,    60,    61,    62,    63,    64,  1577,
     157,   158,  1194,  1195,   157,   158,  1640,   134,  1448,   104,
     157,   158,  1177,   134,   109,   110,   111,   112,   113,   114,
     115,   116,  1369,  1426,   157,   158,  1377,  1577,   159,  1194,
    1381,   157,   158,  1384,   132,   157,   158,   152,  1366,  1367,
    1368,   157,   158,  1371,  1372,   159,  1577,   158,  1369,  1396,
    1397,   157,   158,  1404,   152,   157,   158,  1577,   156,   158,
     159,   156,    77,    78,  1229,   163,   164,    78,   157,   158,
    1396,  1397,  1254,  1424,   154,  1426,   154,  1426,   154,   958,
     154,  1396,  1397,   154,   963,   157,   158,   152,  1029,   157,
     158,   158,   159,   104,   154,   974,   107,   108,   109,   110,
     111,   112,   113,   114,   115,  1577,   157,   158,  1396,  1397,
     157,   158,  1396,  1397,   104,   157,  1577,  1468,   156,   109,
     110,   111,   112,   113,   114,   115,  1473,   160,  1577,  1280,
    1281,   724,   725,  1396,  1397,  1486,  1566,   726,   727,  1321,
    1491,  1571,   732,   733,   160,  1496,  1497,  1498,   160,  1579,
    1651,  1589,  1473,    13,    14,    15,    16,    17,    18,  1100,
    1584,  1585,  1103,   160,  1577,  1743,   177,   160,  1366,  1367,
    1368,  1369,  1370,  1371,  1372,  1466,  1467,  1369,    70,  1566,
    1216,  1217,   157,  1651,  1571,    13,    14,    15,    16,    17,
      18,   152,  1579,  1743,    78,   157,    18,   176,  1396,  1397,
    1685,  1387,  1388,  1150,  1396,  1397,  1153,  1154,  1155,  1225,
    1711,  1651,  1743,   158,   160,  1566,   152,   177,   160,   154,
    1571,   154,   177,  1743,    18,   157,  1577,   157,  1579,   160,
    1177,  1396,  1397,   151,  1426,  1586,  1183,   154,  1254,  1425,
     154,   154,   154,   154,  1191,   154,   154,  1598,   154,   151,
     151,  1198,    13,    14,    15,    16,    17,    18,    70,  1606,
    1611,  1426,   160,  1639,   160,   160,  1448,   154,   154,  1148,
     177,  1743,  1710,  1711,   154,  1473,   176,   154,   151,   176,
     158,  1473,  1743,   154,   160,  1606,   160,   154,   158,   154,
     154,  1606,   158,  1640,  1743,  1242,   154,  1648,    13,    14,
      15,    16,    17,    18,   651,   154,   154,  1654,  1255,   154,
     154,  1894,   154,   154,  1640,   151,  1746,   157,   157,   154,
     154,   154,   215,   154,   154,  1640,   176,   154,   154,   158,
     154,   154,   157,  1654,   154,   151,   154,   158,   152,  1861,
      14,  1972,  1693,   568,  1695,   152,  1225,  1861,   152,   152,
     152,   152,  1640,   159,    74,    91,  1640,  1298,   158,  1746,
     177,   159,   177,   151,   157,   157,  1307,   177,   177,   160,
    1802,   158,   177,   154,   157,   154,   158,  1640,   158,   154,
     158,   157,  1261,  1262,  1263,   154,  1969,   157,   154,  1268,
    1269,   151,  1743,   151,    80,  1746,   152,   152,   152,   151,
     177,     1,   152,   177,     4,  1756,   177,   154,   151,  1760,
     177,   151,   177,  1605,  1606,  1791,   158,   158,   177,   151,
     177,  1922,   160,  1774,   177,   160,   177,   157,  1775,  1859,
     157,   157,  1448,  1784,  1381,   154,    13,  1384,   151,   157,
    1605,   121,  1640,   154,   154,  1631,   151,   159,  1640,  1775,
    1801,  1802,   159,   154,   154,   154,  1654,   154,  2036,    59,
    1775,   157,  1654,   157,   151,   812,   151,   159,   177,   152,
     154,   152,  1859,   152,    74,  1640,   158,  1424,    83,   110,
    2002,   157,  2004,    83,  1922,   157,  2036,  1775,  2002,   157,
    2004,  1775,   151,   160,   151,   157,    96,   151,   154,    99,
      75,   154,   154,   103,   154,  2036,   154,   154,  1859,   154,
      75,    88,  1775,   177,  1865,   151,  2036,   177,  1869,   152,
     154,  2043,  1898,  1874,   154,   157,   177,   104,  1710,  2043,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1971,
     140,    90,   157,   160,   149,   151,   146,   151,  1899,   149,
     104,   154,   152,   153,   151,   109,   110,   111,   112,   113,
     114,   115,  2076,   154,  2036,   165,  1894,   154,   154,   154,
    1762,   154,   156,    75,   168,  2036,   155,  1775,   177,   168,
      75,   177,   177,  1775,   189,   159,   479,  2036,   481,   189,
     190,   191,  1943,  1940,   151,   151,  1947,  1762,   152,   153,
     158,   201,   202,  2125,   154,   205,   154,  2037,   154,  1960,
    1775,  2125,   154,   151,  1940,   168,   153,  2036,  2036,   151,
    1971,  1968,  1973,  2036,  2036,  1940,  1567,   168,   853,   229,
     230,   159,   104,  1984,   152,  1986,  1987,   158,    75,  1586,
     152,  1969,  1968,   151,   168,   168,   246,   153,  2078,   254,
    2037,  1598,  1940,  1968,   254,   177,  1940,   853,   157,   110,
    2011,  2008,   177,   110,  1611,  2041,   151,   153,   268,  2045,
    2046,   154,   159,   154,   151,   151,   154,  1940,   152,   177,
    1968,    75,  2008,   154,  1968,  2036,  2037,   154,  2037,   177,
     915,  2078,  1685,  2008,   693,   177,  2047,   734,  1286,   735,
    1166,  1648,   302,   737,  2080,  1968,   736,  2058,   308,   309,
     310,   316,   738,  2094,  1177,  1640,   316,  2004,   424,   915,
    2008,  1783,  1775,  2033,  2008,  2088,  1903,  2078,  2087,  2078,
    2106,   336,  2075,  1632,  2110,  1632,   336,   337,   338,  1969,
    2110,  2092,  1940,  2094,  2046,  2008,  1968,  1198,  1940,    49,
     111,  1859,   259,   353,  1930,  1372,  2132,   357,  1585,   938,
    1191,   814,  2113,   488,   902,   602,  1645,  1654,  2119,     0,
    1968,   947,   759,   759,   759,  1940,  1968,  1554,  2129,    -1,
      -1,    -1,  2133,     3,    -1,    -1,    -1,  1728,  1729,    -1,
      -1,   391,  2143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1025,    -1,    -1,  1968,    -1,    -1,    -1,    -1,    -1,  1756,
    2008,    -1,    -1,  1760,    -1,    -1,  2008,  1042,  1043,    -1,
     425,    -1,    -1,    -1,    -1,   425,    -1,  1774,   428,  1025,
      -1,    -1,    -1,    -1,    -1,   435,    -1,  1784,    -1,    -1,
      -1,    -1,    -1,  2008,    -1,    -1,  1042,  1043,    -1,    -1,
     455,   451,    -1,    -1,  1801,   455,    -1,    -1,    -1,   459,
      -1,   461,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,  1218,    -1,    -1,    -1,    -1,    -1,    -1,  1894,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   104,    -1,    -1,   499,
    1247,   109,   110,   111,   112,   113,   114,   115,   508,  1850,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,  1865,  2105,
      -1,   814,  1869,    72,   817,   530,   531,  1874,   528,    -1,
     530,   531,   152,   153,   534,  2121,   536,    -1,    -1,   159,
      -1,    -1,    -1,   163,   164,   153,    -1,    -1,   156,  1296,
      -1,    -1,  1899,  1969,    -1,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,  1926,   586,    -1,   588,  1930,
     590,    -1,   592,    -1,    -1,    -1,  1943,    -1,    -1,    -1,
    1947,    -1,    -1,   152,   153,    -1,    -1,   156,   608,   609,
      -1,   611,    -1,  1960,   163,   164,    -1,    -1,    -1,   619,
      -1,    -1,  1901,   623,    -1,    -1,  1973,   632,    -1,    -1,
    1971,    -1,   632,    -1,    -1,    -1,    -1,  1984,    -1,  1986,
    1987,    -1,   642,    -1,    -1,   645,    -1,    -1,    -1,    -1,
      -1,   656,    -1,    -1,    -1,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    62,    -1,  2011,    -1,    -1,    -1,    -1,   669,
      -1,    -1,   672,   673,    -1,   675,    -1,  1302,  1303,    13,
      14,    15,    16,    17,   684,    -1,    -1,   687,   688,   689,
     983,  1316,  1317,    -1,    -1,  2036,  2037,    -1,   991,    99,
    2047,    -1,    -1,    -1,    -1,    -1,  1302,  1303,    -1,    -1,
     110,  2058,   112,    -1,   114,    -1,  1463,    -1,    -1,    -1,
    1316,  1317,    -1,  1470,    -1,  1350,  1351,  1020,    -1,    -1,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,  2078,    72,    -1,
    1487,    -1,    -1,   743,    -1,  2092,    -1,  2094,    -1,    -1,
      -1,    -1,    -1,   153,  1350,  1351,   156,   157,    -1,   759,
     760,    -1,   767,    -1,    -1,    -1,  2113,   767,    -1,    -1,
     104,    -1,  2119,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  2129,    -1,   784,    -1,  2133,   787,    -1,   789,
    1083,    -1,    65,    66,    67,    68,  2143,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,   205,   806,   807,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,   152,   153,
      -1,    -1,   156,    -1,    -1,   825,    -1,   827,    -1,   163,
     164,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,   846,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,     4,
       5,     6,     7,     8,     9,    10,    11,    12,   268,    -1,
     270,   271,    -1,    -1,    -1,  1622,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,   885,    -1,    -1,    -1,    -1,
      -1,    -1,   892,    -1,   899,   152,   153,    -1,   898,   899,
      -1,   174,   302,    -1,    -1,    -1,    -1,    -1,   308,    -1,
     910,    -1,   912,    -1,    -1,    13,    14,    15,    16,    17,
      65,  1546,  1547,   923,    -1,   104,    -1,    -1,    -1,  1676,
     109,   110,   111,   112,   113,   114,   115,   116,   338,    -1,
    1687,    -1,    -1,    -1,   344,    -1,   346,    -1,    -1,     1,
    1546,  1547,     4,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,   962,    -1,  1589,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    72,    -1,    -1,   156,  1271,    -1,
      -1,    -1,   382,    -1,    -1,    -1,  1279,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,   104,    59,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1029,
      -1,    83,    -1,    -1,   132,   435,    -1,    -1,   152,   153,
    1665,    -1,   156,    -1,    -1,    -1,  1046,    99,    -1,   163,
     164,   103,    -1,  1053,   152,   153,  1803,  1804,    -1,    -1,
      -1,   461,    -1,   463,   464,   163,   164,    -1,    -1,  1665,
      -1,    -1,    -1,    -1,    -1,  1700,    -1,    -1,   478,    -1,
    1705,  1706,    -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,
      -1,    -1,    -1,    -1,   146,  1095,    -1,   149,    -1,    -1,
      -1,   153,    -1,    -1,  1700,    -1,    -1,    -1,   508,  1705,
    1706,  1404,   164,   165,   166,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   528,    -1,
      -1,  1131,    -1,   533,    -1,   535,    -1,   189,    -1,    -1,
    1140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1148,   201,
     202,    -1,    -1,   205,    -1,    -1,   556,    -1,   558,   559,
      -1,    -1,    -1,    -1,    -1,    -1,  1913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1468,   576,  1177,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,   588,   177,
      -1,    72,    -1,  1486,  1194,  1195,    -1,    -1,  1491,    -1,
     252,    -1,   254,  1496,  1497,  1498,    -1,    -1,    -1,  1209,
    1215,   611,    -1,   613,   614,  1215,   268,    -1,    -1,    -1,
    1225,    -1,    -1,   104,    -1,  1225,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   635,   636,   289,    72,    -1,
      -1,    -1,   642,    -1,  1991,   297,    -1,    -1,    -1,    -1,
     302,   132,    -1,    -1,  1254,    -1,   308,    -1,  1258,    -1,
      -1,    -1,    -1,    -1,   316,    62,    -1,    -1,    -1,    -1,
     104,   152,   153,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   163,   164,   336,    -1,   338,   339,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,   353,    99,    -1,    -1,   357,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,   111,    -1,    -1,    -1,   152,   153,
      -1,  1321,  1322,    -1,    -1,    -1,   104,    -1,    -1,   163,
     164,   109,   110,   111,   112,   113,   114,   115,   116,   391,
      -1,    -1,   120,    -1,   122,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   153,   107,   108,   109,
     110,   111,   112,   113,   114,   115,  1366,  1367,  1368,  1369,
      -1,  1371,  1372,   425,    -1,   153,    -1,  1377,  1378,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,  1396,  1397,    -1,   451,
    1693,    -1,  1695,   455,    -1,    56,    57,   132,   205,   461,
      -1,    -1,   104,   163,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,  1426,   152,   153,    -1,
    2055,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      91,    -1,    -1,    -1,    -1,    -1,   104,    -1,  1448,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   104,  2055,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   268,    -1,  1473,    -1,  1475,    -1,   877,   530,   531,
      -1,    -1,    -1,   535,   536,   177,   132,    -1,   888,    -1,
     141,    -1,    -1,   144,    -1,    -1,    -1,   155,    -1,    -1,
      -1,    -1,   160,    -1,    -1,   302,   152,   153,   159,    -1,
     156,   308,   912,    -1,    -1,    -1,   568,   163,   164,    -1,
      -1,   573,    -1,   923,   576,   577,    -1,   579,    -1,    -1,
     176,    -1,   932,    -1,    -1,    -1,    -1,    -1,   590,    -1,
     592,   338,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   606,    -1,   608,   609,    -1,   611,
      -1,    -1,    -1,    -1,    -1,    -1,  1566,  1567,  1573,   220,
      -1,  1571,    -1,  1573,    -1,    -1,    -1,  1577,    -1,  1579,
     632,    -1,    -1,   635,    -1,   382,    -1,   639,    -1,    -1,
     642,    -1,    -1,   645,    -1,   647,    -1,    -1,   160,    -1,
      -1,    -1,    -1,    -1,   656,  1605,  1606,    -1,  1150,    -1,
      -1,    -1,    -1,   264,   265,    -1,    -1,   669,    -1,    -1,
     672,   673,    -1,   675,   275,    -1,    -1,    -1,    -1,  1029,
      -1,    -1,   684,    -1,    -1,   687,   688,   689,   104,   290,
    1640,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,  1651,    -1,  1053,  1654,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,   461,    -1,   317,    -1,    -1,    -1,
      -1,    -1,    -1,   324,   325,    -1,    -1,    -1,   329,    -1,
      -1,   478,    -1,    -1,    -1,  1685,    -1,    -1,    -1,    -1,
      -1,   743,   104,    -1,   160,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
    1710,  1711,    -1,    -1,    -1,   767,   367,    -1,    -1,   370,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1728,  1729,
      -1,    -1,   784,    -1,    -1,    -1,    -1,    -1,   535,    -1,
     152,   153,    -1,  1743,    -1,    -1,  1746,    -1,    -1,    -1,
      -1,   163,   164,    -1,   806,   807,    -1,    -1,    -1,   556,
     104,    -1,  1762,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   825,    -1,  1775,    -1,    -1,   104,   576,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,  1194,    -1,    -1,    -1,   850,    -1,
      -1,   853,  1802,    -1,    -1,    -1,    -1,   458,    -1,    -1,
      -1,    -1,    -1,    -1,   611,    -1,   160,    -1,    -1,   470,
     471,    -1,    -1,    -1,    -1,   877,    -1,   153,    -1,  1229,
     156,     1,    -1,   885,     4,  1235,   888,    72,   635,    -1,
     892,    -1,    -1,    -1,    -1,   642,   898,   899,    -1,    -1,
    1850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   910,  1859,
     912,   913,    -1,   915,    13,    14,    15,    16,    17,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  1424,    -1,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    -1,    -1,  1894,   104,    -1,   132,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    72,    -1,    -1,
     962,    -1,    -1,    83,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,  1922,    72,    -1,    -1,    -1,    -1,   163,   164,
    1930,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,   104,
    1940,    -1,    -1,   152,   109,   110,   111,   112,   113,   114,
     115,   602,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,
     109,   110,   111,   112,   113,   114,   115,   132,  1968,  1969,
     140,  1971,  1972,  1025,    -1,    -1,   146,    -1,  1378,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,   152,   153,    -1,
    1042,  1043,    -1,    -1,    -1,   165,    -1,    -1,   163,   164,
      -1,   652,    -1,   152,   153,    -1,    -1,    -1,  2008,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,
     190,    -1,    -1,    -1,    -1,    -1,  1426,    -1,    -1,    -1,
      -1,   201,   202,    -1,    -1,    -1,  2036,  2037,    -1,    -1,
      -1,   104,    -1,  1095,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   850,    -1,    -1,  1598,    -1,    -1,    -1,
     230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,
      -1,    -1,    -1,    -1,    -1,    -1,   246,    -1,  2078,  1131,
     877,   251,   252,    -1,   254,    -1,    -1,    -1,  1140,    -1,
      -1,   888,   104,   156,    -1,    -1,  1148,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,  1648,   277,   120,    -1,
     122,   281,    -1,    -1,    -1,   912,   286,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1177,   104,   297,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
     310,   153,  1194,  1195,   156,    -1,    -1,    -1,    -1,    -1,
     801,    -1,    -1,    -1,   132,    -1,    -1,   808,    -1,    -1,
      -1,    -1,    -1,  1215,    -1,    -1,   336,    -1,    -1,   339,
      -1,    -1,    -1,  1225,   152,   153,    -1,  1229,    -1,    -1,
      -1,   159,    -1,   353,    -1,   163,   164,   357,   104,  1241,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,  1253,  1254,    -1,    -1,  1605,  1258,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1756,    -1,   132,    -1,  1760,    -1,
     104,    -1,     1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  1774,   884,    -1,    -1,   152,   153,    -1,    -1,
     156,    -1,  1784,    -1,    -1,    -1,    -1,   163,   164,   104,
    1302,  1303,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,  1316,  1317,    -1,    -1,    -1,  1321,
    1322,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,   451,    -1,   104,    -1,   455,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,  1350,  1351,
     155,    -1,    -1,    -1,    -1,    -1,  1358,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1366,  1367,  1368,  1369,  1370,  1371,
    1372,    -1,    -1,  1865,   103,    -1,  1378,  1869,    -1,    -1,
     104,   152,  1874,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   104,  1396,  1397,  1746,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,  1899,   132,   120,
      -1,   122,  1762,    -1,    -1,    -1,   536,   146,    -1,    -1,
      -1,    -1,    -1,    -1,  1426,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   165,    -1,    -1,   163,
     164,    -1,   153,    -1,    -1,   156,  1448,  1194,   568,    -1,
      -1,  1943,    -1,   573,    -1,  1947,   104,   577,    -1,   579,
      -1,   109,   110,   111,   112,   113,   114,   115,  1960,   104,
     590,  1473,   592,   202,   109,   110,   111,   112,   113,   114,
     115,   116,  1229,    -1,   132,   120,    -1,   122,   608,   609,
      -1,    -1,  1984,    -1,  1986,  1987,    -1,    -1,    -1,    -1,
    1850,    -1,    -1,   623,   152,   153,    -1,    -1,   156,    -1,
      -1,    -1,   632,    -1,    -1,   163,   164,   637,   153,  2011,
      -1,   156,    -1,    -1,    -1,   645,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,  1546,  1547,    -1,    -1,   277,    -1,
      -1,    -1,  1153,  1154,  1155,  2047,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2058,    -1,   297,    -1,
      -1,  1573,    -1,    -1,    -1,    -1,    -1,   152,   153,    74,
      -1,    -1,  1183,    -1,    -1,    -1,    -1,  1589,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,
    2092,    96,  2094,  1605,  1606,    -1,    -1,   336,    -1,    -1,
     339,  1358,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2113,  1972,   743,   353,    -1,    -1,  2119,   357,    -1,
      -1,  1378,    -1,    -1,    -1,    -1,    -1,  2129,  1640,    -1,
     760,  1242,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1651,
      -1,    -1,  1654,    -1,    -1,    -1,    -1,   152,    -1,    -1,
      -1,    -1,    -1,  1665,   784,    -1,    -1,   104,    -1,   789,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1426,
      -1,    -1,    -1,  1685,    -1,    -1,   806,   807,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,  1700,    -1,
      -1,    -1,    -1,  1705,  1706,   825,    -1,    -1,  1710,  1711,
      -1,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,    -1,    -1,    -1,   149,   455,    -1,  2078,    -1,
      -1,    -1,    -1,   853,   229,   230,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,   246,   176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1762,    -1,    -1,    -1,    -1,   885,    -1,    -1,    -1,    -1,
      -1,    -1,   892,  1775,    -1,    -1,    -1,    -1,   898,    -1,
    1381,    -1,    -1,  1384,    -1,    -1,    -1,    -1,    -1,    -1,
     910,    -1,    -1,   913,   103,   915,    -1,    -1,    -1,    -1,
     920,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
      -1,    -1,    -1,    -1,   309,   310,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,   568,
      -1,    -1,   962,    -1,    -1,    -1,    -1,    -1,   577,    -1,
     579,    -1,    -1,    -1,    -1,    -1,   165,    -1,  1605,    -1,
      -1,   590,    -1,   592,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   608,
     609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1894,   202,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   632,    -1,  1025,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,    -1,    -1,
    1922,    -1,  1042,  1043,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1940,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   459,    -1,  1968,  1969,    -1,    -1,
    1972,    -1,    -1,    -1,    -1,  1095,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1586,    -1,    -1,   297,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   499,    -1,  2008,    -1,    -1,    -1,
      -1,  1131,    -1,   508,   743,  1762,    -1,    -1,    -1,    -1,
    1140,    -1,    -1,    -1,    -1,    -1,    -1,   336,  1148,    -1,
     339,    -1,    -1,   528,    -1,    -1,    -1,    -1,    -1,   534,
      -1,    -1,    -1,    -1,   353,    -1,    -1,    -1,   357,    -1,
      -1,    -1,    -1,  2055,    -1,   784,    -1,  1177,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,  1195,    -1,   806,   807,    -1,
     575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   586,    -1,   588,    -1,    -1,   825,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1225,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1234,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   853,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,   885,    -1,    -1,    -1,
      -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,
      -1,   910,  1302,  1303,   913,    -1,   915,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1316,  1317,    -1,    -1,
    1801,  1321,  1322,    -1,    -1,    -1,   140,    -1,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1972,    -1,   536,    -1,    -1,
    1350,  1351,    -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1366,  1367,  1368,  1369,
    1370,    -1,    -1,    -1,    -1,   189,    -1,    -1,    -1,   568,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,   577,    -1,
     579,    -1,    -1,    -1,    -1,    -1,  1396,  1397,     1,    -1,
      -1,   590,    -1,   592,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   787,    -1,   789,    -1,  1025,    -1,    -1,   608,
     609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1042,  1043,    -1,    -1,    -1,   252,    -1,
     254,    -1,    -1,   632,    -1,   259,    49,    -1,  1448,    52,
      -1,    54,   827,    56,    -1,    -1,   645,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,   846,    -1,  1473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   297,    -1,    -1,  1095,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,   316,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1148,
      -1,    -1,    -1,    -1,    -1,    -1,  1546,  1547,    -1,   152,
      -1,    -1,   155,   156,   743,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,  1177,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   391,    -1,  1579,
       1,    -1,    -1,     4,    -1,    -1,  1195,    -1,    -1,  1589,
      -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,    -1,
      -1,   425,    -1,    -1,    -1,    -1,  1225,   806,   807,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   825,   451,    59,    -1,
    1640,    -1,    -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,
      -1,  1651,    -1,    -1,  1654,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,   853,  1665,    -1,    -1,    -1,    -1,
      -1,  1046,    -1,    -1,    -1,    -1,    -1,  1052,  1053,    -1,
      -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1302,  1303,    -1,   885,    -1,    -1,    -1,
    1700,    -1,    -1,   892,    -1,  1705,  1706,  1316,  1317,    -1,
    1710,  1711,  1321,  1322,    -1,    -1,   530,   531,    -1,   140,
      -1,   910,    -1,    -1,   913,   146,   915,    -1,   149,  1729,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1350,  1351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   573,
      -1,    -1,    -1,   577,    -1,   579,    -1,    -1,   189,    -1,
      -1,    -1,    -1,   962,    -1,  1775,    -1,    -1,   592,    -1,
     201,    -1,    -1,    -1,    -1,    -1,    -1,  1396,  1397,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,   252,    55,   254,  1209,    -1,  1025,    -1,   259,  1448,
      -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,  1042,  1043,   669,    -1,    -1,   672,   673,
      -1,   675,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     684,    -1,    -1,   687,   688,   689,   297,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1894,   316,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1095,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    -1,  1311,  1546,  1547,    -1,
    1940,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   767,    -1,    -1,    -1,    -1,    -1,  1148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1968,  1969,
     391,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1177,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,
      -1,    -1,  1377,    -1,   425,    -1,  1195,    -1,  2008,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     451,  1640,    -1,    -1,    -1,    -1,  1225,  2037,    -1,    -1,
      -1,    -1,  1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2055,  1665,    -1,     1,    -1,
      -1,     4,    -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,
      -1,   885,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   898,   899,    -1,    -1,    -1,    -1,
      -1,  1700,    -1,    -1,    -1,    -1,  1705,  1706,    -1,    -1,
      -1,  1710,  1711,    -1,    -1,    -1,    -1,    -1,    -1,   530,
     531,    -1,    -1,  1302,  1303,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1316,  1317,    -1,
      -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,    -1,    -1,   577,    -1,   579,    -1,
     103,  1350,  1351,    -1,    -1,    -1,  1775,    -1,    -1,    -1,
      -1,   592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1566,    -1,    -1,    -1,    -1,  1571,   140,    -1,    -1,
      -1,    -1,  1577,   146,  1579,    -1,   149,  1396,  1397,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   189,    -1,   669,    -1,
      -1,   672,   673,    -1,   675,    -1,    -1,    -1,   201,  1448,
      -1,    -1,    -1,   684,    -1,    -1,   687,   688,   689,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,
      93,    94,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   252,
      -1,   254,    -1,  1922,   127,    -1,   259,  1131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1140,    -1,    -1,    -1,
      -1,  1940,    -1,    -1,  1148,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   297,    -1,    -1,  1546,  1547,  1968,
    1969,    -1,    -1,  1177,    -1,    -1,    -1,    -1,  1743,    -1,
      -1,  1746,    -1,   316,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2008,
    1589,  1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1802,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1258,    -1,  2055,    -1,   391,    -1,
      -1,  1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1651,    -1,   885,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1665,   898,   899,    -1,
      -1,    -1,   425,    83,  1859,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   306,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   451,    -1,
      -1,  1700,    -1,    -1,    -1,    -1,  1705,  1706,    -1,    -1,
      -1,  1710,  1711,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,  1366,  1367,  1368,  1369,  1370,  1371,  1372,    -1,
      -1,    -1,    -1,    -1,    -1,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1396,  1397,    -1,    -1,  1775,   530,   531,   189,
      -1,    -1,    -1,    -1,    -1,    -1,  1971,    -1,    -1,    -1,
      -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     573,    -1,    -1,    -1,   577,    -1,   579,    -1,    -1,    -1,
      -1,   454,    -1,   456,    -1,    -1,    -1,    -1,    -1,   592,
      -1,    -1,   465,   466,   254,    -1,    -1,    -1,    -1,  1473,
      -1,  2036,  2037,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,  2078,    53,  1894,    55,    -1,    -1,    -1,
    1131,    -1,    -1,   656,    -1,    -1,   316,    -1,    -1,  1140,
      -1,    -1,    -1,    72,    -1,    -1,   669,  1148,    -1,   672,
     673,    -1,   675,  1922,    -1,    -1,   336,    -1,    -1,    -1,
      -1,   684,    -1,    -1,   687,   688,   689,    -1,    -1,    -1,
      -1,  1940,    -1,    -1,    -1,    -1,  1177,    -1,    -1,  1573,
      -1,   574,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1195,    -1,    -1,    -1,    -1,  1968,
    1969,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1606,    -1,  1215,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2008,
      18,    -1,    -1,    -1,   767,   425,  1640,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1258,    -1,    -1,
    1654,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,   455,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2055,    -1,    -1,    -1,
      -1,  1685,    -1,    71,    -1,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    93,    94,    95,    96,    97,
      -1,    99,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   181,   124,   125,    -1,    -1,
     530,   531,    -1,    -1,   132,    -1,   536,    -1,    -1,    -1,
      -1,    -1,   885,    -1,    -1,  1366,  1367,  1368,  1369,  1370,
    1371,  1372,    -1,   151,   152,   898,   899,   155,   156,    -1,
      -1,  1775,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,  1396,  1397,    -1,    -1,   177,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   800,    -1,    -1,
     590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   609,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       0,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1473,    -1,    -1,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   878,   879,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   889,   890,   891,    -1,
      -1,   894,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    77,    -1,   375,
      51,    -1,    53,   379,   380,    -1,  1940,    -1,    -1,    -1,
      -1,    -1,    -1,   389,   390,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,   743,    -1,    -1,    -1,    -1,   404,   405,
      -1,    -1,  1573,    -1,  1968,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   976,    -1,    -1,    -1,   767,    -1,   425,
      -1,    -1,    -1,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   784,  1606,    -1,    -1,  1131,    -1,
      -1,    -1,    -1,    -1,  2008,    -1,    -1,  1140,    -1,   455,
      -1,    -1,    -1,    -1,    -1,  1148,   806,   807,    -1,  1022,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1640,
      -1,    -1,    -1,    -1,    -1,   825,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1654,  1177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1195,    -1,    -1,    -1,  1069,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1685,  1078,  1079,  1080,  1081,    -1,
      -1,    -1,  1215,  1086,  1087,    -1,    -1,    -1,    -1,   239,
      -1,    -1,    -1,  1096,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   892,   253,    -1,    -1,    -1,    -1,    -1,   899,
      -1,    -1,    -1,   263,  1117,    -1,    -1,  1120,    -1,  1122,
      -1,    -1,    -1,    -1,   274,  1258,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,   293,    -1,    -1,    -1,    -1,    -1,   299,
     300,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1775,   315,    -1,    -1,    -1,    -1,
      -1,    -1,   962,    -1,  1177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   335,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1203,    -1,    -1,    -1,    -1,    -1,    -1,  1210,    -1,  1212,
    1213,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1224,    -1,  1226,    -1,  1228,    -1,  1230,    -1,    -1,
      -1,    -1,  1235,  1366,  1367,  1368,  1369,  1370,  1371,  1372,
      -1,    -1,   392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1396,  1397,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   423,   720,   721,   722,   723,   724,   725,
     726,   727,   728,   729,   730,   731,   732,   733,   734,   735,
     736,   737,   738,    -1,  1297,    -1,    -1,    -1,    -1,   449,
      -1,  1304,  1305,   453,    -1,  1095,    -1,    48,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   472,    -1,    -1,  1328,   476,   477,    -1,  1940,
     480,    -1,  1335,    74,    -1,    -1,  1339,    -1,    -1,    -1,
    1473,    -1,    -1,    -1,    -1,   495,   496,   497,   498,    -1,
      -1,    -1,    -1,    -1,   800,    -1,    -1,  1968,    -1,  1362,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   522,  1376,    -1,    -1,    -1,    -1,    -1,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   133,    -1,   135,    -1,    -1,  2008,    -1,    -1,
      -1,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1416,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,   167,    -1,    -1,    -1,
      -1,    -1,   582,    -1,    -1,  1225,    -1,    -1,    -1,   589,
    1573,    -1,    -1,    -1,    -1,   595,    -1,    -1,    -1,  1452,
     191,    -1,    -1,    -1,    -1,    -1,    -1,  1460,    -1,  1462,
      -1,    -1,    -1,    -1,  1254,    -1,    -1,    -1,    -1,   619,
     620,    -1,    -1,  1606,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   229,    -1,
      -1,    -1,   233,    -1,    -1,   236,   237,    -1,    -1,   240,
      -1,    -1,   243,   244,    -1,    -1,    -1,  1640,  1511,  1512,
      -1,    -1,   958,    -1,    -1,    -1,    -1,   963,    -1,    -1,
      -1,  1654,    -1,  1526,  1527,    -1,  1529,    -1,   974,    -1,
      -1,  1321,  1322,    -1,    -1,  1538,    -1,    -1,    -1,    -1,
     690,    -1,    -1,    -1,    -1,  1548,  1549,    -1,    -1,    -1,
      -1,    -1,  1685,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   309,    -1,
    1016,   312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   333,   334,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,   759,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   774,    -1,    -1,    -1,   778,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   787,    -1,    -1,
      -1,    -1,  1775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1448,   809,
      -1,    -1,    -1,  1666,  1667,    -1,    -1,    -1,   818,    -1,
      -1,    -1,   165,    -1,   824,    -1,  1679,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1148,   444,    -1,    -1,    -1,    -1,   201,   202,
      -1,   861,    -1,  1716,  1717,   865,    -1,    -1,    -1,   869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     233,    -1,    -1,    -1,    -1,    -1,   896,   240,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   499,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     511,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,
      -1,    -1,    -1,  1573,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1244,    -1,
      -1,    -1,   952,  1806,    -1,    -1,    -1,  1940,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1261,  1262,  1263,    -1,   312,
      -1,    -1,  1268,  1269,  1827,    -1,    -1,  1830,  1831,    -1,
      -1,    -1,    -1,    -1,  1837,  1968,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   336,   337,   586,  1292,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1651,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2008,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1329,  1330,  1035,   627,   628,    -1,  1039,
      -1,    -1,    -1,    -1,    -1,    -1,  1046,    -1,    -1,   640,
      -1,    -1,    -1,    -1,    -1,    -1,  1056,    -1,    -1,    -1,
      -1,    -1,    -1,  1063,    -1,    -1,    -1,    -1,    -1,    -1,
    1710,  1711,  1072,    -1,  1074,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   444,   445,    -1,   447,   448,  1106,    -1,    -1,    -1,
    1110,    -1,   455,    -1,    -1,    -1,   459,  1970,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,    -1,    -1,
      -1,    -1,  1132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   500,    -1,    -1,
      -1,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   763,   764,    -1,    -1,    -1,    -1,   769,    -1,
      -1,  2034,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   536,    -1,    -1,    -1,    -1,    -1,   790,
      -1,    -1,   793,   794,    -1,   796,  2059,   798,   799,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2074,    -1,  1223,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2090,    -1,    -1,
      -1,    -1,    -1,    -1,   587,    -1,   837,   590,  1248,    -1,
     841,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   608,   609,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   619,    -1,    -1,    -1,
     623,    -1,  1922,    -1,    -1,    -1,    -1,   630,    -1,   632,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   907,   908,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     921,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1342,    -1,    -1,    -1,  1346,    -1,    -1,  1645,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1383,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   391,    -1,    -1,
     743,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   759,   760,    -1,   149,
      -1,    -1,    -1,   153,    -1,    -1,   769,   770,    -1,   772,
     773,    -1,    -1,  1433,    -1,   165,  1436,    -1,    -1,    -1,
      -1,   784,    48,    -1,   787,    -1,   789,   790,    -1,    -1,
      -1,    -1,    -1,   796,  1454,    -1,    -1,    -1,    -1,   189,
      -1,  1052,    -1,   806,   807,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   202,    -1,    -1,   205,    -1,    -1,    -1,    -1,
      -1,    -1,   825,    -1,    -1,    -1,   829,    -1,    -1,    -1,
     833,    -1,    -1,    -1,   837,   838,    -1,    -1,   841,   842,
      -1,    -1,  1093,    -1,    -1,    -1,   849,  1507,    -1,  1100,
      -1,    -1,  1103,    -1,   120,   189,  1516,    -1,    -1,    -1,
    1520,    -1,    -1,    -1,   254,    -1,    -1,   133,   202,   135,
      -1,    -1,    -1,    -1,  1534,  1535,   530,   531,   268,    -1,
      -1,   215,    -1,   217,    -1,    -1,    -1,    -1,    -1,   892,
     893,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1560,    -1,    -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   308,    -1,
     923,    -1,    -1,    -1,    -1,    -1,   316,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1901,   336,    -1,   338,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   962,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     236,   237,    -1,   307,   240,    -1,    -1,   243,   244,    -1,
    1231,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1239,  1240,
      -1,    -1,    -1,    -1,    -1,  1655,  1656,    -1,    -1,    -1,
      -1,   391,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   669,    -1,    -1,    -1,    -1,
      -1,   675,    -1,    -1,    -1,    -1,  1029,    -1,    -1,    -1,
     684,    -1,    -1,    -1,    -1,   425,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1046,  1047,    -1,  2002,  1298,  2004,   703,
    1053,    -1,    -1,    -1,    -1,    -1,  1307,    -1,    -1,  1310,
      -1,  1312,  1313,    -1,    -1,   455,    -1,   333,   334,    -1,
      -1,   461,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   349,    -1,   739,    -1,  2043,    -1,    -1,
      -1,    -1,  1095,    -1,    -1,    -1,    -1,  1100,  1101,    -1,
    1103,  1104,  1353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2076,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1789,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     530,   531,    -1,    -1,    -1,    -1,   536,   481,    -1,    -1,
      -1,    -1,    -1,   487,    -1,    -1,  1816,    -1,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2125,
      -1,    -1,    -1,    -1,    -1,  1835,  1427,    -1,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     590,    -1,    -1,  1863,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   609,
      -1,   611,  1225,    -1,    -1,    -1,    -1,    -1,  1231,  1232,
    1890,    -1,    -1,  1893,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,    -1,    -1,   511,   580,    -1,    -1,    -1,
      -1,  1254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1515,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   609,    -1,    -1,    -1,   669,
      -1,    -1,   672,   673,    -1,   675,    -1,    -1,   622,    -1,
      -1,    -1,    -1,  1544,   684,  1298,  1299,   687,   688,   689,
      -1,    -1,    -1,    -1,  1307,  1308,    -1,  1310,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1321,  1322,
      -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,    -1,    -1,
      -1,    -1,  1583,    -1,    -1,    -1,    -1,  1997,    -1,    -1,
      -1,    -1,    -1,   677,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   743,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,   628,    -1,    -1,    -1,    -1,   701,   702,    -1,
      -1,   705,    -1,   707,   640,    -1,    -1,   767,    -1,   713,
      -1,   715,   716,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   784,    -1,    -1,    -1,    -1,  1650,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   743,
      -1,    -1,    -1,    -1,    -1,    -1,   806,   807,    -1,    -1,
      -1,    -1,   756,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   767,    -1,   825,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1448,    -1,   781,    -1,    -1,
     784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1475,    -1,    -1,    -1,    -1,   811,    -1,    -1,
     814,    -1,    -1,    -1,  1735,  1736,    -1,    -1,    -1,    -1,
      -1,    -1,  1743,    -1,    -1,    -1,  1747,   763,   764,    -1,
      -1,    -1,   892,   769,    -1,    -1,    -1,    -1,    -1,   899,
      -1,    -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,    -1,
      -1,    -1,   912,    -1,   790,    -1,    -1,   793,   794,    -1,
     796,    -1,   798,   799,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1567,   899,    -1,    -1,    -1,    -1,
      -1,   837,   962,    -1,    -1,   841,    -1,    -1,   912,   913,
    1583,    -1,    -1,    -1,    -1,    -1,   920,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   948,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   962,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   970,    -1,    -1,    -1,
      -1,   907,   908,   977,    -1,    -1,    -1,    -1,  1651,    -1,
      -1,    -1,    -1,    -1,    -1,   921,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1926,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1023,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1095,    -1,  1710,  1711,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1728,  1729,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1393,
      -1,  1744,  1396,  1397,    -1,    -1,    -1,    -1,  1402,    -1,
    1140,    -1,  1406,    -1,  1408,    -1,  1090,    -1,  1092,    -1,
    1094,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2036,  1052,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1194,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,    -1,  1093,    -1,    -1,
      -1,  1165,  1166,    -1,  1100,  1225,    -1,  1103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1850,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1254,    -1,    -1,    -1,  1258,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1229,    -1,    -1,  1552,    -1,
      -1,  1235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1922,
    1254,    -1,    -1,  1926,  1927,    -1,    -1,  1930,    -1,    -1,
      -1,  1321,  1322,    -1,    -1,    -1,    -1,  1271,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,
      -1,    -1,  1286,    -1,    -1,  1289,    -1,    -1,    -1,    -1,
      -1,    -1,  1616,  1617,    -1,  1231,    -1,    -1,  1971,    -1,
      -1,    -1,    -1,  1239,  1240,    -1,  1366,  1367,  1368,    -1,
      -1,  1371,  1372,    -1,    -1,    -1,  1640,    -1,  1378,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1341,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1357,  1358,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1298,  2036,  2037,    -1,  1426,    -1,    -1,    -1,
      -1,  1307,    -1,    -1,  1310,    -1,  1312,  1313,    -1,    -1,
      -1,    -1,  1386,    -1,    -1,    -1,    -1,    -1,  1448,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1404,    -1,    -1,  1407,    -1,  2078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1353,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1758,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1767,  1448,  1769,    -1,    -1,  1772,  1773,
      -1,  1775,    -1,    -1,  1458,  1459,  1780,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1468,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1486,    -1,  1488,    -1,    -1,    -1,    -1,    -1,
      -1,  1427,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,  1573,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,  1605,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1880,    -1,    -1,    -1,
      -1,  1885,  1886,    -1,    -1,    -1,    -1,    72,    -1,  1573,
      -1,    -1,    -1,    -1,  1578,    -1,    -1,    -1,    -1,  1515,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,  1544,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1948,  1685,  1950,    -1,    -1,  1953,
    1954,    -1,  1636,    -1,  1958,  1959,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1710,  1711,    -1,    -1,   159,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,  1683,
      -1,    -1,  1686,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1762,    -1,    -1,  2029,  2030,  2031,    -1,    -1,
      -1,    -1,    -1,    -1,  1650,    -1,    -1,  1721,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,  2050,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2064,  2065,  2066,    -1,    71,    -1,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,  1735,
    1736,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,  1747,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,  1894,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
     177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1922,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,  1902,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,  1969,
      -1,    -1,  1972,    -1,    -1,    -1,  1852,    -1,    71,    -1,
      73,    74,    -1,    76,    -1,    -1,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    -1,
      93,    94,    95,    96,    97,    -1,    99,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    71,    72,    73,    74,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    93,    94,    95,    96,    97,
      -1,    99,    -1,   101,   102,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,
      73,    74,    -1,    76,    -1,    -1,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    -1,
      93,    94,    95,    96,    97,    -1,    99,    -1,   101,   102,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   147,
     148,    -1,    -1,    -1,   152,   153,   154,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   146,   147,   148,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   177,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,   177,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,     5,    72,    -1,    -1,    -1,    -1,    77,    78,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,     5,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,
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
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,
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
      -1,   103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
       3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,
     163,   164,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,
       3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,
     163,   164,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,     3,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    72,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,   107,   108,    -1,    -1,    -1,   163,   164,    -1,
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
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
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
      -1,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,   155,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    13,    14,    15,    16,    17,
      18,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,   156,    -1,    -1,    -1,   107,
     108,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,   155,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,   107,   108,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,
     156,   104,    -1,    -1,   107,   108,    -1,   163,   164,    71,
      -1,    73,    -1,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    18,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    49,   155,   156,    52,    -1,    54,   160,    56,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,   177,    73,    -1,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,    -1,   104,    -1,   106,
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
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
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
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,   107,   108,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,    -1,    -1,    72,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
     107,   108,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,    -1,   163,   164,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,   155,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    13,    14,    15,    16,    17,    18,    72,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,   107,   108,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
     155,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    72,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,   107,
     108,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    13,    14,    15,    16,    17,    18,    72,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,   155,    -1,    51,
      -1,    53,   107,   108,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,   107,   108,    -1,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    13,    14,    15,    16,
      17,   163,   164,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   107,   108,    -1,    -1,    -1,   163,   164,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      49,    -1,    -1,    52,    -1,    54,    72,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    49,    -1,    -1,
      52,    -1,    54,   132,    56,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   147,   148,
      -1,    73,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,    54,
     132,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,   148,    -1,    73,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,   159,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,   154,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    49,   124,   125,    52,    -1,    54,
      -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    49,   124,
     125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   179,   393,   394,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      99,   103,   104,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   119,   132,   152,   153,   155,   156,   163,   164,
     182,   183,   184,   199,   282,   283,   284,   285,   286,   287,
     288,   289,   290,   291,   292,   293,   295,   298,   300,   301,
     302,   303,   304,   305,   306,   307,   308,   310,   312,   313,
     314,   316,   317,   321,   322,   323,   324,   325,   327,   333,
     334,   335,   336,   347,   351,   385,   388,   398,   404,   406,
     412,   416,   421,   422,   423,   424,   425,   426,   427,   428,
     450,   468,   469,   470,   471,     0,   179,   104,   183,   199,
     286,   288,   298,   301,   304,   313,   317,   322,   118,   152,
      58,    61,    62,    64,   152,   152,   410,   411,   412,   309,
     310,   107,   108,   183,   365,   386,   387,   365,   152,   398,
     152,   152,     4,   104,   107,   108,   302,   307,   308,   152,
     199,   411,   416,   422,   423,   424,   426,   427,   428,   107,
     324,   157,   179,   289,   298,   301,   421,   425,   467,   468,
     471,   472,   177,   180,   149,   160,   176,   220,   368,    89,
     158,   405,   365,   180,   180,   180,   177,   107,   108,   152,
     199,   294,   407,   416,   417,   418,   419,   420,   421,   425,
     429,   430,   431,   432,   433,   439,     3,    47,    48,    50,
      55,   315,     3,   156,   199,   288,   302,   306,   308,   318,
     323,   401,   421,   425,   471,    69,   286,   288,   301,   313,
     317,   322,   402,   421,   425,    65,   307,   307,   302,   308,
     296,   307,   308,   315,   334,   302,   307,   302,   155,   410,
     158,   180,   152,   160,   228,   410,   410,   179,   277,   278,
     156,   298,   301,   469,   365,   365,   398,   176,   301,   152,
     199,   407,   416,   421,   430,   156,   199,   471,   399,   400,
      65,    66,    67,    68,   156,   174,   365,   374,   376,   380,
     382,   383,   323,    57,   154,   156,   199,   297,   301,   305,
     306,   312,   313,   319,   320,   321,   322,   326,   333,   334,
     351,   361,   363,   450,   463,   464,   465,   466,   471,   472,
     107,   108,   160,   183,   323,   439,   412,   152,   381,   382,
     152,   152,   118,   185,   186,    49,    52,    54,    56,    73,
     101,   102,   104,   106,   116,   117,   120,   121,   122,   124,
     125,   152,   156,   162,   165,   166,   167,   168,   181,   182,
     185,   187,   190,   198,   199,   200,   201,   204,   205,   206,
     207,   208,   209,   210,   211,   212,   213,   214,   215,   216,
     222,   323,   154,   156,   198,   199,   215,   217,   298,   323,
     366,   367,   384,   467,   472,   301,   422,   423,   424,   426,
     427,   428,   154,   154,   154,   154,   154,   154,   154,   156,
     298,   450,   469,   156,   163,   199,   217,   288,   289,   297,
     299,   301,   313,   320,   322,   356,   357,   360,   361,   362,
     463,   471,   152,   421,   425,   471,   152,   158,   104,   155,
     156,   160,   182,   184,   217,   369,   370,   371,   372,   373,
      22,   369,   152,   365,   228,   152,   158,   158,   158,   411,
     416,   418,   419,   420,   429,   431,   432,   433,   301,   417,
     430,   158,    99,   409,   156,   410,   447,   450,   410,   410,
     405,   277,   152,   410,   447,   405,   410,   410,   301,   407,
     152,   152,   300,   301,   298,   301,   179,   298,   467,   472,
     325,   160,   405,   277,   365,   365,   368,   288,   306,   403,
     421,   425,   160,   405,   277,   386,   301,   313,   301,   301,
     107,   324,   107,   108,   183,   323,   328,   386,   179,   183,
     364,   151,   179,     3,   293,   295,   301,   305,   228,   179,
     179,   409,   152,   409,   180,   217,   411,   416,   301,   152,
     179,   365,   396,   160,   365,   160,   365,   134,   163,   164,
     379,   154,   158,   365,   383,   154,   410,   410,   157,   179,
     299,   301,   313,   320,   322,   462,   463,   471,   472,   152,
     156,   164,   176,   199,   450,   452,   453,   454,   455,   456,
     457,   474,   199,   326,   471,   301,   320,   307,   302,   410,
     154,   299,   301,   464,   299,   450,   464,    10,   162,   167,
     350,   352,   353,   348,   350,   374,   176,   374,    13,    88,
     104,   107,   108,   182,   413,   414,   415,   154,   118,   152,
     198,   152,   152,   152,   201,   152,   198,   152,   104,   107,
     108,   302,   307,   308,   152,   198,   198,    19,    21,    85,
     156,   165,   166,   202,   203,   217,   224,   228,   336,   366,
     471,   158,   179,   152,   187,   156,   161,   156,   161,   121,
     123,   124,   125,   152,   155,   156,   160,   161,   201,   201,
     169,   163,   170,   171,   165,   166,   126,   127,   128,   129,
     172,   173,   130,   131,   164,   162,   174,   132,   133,   175,
     154,   158,   155,   179,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   176,   219,   220,   221,   152,
     199,   443,   444,   445,   446,   447,   154,   158,   154,   154,
     154,   154,   154,   154,   152,   410,   447,   450,   152,   447,
     450,   179,   298,   469,   179,   180,   180,   152,   164,   199,
     416,   434,   435,   436,   437,   438,   439,   440,   441,   442,
     134,   471,   180,   180,   365,   365,   179,   179,   179,   156,
     184,   179,   370,   159,   158,   473,   369,   155,   156,   159,
     373,   153,   217,   223,   152,   179,   179,   179,   179,   416,
     418,   419,   420,   429,   431,   432,   433,   154,   154,   154,
     154,   154,   154,   154,   417,   430,   410,   152,   368,   157,
     179,   228,   405,   179,   228,   407,   224,   367,   224,   367,
     407,   396,   228,   405,   409,   160,   160,   405,   277,   396,
     228,   405,   330,   331,   329,   160,   134,   301,   358,   359,
     362,   363,   154,   158,    70,   279,   280,   180,   301,   293,
     163,   217,   179,   416,   357,   398,   396,   157,   179,   152,
     378,   376,   377,    78,   311,   183,   160,   183,   439,   299,
     450,   464,   301,   305,   471,   179,   453,   454,   455,   157,
     179,    18,   217,   301,   452,   474,   410,   410,   450,   299,
     462,   472,   301,   183,   410,   299,   464,   323,   158,   473,
     365,   350,   160,   154,   367,   154,   154,   158,   152,   177,
     366,   187,   156,   366,   366,   366,   217,   366,   154,   366,
     366,   366,   179,   154,   165,   166,   203,    18,   303,   154,
     158,   154,   163,   164,   154,   223,   217,   160,   217,   183,
     217,   183,   116,   156,   183,   153,   191,   192,   193,   217,
     116,   156,   183,   336,   217,   191,   183,   201,   204,   204,
     204,   205,   205,   206,   206,   207,   207,   207,   207,   208,
     208,   209,   210,   211,   212,   213,   159,   224,   177,   185,
     156,   183,   217,   160,   217,   179,   444,   445,   446,   301,
     443,   410,   410,   217,   367,   152,   410,   447,   450,   152,
     447,   450,   179,   179,   157,   157,   152,   416,   435,   436,
     437,   440,    18,   301,   434,   438,   152,   410,   456,   474,
     410,   410,   474,   152,   410,   456,   410,   410,   180,   216,
     365,   157,   158,   157,   158,   474,   474,   134,   355,   356,
     357,   355,   365,   179,   215,   216,   217,   408,   473,   369,
     371,   151,   179,   154,   158,   179,   355,   183,   407,   183,
     154,   154,   154,   154,   154,   154,   152,   410,   447,   450,
     152,   410,   447,   450,   407,   185,   450,   217,   308,   323,
     448,   228,   358,   154,   154,   154,   154,   394,   395,   228,
     151,   179,   396,   228,   405,   395,   228,   160,   160,   160,
     337,   180,   180,   183,   281,   365,    18,    71,    73,    76,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    93,    94,    95,    96,    97,    99,   107,   108,
     119,   179,   224,   225,   226,   227,   228,   229,   230,   232,
     233,   243,   249,   250,   251,   252,   253,   254,   259,   260,
     266,   267,   268,   282,   301,   305,   365,   406,    70,   177,
     180,   180,   180,   355,   180,   397,   395,   286,   288,   298,
     389,   390,   391,   392,   384,   176,   375,   375,   352,   410,
     410,   299,   464,   156,   163,   199,   217,   323,   217,   301,
     358,   154,   154,   154,     5,   301,   410,   452,   160,   183,
     439,    10,   353,   151,   176,   354,   160,   352,   160,   154,
     414,   191,   154,   158,   179,   158,   154,   154,   158,   154,
     201,   154,   154,   154,   201,    18,   303,   217,   154,   154,
     153,   160,   201,   157,   180,   191,   157,   157,   116,   120,
     122,   184,   194,   195,   196,   154,   158,   194,   157,   158,
     151,   215,   159,   154,   194,   180,   370,   358,   154,   154,
     154,   443,   179,   179,   358,   358,   440,   154,   154,   154,
     154,   152,   416,   439,   434,   438,   179,   179,   157,   180,
     474,   179,   179,   180,   180,   180,   180,   368,   194,   134,
     168,   180,   180,   151,   369,   217,   410,   153,   217,   355,
     180,   176,   152,   410,   447,   450,   152,   410,   447,   450,
     179,   179,   409,   154,   146,   168,   180,   449,   158,   180,
     180,   397,   389,   395,   228,   397,   337,   337,   337,     3,
       5,    10,    73,   151,   283,   290,   291,   298,   301,   338,
     343,   467,   154,   158,   158,   177,   152,    61,    62,   177,
     228,   282,   406,   152,    18,   226,   152,   152,   177,   365,
     177,   365,   163,   365,   160,   225,   152,   152,   152,   228,
     217,   218,   218,    14,   269,    74,   234,   177,   180,   230,
      78,   177,   365,    91,   255,   364,   301,   159,   281,   177,
     157,   157,   180,   158,   397,   407,   180,   177,   180,   177,
     180,   154,   367,   381,   381,   473,   350,   350,   179,   180,
     180,   180,   217,   180,   152,   410,   456,   450,   300,     5,
     163,   180,   217,   352,   410,   410,   323,   365,   160,   216,
     352,   473,   151,   179,   154,   297,   183,    78,   188,   189,
     366,   201,   201,   201,   201,   201,   160,   370,   158,   151,
     197,   156,   195,   197,   197,   157,   158,   123,   155,   193,
     157,   223,   215,   177,   157,   473,   180,   152,   410,   447,
     450,   358,   358,   180,   180,   154,   152,   410,   447,   450,
     152,   410,   456,   416,   410,   410,   358,   358,   157,   357,
     360,   360,   361,   154,   158,   158,   154,   180,   216,   216,
     157,   157,   180,   180,   154,   217,   179,   179,   358,   358,
     368,   410,   158,   217,   217,   308,   323,   157,   154,   151,
     180,   397,   151,   151,   151,   151,   298,   298,   336,   344,
     467,   298,   343,   152,   332,   177,   177,   152,   159,   199,
     339,   340,   346,   416,   417,   430,   158,   177,   365,   179,
     365,   191,   177,   228,   177,   228,   224,    80,   154,   224,
     235,   282,   284,   287,   293,   301,   305,   146,   147,   148,
     153,   154,   177,   224,   244,   245,   246,   282,   177,   177,
     224,   177,   370,   177,   224,   223,   224,   111,   112,   113,
     114,   115,   261,   263,   264,   177,    98,   177,    84,   152,
     152,   180,   151,   177,   177,   152,   226,   228,   410,   177,
     154,   179,   151,   151,   179,   158,   158,   151,   160,   160,
     157,   157,   157,   180,   154,   179,   217,   217,   180,   157,
     180,   473,   349,   350,   354,   354,   370,   473,   151,   389,
     451,   452,   154,   159,   154,   158,   159,   370,   473,   223,
     121,   194,   195,   156,   195,   156,   195,   157,   151,   154,
     179,   180,   180,   154,   154,   179,   179,   180,   180,   180,
     179,   179,   157,   180,   154,   410,   358,   358,   180,   180,
     224,   449,   151,   151,   332,   332,   332,   339,   152,   199,
     341,   342,   447,   458,   459,   460,   461,   177,   158,   177,
     339,   177,   384,   411,   416,   217,   301,   158,   177,   345,
     346,   345,   365,   134,   362,   363,   154,   154,   152,   226,
     154,   224,   301,   146,   147,   148,   168,   177,   247,   248,
     226,   225,   177,   248,   154,   159,   224,   153,   224,   225,
     246,   177,   473,   154,   154,   154,   228,   263,   264,   152,
     217,   152,   185,   235,   201,   256,   110,     1,   226,   410,
     390,   179,   179,   352,   352,   157,   358,   180,   180,   157,
     157,   151,   350,   160,   473,   151,   180,   154,   217,   189,
     217,   473,   151,   157,   157,   194,   194,   358,   154,   154,
     358,   358,   154,   154,   157,   158,   134,   357,   134,   157,
     180,   180,   154,   154,   157,   217,   177,   459,   460,   461,
     301,   458,   158,   177,   410,   410,   177,   154,   416,   410,
     226,    77,    78,   160,   238,   239,   240,   154,   224,    75,
     226,   224,   153,   224,    75,   177,   107,   153,   224,   225,
     246,   153,   224,   226,   245,   248,   248,   177,   224,   151,
     160,   240,   226,   152,   179,   177,   185,   154,   159,   154,
     154,   158,   159,   254,   258,   365,   407,   473,   473,   180,
     157,   157,   160,   352,   151,   151,   151,   157,   157,   180,
     180,   180,   179,   180,   154,   154,   154,   154,   154,   458,
     410,   340,     1,   216,   236,   237,   408,     1,   159,     1,
     179,   226,   238,    75,   177,   154,   226,    75,   177,   168,
     168,   226,   225,   248,   248,   177,   107,   224,   168,   168,
      75,   153,   224,   153,   224,   225,   177,     1,   179,   179,
     265,   299,   301,   467,   159,   177,   156,   185,   270,   271,
     272,   226,   201,   191,    75,   109,   255,   257,   151,   151,
     154,   352,   473,   154,   154,   154,   360,   152,   410,   447,
     450,   342,   134,     1,   158,   159,   151,   275,   276,   282,
     226,    75,   177,   226,   224,   153,   153,   224,   153,   224,
     153,   224,   225,   153,   224,   153,   224,   226,   168,   168,
     168,   168,   151,   275,   265,   180,   152,   199,   407,   458,
     183,   159,   104,   152,   154,   159,   158,    75,   154,   226,
     152,   226,   226,   473,   151,   179,   216,   236,   239,   241,
     242,   282,   226,   168,   168,   168,   168,   153,   153,   224,
     153,   224,   153,   224,   241,   180,   177,   262,   301,   270,
     157,   216,   177,   270,   272,   226,   224,   110,   110,   151,
     358,   226,   231,   180,   239,   153,   153,   224,   153,   224,
     153,   224,   180,   262,   215,   154,   159,   185,   154,   154,
     159,   154,   258,    75,   253,   180,     1,   226,   151,   231,
     151,   154,   228,   185,   273,   152,   177,   273,   226,    75,
     154,   228,   158,   159,   216,   154,   226,   185,   183,   274,
     154,   177,   154,   158,   177,   183
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   178,   179,   180,   181,   181,   181,   181,   181,   182,
     182,   182,   182,   182,   182,   182,   183,   183,   184,   184,
     185,   186,   186,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   188,   188,
     189,   189,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   191,   191,   192,   192,
     193,   193,   194,   194,   195,   195,   195,   195,   195,   195,
     195,   196,   196,   196,   197,   197,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     199,   199,   199,   200,   200,   200,   200,   201,   201,   201,
     201,   201,   201,   201,   201,   201,   202,   202,   202,   202,
     203,   203,   204,   204,   205,   205,   205,   205,   206,   206,
     206,   207,   207,   207,   208,   208,   208,   208,   208,   209,
     209,   209,   210,   210,   211,   211,   212,   212,   213,   213,
     214,   214,   215,   215,   215,   216,   217,   217,   217,   218,
     218,   219,   219,   220,   220,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   222,   222,   223,   223,
     223,   223,   224,   224,   225,   225,   226,   226,   226,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   226,   227,
     227,   228,   228,   229,   229,   230,   230,   230,   230,   230,
     231,   231,   231,   232,   233,   233,   233,   233,   233,   233,
     233,   234,   234,   235,   235,   235,   235,   236,   236,   236,
     237,   237,   238,   238,   238,   238,   238,   239,   239,   240,
     241,   241,   242,   242,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   244,   244,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   246,   246,   246,   247,   247,   248,   248,
     248,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   250,   250,   251,   252,   253,   254,   254,   255,   255,
     256,   256,   257,   258,   258,   258,   258,   258,   258,   259,
     259,   260,   260,   260,   261,   261,   262,   262,   263,   263,
     263,   263,   264,   265,   265,   265,   265,   265,   266,   267,
     267,   268,   268,   268,   268,   268,   269,   269,   270,   270,
     271,   271,   272,   272,   273,   273,   273,   274,   274,   275,
     275,   276,   276,   277,   277,   278,   278,   279,   279,   280,
     280,   281,   281,   282,   282,   282,   283,   283,   284,   284,
     284,   284,   284,   285,   285,   285,   286,   286,   286,   287,
     287,   287,   287,   287,   288,   288,   289,   289,   290,   290,
     290,   291,   291,   291,   291,   291,   292,   292,   293,   293,
     293,   293,   294,   294,   295,   295,   295,   295,   296,   296,
     296,   297,   297,   297,   298,   298,   298,   299,   299,   299,
     300,   300,   301,   301,   302,   302,   303,   303,   303,   303,
     303,   304,   305,   305,   305,   306,   306,   307,   307,   307,
     307,   307,   307,   307,   307,   307,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   309,   309,   310,   311,   311,   312,
     312,   312,   312,   312,   313,   313,   314,   314,   314,   314,
     315,   315,   315,   315,   315,   315,   316,   316,   316,   316,
     317,   318,   317,   317,   319,   319,   319,   319,   320,   320,
     320,   321,   321,   321,   321,   322,   322,   322,   323,   323,
     323,   323,   323,   323,   324,   324,   324,   325,   325,   326,
     326,   328,   327,   329,   327,   330,   327,   331,   327,   327,
     332,   332,   333,   333,   334,   334,   335,   335,   335,   336,
     336,   336,   336,   336,   336,   336,   336,   337,   337,   338,
     338,   338,   338,   338,   338,   338,   338,   338,   338,   338,
     339,   339,   339,   340,   340,   340,   341,   341,   341,   342,
     343,   343,   344,   344,   345,   345,   346,   347,   348,   347,
     347,   347,   347,   349,   347,   347,   347,   347,   347,   350,
     350,   351,   351,   352,   352,   352,   352,   353,   353,   354,
     354,   354,   355,   355,   355,   355,   355,   355,   355,   356,
     356,   356,   356,   357,   357,   358,   358,   358,   358,   359,
     359,   359,   359,   360,   360,   360,   360,   360,   361,   361,
     361,   361,   361,   362,   362,   363,   363,   364,   364,   365,
     365,   365,   366,   366,   366,   367,   367,   368,   368,   368,
     368,   369,   369,   370,   370,   370,   370,   370,   371,   371,
     372,   372,   373,   373,   373,   373,   373,   374,   374,   375,
     375,   377,   376,   378,   376,   376,   376,   379,   379,   379,
     379,   380,   380,   380,   380,   381,   381,   382,   382,   383,
     383,   384,   384,   384,   384,   385,   385,   385,   386,   386,
     387,   387,   388,   388,   388,   388,   389,   389,   390,   390,
     391,   391,   391,   392,   392,   393,   393,   394,   394,   395,
     395,   396,   397,   398,   398,   398,   398,   398,   398,   398,
     398,   398,   398,   398,   399,   398,   400,   398,   401,   398,
     402,   398,   403,   398,   404,   404,   404,   405,   405,   406,
     406,   406,   406,   406,   406,   406,   406,   406,   406,   407,
     407,   407,   408,   409,   409,   410,   410,   411,   411,   412,
     413,   413,   414,   414,   414,   415,   415,   415,   415,   415,
     415,   416,   416,   417,   417,   417,   417,   418,   418,   418,
     418,   419,   419,   419,   419,   419,   419,   419,   420,   420,
     420,   420,   421,   421,   421,   422,   422,   422,   422,   422,
     423,   423,   423,   423,   424,   424,   424,   424,   424,   424,
     425,   425,   425,   426,   426,   426,   426,   426,   427,   427,
     427,   427,   428,   428,   428,   428,   428,   428,   429,   429,
     430,   430,   430,   430,   431,   431,   431,   431,   432,   432,
     432,   432,   432,   432,   432,   433,   433,   433,   433,   433,
     434,   434,   434,   434,   434,   435,   435,   435,   436,   436,
     436,   436,   437,   437,   437,   438,   438,   438,   438,   438,
     439,   439,   440,   440,   440,   441,   441,   442,   442,   443,
     443,   443,   444,   444,   444,   444,   444,   445,   445,   445,
     445,   446,   446,   446,   447,   447,   447,   447,   447,   448,
     448,   448,   448,   448,   448,   449,   449,   450,   450,   450,
     450,   451,   451,   452,   452,   452,   452,   453,   453,   453,
     453,   453,   454,   454,   454,   454,   455,   455,   455,   456,
     456,   456,   457,   457,   457,   457,   457,   457,   458,   458,
     458,   459,   459,   459,   459,   459,   460,   460,   460,   460,
     461,   461,   462,   462,   462,   463,   463,   464,   464,   464,
     464,   464,   464,   465,   465,   465,   465,   465,   465,   465,
     465,   465,   465,   466,   466,   466,   466,   467,   467,   467,
     468,   468,   469,   469,   469,   469,   469,   469,   470,   470,
     470,   470,   470,   470,   471,   471,   471,   472,   472,   473,
     473,   474,   474
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
       1,     2,     2,     2,     3,     5,    10,     7,     5,    10,
       7,     5,     7,     1,     1,     1,     2,     1,     3,     1,
       1,     3,     2,     3,     3,     2,     2,     1,     2,     2,
       0,     1,     2,     3,     4,     6,     5,     7,     6,     7,
       7,     8,     4,     6,     5,     7,     1,     3,     4,     5,
       4,     3,     5,     1,     2,     3,     3,     3,     5,     5,
       5,     5,     3,     5,     5,     5,     3,     4,     5,     5,
       5,     5,     7,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       6,     3,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     2,     3,     3,     2,     3,     2,     3,
       3,     6,     2,     2,     3,     3,     3,     3,     3,     3,
       5,     1,     1,     5,     5,     4,     0,     1,     4,     6,
       1,     3,     4,     3,     5,     3,     3,     6,     7,     3,
       5,     3,     3,     4,     8,     9,     0,     2,     1,     1,
       1,     1,     2,     1,     2,     2,     2,     1,     3,     1,
       1,     6,     8,    10,    12,    14,     0,     1,     0,     1,
       1,     3,     4,     7,     0,     1,     3,     1,     3,     0,
       1,     1,     2,     0,     1,     4,     5,     0,     1,     3,
       4,     1,     3,     2,     2,     1,     7,     5,     1,     1,
       1,     1,     1,     2,     3,     6,     3,     3,     4,     1,
       2,     2,     3,     8,     8,     8,     5,     9,     2,     2,
       5,     3,     5,     4,     3,     4,     4,     7,     2,     1,
       1,     1,     3,     6,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     2,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     5,     0,     1,     1,
       2,     2,     3,     3,     1,     3,     1,     2,     2,     2,
       4,     4,     4,     4,     1,     1,     1,     2,     2,     3,
       1,     0,     3,     2,     1,     2,     2,     3,     1,     2,
       2,     1,     2,     2,     3,     1,     2,     2,     1,     2,
       3,     1,     2,     3,     1,     3,     4,     1,     1,     1,
       1,     0,     7,     0,     8,     0,     8,     0,     8,     1,
       0,     3,     3,     3,     1,     1,     2,     1,     1,     1,
       2,     1,     2,     1,     2,     1,     2,     0,     2,     3,
       4,     4,     4,     3,     2,     2,     3,     3,     2,     1,
       0,     1,     4,     1,     2,     2,     0,     1,     4,     1,
       2,     3,     1,     2,     0,     1,     2,     6,     0,     9,
       8,     9,     8,     0,    13,    11,    12,    11,     1,     0,
       1,     3,     3,     3,     2,     5,     5,     1,     1,     0,
       2,     5,     0,     1,     1,     1,     5,     5,     5,     1,
       5,     5,     9,     1,     5,     0,     1,     1,     5,     1,
       1,     5,     5,     1,     3,     3,     4,     1,     1,     1,
       1,     2,     1,     3,     3,     2,     3,     1,     3,     1,
       1,     1,     1,     1,     2,     1,     1,     0,     2,     2,
       4,     1,     4,     0,     1,     2,     3,     4,     2,     2,
       1,     2,     2,     5,     5,     7,     6,     1,     3,     0,
       2,     0,     5,     0,     5,     3,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     5,
       6,     1,     1,     3,     3,     2,     3,     3,     2,     4,
       1,     4,     7,     5,    10,     8,     1,     4,     2,     2,
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
       3,     5,     8,     3,     2,     3,     7,     5,     1,     1,
       1,     3,     3,     3,     5,     1,     1,     5,     5,     6,
       6,     0,     1,     1,     3,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     5,     8,     3,     1,
       2,     1,     2,     6,     5,     6,     7,     7,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       8,     3,     1,     1,     2,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     2,     4,     3,     2,     3,     2,
       4,     3,     2,     6,     6,     6,     7,     1,     2,     1,
       1,     1,     2,     3,     2,     3,     2,     3,     3,     4,
       2,     3,     4,     2,     5,     6,     7,     6,     6,     0,
       1,     0,     2
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
#line 7586 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7592 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7598 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7604 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7610 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7616 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7622 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7628 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7634 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7640 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7650 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7656 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7662 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7668 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7674 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7680 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7686 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7692 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7702 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7708 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7714 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7720 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7726 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7732 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7738 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7749 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7758 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7764 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7770 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7776 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7782 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7788 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7798 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7804 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7811 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7817 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7823 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7829 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7835 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7841 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7847 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7853 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7859 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7865 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7871 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7877 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7883 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7889 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7895 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7901 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7911 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7917 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7923 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7929 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7935 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7941 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7947 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7953 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7959 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7965 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7971 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7977 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7983 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7991 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7997 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8006 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8012 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8018 "Parser/parser.cc"
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
#line 8038 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8044 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8050 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8056 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8062 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8068 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8074 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8080 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8086 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8095 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8101 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8107 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8113 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8119 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8125 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8131 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8137 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8149 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8173 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8179 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8185 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8227 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8233 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8239 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8245 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8251 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8257 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8263 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8269 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8275 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8281 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8287 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8293 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8299 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8305 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8311 "Parser/parser.cc"
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
#line 8323 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8329 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8335 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8341 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8347 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8353 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8359 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8365 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8371 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8377 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8383 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8389 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8395 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8401 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8407 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8413 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8419 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8425 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8431 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8437 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8443 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8449 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8455 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8461 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8467 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8473 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8484 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8490 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8496 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8502 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8508 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8526 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8532 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8538 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8544 "Parser/parser.cc"
    break;

  case 214:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8550 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8570 "Parser/parser.cc"
    break;

  case 217:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8576 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8591 "Parser/parser.cc"
    break;

  case 220:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8597 "Parser/parser.cc"
    break;

  case 221:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8603 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8609 "Parser/parser.cc"
    break;

  case 223:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8615 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8621 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8627 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8633 "Parser/parser.cc"
    break;

  case 227:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8639 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8645 "Parser/parser.cc"
    break;

  case 230:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8651 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8657 "Parser/parser.cc"
    break;

  case 232:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8663 "Parser/parser.cc"
    break;

  case 233:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8669 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8675 "Parser/parser.cc"
    break;

  case 235:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8681 "Parser/parser.cc"
    break;

  case 236:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8687 "Parser/parser.cc"
    break;

  case 238:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8693 "Parser/parser.cc"
    break;

  case 239:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 240:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8705 "Parser/parser.cc"
    break;

  case 242:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 244:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8732 "Parser/parser.cc"
    break;

  case 246:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8744 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8750 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8759 "Parser/parser.cc"
    break;

  case 250:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8771 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8777 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8786 "Parser/parser.cc"
    break;

  case 254:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8792 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8798 "Parser/parser.cc"
    break;

  case 257:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8817 "Parser/parser.cc"
    break;

  case 258:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 261:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8841 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 263:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8853 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8859 "Parser/parser.cc"
    break;

  case 265:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8865 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8874 "Parser/parser.cc"
    break;

  case 267:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8883 "Parser/parser.cc"
    break;

  case 268:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8889 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8898 "Parser/parser.cc"
    break;

  case 270:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8907 "Parser/parser.cc"
    break;

  case 271:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8913 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8919 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8925 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8931 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8937 "Parser/parser.cc"
    break;

  case 276:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8943 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8949 "Parser/parser.cc"
    break;

  case 278:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8955 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8964 "Parser/parser.cc"
    break;

  case 280:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8974 "Parser/parser.cc"
    break;

  case 281:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8980 "Parser/parser.cc"
    break;

  case 282:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8986 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8995 "Parser/parser.cc"
    break;

  case 284:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9005 "Parser/parser.cc"
    break;

  case 285:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9011 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9020 "Parser/parser.cc"
    break;

  case 287:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9030 "Parser/parser.cc"
    break;

  case 288:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9036 "Parser/parser.cc"
    break;

  case 289:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9042 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9048 "Parser/parser.cc"
    break;

  case 291:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9054 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9063 "Parser/parser.cc"
    break;

  case 293:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9073 "Parser/parser.cc"
    break;

  case 294:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9088 "Parser/parser.cc"
    break;

  case 296:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9098 "Parser/parser.cc"
    break;

  case 297:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9104 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9113 "Parser/parser.cc"
    break;

  case 299:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9123 "Parser/parser.cc"
    break;

  case 300:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9129 "Parser/parser.cc"
    break;

  case 301:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9138 "Parser/parser.cc"
    break;

  case 302:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9147 "Parser/parser.cc"
    break;

  case 303:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9153 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9159 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9165 "Parser/parser.cc"
    break;

  case 306:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9171 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9177 "Parser/parser.cc"
    break;

  case 309:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9183 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9189 "Parser/parser.cc"
    break;

  case 311:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 312:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9201 "Parser/parser.cc"
    break;

  case 313:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9207 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9213 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9219 "Parser/parser.cc"
    break;

  case 316:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9225 "Parser/parser.cc"
    break;

  case 317:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9231 "Parser/parser.cc"
    break;

  case 318:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9237 "Parser/parser.cc"
    break;

  case 319:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9243 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9249 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9255 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9261 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9267 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9273 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9279 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9285 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9291 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9297 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9303 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9309 "Parser/parser.cc"
    break;

  case 333:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9315 "Parser/parser.cc"
    break;

  case 334:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].en) ) { SemanticError( yylloc, "mutex argument list cannot be empty." ); (yyval.sn) = nullptr; }
			(yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 9324 "Parser/parser.cc"
    break;

  case 335:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9330 "Parser/parser.cc"
    break;

  case 336:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9336 "Parser/parser.cc"
    break;

  case 338:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9342 "Parser/parser.cc"
    break;

  case 339:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9348 "Parser/parser.cc"
    break;

  case 341:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9354 "Parser/parser.cc"
    break;

  case 342:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9360 "Parser/parser.cc"
    break;

  case 343:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9366 "Parser/parser.cc"
    break;

  case 344:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9372 "Parser/parser.cc"
    break;

  case 345:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9378 "Parser/parser.cc"
    break;

  case 346:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 347:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9390 "Parser/parser.cc"
    break;

  case 348:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9396 "Parser/parser.cc"
    break;

  case 349:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 350:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9408 "Parser/parser.cc"
    break;

  case 351:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9414 "Parser/parser.cc"
    break;

  case 352:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 353:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 354:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 355:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9438 "Parser/parser.cc"
    break;

  case 356:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9444 "Parser/parser.cc"
    break;

  case 357:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9450 "Parser/parser.cc"
    break;

  case 358:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9456 "Parser/parser.cc"
    break;

  case 359:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9462 "Parser/parser.cc"
    break;

  case 360:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9468 "Parser/parser.cc"
    break;

  case 361:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9474 "Parser/parser.cc"
    break;

  case 362:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 364:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 365:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9492 "Parser/parser.cc"
    break;

  case 366:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9498 "Parser/parser.cc"
    break;

  case 371:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 372:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 373:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 374:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9522 "Parser/parser.cc"
    break;

  case 375:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 376:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9534 "Parser/parser.cc"
    break;

  case 377:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9540 "Parser/parser.cc"
    break;

  case 378:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9546 "Parser/parser.cc"
    break;

  case 381:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9552 "Parser/parser.cc"
    break;

  case 382:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9558 "Parser/parser.cc"
    break;

  case 383:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9564 "Parser/parser.cc"
    break;

  case 384:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9570 "Parser/parser.cc"
    break;

  case 385:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9576 "Parser/parser.cc"
    break;

  case 386:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9582 "Parser/parser.cc"
    break;

  case 387:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9591 "Parser/parser.cc"
    break;

  case 388:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9600 "Parser/parser.cc"
    break;

  case 389:
#line 1765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9606 "Parser/parser.cc"
    break;

  case 392:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9612 "Parser/parser.cc"
    break;

  case 393:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9618 "Parser/parser.cc"
    break;

  case 395:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9624 "Parser/parser.cc"
    break;

  case 396:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9630 "Parser/parser.cc"
    break;

  case 403:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9641 "Parser/parser.cc"
    break;

  case 406:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9647 "Parser/parser.cc"
    break;

  case 407:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9653 "Parser/parser.cc"
    break;

  case 411:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9659 "Parser/parser.cc"
    break;

  case 413:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9665 "Parser/parser.cc"
    break;

  case 414:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9671 "Parser/parser.cc"
    break;

  case 415:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9677 "Parser/parser.cc"
    break;

  case 416:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9683 "Parser/parser.cc"
    break;

  case 417:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9689 "Parser/parser.cc"
    break;

  case 418:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9695 "Parser/parser.cc"
    break;

  case 420:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9701 "Parser/parser.cc"
    break;

  case 421:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9707 "Parser/parser.cc"
    break;

  case 422:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 423:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9724 "Parser/parser.cc"
    break;

  case 424:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9730 "Parser/parser.cc"
    break;

  case 425:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9736 "Parser/parser.cc"
    break;

  case 426:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9742 "Parser/parser.cc"
    break;

  case 427:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9748 "Parser/parser.cc"
    break;

  case 428:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9757 "Parser/parser.cc"
    break;

  case 429:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9766 "Parser/parser.cc"
    break;

  case 430:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9775 "Parser/parser.cc"
    break;

  case 431:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// if type_specifier is an anon aggregate => name 
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();		// watchout frees $2 and $3
		}
#line 9785 "Parser/parser.cc"
    break;

  case 432:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9794 "Parser/parser.cc"
    break;

  case 433:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9803 "Parser/parser.cc"
    break;

  case 434:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9812 "Parser/parser.cc"
    break;

  case 435:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addType( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9821 "Parser/parser.cc"
    break;

  case 436:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9829 "Parser/parser.cc"
    break;

  case 437:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9837 "Parser/parser.cc"
    break;

  case 438:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9843 "Parser/parser.cc"
    break;

  case 441:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9858 "Parser/parser.cc"
    break;

  case 442:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9864 "Parser/parser.cc"
    break;

  case 443:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9870 "Parser/parser.cc"
    break;

  case 447:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Missing ';' after end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 9881 "Parser/parser.cc"
    break;

  case 455:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9892 "Parser/parser.cc"
    break;

  case 460:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9898 "Parser/parser.cc"
    break;

  case 463:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9904 "Parser/parser.cc"
    break;

  case 466:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9910 "Parser/parser.cc"
    break;

  case 467:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9916 "Parser/parser.cc"
    break;

  case 468:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9922 "Parser/parser.cc"
    break;

  case 469:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9928 "Parser/parser.cc"
    break;

  case 470:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 9934 "Parser/parser.cc"
    break;

  case 471:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9940 "Parser/parser.cc"
    break;

  case 473:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9946 "Parser/parser.cc"
    break;

  case 474:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9952 "Parser/parser.cc"
    break;

  case 476:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9958 "Parser/parser.cc"
    break;

  case 477:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9964 "Parser/parser.cc"
    break;

  case 478:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9970 "Parser/parser.cc"
    break;

  case 479:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9976 "Parser/parser.cc"
    break;

  case 480:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9982 "Parser/parser.cc"
    break;

  case 481:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9988 "Parser/parser.cc"
    break;

  case 482:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9994 "Parser/parser.cc"
    break;

  case 483:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10000 "Parser/parser.cc"
    break;

  case 484:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10006 "Parser/parser.cc"
    break;

  case 485:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10012 "Parser/parser.cc"
    break;

  case 486:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10018 "Parser/parser.cc"
    break;

  case 487:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10024 "Parser/parser.cc"
    break;

  case 488:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10030 "Parser/parser.cc"
    break;

  case 489:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10036 "Parser/parser.cc"
    break;

  case 490:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10042 "Parser/parser.cc"
    break;

  case 491:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10048 "Parser/parser.cc"
    break;

  case 492:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10054 "Parser/parser.cc"
    break;

  case 493:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10060 "Parser/parser.cc"
    break;

  case 494:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10066 "Parser/parser.cc"
    break;

  case 495:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10072 "Parser/parser.cc"
    break;

  case 496:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10078 "Parser/parser.cc"
    break;

  case 497:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10084 "Parser/parser.cc"
    break;

  case 498:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10090 "Parser/parser.cc"
    break;

  case 499:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10096 "Parser/parser.cc"
    break;

  case 500:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10102 "Parser/parser.cc"
    break;

  case 501:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10108 "Parser/parser.cc"
    break;

  case 502:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10114 "Parser/parser.cc"
    break;

  case 503:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10120 "Parser/parser.cc"
    break;

  case 504:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10126 "Parser/parser.cc"
    break;

  case 505:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10132 "Parser/parser.cc"
    break;

  case 506:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10138 "Parser/parser.cc"
    break;

  case 507:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10144 "Parser/parser.cc"
    break;

  case 508:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10150 "Parser/parser.cc"
    break;

  case 509:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10156 "Parser/parser.cc"
    break;

  case 510:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10162 "Parser/parser.cc"
    break;

  case 511:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10168 "Parser/parser.cc"
    break;

  case 512:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10174 "Parser/parser.cc"
    break;

  case 514:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10180 "Parser/parser.cc"
    break;

  case 516:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10186 "Parser/parser.cc"
    break;

  case 517:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10192 "Parser/parser.cc"
    break;

  case 518:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10198 "Parser/parser.cc"
    break;

  case 520:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 521:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 522:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10216 "Parser/parser.cc"
    break;

  case 523:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10222 "Parser/parser.cc"
    break;

  case 525:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10228 "Parser/parser.cc"
    break;

  case 527:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10234 "Parser/parser.cc"
    break;

  case 528:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10240 "Parser/parser.cc"
    break;

  case 529:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10246 "Parser/parser.cc"
    break;

  case 530:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10252 "Parser/parser.cc"
    break;

  case 531:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10258 "Parser/parser.cc"
    break;

  case 532:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10264 "Parser/parser.cc"
    break;

  case 533:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10270 "Parser/parser.cc"
    break;

  case 534:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10276 "Parser/parser.cc"
    break;

  case 535:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10282 "Parser/parser.cc"
    break;

  case 536:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10293 "Parser/parser.cc"
    break;

  case 537:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10299 "Parser/parser.cc"
    break;

  case 538:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10305 "Parser/parser.cc"
    break;

  case 539:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10311 "Parser/parser.cc"
    break;

  case 540:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10322 "Parser/parser.cc"
    break;

  case 541:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10328 "Parser/parser.cc"
    break;

  case 542:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10334 "Parser/parser.cc"
    break;

  case 543:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10343 "Parser/parser.cc"
    break;

  case 545:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 546:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 547:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10361 "Parser/parser.cc"
    break;

  case 549:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 550:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10373 "Parser/parser.cc"
    break;

  case 552:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 553:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 554:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 556:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 557:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 558:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 559:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 560:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 562:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10427 "Parser/parser.cc"
    break;

  case 563:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10433 "Parser/parser.cc"
    break;

  case 564:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10439 "Parser/parser.cc"
    break;

  case 565:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10445 "Parser/parser.cc"
    break;

  case 566:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10451 "Parser/parser.cc"
    break;

  case 567:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10462 "Parser/parser.cc"
    break;

  case 571:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10468 "Parser/parser.cc"
    break;

  case 572:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 573:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10483 "Parser/parser.cc"
    break;

  case 574:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10500 "Parser/parser.cc"
    break;

  case 575:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10509 "Parser/parser.cc"
    break;

  case 576:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10519 "Parser/parser.cc"
    break;

  case 577:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10528 "Parser/parser.cc"
    break;

  case 578:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10538 "Parser/parser.cc"
    break;

  case 580:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10544 "Parser/parser.cc"
    break;

  case 581:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10550 "Parser/parser.cc"
    break;

  case 582:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10560 "Parser/parser.cc"
    break;

  case 583:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10575 "Parser/parser.cc"
    break;

  case 586:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10581 "Parser/parser.cc"
    break;

  case 587:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10587 "Parser/parser.cc"
    break;

  case 588:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10593 "Parser/parser.cc"
    break;

  case 589:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10599 "Parser/parser.cc"
    break;

  case 590:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10605 "Parser/parser.cc"
    break;

  case 591:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10611 "Parser/parser.cc"
    break;

  case 592:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10617 "Parser/parser.cc"
    break;

  case 593:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10623 "Parser/parser.cc"
    break;

  case 594:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10629 "Parser/parser.cc"
    break;

  case 595:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10635 "Parser/parser.cc"
    break;

  case 596:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10641 "Parser/parser.cc"
    break;

  case 597:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10647 "Parser/parser.cc"
    break;

  case 598:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10653 "Parser/parser.cc"
    break;

  case 599:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10666 "Parser/parser.cc"
    break;

  case 600:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 601:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10678 "Parser/parser.cc"
    break;

  case 602:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10691 "Parser/parser.cc"
    break;

  case 603:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10697 "Parser/parser.cc"
    break;

  case 606:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10703 "Parser/parser.cc"
    break;

  case 607:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10709 "Parser/parser.cc"
    break;

  case 610:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10715 "Parser/parser.cc"
    break;

  case 612:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 613:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 614:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 615:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 616:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10745 "Parser/parser.cc"
    break;

  case 618:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 620:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 621:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10763 "Parser/parser.cc"
    break;

  case 623:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10769 "Parser/parser.cc"
    break;

  case 624:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10775 "Parser/parser.cc"
    break;

  case 626:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10781 "Parser/parser.cc"
    break;

  case 627:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 628:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10793 "Parser/parser.cc"
    break;

  case 629:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 630:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 631:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10816 "Parser/parser.cc"
    break;

  case 632:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10824 "Parser/parser.cc"
    break;

  case 633:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10833 "Parser/parser.cc"
    break;

  case 634:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10841 "Parser/parser.cc"
    break;

  case 635:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10849 "Parser/parser.cc"
    break;

  case 636:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10857 "Parser/parser.cc"
    break;

  case 637:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10865 "Parser/parser.cc"
    break;

  case 639:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10871 "Parser/parser.cc"
    break;

  case 640:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10877 "Parser/parser.cc"
    break;

  case 641:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 642:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 643:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 644:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10901 "Parser/parser.cc"
    break;

  case 645:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 646:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 648:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10919 "Parser/parser.cc"
    break;

  case 649:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10925 "Parser/parser.cc"
    break;

  case 650:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10931 "Parser/parser.cc"
    break;

  case 651:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10937 "Parser/parser.cc"
    break;

  case 652:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10943 "Parser/parser.cc"
    break;

  case 653:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10949 "Parser/parser.cc"
    break;

  case 656:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 657:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10961 "Parser/parser.cc"
    break;

  case 658:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10967 "Parser/parser.cc"
    break;

  case 660:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10973 "Parser/parser.cc"
    break;

  case 661:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10979 "Parser/parser.cc"
    break;

  case 662:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10985 "Parser/parser.cc"
    break;

  case 664:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10991 "Parser/parser.cc"
    break;

  case 665:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10997 "Parser/parser.cc"
    break;

  case 666:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11003 "Parser/parser.cc"
    break;

  case 668:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11009 "Parser/parser.cc"
    break;

  case 671:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11015 "Parser/parser.cc"
    break;

  case 672:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 674:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11027 "Parser/parser.cc"
    break;

  case 675:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11033 "Parser/parser.cc"
    break;

  case 676:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11039 "Parser/parser.cc"
    break;

  case 681:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11045 "Parser/parser.cc"
    break;

  case 683:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11051 "Parser/parser.cc"
    break;

  case 684:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11057 "Parser/parser.cc"
    break;

  case 685:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11063 "Parser/parser.cc"
    break;

  case 686:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11069 "Parser/parser.cc"
    break;

  case 687:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11075 "Parser/parser.cc"
    break;

  case 688:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11081 "Parser/parser.cc"
    break;

  case 694:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11087 "Parser/parser.cc"
    break;

  case 697:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11093 "Parser/parser.cc"
    break;

  case 698:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11099 "Parser/parser.cc"
    break;

  case 699:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11105 "Parser/parser.cc"
    break;

  case 700:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11111 "Parser/parser.cc"
    break;

  case 701:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11117 "Parser/parser.cc"
    break;

  case 702:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11123 "Parser/parser.cc"
    break;

  case 703:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11129 "Parser/parser.cc"
    break;

  case 705:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11135 "Parser/parser.cc"
    break;

  case 706:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11141 "Parser/parser.cc"
    break;

  case 707:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11147 "Parser/parser.cc"
    break;

  case 709:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11153 "Parser/parser.cc"
    break;

  case 711:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11159 "Parser/parser.cc"
    break;

  case 712:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11165 "Parser/parser.cc"
    break;

  case 713:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11171 "Parser/parser.cc"
    break;

  case 714:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11177 "Parser/parser.cc"
    break;

  case 715:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11183 "Parser/parser.cc"
    break;

  case 716:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11189 "Parser/parser.cc"
    break;

  case 718:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 719:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11201 "Parser/parser.cc"
    break;

  case 720:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11207 "Parser/parser.cc"
    break;

  case 721:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11218 "Parser/parser.cc"
    break;

  case 722:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11224 "Parser/parser.cc"
    break;

  case 723:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11230 "Parser/parser.cc"
    break;

  case 724:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11236 "Parser/parser.cc"
    break;

  case 725:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11245 "Parser/parser.cc"
    break;

  case 726:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 727:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11257 "Parser/parser.cc"
    break;

  case 728:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11263 "Parser/parser.cc"
    break;

  case 729:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11269 "Parser/parser.cc"
    break;

  case 730:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11275 "Parser/parser.cc"
    break;

  case 731:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11281 "Parser/parser.cc"
    break;

  case 732:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11287 "Parser/parser.cc"
    break;

  case 733:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11293 "Parser/parser.cc"
    break;

  case 734:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11299 "Parser/parser.cc"
    break;

  case 735:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11305 "Parser/parser.cc"
    break;

  case 738:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 739:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 740:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11323 "Parser/parser.cc"
    break;

  case 741:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 743:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11335 "Parser/parser.cc"
    break;

  case 744:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11341 "Parser/parser.cc"
    break;

  case 745:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11347 "Parser/parser.cc"
    break;

  case 746:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 747:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 748:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 749:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 750:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11380 "Parser/parser.cc"
    break;

  case 751:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11389 "Parser/parser.cc"
    break;

  case 752:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11398 "Parser/parser.cc"
    break;

  case 753:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11404 "Parser/parser.cc"
    break;

  case 754:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11413 "Parser/parser.cc"
    break;

  case 755:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 757:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 762:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 763:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 764:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 766:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11449 "Parser/parser.cc"
    break;

  case 767:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11455 "Parser/parser.cc"
    break;

  case 768:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11461 "Parser/parser.cc"
    break;

  case 769:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11467 "Parser/parser.cc"
    break;

  case 771:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11473 "Parser/parser.cc"
    break;

  case 772:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11479 "Parser/parser.cc"
    break;

  case 773:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 774:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11502 "Parser/parser.cc"
    break;

  case 775:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11508 "Parser/parser.cc"
    break;

  case 776:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11514 "Parser/parser.cc"
    break;

  case 777:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11520 "Parser/parser.cc"
    break;

  case 778:
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11526 "Parser/parser.cc"
    break;

  case 779:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11532 "Parser/parser.cc"
    break;

  case 780:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11538 "Parser/parser.cc"
    break;

  case 782:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11547 "Parser/parser.cc"
    break;

  case 783:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 11553 "Parser/parser.cc"
    break;

  case 784:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11562 "Parser/parser.cc"
    break;

  case 785:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11572 "Parser/parser.cc"
    break;

  case 786:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11581 "Parser/parser.cc"
    break;

  case 787:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11591 "Parser/parser.cc"
    break;

  case 788:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11600 "Parser/parser.cc"
    break;

  case 789:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11610 "Parser/parser.cc"
    break;

  case 790:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11619 "Parser/parser.cc"
    break;

  case 791:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11629 "Parser/parser.cc"
    break;

  case 792:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11638 "Parser/parser.cc"
    break;

  case 793:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11648 "Parser/parser.cc"
    break;

  case 795:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11654 "Parser/parser.cc"
    break;

  case 796:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 797:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11666 "Parser/parser.cc"
    break;

  case 798:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11678 "Parser/parser.cc"
    break;

  case 799:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11689 "Parser/parser.cc"
    break;

  case 800:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11698 "Parser/parser.cc"
    break;

  case 801:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11707 "Parser/parser.cc"
    break;

  case 802:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11713 "Parser/parser.cc"
    break;

  case 803:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11719 "Parser/parser.cc"
    break;

  case 804:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11725 "Parser/parser.cc"
    break;

  case 805:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11734 "Parser/parser.cc"
    break;

  case 806:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11740 "Parser/parser.cc"
    break;

  case 807:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11746 "Parser/parser.cc"
    break;

  case 808:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11752 "Parser/parser.cc"
    break;

  case 812:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11758 "Parser/parser.cc"
    break;

  case 813:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11764 "Parser/parser.cc"
    break;

  case 814:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11774 "Parser/parser.cc"
    break;

  case 815:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11780 "Parser/parser.cc"
    break;

  case 818:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11786 "Parser/parser.cc"
    break;

  case 819:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11792 "Parser/parser.cc"
    break;

  case 821:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11798 "Parser/parser.cc"
    break;

  case 822:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11804 "Parser/parser.cc"
    break;

  case 823:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11810 "Parser/parser.cc"
    break;

  case 824:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11816 "Parser/parser.cc"
    break;

  case 829:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11822 "Parser/parser.cc"
    break;

  case 830:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11828 "Parser/parser.cc"
    break;

  case 831:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11834 "Parser/parser.cc"
    break;

  case 832:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11840 "Parser/parser.cc"
    break;

  case 833:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11846 "Parser/parser.cc"
    break;

  case 835:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11852 "Parser/parser.cc"
    break;

  case 836:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11858 "Parser/parser.cc"
    break;

  case 837:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11864 "Parser/parser.cc"
    break;

  case 838:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11870 "Parser/parser.cc"
    break;

  case 839:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11876 "Parser/parser.cc"
    break;

  case 840:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11882 "Parser/parser.cc"
    break;

  case 841:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11888 "Parser/parser.cc"
    break;

  case 842:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11894 "Parser/parser.cc"
    break;

  case 843:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11900 "Parser/parser.cc"
    break;

  case 844:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11906 "Parser/parser.cc"
    break;

  case 845:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11912 "Parser/parser.cc"
    break;

  case 846:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11918 "Parser/parser.cc"
    break;

  case 847:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11924 "Parser/parser.cc"
    break;

  case 848:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11930 "Parser/parser.cc"
    break;

  case 849:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11936 "Parser/parser.cc"
    break;

  case 850:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11942 "Parser/parser.cc"
    break;

  case 851:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11948 "Parser/parser.cc"
    break;

  case 852:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11954 "Parser/parser.cc"
    break;

  case 854:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11960 "Parser/parser.cc"
    break;

  case 855:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11966 "Parser/parser.cc"
    break;

  case 856:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11972 "Parser/parser.cc"
    break;

  case 857:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11978 "Parser/parser.cc"
    break;

  case 858:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11984 "Parser/parser.cc"
    break;

  case 859:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11990 "Parser/parser.cc"
    break;

  case 860:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11996 "Parser/parser.cc"
    break;

  case 861:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12002 "Parser/parser.cc"
    break;

  case 862:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12008 "Parser/parser.cc"
    break;

  case 863:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12014 "Parser/parser.cc"
    break;

  case 864:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12020 "Parser/parser.cc"
    break;

  case 865:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12026 "Parser/parser.cc"
    break;

  case 866:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12032 "Parser/parser.cc"
    break;

  case 867:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12038 "Parser/parser.cc"
    break;

  case 868:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12044 "Parser/parser.cc"
    break;

  case 869:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12050 "Parser/parser.cc"
    break;

  case 873:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12056 "Parser/parser.cc"
    break;

  case 874:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12062 "Parser/parser.cc"
    break;

  case 875:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12068 "Parser/parser.cc"
    break;

  case 876:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12074 "Parser/parser.cc"
    break;

  case 877:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12080 "Parser/parser.cc"
    break;

  case 878:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12086 "Parser/parser.cc"
    break;

  case 879:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12092 "Parser/parser.cc"
    break;

  case 880:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12098 "Parser/parser.cc"
    break;

  case 881:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12104 "Parser/parser.cc"
    break;

  case 882:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12110 "Parser/parser.cc"
    break;

  case 883:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12116 "Parser/parser.cc"
    break;

  case 884:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12122 "Parser/parser.cc"
    break;

  case 885:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12128 "Parser/parser.cc"
    break;

  case 886:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12134 "Parser/parser.cc"
    break;

  case 887:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12140 "Parser/parser.cc"
    break;

  case 888:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12149 "Parser/parser.cc"
    break;

  case 889:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12155 "Parser/parser.cc"
    break;

  case 890:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 892:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 893:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 894:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 895:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 896:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 897:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12197 "Parser/parser.cc"
    break;

  case 898:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 899:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12209 "Parser/parser.cc"
    break;

  case 900:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 901:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 902:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 903:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12233 "Parser/parser.cc"
    break;

  case 904:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 905:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 906:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 907:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12257 "Parser/parser.cc"
    break;

  case 908:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12263 "Parser/parser.cc"
    break;

  case 909:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 910:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12275 "Parser/parser.cc"
    break;

  case 911:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 913:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 914:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12293 "Parser/parser.cc"
    break;

  case 915:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 916:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 917:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 918:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 919:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 920:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 921:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12335 "Parser/parser.cc"
    break;

  case 922:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 923:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 924:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12353 "Parser/parser.cc"
    break;

  case 925:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 926:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 928:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 929:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 930:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 931:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 932:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 933:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 934:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 935:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 936:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 937:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 938:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 940:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 941:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 942:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 943:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 944:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 945:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 946:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 948:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12479 "Parser/parser.cc"
    break;

  case 949:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12485 "Parser/parser.cc"
    break;

  case 950:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12491 "Parser/parser.cc"
    break;

  case 951:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12497 "Parser/parser.cc"
    break;

  case 952:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12503 "Parser/parser.cc"
    break;

  case 953:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12509 "Parser/parser.cc"
    break;

  case 954:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12515 "Parser/parser.cc"
    break;

  case 955:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 956:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 957:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12533 "Parser/parser.cc"
    break;

  case 959:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 960:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 962:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12551 "Parser/parser.cc"
    break;

  case 963:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12557 "Parser/parser.cc"
    break;

  case 965:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12563 "Parser/parser.cc"
    break;

  case 966:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12569 "Parser/parser.cc"
    break;

  case 967:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 12575 "Parser/parser.cc"
    break;

  case 968:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12581 "Parser/parser.cc"
    break;

  case 969:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 12587 "Parser/parser.cc"
    break;

  case 970:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12593 "Parser/parser.cc"
    break;

  case 971:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12599 "Parser/parser.cc"
    break;

  case 974:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12605 "Parser/parser.cc"
    break;

  case 975:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12611 "Parser/parser.cc"
    break;

  case 976:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12617 "Parser/parser.cc"
    break;

  case 977:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12623 "Parser/parser.cc"
    break;

  case 978:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12629 "Parser/parser.cc"
    break;

  case 979:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12635 "Parser/parser.cc"
    break;

  case 980:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12641 "Parser/parser.cc"
    break;

  case 981:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12647 "Parser/parser.cc"
    break;

  case 983:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12653 "Parser/parser.cc"
    break;

  case 984:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12659 "Parser/parser.cc"
    break;

  case 985:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12665 "Parser/parser.cc"
    break;

  case 986:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12671 "Parser/parser.cc"
    break;

  case 987:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12677 "Parser/parser.cc"
    break;

  case 988:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12683 "Parser/parser.cc"
    break;

  case 990:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12689 "Parser/parser.cc"
    break;

  case 992:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12695 "Parser/parser.cc"
    break;

  case 993:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12701 "Parser/parser.cc"
    break;

  case 994:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 12707 "Parser/parser.cc"
    break;

  case 995:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12713 "Parser/parser.cc"
    break;

  case 996:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12719 "Parser/parser.cc"
    break;

  case 997:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12725 "Parser/parser.cc"
    break;

  case 999:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12731 "Parser/parser.cc"
    break;

  case 1000:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12737 "Parser/parser.cc"
    break;

  case 1001:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12743 "Parser/parser.cc"
    break;

  case 1002:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12749 "Parser/parser.cc"
    break;

  case 1003:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12755 "Parser/parser.cc"
    break;

  case 1004:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12761 "Parser/parser.cc"
    break;

  case 1005:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12767 "Parser/parser.cc"
    break;

  case 1007:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12773 "Parser/parser.cc"
    break;

  case 1008:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12779 "Parser/parser.cc"
    break;

  case 1009:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12785 "Parser/parser.cc"
    break;

  case 1010:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12791 "Parser/parser.cc"
    break;

  case 1011:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12797 "Parser/parser.cc"
    break;

  case 1014:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12803 "Parser/parser.cc"
    break;

  case 1017:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12809 "Parser/parser.cc"
    break;

  case 1018:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12815 "Parser/parser.cc"
    break;

  case 1019:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12821 "Parser/parser.cc"
    break;

  case 1020:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12827 "Parser/parser.cc"
    break;

  case 1021:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12833 "Parser/parser.cc"
    break;

  case 1022:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12839 "Parser/parser.cc"
    break;

  case 1023:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 1024:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12851 "Parser/parser.cc"
    break;

  case 1025:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 1026:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 1027:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 1028:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 1029:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 1030:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12887 "Parser/parser.cc"
    break;

  case 1031:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 1032:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 1033:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 1034:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12911 "Parser/parser.cc"
    break;

  case 1035:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12917 "Parser/parser.cc"
    break;

  case 1036:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12923 "Parser/parser.cc"
    break;

  case 1038:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12929 "Parser/parser.cc"
    break;

  case 1042:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12935 "Parser/parser.cc"
    break;

  case 1043:
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12941 "Parser/parser.cc"
    break;

  case 1044:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12947 "Parser/parser.cc"
    break;

  case 1045:
#line 4011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12953 "Parser/parser.cc"
    break;

  case 1046:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12959 "Parser/parser.cc"
    break;

  case 1047:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12965 "Parser/parser.cc"
    break;

  case 1048:
#line 4022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12971 "Parser/parser.cc"
    break;

  case 1049:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12977 "Parser/parser.cc"
    break;

  case 1050:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12983 "Parser/parser.cc"
    break;

  case 1051:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12989 "Parser/parser.cc"
    break;

  case 1052:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12995 "Parser/parser.cc"
    break;

  case 1053:
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13001 "Parser/parser.cc"
    break;

  case 1054:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13007 "Parser/parser.cc"
    break;

  case 1055:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13013 "Parser/parser.cc"
    break;

  case 1056:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13019 "Parser/parser.cc"
    break;

  case 1057:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13025 "Parser/parser.cc"
    break;

  case 1058:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13031 "Parser/parser.cc"
    break;

  case 1061:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13037 "Parser/parser.cc"
    break;

  case 1062:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13043 "Parser/parser.cc"
    break;


#line 13047 "Parser/parser.cc"

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
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
