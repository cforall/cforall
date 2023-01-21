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
#define YYLAST   22136

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  296
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1057
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2137

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
    1923,  1928,  1939,  1945,  1950,  1955,  1960,  1969,  1973,  1980,
    1982,  1983,  1984,  1990,  1992,  1997,  1998,  1999,  2008,  2009,
    2010,  2014,  2015,  2022,  2031,  2032,  2033,  2038,  2039,  2048,
    2049,  2054,  2055,  2059,  2061,  2063,  2065,  2067,  2071,  2076,
    2077,  2079,  2089,  2090,  2095,  2097,  2099,  2101,  2103,  2105,
    2108,  2110,  2112,  2117,  2119,  2121,  2123,  2125,  2127,  2129,
    2131,  2133,  2135,  2137,  2139,  2141,  2143,  2145,  2147,  2149,
    2151,  2153,  2155,  2157,  2159,  2161,  2163,  2165,  2167,  2169,
    2171,  2176,  2177,  2181,  2188,  2189,  2195,  2196,  2198,  2200,
    2202,  2207,  2209,  2214,  2215,  2217,  2219,  2224,  2226,  2228,
    2230,  2232,  2234,  2239,  2246,  2248,  2250,  2255,  2263,  2262,
    2266,  2274,  2275,  2277,  2279,  2284,  2285,  2287,  2292,  2293,
    2295,  2297,  2302,  2303,  2305,  2310,  2312,  2314,  2316,  2317,
    2319,  2324,  2326,  2328,  2333,  2340,  2344,  2345,  2350,  2349,
    2354,  2353,  2372,  2371,  2383,  2382,  2393,  2398,  2399,  2404,
    2410,  2424,  2425,  2429,  2431,  2433,  2439,  2441,  2443,  2445,
    2447,  2449,  2451,  2453,  2459,  2460,  2465,  2474,  2476,  2478,
    2487,  2489,  2490,  2491,  2493,  2495,  2496,  2501,  2502,  2503,
    2508,  2510,  2513,  2520,  2521,  2522,  2528,  2533,  2535,  2541,
    2542,  2548,  2549,  2553,  2558,  2561,  2560,  2564,  2567,  2574,
    2579,  2578,  2587,  2592,  2597,  2602,  2607,  2608,  2613,  2615,
    2620,  2622,  2624,  2626,  2631,  2632,  2638,  2639,  2640,  2647,
    2648,  2650,  2651,  2652,  2654,  2656,  2663,  2664,  2666,  2668,
    2673,  2674,  2680,  2681,  2683,  2684,  2689,  2690,  2691,  2693,
    2701,  2702,  2704,  2707,  2709,  2713,  2714,  2715,  2717,  2719,
    2724,  2726,  2731,  2733,  2742,  2744,  2749,  2750,  2751,  2755,
    2756,  2757,  2762,  2763,  2768,  2769,  2770,  2771,  2775,  2776,
    2781,  2782,  2783,  2784,  2785,  2799,  2800,  2805,  2806,  2812,
    2814,  2817,  2819,  2821,  2844,  2845,  2851,  2852,  2858,  2857,
    2867,  2866,  2870,  2876,  2882,  2883,  2885,  2889,  2894,  2896,
    2898,  2900,  2906,  2907,  2911,  2912,  2917,  2919,  2926,  2928,
    2929,  2931,  2936,  2938,  2940,  2945,  2947,  2952,  2957,  2965,
    2967,  2972,  2973,  2978,  2979,  2983,  2984,  2985,  2990,  2992,
    2998,  3000,  3005,  3007,  3013,  3014,  3018,  3022,  3026,  3028,
    3029,  3031,  3033,  3035,  3037,  3039,  3041,  3042,  3047,  3050,
    3049,  3061,  3060,  3073,  3072,  3084,  3083,  3095,  3094,  3108,
    3114,  3116,  3122,  3123,  3134,  3141,  3146,  3152,  3155,  3158,
    3162,  3168,  3171,  3174,  3179,  3180,  3181,  3185,  3191,  3192,
    3202,  3203,  3207,  3208,  3213,  3218,  3219,  3225,  3226,  3228,
    3233,  3234,  3235,  3236,  3237,  3239,  3274,  3276,  3281,  3283,
    3284,  3286,  3291,  3293,  3295,  3297,  3302,  3304,  3306,  3308,
    3310,  3312,  3314,  3319,  3321,  3323,  3325,  3334,  3336,  3337,
    3342,  3344,  3346,  3348,  3350,  3355,  3357,  3359,  3361,  3366,
    3368,  3370,  3372,  3374,  3376,  3388,  3389,  3390,  3394,  3396,
    3398,  3400,  3402,  3407,  3409,  3411,  3413,  3418,  3420,  3422,
    3424,  3426,  3428,  3443,  3448,  3453,  3455,  3456,  3458,  3463,
    3465,  3467,  3469,  3474,  3476,  3478,  3480,  3482,  3484,  3486,
    3491,  3493,  3495,  3497,  3499,  3509,  3511,  3513,  3514,  3516,
    3521,  3523,  3525,  3530,  3532,  3534,  3536,  3541,  3543,  3545,
    3559,  3561,  3563,  3564,  3566,  3571,  3573,  3578,  3580,  3582,
    3587,  3589,  3594,  3596,  3613,  3614,  3616,  3621,  3623,  3625,
    3627,  3629,  3634,  3635,  3637,  3639,  3644,  3646,  3648,  3654,
    3656,  3659,  3662,  3664,  3668,  3670,  3672,  3673,  3675,  3677,
    3681,  3683,  3688,  3690,  3692,  3694,  3729,  3730,  3734,  3735,
    3737,  3739,  3744,  3746,  3748,  3750,  3752,  3757,  3758,  3760,
    3762,  3767,  3769,  3771,  3777,  3778,  3780,  3789,  3792,  3794,
    3797,  3799,  3801,  3815,  3816,  3818,  3823,  3825,  3827,  3829,
    3831,  3836,  3837,  3839,  3841,  3846,  3848,  3856,  3857,  3858,
    3863,  3864,  3869,  3871,  3873,  3875,  3877,  3879,  3886,  3888,
    3890,  3892,  3894,  3897,  3899,  3901,  3903,  3905,  3910,  3912,
    3914,  3919,  3945,  3946,  3948,  3952,  3953,  3957,  3959,  3961,
    3963,  3965,  3967,  3974,  3976,  3978,  3980,  3982,  3984,  3989,
    3991,  3993,  4000,  4002,  4020,  4022,  4027,  4028
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
     405,   125,    40,    64,    41,    46,    91,    93,    44,    58,
     123,    96,    94,    42,    38,    43,    45,    33,   126,    92,
      47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1789)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1056)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     192, 12383,   212,   238, 17076,   150, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,   122,   875,
     138, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,    25,   342,
   -1789, -1789, -1789, -1789, -1789, -1789,  4210,  4210,   188, 12383,
     201,   357, 15366, -1789,   374, -1789, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789,  2949, -1789,   571,   379, -1789, -1789,
   -1789, -1789, -1789, 16924, -1789, -1789,   365,   400,   406,    40,
   -1789,  4210,   400,   400,   400,   410,  4364,   701,   960, 12545,
   -1789, -1789, -1789, 16772,  2194, -1789, -1789, -1789,  2731,   774,
    6645,   847,   955,  2731,  1092,   463, -1789, -1789, -1789, -1789,
     643, -1789, -1789, -1789, -1789,   627, -1789, -1789, -1789, -1789,
   -1789,   649,   647,   643, -1789,   643,   653, -1789, -1789, -1789,
   17942,  4210, -1789, -1789,  4210, -1789, 12383, -1789,   652, 17995,
   -1789, -1789,  4377,  8669, -1789,  1107,  1107,   688,  2747, -1789,
   -1789, -1789, -1789,   333, 14508,  2592,   643, -1789, -1789, -1789,
   -1789, -1789, -1789,   711, -1789,   709,   750,   753, -1789,   813,
   21419, -1789, -1789, -1789, -1789, -1789, -1789, -1789, 15833,  3474,
    2949,   420,   789,   801,   806,   824,   827,   849, -1789, -1789,
   18147, 11324,   788, -1789, 17523, -1789, -1789, -1789, -1789,   868,
   -1789, -1789,   870, -1789,  6294,  1003, 19643, -1789,   894,  4210,
     647,   897,   896,   899,   902, -1789, -1789, -1789,  3209,  2852,
     917,   953,   221, -1789, -1789,   643,   643,    77,    80,   343,
      77, -1789,   643,   643, -1789,  3725, -1789, -1789,   921,   937,
    1107, 14400, -1789, 16924, -1789, -1789,  2731, -1789,  1373,   463,
     942,  1029,    80,  4210,   406, -1789, 14025, -1789,  1107,  1107,
     952,  1029,    80,  4210, -1789,  6128, -1789, -1789,  1107, -1789,
    1107, -1789,   639,  3886,  4210, -1789,  1702,   935, -1789, -1789,
   -1789, 16623,   647,   219, -1789, -1789, 19022, -1789,   953,     2,
   -1789, 21419,  8669,  3367,  3725, -1789,   385, -1789, -1789, -1789,
   17995,  4210, -1789,   971, -1789, -1789, -1789, -1789,  4210,  2285,
     315,   263, -1789,  4210,   709, -1789,   486,   643,   643,   958,
   18200,   940, 14991, 14561,  2731,  2731, -1789,  2731,  1107,  2731,
    1107, -1789, -1789,   643, -1789,   984, -1789, 18352, -1789, -1789,
   -1789, 18405,   868, -1789,   327,   635,    97,   629,   463,   995,
   -1789,  2747,   989,   709,  2747,  1336, -1789,  1013,  1056, 21493,
    1024,  1034,  1040, 21419, 21567,  1050, 22028, -1789, -1789, -1789,
   -1789, -1789, -1789, 21641, 21641, 15677,  1049,  3704, -1789, -1789,
   -1789, -1789,   456, -1789,   678, -1789,  1501, -1789, 21419, 21419,
   -1789,  1041,   495,   934,  1002,   457,  1017,  1057,  1051,  1062,
    1115,    27, -1789,   302, -1789,  1095, -1789,   997,  4431, 16145,
   -1789, -1789,   634,  1095, -1789, -1789,   615, -1789, -1789,  3474,
    1099,  1120,  1130,  1132,  1155,  1157, -1789, -1789,   387,  1068,
   -1789,   734,  1068, -1789, -1789, 17942, -1789,  1042,  1156, 16301,
   -1789, -1789,  4226,  4129,  1181, 14991,  1184,   636,   743, -1789,
   -1789, -1789, -1789, -1789,  4210,  4440, -1789, -1789, -1789, -1789,
   -1789, -1789,  8456,  3353,  1049,  6294,  1166,  1185, -1789, -1789,
    1196, 19643,   786, -1789, -1789, -1789, 19717,  1208, -1789, -1789,
   -1789, -1789, -1789,  3209,   765,  1203,  1210,  1212,   772,  1214,
    1219,  1221,  2852, -1789, -1789,   643,  1232,   406,  1233, -1789,
   -1789,  1249, -1789, -1789,   647,  1029, -1789, -1789, -1789,   647,
   -1789, -1789,  3725, -1789, 16145, 16145, -1789,  1107,  4377, 19073,
   15152, -1789, -1789, -1789, -1789, -1789,   647,  1029,     2, -1789,
   -1789,  2731,  1255,  1029,    80, -1789,   647,  1029, -1789,  8015,
   -1789,  1107,  1107, -1789, -1789,  1261,   230,  1266,   463,  1268,
   -1789, 17237, -1789,   808, -1789,  1341, 19472, -1789,  4377, 16712,
   14400, -1789, 16623, 21715, -1789, -1789, -1789, -1789, -1789,  3367,
     821,  3725, -1789, 15152,   953, 12383, -1789,  1325, -1789,  1347,
   -1789, -1789, -1789, -1789, -1789,  2747, -1789, -1789,  1431,  4013,
    2797, 18405, 11324, -1789, 18557, -1789,  1107,  1107, -1789, -1789,
     868, -1789,   867,  1358,  1506, 21419,  1119,  1249,  1355, -1789,
     643,   643, -1789,  1068, -1789, 18200, -1789, -1789, 17684,  1107,
    1107, -1789,  4013,   643, -1789, 18877, -1789, -1789, 18352, -1789,
     333, -1789, -1789, -1789,  1374,  4210,   995,  1375,   829, 17995,
     858, -1789, -1789, -1789, -1789, -1789, -1789,   859, -1789,  1384,
    1362, -1789, 15989, -1789,  3704, 18610, 18610, -1789, 15989, -1789,
   21419, -1789, -1789, -1789, -1789, -1789, -1789, 15989, -1789, -1789,
   17737, 18610, 18610,   997,  1317,  1453,   578,  1528, -1789,   884,
    1394,  1070,  1397, -1789, 19717, 21419, 19791,  1405, 21419,  1702,
   21419,  1702, -1789,  2729, -1789, -1789, 19865,  2270, 21419, 19865,
    1702, -1789, -1789, 21419, 21419, 21419, 21419, 21419, 21419, 21419,
   21419, 21419, 21419, 21419, 21419, 21419, 21419, 21419, 21419, 21419,
   21419, 21419, 19939,  1377,   813,  3182, 11324, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,  1408, 21419,
   -1789, -1789,   634,  1546, -1789, -1789,   643,   643, -1789, -1789,
   16145, -1789,   417,  1068, -1789,   889,  1068, -1789, -1789, -1789,
    1249, -1789, -1789,  1249, 21789, -1789, -1789, 11324,  1400,  1413,
    2964,  1553,  2434,   444,  1355, -1789,   643,   643,  1355,   452,
   -1789,   643,   643, 21419,  4210,  1098,  1134,  1355,   235, 14347,
   14347,  4210, -1789, -1789, 21419,  1196, -1789,  6294,  1423, -1789,
    1532, -1789, -1789, -1789, -1789, -1789,   912, -1789, 14347,  1702,
    4377,  1702,   915,  1421,  1422,  1425,   926,  1426,  1432,  1441,
     458,  1068, -1789, -1789,   481,  1068, -1789, -1789, -1789,  4377,
     813, -1789,  1068, 19167, -1789,   647, 17237, -1789, -1789,   927,
    1442,   959,  1444, -1789,  1450, -1789,   647, -1789, -1789,   647,
    1029,  1450, -1789,   647,  1417,  1443,  1451, -1789, -1789, 17684,
   -1789,  1448, -1789, -1789, -1789,  1702,  4210, 10468,  1534,  1433,
   18771, -1789,  1156, -1789, 14347,   928, -1789, -1789,  1450, -1789,
   17995, 16145,  1437, -1789,  1437, -1789, -1789, -1789,    97,   643,
     643, -1789, 18352, -1789, 11489, 16457, -1789, 17237,  1460,  1461,
    1467, -1789,  7339,   643, -1789,  1119, -1789, -1789, -1789, -1789,
    1249, -1789, -1789, -1789,  1107, -1789,  2840, -1789, -1789,   463,
     356,  1465,  1447,  1469,    97, -1789, -1789,  1471,  1473,  1336,
   19865, -1789,  1478,  1476,   379,  1477,  1484,  1485,  1494,  1500,
   21419,  1505,  1509,  1512, 11324, 21419, -1789, -1789,  1575, -1789,
   -1789, -1789, 21419, -1789,  1515,  1517,  7639,  1163, -1789, 19865,
    1463, -1789,  1498, -1789, -1789,  4063, -1789, -1789,   972, -1789,
   -1789, -1789, -1789,  4063, -1789, -1789,  1194,   654, -1789, -1789,
    1041,  1041,  1041,   495,   495,   934,   934,  1002,  1002,  1002,
    1002,   457,   457,  1017,  1057,  1051,  1062,  1115, 21419,  1106,
   -1789,  1518,  4063, -1789, -1789,  6294, -1789, 17237,  1519,  1521,
    1523,  1546, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
    1249, -1789, -1789,  1249, 17237, 17237, -1789, -1789,  2964,   850,
    1526,  1527,  1529,  1535,  2166,  2434, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
    1533, -1789,  1355, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
   -1789,  1537,  1539, -1789,   406,  4063,  1205,    48, -1789, -1789,
    1543, -1789, 19643, -1789, 21419,   643, 20013, 14347, -1789, -1789,
   -1789,  1524,   491,  1068, -1789,   521,  1068, -1789, -1789, -1789,
   -1789,  1249, -1789, -1789, -1789,  1249,   953,  1541,  1249,    41,
   -1789,  1095,  1545, -1789, -1789, -1789, -1789, -1789, -1789,  1548,
   -1789, -1789,  1450, -1789,   647, -1789, -1789, -1789, -1789, -1789,
   13186,  1551,  1562, -1789,   146, -1789,   598,   368, 11159,  1569,
   15512,  1571,  1574,   881,  1604,  2143, 20087,  1577, -1789, -1789,
    1578,  1580, -1789, -1789,   647, 21419, 21419,  1683,  1579,   684,
   -1789,  1659,  1581,  1559, -1789, -1789, -1789, 10293, -1789, -1789,
   -1789, -1789, -1789,  2052, -1789, -1789, -1789,  1646, -1789, -1789,
   -1789,  1702, -1789, -1789, 13031, 16924,  1582, -1789,  4210, -1789,
    1563,  1586,  1589, -1789,  1220, -1789, -1789, -1789, -1789,  4377,
   -1789, -1789,  1573,  1576,  1023, 17995,   709,   709,  1374,   995,
     995, -1789, -1789,  1049,  1156, 16301, -1789,  1095, -1789, 11654,
   -1789,   551,  1068, -1789,  1107, 12217, -1789, -1789,    97,   643,
     643,   333,  4210, -1789, 20161, -1789,    97,  1374,  1600, -1789,
   -1789,  1039,   687, 17684, 11324,  1702, -1789,   687, 17790,   687,
   -1789, 21419, 21419, 21419, -1789, -1789, -1789, -1789, 21419, 21419,
    1597,  6294, -1789, -1789,  1596,   714, -1789, -1789, -1789,  3020,
   -1789, -1789,  1224, -1789,    38, -1789, 19865,  1229, -1789, 19717,
   -1789, -1789, 21419,  1583,  1246,  1251,  1196, -1789,   611,  1068,
   -1789, -1789, 17237, 17237, -1789, -1789,  1598,   690,  1068, -1789,
     693,  2996,   643,   643, -1789, -1789, 17237, 17237, -1789,  1601,
   -1789, 15152, 15152,  1610,  1609,  1611,  1616, -1789,  1613, 21419,
   21419,  1260,  1615, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
    1620, 21419, -1789, -1789, -1789,  1249, -1789, -1789, -1789,  1249,
   17237, 17237,   406,   643, -1789, -1789,  1262, 21419, 19316,  1618,
    1623,  1627, -1789, -1789,  1628, 13341, 13496, 13651, 17995, 14400,
   18610, 18610,  1631, -1789,  1603,  1607,  2500, 13864, -1789,   174,
    4210, -1789, -1789,  4210, -1789,  9675,    24,    30, -1789, -1789,
   -1789, -1789, 21419,  1633,  1707, 10993, 10643, -1789,  1612, -1789,
    1617, 21419,  1619,  6294,  1621, 21419, 19717, 21419,  1127, -1789,
    1622,   123, -1789,   102,  1636, -1789, -1789,  1641, -1789,  1624,
   -1789,  1626,  1643, 15512,   805, 14186,   643,   309, -1789, -1789,
   -1789,  1650, -1789,  1668, -1789,  1669, -1789,  1639, -1789,  1652,
   -1789, -1789, -1789, -1789,  1670,  1662,  1664, 11819,  1673,  1674,
    1675, -1789,  1680, -1789, -1789, -1789,  1249, 21419, 21419,  1156,
    1679, -1789,  1374, -1789,   995,    39,  1447,  6294, -1789,  1374,
    1676, -1789, 17995, -1789,   993,  1685,  1678,  1054, -1789,  1681,
   -1789, -1789, -1789, -1789, -1789,  6294,  1196, 19717, -1789,  1728,
    4063, -1789,  1728,  1728, -1789,  4063,  3508,  3806, -1789, -1789,
    1284, -1789, -1789, -1789,  1699,  1697, -1789, -1789, -1789,  1249,
   -1789, -1789,  1700,  1703,   643, -1789, -1789, -1789,  1249, -1789,
   -1789, -1789,  1704, -1789, -1789, -1789, -1789, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789,  1696, -1789, -1789, -1789,
   -1789,  1706,  1711,   643, -1789, 17237, 17237, -1789, -1789, -1789,
   -1789, 21419, -1789,    41, -1789,  1095, -1789, -1789, -1789,  1705,
   -1789,  1631,  1631,  1631,  4348,  1026,  1689,   380, -1789,  4348,
     402, 16145, -1789, -1789, -1789,  3907, 21419,  3941,   443, -1789,
   -1789,    93,  1708,  1708,  4210, -1789, -1789, 17389, -1789, 21419,
    1710,  1715, -1789, -1789, -1789, -1789,  1064,  1718, 15512,  1581,
    1719, 21419,   365,  1714,   410, 13813, 17995, -1789, -1789, -1789,
    1069, 15512, 21419,   723,   707, -1789, 21419, 19489, -1789, -1789,
     447, -1789,  1196, -1789,  1065,  1072,  1091, -1789, -1789, -1789,
   -1789,   647,  1127,  1722, -1789, -1789, 21419, -1789,  1723,   813,
   11159, -1789, -1789, -1789, -1789, 21419,  1767, -1789, 10118, -1789,
     643, 15152, -1789, -1789, 17995, -1789, -1789, -1789,    97,    97,
   -1789, -1789, -1789,  1721, -1789, 17237, -1789, -1789,  1725, -1789,
    1730,  1734,   995,  1731, -1789, -1789,  1196,  1737, -1789, -1789,
    1740, -1789, -1789, 21419, -1789, 17790, 21419,  1196,  1745,  1295,
   -1789,  1297, -1789,  4063, -1789,  4063, -1789, -1789, -1789, -1789,
   17237,  1744,  1746, -1789, -1789, 17237, 17237,  1748,  1751,  1300,
   14669, 14830, -1789,  1742, -1789, -1789, -1789, -1789,  1753,  1754,
    1302, 21419, -1789, -1789, -1789, -1789,   469,  1026,  1592,   487,
   -1789, -1789, -1789, -1789,   643,   643, -1789, -1789, -1789,   497,
   -1789,  1094,  3907,   879, -1789,  3941,   643, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789,   534, 15512,   437, 20235,  1804,
   15512,  1581, 15313, -1789, -1789, -1789, -1789, 21419, -1789, 20309,
    1835,  1736, 19566, 20383, 15512, 10818,  1581,   581,  1111,  1738,
   21419, -1789,  1763,   474, 15512, -1789, -1789,  1766, -1789, -1789,
    1743,   813,   730,  1765,  1768,  1305,  1831, -1789, -1789, -1789,
   -1789,  4210,  4377,  1374,  1374, -1789, -1789,  1771,  1773, -1789,
   -1789, -1789,  1764,    97,  1772, -1789,  1781, -1789, -1789, -1789,
   -1789,  1785, -1789, -1789, -1789,  1315,  1319, -1789, -1789, -1789,
   -1789, -1789, -1789, -1789, -1789, -1789, -1789,  1788, -1789, -1789,
    1789,  1790, -1789, -1789, -1789, -1789, -1789,  1793,  1794,  1797,
    1592, -1789,   643, -1789, -1789, -1789, -1789, -1789,  1783,  4348,
   -1789, -1789,  4034,    88, 11987, -1789, 15407, -1789,    47,  1108,
   15512,  1880,   594,  1791,   340, 15512, 21419,  1801,   581,  1111,
    1780, 21863,  1796,   533,  1883, -1789, 20457, 20531, 21419,  1581,
    1798, 12151, -1789, -1789, -1789, 18824, -1789,  1811,  1806,    61,
   15512, -1789, 21419, 19865,   314, -1789, -1789, -1789,  1822,  1827,
    1830, -1789, -1789,    97,  1374, -1789, -1789, -1789, -1789, -1789,
    1832,  1833,  1838, 15152,  1823, -1789, -1789,   712,  1068, -1789,
   -1789,  1026, -1789, -1789,   216, -1789,   177, -1789, -1789, -1789,
    1834, 12707, -1789, -1789, 15512, -1789,    64, -1789, 15512, 21419,
    1836, 20605, -1789, -1789, 20679, 20753, 21419,  1801,  1581, 20827,
   20901, 15512,  1825,   554,  1829,   606, -1789, -1789,  1847, 12707,
   18824, -1789,  4145, 18557,  1702,  1840, -1789,  1896,  1851,   745,
    1846, -1789,  1930, -1789,  1118, 15512,  1856, 15512, 15512, -1789,
   -1789, -1789,  1374,  1858, -1789, -1789, -1789, -1789, -1789, -1789,
   -1789,  1249, -1789, 21419, -1789, 21419, -1789, -1789,  1403, 12869,
   -1789, -1789, 15512, -1789, -1789,  1581, -1789, -1789,  1581,  1843,
     614,  1844,   669, -1789, -1789,  1581, -1789,  1581, -1789,  1857,
   20975, 21049, 21123, -1789,  1403, -1789,  1837,  3107,  2673, -1789,
   -1789, -1789,    61,  1860, 21419,  1841,    61,    61, 15512, -1789,
   -1789, 21419,  1905,  1909,  1870, -1789, 17237, -1789, -1789, 15407,
   -1789,  1403, -1789, -1789,  1869, 21197, 21271, 21345, -1789, -1789,
    1581, -1789,  1581, -1789,  1581, -1789,  1837, 21419,  1871,  2673,
    1864,   813,  1874, -1789,   754, -1789, -1789,  1123,  1831,   332,
   -1789, -1789, -1789,  9805,  1873, 15407, -1789, -1789,  1581, -1789,
    1581, -1789,  1581,  1878,  1885, -1789,   647,   813,  1882, -1789,
    1863,   813, -1789, -1789, 15512,  1966,  1890, -1789, -1789, -1789,
    9989, -1789,   647, -1789, -1789,  1329, 21419, -1789,  1131, -1789,
   15512, -1789, -1789,   813,  1702,  1891,  1872, -1789, -1789, -1789,
    1139, -1789, -1789,  1881,  1702, -1789, -1789
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
     496,   497,   498,   499,   500,   501,   508,   509,   810,   511,
     584,   585,   588,   590,   586,   592,     0,     0,     0,   457,
       0,     0,    16,   555,   561,     9,    10,    11,    12,    13,
      14,    15,   768,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   826,   457,   769,   406,     0,   409,   694,   411,
     420,     0,   410,   440,   441,     0,     0,     0,     0,   538,
     459,   461,   467,   457,   469,   472,   523,   510,   445,   516,
     521,   446,   533,   447,   548,   552,   558,   537,   564,   576,
     810,   581,   582,   565,   635,   412,   413,     3,   776,   789,
     462,     0,     0,   810,   848,   810,     2,   865,   866,   867,
     457,     0,  1035,  1036,     0,     1,   457,    16,     0,   457,
     429,   430,     0,   538,   451,   452,   453,   779,     0,   587,
     589,   591,   593,     0,   457,     0,   811,   812,   583,   512,
     687,   688,   686,   747,   742,   732,     0,     0,   777,     0,
       0,   474,   770,   774,   775,   771,   772,   773,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,   559,
     457,   457,     0,  1037,   538,   855,   873,  1041,  1034,  1032,
    1039,   405,     0,   164,   700,   163,     0,   414,     0,     0,
       0,     0,     0,     0,     0,   404,   925,   926,     0,     0,
     439,   808,   810,   804,   829,   810,   810,   806,     2,   810,
     805,   886,   810,   810,   883,     0,   531,   532,     0,     0,
     457,   457,     2,   457,   421,   460,   470,   524,     0,   553,
       0,   792,     2,     0,   694,   422,   538,   517,   534,   549,
       0,   792,     2,     0,   473,   518,   525,   526,   535,   540,
     550,   554,     0,   568,     0,   762,     2,     2,   790,   847,
     849,   457,     0,     2,     2,  1045,   538,  1048,   808,   808,
       3,     0,   538,     0,     0,   432,   810,   806,   805,     2,
     457,     0,   766,     0,   728,   730,   729,   731,     0,     0,
     724,     0,   714,     0,   723,   734,     0,   810,   810,     2,
     457,  1056,   458,   457,   469,   448,   516,   449,   541,   450,
     548,   545,   566,   810,   567,     0,   675,   457,   676,  1010,
    1011,   457,   677,   679,   555,   561,   636,   638,   639,   636,
     813,     0,   745,   733,     0,   817,    21,     0,    20,     0,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   457,     2,     0,   103,   104,
     105,   106,    87,    24,    88,    42,    86,   107,     0,     0,
     122,   124,   128,   131,   134,   139,   142,   144,   146,   148,
     150,   152,   155,     0,    26,     0,   562,     2,   107,   457,
     156,   739,   690,   552,   692,   738,     0,   689,   693,     0,
       0,     0,     0,     0,     0,     0,   827,   853,   810,   863,
     871,   875,   881,     2,  1043,   457,  1046,     2,   100,   457,
       3,   674,     0,  1056,     0,   458,   516,   541,   548,     3,
       3,   656,   660,   670,   676,   677,     2,   856,   874,  1033,
       2,     2,    23,     0,     2,   700,    24,     0,   698,   701,
    1054,     0,     0,   707,   696,   695,     0,     0,   794,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   832,   889,   810,     0,   694,     2,   828,
     836,   953,   830,   831,     0,   792,     2,   885,   893,     0,
     887,   888,     0,   435,   457,   457,   522,   458,     0,   538,
     457,  1038,  1042,  1040,   539,   766,     0,   792,   808,   415,
     423,   471,     0,   792,     2,   766,     0,   792,   743,   519,
     520,   536,   551,   557,   560,   555,   561,   579,   580,     0,
     744,   457,   684,     0,   201,   398,   457,     3,     0,   538,
     457,   791,   457,     0,   417,     2,   418,   763,   437,     0,
       0,     0,     2,   457,   808,   457,   766,     0,     2,     0,
     727,   726,   725,   720,   468,     0,   718,   735,   514,     0,
       0,   457,   457,  1012,   458,   454,   455,   456,  1016,  1007,
    1008,  1014,     2,     2,   101,     0,   972,   986,  1056,   968,
     810,   810,   977,   984,   682,   457,   546,   678,   458,   542,
     543,   547,     0,   810,  1022,   458,  1027,  1019,   457,  1024,
       0,   645,   637,   644,  1054,     0,   636,     0,     0,   457,
       0,   825,   824,   820,   822,   823,   821,     0,   815,   818,
       0,    22,   457,    94,     0,   457,   457,    89,   457,    96,
       0,    32,    36,    37,    33,    34,    35,   457,    92,    93,
     457,   457,   457,     2,   103,   104,     0,     0,   182,     0,
       0,   582,     0,  1032,     0,     0,     0,     0,     0,     0,
       0,     0,    55,     0,    61,    62,    66,     0,     0,    66,
       0,    90,    91,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   163,     0,
     161,   162,     2,   937,   691,   934,   810,   810,   942,   563,
     457,   854,   810,   864,   872,   876,   882,     2,   857,   859,
     861,     2,   877,   879,     0,  1044,  1047,   457,     0,     0,
       2,   101,   972,   810,  1056,   907,   810,   810,  1056,   810,
     922,   810,   810,     3,   678,     0,     0,  1056,  1056,   457,
     457,     0,     2,   709,     0,  1054,   706,  1055,     0,   702,
       0,     2,   705,   708,   179,   178,     0,     2,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     810,   841,   845,   884,   810,   898,   903,   833,   890,     0,
       0,   443,   949,     0,   795,     0,   457,   796,   436,     0,
       0,     0,     0,   434,     2,   797,     0,   419,   766,     0,
     792,     2,   798,     0,     0,     0,     0,   594,   663,   458,
       3,     3,   667,   666,   868,     0,     0,   457,   399,     0,
     538,     3,   100,     3,   457,     0,     3,   767,     2,   722,
     457,   457,   716,   715,   716,   515,   513,   638,   636,   810,
     810,  1018,   457,  1023,   458,   457,  1009,   457,     0,     0,
       0,   987,     0,   810,  1057,   973,   974,   683,   970,   971,
     985,  1013,  1017,  1015,   544,   579,     0,  1021,  1026,   641,
     636,     0,   646,     0,   636,   748,   746,     0,     0,   817,
      66,   778,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,   121,   120,     0,   117,
     116,    27,     0,    28,     0,     0,     0,     0,     3,    66,
       0,    51,     0,    52,    59,     0,    58,    70,     0,    67,
      68,    71,    54,     0,    53,    57,     0,     0,    50,   123,
     125,   126,   127,   129,   130,   132,   133,   137,   138,   135,
     136,   140,   141,   143,   145,   147,   149,   151,     0,     0,
     408,     0,     0,    29,     3,   700,   157,   457,     0,     0,
       0,   938,   939,   935,   936,   741,   740,     2,   858,   860,
     862,     2,   878,   880,   457,   457,   963,   962,     2,     0,
       0,     0,     0,     0,   810,   973,   910,   927,     2,   905,
     913,   680,   908,   909,   681,     2,   920,   930,   923,   924,
       0,     3,  1056,   427,     2,  1049,     2,   671,   672,   650,
       3,     3,     3,     3,   694,     0,   155,     0,     3,     3,
       0,   703,     0,   697,     0,   810,     0,   457,     3,   431,
     433,     0,   810,   842,   846,   810,   899,   904,     2,   834,
     837,   839,     2,   891,   894,   896,   808,     0,   950,     3,
     954,   955,     3,   800,     3,   528,   527,   530,   529,     2,
     767,   801,     2,   799,     0,   767,   802,   594,   594,   594,
     457,     0,     0,   685,     0,   402,     0,     0,   457,     0,
       2,     0,     0,     0,     0,     0,   184,     0,   332,   333,
       0,     0,   371,   370,     0,   159,   159,   377,   555,   561,
     198,     0,   185,     0,   209,   186,   187,   457,   203,   188,
     189,   190,   191,     0,   192,   193,   338,     0,   194,   195,
     196,     0,   197,   205,   538,   457,     0,   207,     0,   396,
       0,     0,     0,     3,     0,   780,   767,   755,   756,     0,
       3,   751,     3,     3,     0,   457,   732,   732,  1054,   636,
     636,  1020,  1025,     2,   100,   457,     3,   553,     3,   458,
       3,   810,   980,   983,   457,     3,   969,   975,   636,   810,
     810,     0,     0,   624,     0,   640,   636,  1054,     2,   814,
     816,     0,    95,   457,   457,     0,    99,    97,   457,     0,
     111,     0,     0,     0,   115,   119,   118,   183,     0,     0,
       0,   700,   108,   176,     0,     0,    45,    46,    84,     0,
      84,    84,     0,    72,    74,    48,     0,     0,    44,     0,
      47,   154,     0,     0,     0,     0,  1054,     3,   810,   945,
     948,   940,   457,   457,     3,     3,     0,   810,   916,   919,
     810,     0,   810,   810,   911,   928,   457,   457,  1050,     0,
     673,   457,   457,     0,     0,     0,     0,   416,     3,     0,
       0,     0,     0,   699,   704,     3,   793,   181,   180,     3,
       0,     0,     2,   835,   838,   840,     2,   892,   895,   897,
     457,   457,   694,   810,   961,   960,     0,     0,     0,     0,
       0,     0,   767,   803,     0,   457,   457,   457,   457,   457,
     457,   457,   577,   606,     3,     3,   607,   538,   595,     0,
       0,   850,     2,     0,   400,    66,     0,     0,   323,   324,
     206,   208,     0,     0,     0,   457,   457,   319,     0,   317,
       0,     0,     0,   700,     0,     0,     0,     0,     0,   160,
       0,     0,   378,     0,     0,     3,   213,     0,   204,     0,
     314,     0,     0,     2,     0,   538,   810,     0,   397,   965,
     964,     0,     2,     0,   758,     2,   753,     0,   754,     0,
     736,   717,   721,   719,     0,     0,     0,   457,     0,     0,
       0,     3,     0,     2,   976,   978,   979,     0,     0,   100,
       0,     3,  1054,   630,   636,   646,   646,   700,   647,  1054,
       0,   749,   457,   819,   966,     0,     0,     0,    38,     0,
     112,   114,   113,   110,   109,   700,  1054,     0,    65,    81,
       0,    75,    82,    83,    60,     0,     0,     0,    69,    56,
       0,   153,   407,    30,     0,     0,     2,   941,   943,   944,
       3,     3,     0,     0,   810,     2,   912,   914,   915,     2,
     929,   931,     0,   906,   921,     3,     3,  1051,     3,   658,
     657,   661,  1053,     2,     2,  1052,     0,     3,   807,   710,
     711,     0,     0,   810,   438,   457,   457,     3,     3,   444,
     809,     0,   956,     0,   957,   958,   952,   900,   784,     0,
     786,   577,   577,   577,   607,   613,   582,     0,   619,   607,
       0,   457,   569,   605,   601,     0,     0,     0,     0,   608,
     610,   810,   621,   621,     0,   602,   617,   457,   403,     0,
       0,    67,   327,   328,   325,   326,     0,     0,     2,   224,
       0,     0,   226,   411,   225,   538,   457,   305,   304,   306,
       0,     2,   184,   264,     0,   257,     0,   184,   320,   318,
       0,   312,  1054,   321,     0,     0,     0,   359,   360,   361,
     362,     0,   352,     0,   353,   329,     0,   330,     0,     0,
     457,   215,   202,   316,   315,     0,   350,   369,     0,   401,
     810,   457,   782,   737,   457,     2,     2,   629,   636,   636,
    1028,  1029,  1030,     0,   981,   457,     3,     3,     0,   989,
       0,     0,   636,     0,   643,   642,  1054,     0,   627,     3,
       0,   967,    98,     0,    31,   457,     0,  1054,     0,     0,
      85,     0,    73,     0,    79,     0,    77,    43,   158,   946,
     457,     0,     0,   851,   869,   457,   457,     0,     0,     0,
     457,   457,   713,     0,   424,   426,     3,     3,     0,     0,
       0,     0,   788,   573,   575,   571,     0,     0,   996,     0,
     614,  1001,   616,   993,   810,   810,   600,   620,   604,     0,
     603,     0,     0,     0,   623,     0,   810,   596,   611,   622,
     612,   618,   665,   669,   668,     0,     2,     0,     0,   245,
       2,   227,   538,   310,   308,   311,   307,     0,   309,     0,
     253,     0,   184,     0,     2,   457,   265,     0,   290,     0,
       0,   313,     0,     0,     2,   336,   363,     0,   354,     2,
       0,     0,     0,     0,   341,     0,   337,   200,   199,   425,
     752,     0,     0,  1054,  1054,  1031,     3,     0,     0,   988,
     990,   628,     0,   636,     0,   626,     2,    49,    41,    39,
      40,     0,    63,   177,    76,     0,     0,     3,   852,   870,
       3,     3,   917,   932,   428,     2,   655,     3,   654,   712,
       0,     0,   843,   901,   951,   959,   598,     0,     0,     0,
     997,   998,   810,   599,   994,   995,   597,   578,     0,     0,
     214,   335,     0,     0,     0,   238,     2,   216,     0,     0,
       2,   247,   262,   273,   267,     2,   184,   302,     0,   277,
       0,     0,   268,   266,   255,   258,     0,     0,   184,   291,
       0,     0,   219,   334,     2,   457,   331,     0,     0,   379,
       2,   339,     0,    66,     0,   351,   757,   759,     0,     0,
       0,   991,   992,   636,  1054,   648,   750,    64,    80,    78,
       0,     0,     0,   457,     0,   844,   902,   810,  1004,  1006,
     999,     0,   609,   233,   228,   231,     0,   230,   237,   236,
       0,   457,   240,   239,     2,   249,     0,   246,     2,     0,
       0,     0,   254,   259,     0,     0,   184,   303,   278,     0,
       0,     2,     0,   293,   294,   292,   261,   322,     0,   457,
     457,     3,   364,   458,   368,     0,   372,     0,     0,     0,
     380,   381,   222,   342,     0,     2,     0,     2,     2,   632,
     634,   982,  1054,     0,   947,   918,   933,   659,     2,  1000,
    1002,  1003,   615,     0,   235,     0,   234,   218,   241,   457,
     392,   250,     2,   251,   248,   263,   276,   274,   270,   282,
     280,   281,   279,   260,   275,   271,   272,   269,   256,     0,
       0,     0,     0,   221,   241,     3,   357,     0,   996,   365,
     366,   367,   379,     0,     0,     0,   379,     0,     2,   340,
     347,     0,   344,   346,     0,   633,   457,   229,   232,     2,
       3,   242,   393,   252,     0,     0,     0,     0,   301,   299,
     296,   300,   297,   298,   295,     3,   357,     0,     0,   997,
       0,     0,     0,   373,     0,   382,   223,     0,   337,     0,
     631,     3,   210,     0,     0,     2,   289,   287,   284,   288,
     285,   286,   283,     0,     0,   358,     0,   385,     0,   383,
       0,   385,   343,   345,     2,     0,     0,   212,   211,   217,
       0,   220,     0,   355,   386,     0,     0,   374,     0,   348,
       2,  1005,   356,     0,     0,     0,     0,   349,   387,   388,
       0,   384,   375,     0,     0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1789,  6486,  5823, -1789,    -1,   352,  1456,    57, -1789,  -316,
   -1789,   382, -1789,  -682,   685,   783,  -960,  -983, -1789,   223,
    5920,  1842, -1789,  1462, -1789,  1376,   591,   784,   785,   672,
     787,  1334,  1337,  1335,  1339,  1343, -1789,   119,  -151,  8378,
     910, -1789,  1649, -1789, -1789,  -651,  7557, -1067,  1594, -1789,
     205, -1789,   905,    -7, -1789, -1789, -1789,   455,    91, -1789,
   -1788, -1624,   317,    68, -1789, -1789, -1789,   323, -1495, -1789,
   -1399, -1789, -1789, -1789, -1789,    17, -1737,   203, -1789, -1789,
      22, -1789, -1789, -1789,    33,   482,   483,   147, -1789, -1789,
   -1789, -1789,  -738, -1789,    69,    12, -1789,   151, -1789,    82,
   -1789, -1789, -1789,   929,  -696,  -954, -1309, -1789,    16, -1211,
     132,  2469,  -823,  -675, -1789,  -284, -1789,    87,  -133,   695,
    -286,  -248,  3783,   281,  -609, -1789,    37,   152,   622,  2200,
   -1789,  2066, -1789,     3,  4303, -1789, -1789, -1789,   170, -1789,
   -1789,  1991,    76,  4892,  3084,   -59,  1862,  -323, -1789, -1789,
   -1789, -1789, -1789,  -213,  4665,  5194, -1789,  -380,   271, -1789,
    -639,   269, -1789,   204,   761, -1789,   559,   -51, -1789, -1789,
   -1789,  -317,  5562,  -824,  1197,    74,  -558,  -674,   -24,  1466,
   -1789, -1225,  -152,   496,  1342,   943,  2706,  -200,  -491,  -251,
    -175,  -439,  1327, -1789,  1653,    71,  1242,  1542, -1789, -1789,
   -1789, -1789,   320,  -167,     9,  -876, -1789,   -78, -1789, -1789,
     677,   498, -1789, -1789, -1789,  2140,  -757,  -427,  -749,     4,
   -1789, -1789, -1789, -1789, -1789, -1789,   -99,  -842,  -134, -1761,
    -171,  8216,   -58,  7094, -1789,  1202, -1789,   848,  -157,  -222,
    -218,  -209,    10,   -73,   -60,   -48,   465,    18,    29,    63,
    -204,   -56,  -198,  -182,  -180,  -715,  -660,  -646,  -632,  -699,
     -95,  -628, -1789, -1789,  -687,  1395,  1396,  1398,  1468, -1789,
     612,  7336, -1789,  -532,  -584,  -555,  -548,  -730, -1789, -1643,
   -1685, -1680, -1672,  -604,  -125,  -300, -1789, -1789,   -33,    21,
     -86, -1789,  7995,   143,   283,  -518
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1161,   222,   392,   393,    81,    82,   394,   368,   395,
    1467,  1468,   396,   978,   979,   980,  1272,  1273,  1274,  1479,
     418,   398,   399,   400,   686,   687,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   420,  1077,   688,
    1400,   749,   216,   751,   414,   816,  1162,  1163,  1164,  1165,
    1166,  1167,  1168,  2083,  1169,  1170,  1405,  1590,  1925,  1926,
    1855,  1856,  1857,  2050,  2051,  1171,  1604,  1605,  1606,  1758,
    1759,  1172,  1173,  1174,  1175,  1176,  1177,  1413,  1785,  1978,
    1895,  1178,  1179,  1622,  2068,  1623,  1624,  1961,  1180,  1181,
    1182,  1403,  1969,  1970,  1971,  2115,  2130,  1998,  1999,   292,
     293,   877,   878,  1134,    84,    85,    86,    87,    88,    89,
     451,    91,    92,    93,    94,    95,   230,   568,   453,   422,
     454,    98,   302,   100,   101,   102,   333,   334,   105,   106,
     168,   107,   896,   335,   154,   110,   250,   111,   155,   258,
     337,   338,   339,   156,   415,   116,   117,   341,   118,   559,
     866,   864,   865,  1562,   342,   343,   121,   122,  1130,  1368,
    1568,  1569,  1719,  1720,  1369,  1557,  1738,  1570,   123,   646,
    1662,   643,   344,   644,   645,  1235,  1070,   459,   460,   870,
     871,   461,   462,   872,   346,   563,  1186,   424,   425,   217,
     479,   480,   481,   482,   483,   321,  1206,   322,   894,   892,
     593,   323,   362,   324,   325,   426,   125,   174,   175,   126,
    1200,  1201,  1202,  1203,     2,  1119,  1120,   585,  1195,   127,
     312,   313,   260,   270,   542,   128,   220,   129,   231,  1079,
     857,   509,   166,   130,   657,   658,   659,   131,   233,   234,
     235,   236,   307,   133,   134,   135,   136,   137,   138,   139,
     239,   308,   241,   242,   243,   784,   785,   786,   787,   788,
     244,   790,   791,   792,   754,   755,   756,   757,   510,  1112,
    1347,   140,  1670,   618,   619,   620,   621,   622,   623,  1722,
    1723,  1724,  1725,   608,   464,   349,   350,   351,   427,   208,
     142,   143,   144,   353,   808,   624
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   192,   526,    80,   108,   691,   495,   567,   363,   626,
     496,   132,   345,   539,   193,  1204,   190,   199,   305,   497,
     150,   922,   141,  1277,   498,   141,   194,   987,   908,   413,
     499,   331,  1837,   850,   852,  1187,   805,  1838,   103,   348,
     240,   485,   647,   967,   603,  1839,   500,   636,   501,  1894,
     207,   639,  1284,  1050,   297,    80,    80,   909,    80,  1057,
     507,   634,   108,   178,   910,   637,  1022,  1046,  1933,   132,
     359,   697,   503,    80,  1208,  1841,  1592,   113,   960,  1394,
     141,   495,    80,  1047,   916,   496,  1519,  1520,    96,  1928,
      80,  1927,   195,   205,   497,    80,   103,    58,    80,   498,
     917,   506,    80,   196,  1125,   499,   237,   640,   854,   261,
    1237,   523,  1770,   271,   446,  1318,   207,   430,   861,   264,
    1040,   500,  1934,   501,   141,  1071,  1071,   574,   576,   218,
     431,  1196,   626,    90,  1041,   113,   151,   197,   514,  2002,
      80,   519,   432,    80,  1071,    80,    96,   503,  1042,   108,
     300,    80,  1043,   104,   575,   192,   132,    80,   936,   888,
     731,  1486,   536,   603,    80,    58,   218,   141,   193,  -394,
     493,   112,   546,   504,  1593,  1593,  1363,   164,  1994,   366,
     194,  1183,  1319,   103,   287,   692,  1628,  1344,    80,    80,
     287,    90,  -760,  1487,   282,   548,   908,  1920,   219,   205,
    -792,  1582,   732,    80,  1763,   609,   560,  1584,   433,  1345,
    1071,   104,   145,   476,   467,  1234,  1320,  1967,    80,   434,
     532,  1626,   113,   554,  1935,   909,   210,    80,    80,   112,
     192,   254,   910,    96,  1927,   265,   367,  -792,  -761,   205,
    -394,  2003,  1073,   193,    80,   579,   195,  1929,   504,   508,
     916,   256,    80,   435,  1629,   194,   841,   196,  1241,   641,
    1088,  1933,    80,   205,   642,    80,  1051,  1871,   157,  1026,
    1054,   823,    80,   268,   158,   824,   543,   532,    90,  1067,
    1068,   626,    80,    80,   825,    80,  1481,  1265,   881,   826,
     163,   197,   210,    58,   108,   827,  1381,  1933,   104,   412,
    1627,   903,    80,    80,  1373,   626,   809,  1364,  -395,  1050,
      80,   828,   626,   829,   205,   901,   112,    80,    80,  2030,
     515,  1592,    80,  1374,   609,  1187,  1193,   985,   570,   933,
    1304,   428,  1574,   597,  1291,  1995,  1996,   288,   928,   921,
     177,  1894,  1837,   185,   537,   837,  1305,  1838,   943,  1256,
    1993,  1575,   927,   179,   547,  1839,   148,   823,   789,   776,
      80,   824,    20,    80,   656,  1352,  1231,   113,  1876,  1877,
     825,  1351,   597,   286,  2049,   826,  1354,   508,  1040,  -395,
     255,   827,   188,  1227,  1320,  1841,   207,   575,   848,  1975,
    -574,   276,  1041,   279,   853,   281,   533,   828,   540,   829,
    2049,  1363,  1363,  1363,  1452,  1412,  1042,  2104,   172,   172,
    1296,   615,  1459,  1071,   199,    58,   845,   594,   541,  1593,
     430,   595,  1151,  1976,   837,   488,  1920,  2085,    80,  1376,
    1377,  1122,   648,   431,   255,   650,   279,   281,   856,   467,
      63,    64,  1380,   172,   859,   432,   838,  1423,   863,   590,
     331,    80,    80,   533,   908,  1365,   733,    58,   348,    58,
     734,   112,   942,    80,    80,   945,   946,  1373,   947,  1944,
    1945,  1183,    80,   611,   476,  1519,  1520,   949,   591,   592,
     951,   952,   953,   909,   256,   255,  1639,  -925,    76,    58,
     910,  1445,    80,   172,  -925,   516,   172,   571,   962,   508,
     609,    80,  1682,  1684,  1686,   900,   430, -1055,  1941,   180,
     172,   433,   467,  1227,  1852,  1853,    58,   357,   641,   431,
    1681,    80,   434,   642,    58,   838,   188,    80,   287,  1329,
      58,   432,  1364,  1364,  1364,  1761,   200,   582,  1727,   767,
    1769,   508,   211,   508,  1310,  1378,   626,   255,   206,   279,
     281,  1852,  1853,    58,  1593,   213,   435,  1728,    -3,   886,
    1574,   238,   210,    58,   262,    80,   214,    80,   272,  1027,
    1511,   172,   286,   508,   436,   108,  1286,   255,    80,  1730,
      80,   626,   215,   255,    80,   721,   722,   225,   108,   887,
     690,   467,   611,    58,    80,   132,  1048,  1854,    80,    80,
     613,  1736,  1212,  1549,  1055,   962,   141,   256,   613,   570,
    1098,   922,   698,   255,   508,   172,  1211,   699,   282,   631,
    1737,   281,   103,    58,  1771,   172,   860,  1736,  1490,   723,
     724,    80,  1081,  1102,  1881,   557,   172,   508,   562,   905,
     598,   282,  1061,  1332,    80,  1842,  1836,   508,   113,    14,
      15,    16,    17,    18,   206,  1736,   159,   674,   714,   160,
     161,   113,   162,   172,  1843,   715,   716,   428,   428,   468,
     172,   172,    96,  1336,  1846,   172,   554,   508,   198,    64,
    1365,  1365,  1365,    58,   186,  1731,  1090,   789,  1987,  1591,
    1607,   962,   962,  1580,   206,  1870,    97,   463,    80,   152,
      80,  1950,    80,  1443,   245,  1106,    80,   613,    58,    80,
     255,  1850,   962,   172,  1431,    58,   172,    90,   206,   844,
    1593,   256,  2020,  1815,   847,  1816,   274,  1753,  1754,  1755,
     275,   544,   955,   278,    80,   280,   255,   104,   631,   281,
     268,   855,   112,   956,   957,  1615,   553,    64,  1593,  1756,
    1375,   862,   962,   674,    97,   112,    14,    15,    16,    17,
      18,  1124,    58,  1496,   962,    58,    73,   508,   923,   759,
    -455,  1939,   962,   760,  2022,  1072,  1072,  -451,   203,    80,
     541,    80,  2055,   255,    58,   284,   752,   188,  1593,  -625,
     508,  1011,   428,    80,  1072,  -926,  -625,    78,    79,  1943,
      80,   286,  -926,   255,   172,  1280,   476,   287,   255,    80,
     255,  1956,  1276,  -394,   905,    58,   172,   172,    80,    80,
      80,   331,  1476,  1317,  1793,  1794,  1679,   962,   301,   348,
     255,  1230,   255,   255,   700,   295,   188,  2057,    80,   701,
    1260,    97,  1505,  -688,   203,  1509,   508,  1261,  -781,   613,
     255,   181,     6,     7,     8,     9,    10,    11,    12,    13,
    1072,  1764,   255,   361,  1988,  1478,  1765,   690,   508,  1753,
    1754,  1755,  1276,   690,    80,    80,   476,  -456,   274,  2013,
     108,   962,   690,   319,  1888,   255,   771,   631,   281,  1889,
     508,  1756,  1435,  1436,   468,   444,   626,  1107,   141,  2035,
    1762,   690,   364,   428,  2036,   365,  1197,  1324,  2100,   255,
     631,   141,   412,  2101,  1185,  1716,   255,   286,   148,   436,
    1729,   508,   191,  1076,   516,    80,   833,   931,   508,   921,
     886,   366,  1671,   159,  1591,  1342,   160,   161,   656,   162,
     466,   810,   811,   437,   232,   812,   528,   172,   531,  1303,
     789,   897,   899,   113,  1612,   438,   274,   275,  -452,   630,
     439,   280,   874,   875,    96,   287,   875,   468,    14,    15,
      16,    17,    18,   582,    80,   436,  1447,   508,   440,  1904,
    1556,   441,    80,   935,   925,   147,    97,   595,   170,   171,
      65,    66,    67,    68,    69,    70,    71,   172,   675,    73,
     306,  -442,  1048,   442,   436,   531,   613,   246,   247,    90,
     248,    80,   937,   938,   476,   249,   595,   939,  1666,   612,
     470,   256,  1198,   613,  -442,   484,   463,    58,   471,   104,
      78,    79,   541,   436,   428,   508,  1677,    80,   961,   363,
     363,  1031,   962,    80,    80,   508,   486,   112,  1469,   489,
    1113,   971,   506,   973,   490,   976,   256,   491,  1387,   984,
     492,  1121,   988,  1072,  1123,  1424,  1085,   286,  1126,  1607,
    1086,   508,    73,   524,    80,   505,   494,   232,   516,  1982,
     582,  1115,   508,  1458,   508,   962,   564,  1013,  1080,   525,
     609,  1539,   612,   306,   675,  -453,   613,  1370,   463,   717,
     718,  1594,   535,    78,   614,    14,    15,    16,    17,    18,
    1464,   331,   545,  1117,   203,   601,   615,   962,   218,   348,
      14,    15,    16,    17,    18,    73,  1275,  1281,   719,   720,
    1276,   586,    14,    15,    16,    17,    18,  1663,   633,   476,
     775,   108,    80,    80,    80,   612,   172,   725,   726,   613,
     255,   580,   306,   172,   200,   694,    78,   614,    73,   141,
    1521,   255,   642,   274,    58,   649,   476,   660,  1527,  1528,
     108,  1089,    80,  1091,   661,  1185,   664,  1430,  1717,    58,
      80,   760,   508,    80,    80,   255,   665,    80,   141,    78,
      79,    58,   666,  1463,   261,   271,   255,  1276,    80,   601,
     694,   264,   670,  1076,  1185,   255,   141,   694,  1674,  1572,
     713,  1974,  1675,   728,   113,  1753,  1754,  1755,  1747,  1773,
     886,   727,   962,   962,   443,    96,  1774,  1133,   172,   172,
    1086,    80,   463,   964,   965,  2000,   729,  1756,  1617,  1618,
    1619,  1620,  1621,   113,    80,  1775,  1757,   730,  1847,   962,
     735,    73,   760,   761,    96,  1063,  1064,  1753,  1754,  1755,
     476,   152,  1936,  2000,   962,  1282,   962,    97,    80,   962,
      90,   612,  2039,   463,   762,   613,  1276,  2102,  1229,  1756,
      97,   962,    78,   614,   763,  2126,   764,  1518,  -185,  2123,
     104,  1065,  1066,  2133,  2070,   463,   463,  2134,  2074,    90,
      80,   783,   255,  2052,   331,   990,   991,   992,   112,   765,
    1573,   766,   348,    -3,   463,   793,   254,   265,  -454,   104,
    1263,  1086,  1370,  1370,  1370,   -17,   255,  1558,  1370,  1353,
    -121,  -121,  -121,  -121,  -121,  -121,   256,   112,  1713,  1714,
    1715,   822,  1379,   495,   806,  1802,  1594,   496,   428,   651,
     232,  1278,  1279,   412,   807,   268,   497,   830,   923,  1398,
     817,   498,  -156,  -156,   831,    80,   832,   499,   834,    80,
     306,   541,    80,   835,   150,   836,   306,  1065,  1422,   141,
     463,  1484,  1485,   500,   840,   501,  1489,  1485,   108,   108,
     842,    19,   476,   997,   998,   999,  1000,  1572,  1355,  1356,
    1357,  1491,  1572,  1493,  1485,   294,   141,   141,  1037,  1477,
     503,   876,   476,   886,    80,   858,   306,  1529,  1477,  1037,
    1541,  -572,  1596,  1596,   652,   543,  -570,   885,   867,   306,
      48,    49,    50,    51,    52,    53,    54,    55,   412,   412,
     653,  1687,  1086,   654,   655,    65,    66,    67,    68,    69,
      70,    71,  1813,  1086,  1814,  1485,   476,  1824,  1825,  1834,
     962,   113,   113,  1892,  1893,   255,  -120,  -120,  -120,  -120,
    -120,  -120,  1908,  1485,   476,  1469,  1909,  1485,  1197,    80,
    1852,  1853,   889,   141,    80,    80,    80,  2123,  2124,  1521,
     151,  1434,   172,  1482,  1483,   172,   172,   172,  1573,   891,
     255,   993,   994,  1573,   995,   996,   255,  1732,   331,   895,
     823,   504,  1001,  1002,   824,   911,   348,    90,    90,   172,
    1460,  1739,  1739,   825,   913,   172,  1432,  1433,   826,  1664,
    1665,   615,   930,   562,   827,   934,   940,   104,   104,   941,
     172,    14,    15,    16,    17,    18,   959,   540,   963,  1521,
     828,   966,   829,    80,  1010,   112,   112,  1036,    80,    14,
      15,    16,    17,    18,    80,   969,    80,   541,  1015,  1494,
    1037,  1044,    97,    80,  1083,  1092,  1093,  1127,   837,  1094,
    1095,  1583,  1585,   463,   172,  1199,  1096,   476,    14,    15,
      16,    17,    18,  1255,  1198,  1097,  1116,  1465,  1118,  1962,
     476,  -764,  -664,  1128,  1188,    14,    15,    16,    17,    18,
    1189,  1129,   264,  1205,  1221,  1222,  1233,   141,    58,  1637,
    1266,  1223,   702,  1234,   703,   704,   705,  1239,  1039,  1236,
     783,  1238,  1242,   108,  1243,  1245,   147,   476,  1246,  1247,
     626,    65,    66,    67,    68,    69,    70,    71,   255,   262,
     272,   141,  1248,   706,  1249,  1267,   707,   708,  1897,  1251,
    1197,   709,   710,  1252,    58,   141,  1253,  1596,   306,  1258,
     477,  1259,  1283,  1288,  1962,  1289,  1827,  1290,    73,   838,
    1297,  1298,    80,  1299,    80,    75,  1782,   306,   802,  1300,
    1308,  -652,  1572,  -651,  1323,  1343,   255,  1402,   752,  -765,
    1331,  1924,   508,  1348,   428,  1371,   113,   518,   147,    78,
      79,   170,   171,    65,    66,    67,    68,    69,    70,    71,
    1372,  1382,   172,  1385,    73,   172,  1386,   254,   265,  1395,
    1396,    80,  1397,  1404,    80,  1661,  1406,  1412,  -687,   962,
    1418,  1416,  1667,  1419,  1717,   476,  1420,   256,   508,   476,
    1426,  1461,  1504,  1428,  1477,    78,    79,  1475,  1517,  1678,
    1492,  1521,    90,   476,  1522,   172,   268,  1523,   108,  1524,
    1525,  1485,  1530,   476,  1533,  1546,  1198,  1547,  1548,  1550,
    1563,  1389,   104,  1561,  1564,  1375,   141,  1587,  1630,  1608,
      80,    80,  1632,  1573,  1609,  1635,  1611,  1645,  1613,  1625,
     112,  1633,  1596,  1634,  1640,   495,   147,   463,   463,   496,
    1646,    65,    66,    67,    68,    69,    70,    71,   497,  1642,
    1643,  1647,  1648,   498,  1649,  1366,  1776,  1668,  2029,   499,
    1650,  1651,  1652,    97,  1654,   667,  1659,  1673,  1887,  1672,
    1676,   113,  2047,    83,  1924,   500,   149,   501,    80,  1680,
    1688,  1689,  1964,  1702,  1693,   476,  1712,  1694,   436,   476,
     711,   712,    97,  1529,   476,  1704,  1726,  1566,  1276,  1746,
    1748,   503,   219,  1750,  1779,  1781,   255,  1786,  1795,  1860,
     544,   711,  1799,  2072,   540,  1801,  1039,  1800,  1805,   476,
     758,  1803,  1302,   783,  1807,  1772,  1812,    90,  1818,  1829,
    1819,    83,  1822,   873,   541,  1823,   769,  1832,  1833,   772,
    1865,   711,   837,  1866,  1880,  1878,   189,   104,  1884,  1890,
    1886,  1151,  1891,  1905,  1903,    83,   172,  1964,  1901,   803,
    1902,   477,  1906,   476,   108,   112,  1907,   476,   229,   508,
     172,   253,  -653,  1915,  1916,    83,  1968,  1917,  1918,  1804,
     476,  1919,   141,   172,   192,  1938,  -555,  1946,  1951,  1940,
    1811,    80,   108,    80,  1949,  2125,   518,   193,  1596,   579,
    1965,   412,   504,  1979,   476,  1957,   476,   476,  1980,   194,
     141,  1825,   149,  1966,  1981,  1997,  1984,  1985,    83,  2006,
     172,   149,  1986,  2019,   304,   310,  1596,  2021,  2023,  2032,
    2033,   476,   108,  2034,  2037,  2038,   330,   113,  2041,  2045,
    2058,  2054,  2056,   838,  2067,  2078,   255,  2071,  2073,  2079,
     141,  2080,  2086,  2097,  2109,  2096,    80,    80,  2099,  2111,
     419,   189,   189,   255,  2116,   113,  1596,   476,   205,  2112,
    2117,  2120,   149,   449,  2121,  2131,   253,   306,   476,  2132,
    1366,  1366,  1366,   152,  1554,  1555,  1559,  1809,  2135,  1488,
    1581,  1003,   958,    90,  1005,  1004,  1401,   750,    80,  1006,
     229,   229,  1408,  1743,  1007,   113,  1898,  1899,  2110,   467,
      97,    97,   476,   104,   476,  1783,  2048,   304,  1875,  1968,
    1882,    90,  2065,  1968,  1968,    83,  2105,  1977,   172,  2094,
    2103,   112,   172,   476,  1777,  1778,  2075,  2025,   253,   476,
    2024,   104,   412,  2118,   412,   169,   172,  1417,  1922,   476,
     534,   255,  1560,    80,  1414,  1992,   172,  1232,  2098,   112,
    1409,    90,  1740,    80,  1082,   813,  1207,   893,   310,  1669,
       3,  1240,  1790,   172,   310,   304,   304,  1018,  1019,  1512,
    1020,   104,   149,   412,  2114,  1711,   147,  1199,  2114,   170,
     171,    65,    66,    67,    68,    69,    70,    71,     0,   112,
       0,     0,   330,   616,   625,   989,     0,     0,     0,     0,
    2128,     0,     0,     0,     0,     0,  2095,  1983,   873,   330,
       0,     0,   457,   330,     0,     0,   463,   463,   181,     6,
       7,     8,     9,    10,    11,    12,    13,     0,   172,     0,
       0,     0,   172,     0,  1571,     0,     0,   172,     0,     0,
     758,   758,     0,     0,     0,     0,     0,   419,     0,  1410,
    1029,     0,     0,  1032,     0,   412,     0,     0,    58,     0,
       0,     0,   172,     0,   255,     0,     0,   147,     0,   873,
     170,   171,    65,    66,    67,    68,    69,    70,    71,   273,
       0,   419,   187,   477,   753,  2044,   803,     0,     0,     0,
     147,   189,     0,   226,   227,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,   172,   149,    19,     0,
     172,   449,     0,     0,   518,   782,     0,   625,  1100,   257,
       0,  2113,  1104,   172,     0,     0,  1391,     0,     0,     0,
     277,     0,  1114,     0,     0,     0,  2031,  2122,  1301,    75,
       0,   606,     0,     0,   629,    97,     0,   172,     0,   172,
     172,    52,    53,    54,    55,   229,     0,     0,   606,  1199,
       0,     0,   606,     0,   229,     0,     0,     0,     0,     0,
     255,     0,     0,   257,   172,     0,     0,     0,     0,   873,
       0,     0,     0,     0,   304,     0,   419,   419,     0,     0,
     304,     0,   330,  1220,   147,     0,   873,   873,     0,    65,
      66,    67,    68,    69,    70,    71,   982,     0,     0,   147,
     172,     0,   170,   171,    65,    66,    67,    68,    69,    70,
      71,   172,  1571,     0,   257,     0,     0,  1571,     0,   463,
     304,     0,  1250,  1733,     0,  1571,     0,  1254,     0,     0,
       0,   304,     0,   304,     0,   330,   983,    83,  1262,     0,
       0,  1271,     0,     0,     0,   172,     0,   172,     0,  1271,
     606,     0,     0,   330,   449,   588,   625,    14,    15,    16,
      17,    18,     0,     0,   616,     0,   172,     0,   616,     0,
      97,     0,   172,     0,     0,     0,   257,   330,  1271,     0,
       0,   477,   172,     0,     0,     0,  2129,   625,     0,     0,
     330,     0,     0,  1287,     0,     0,  2136,     0,     0,   758,
       0,   149,     0,     0,     0,     0,   257,     0,     0,     0,
    1294,  1295,   257,   245,   419,     0,    58,   149,   149,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,   457,   149,   149,   149,     0,     0,     0,     0,     0,
       0,  1271,   257,     0,     0,     0,     0,     0,   147,     0,
       0,   226,   227,    65,    66,    67,    68,    69,    70,    71,
       0,     0,   202,     0,     0,     0,     0,     0,     0,     0,
    1334,     0,     0,  1338,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,   457,     0,   676,     0,   449,     0,
    1848,     0,     0,  1571,     0,     0,   780,    75,     0,     0,
     613,     0,   606,   457,   753,   753,     0,    78,   781,     0,
     667,     0,   419,     0,   147,     0,     0,   226,   227,    65,
      66,    67,    68,    69,    70,    71,   606,     0,   202,   449,
       0,     0,   782,     0,   782,     0,    97,     0,     0,   606,
       0,     0,    73,   202,   873,   873,     0,     0,     0,     0,
     306,   330,   330,     0,     0,     0,     0,     0,   873,   873,
       0,     0,  1565,    75,    97,   257,     0,   202,     0,  1566,
     330,     0,   304,    78,    79,     0,     0,     0,     0,     0,
     452,     0,   676,     0,     0,     0,     0,   711,     0,     0,
       0,   304,   873,   873,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,    97,     0,   147,  1571,     0,   354,
     355,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,  1470,  1471,  1472,     0,   477,     0,   419,
    1473,  1474,   202,     0,     0,  1271,   330,   457,     0,   257,
       0,     0,   149,   419,  1384,   181,     6,     7,     8,     9,
      10,    11,    12,    13,   330,    58,  1215,    76,     0,   257,
       0,     0,   356,     0,     0,     0,  1498,   616,  1500,  1501,
       0,     0,   173,   176,     0,  1507,     0,     0,   457,   257,
       0,     0,  1515,  1516,     0,     0,     0,   147,     0,   202,
     226,   227,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,   449,   221,     0,   202,
       0,     0,     0,     0,   257,    73,  1537,  1538,     0,     0,
     306,     0,   314,   315,   316,   317,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2027,    75,     0,   257,   508,
       0,     0,     0,   147,     0,   257,    78,    79,    65,    66,
      67,    68,    69,    70,    71,   974,     0,   298,     0,   477,
     299,   147,     0,     0,   170,   171,    65,    66,    67,    68,
      69,    70,    71,   753,   320,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,   580,   306,   873,   873,     0,
     782,     0,     0,     0,     0,   975,     0,   782,   202,     0,
       0,     0,     0,   606,     0,     0,   629,     0,     0,     0,
       0,   147,     0,   318,   226,   227,    65,    66,    67,    68,
      69,    70,    71,   477,     0,     0,     0,   306,   202,  1744,
       0,   319,     0,     0,    58,   487,     0,     0,     0,   330,
       0,   477,     0,     0,     0,     0,  1271,     0,     0,     0,
       0,  1271,  1271,  1271,   147,   457,     0,   226,   227,    65,
      66,    67,    68,    69,    70,    71,   147,   898,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,   538,
       0,     0,   149,     0,     0,     0,     0,     0,     0,   173,
     419,     0,     0,     0,    73,     0,     0,     0,     0,     0,
     173,     0,     0,   202,   202,     0,     0,   873,     0,   452,
    1228,  1706,  1707,     0,   228,    75,     0,  1636,     0,   419,
       0,     0,     0,     0,     0,    78,    79,   584,     0,     0,
       0,    58,     0,  1721,   587,   589,   253,    83,     0,   596,
       0,     0,   873,     0,     0,     0,     0,   873,   873,     0,
       0,   304,     0,  1110,     0,     0,     0,   149,     0,     0,
       0,     0,   202,   147,     0,     0,     0,   449,    65,    66,
      67,    68,    69,    70,    71,     0,     0,   320,   147,   257,
     320,   452,     0,    65,    66,    67,    68,    69,    70,    71,
     257,    73,     0,     0,     0,   115,   449,     0,   115,     0,
     149,     0,     0,     0,   202,     0,    73,  1784,     0,     0,
     147,    74,    75,     0,   257,    65,    66,    67,    68,    69,
      70,    71,    78,    79,     0,     0,  1038,    75,   202,     0,
     613,  1796,     0,     0,   147,     0,     0,    78,    79,    65,
      66,    67,    68,    69,    70,    71,  1268,     0,     0,  1271,
    1269,  1271,  1270,   115,     0,     0,     0,     0,  1301,    75,
       0,     0,     0,   330,   330,     0,  1817,     0,   221,     0,
       0,  1820,  1821,     0,     0,     0,     0,   115,     0,     0,
     797,   798,     0,    75,     0,     0,  1480,     0,     0,    58,
       0,     0,  1749,   259,     0,  1721,  1721,   115,     0,     0,
       0,     0,     0,     0,     0,  1760,     0,   149,   149,   149,
     149,     0,   149,   149,     0,   452,   606,     0,  1567,   310,
       0,   147,     0,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,   115,     0,     0,   419,   419,   202,
     115,     0,  1788,   115,     0,   457,     0,   259,     0,    73,
       0,     0,     0,     0,     0,     0,   452,   326,   115,   358,
       0,     0,     0,     0,     0,     0,     0,   253,     0,  2027,
      75,     0,     0,   508,     0,     0,     0,     0,   452,   452,
      78,    79,   423,     0,     0,     0,     0,     0,     0,   449,
       0,    58,     0,     0,   115,   423,   147,   452,   259,   553,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,   320,     0,     0,   149,     0,   616,     0,  1721,     0,
       0,     0,     0,   147,     0,     0,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,   115,  1012,     0,
    1851,    73,     0,     0,  1861,     0,     0,     0,     0,     0,
     259,   932,     0,   452,  1973,     0,     0,     0,  1874,     0,
     202,   228,    75,     0,     0,     0,     0,   558,  1883,     0,
       0,     0,    78,    79,     0,   115,     0,     0,     0,     0,
     259,     0,     0,     0,   257,  1990,   259,     0,   873,  1721,
       0,     0,     0,     0,   115,     0,  1567,  1718,     0,     0,
       0,  1567,     0,   419,     0,     0,     0,  1567,     0,  1567,
       0,     0,     0,     0,   115,     0,   259,   115,     0,   257,
       0,     0,     0,   202,     0,     0,     0,     0,   457,     0,
    1721,   115,     0,     0,     0,   115,     0,   310,   149,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1932,     0,     0,     0,  1937,     0,     0,   147,     0,  1942,
     198,    64,    65,    66,    67,    68,    69,    70,    71,   423,
       0,   147,   419,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,   330,  1972,     0,   149,    14,    15,    16,
      17,    18,     0,     0,     0,  1721,  1721,     0,     0,    73,
    1062,     0,     0,   423,     0,     0,    75,  1074,     0,   802,
       0,     0,  2081,     0,     0,     0,     0,   149,     0,   303,
      75,     0,     0,     0,     0,     0,     0,     0,  2001,   115,
      78,    79,  2004,   423,     0,     0,     0,  1721,     0,   259,
       0,     0,   330,   330,     0,  2018,    58,     0,  1544,     0,
       0,     0,     0,     0,     0,     0,   452,     0,     0,  1718,
    1718,     0,     0,     0,     0,     0,     0,   257,     0,  2040,
       0,  2042,  2043,     0,  1567,     0,     0,  1567,   147,     0,
       0,     0,  1135,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,   310,     0,  2053,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,   419,   423,   423,
       0,     0,   147,   259,   115,   257,     0,    65,    66,    67,
      68,    69,    70,    71,  1268,     0,    74,    75,  1269,     0,
    1270,     0,  2076,     0,   304,     0,     0,    78,    79,     0,
       0,     0,     0,  2082,     0,   115,     0,     0,     0,     0,
     115,     0,     0,   259,   115,     0,   115,     0,     0,     0,
       0,    75,     0,     0,  1683,     0,     0,   115,     0,   115,
       0,     0,     0,     0,   202,     0,     0,  2108,     0,  2082,
       0,     0,  1718,   358,   202,   115,   423,     0,   259,     0,
       0,  1567,     0,     0,     0,     0,     0,     0,  2119,     0,
       0,     0,     0,     0,  2108,     0,     0,     0,     0,   115,
       0,     0,   259,   202,  2127,     0,   558,     0,     0,   259,
       0,     0,   115,     0,   929,     0,     0,   149,     0,     0,
       0,     0,     0,   115,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,   423,     0,     0,   115,
     115,     0,   423,     0,     0,   330,     0,     0,     0,     0,
       0,   423,     0,  1718,   115,   115,   115,     0,     0,     0,
       0,     0,     0,   149,     0,     0,     0,     0,     0,     0,
     452,   452,     0,     0,    99,     0,     0,   153,     0,     0,
       0,     0,     0,     0,     0,   257,     0,    58,     0,     0,
       0,   149,   149,     0,  2028,   310,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     423,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,   226,   227,    65,    66,    67,    68,    69,    70,
      71,   149,    99,     0,   423,     0,     0,     0,     0,  1388,
    1390,  1392,     0,     0,     0,     0,   696,    73,     0,    76,
     386,   423,     0,     0,     0,     0,   204,     0,     0,  2028,
    2028,     0,     0,     0,     0,     0,   606,   303,    75,  1411,
       0,     0,     0,   115,   115,     0,   266,     0,    78,    79,
       0,     0,     0,     0,  1135,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   202,     0,     0,     0,
     147,  2028,     0,     0,     0,    65,    66,    67,    68,    69,
      70,    71,  1268,   296,     0,     0,  1269,  1111,  1270,    99,
     115,     0,     0,     0,     0,     0,     0,     0,  1456,     0,
       0,     0,     0,     0,     0,     0,     0,   332,     0,     0,
       0,   606,   257,   259,    14,    15,    16,    17,    18,    75,
       0,   423,  1685,     0,   259,     0,     0,     0,   115,     0,
       0,     0,   429,     0,   115,   423,     0,     0,     0,    58,
       0,     0,     0,   296,   455,     0,   115,     0,  1217,   423,
     147,   115,     0,   555,   556,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   147,   502,    58,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,   522,     0,
     202,     0,     0,   527,   529,  1923,   204,     0,   423,    73,
       0,    76,     0,     0,     0,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,   549,  1565,
      75,   551,     0,   552,     0,     0,     0,     0,     0,     0,
      78,    79,     0,    73,   569,     0,  1576,     0,     0,  1578,
       0,     0,     0,   369,     0,     0,   370,   581,   371,     0,
     372,     0,     0,  1565,    75,     0,     0,     0,     0,     0,
       0,   115,     0,     0,    78,    79,     0,   373,     0,     0,
     202,     0,     0,   604,     0,     0,   628,   147,   115,   115,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
     635,     0,     0,     0,   635,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,   257,     0,     0,    73,   147,    76,   452,
     452,   115,    65,    66,    67,    68,    69,    70,    71,  1268,
       0,     0,     0,  1269,     0,  1270,   385,     0,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,    75,     0,     0,     0,
       0,     0,   423,     0,     0,     0,     0,     0,   296,     0,
       0,     0,   604,   147,     0,     0,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,   147,
       0,   423,   226,   227,    65,    66,    67,    68,    69,    70,
      71,    73,     0,     0,     0,     0,     0,     0,   259,   115,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
    1741,   780,    75,     0,     0,   613,     0,     0,     0,   115,
       0,     0,    78,   781,     0,     0,     0,  2027,    75,   423,
       0,   508,     0,  1217,   109,   615,     0,     0,    78,    79,
       0,     0,     0,   455,   147,  1455,     0,   170,   171,    65,
      66,    67,    68,    69,    70,    71,     0,   115,   423,     0,
     147,     0,   115,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   869,     0,     0,     0,     0,   529,
       0,     0,     0,   880,     0,   569,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,   332,     0,    99,     0,
       0,     0,     0,     0,     0,     0,   115,   115,   466,     0,
       0,     0,   452,     0,   635,   904,     0,     0,     0,     0,
     115,   115,     0,     0,     0,   115,   115,     0,     0,   915,
       0,     0,     0,     0,     0,     0,   267,     0,   604,     0,
       0,     0,     0,   924,     0,     0,     0,     0,     0,     0,
       0,   635,     0,     0,   115,   115,     0,     0,     0,     0,
       0,     0,  1545,     0,     0,     0,     0,     0,     0,   115,
     115,   115,   115,   115,   115,   115,     0,     0,     0,   109,
       0,   259,   147,     0,     0,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,   336,   147,   423,
     423,   226,   227,    65,    66,    67,    68,    69,    70,    71,
      73,   147,     0,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,    73,  1896,     0,   259,
    1565,    75,     0,     0,   456,     0,     0,  1566,     0,    73,
       0,    78,    79,     0,     0,     0,   228,    75,     0,   455,
       0,   423,     0,     0,     0,     0,     0,    78,    79,   303,
      75,     0,     0,     0,     0,     0,  1021,     0,     0,     0,
      78,    79,     0,     0,   147,     0,   115,   170,   171,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
     904,     0,     0,     0,     0,  1045,   737,   738,   739,   740,
     741,   742,   743,   744,   745,   746,   747,     0,   550,     0,
     213,     0,   455,   455,     0,     0,     0,     0,     0,     0,
       0,     0,   470,     0,   109,     0,     0,     0,     0,     0,
       0,   455,     0,     0,     0,     0,     0,   748,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
     115,     0,     0,     0,     0,     0,     0,     0,     0,   869,
       0,     0,     0,   605,     0,     0,   267,     0,     0,     0,
       0,     0,     0,     0,     0,   423,     0,     0,     0,     0,
     605,     0,     0,     0,   605,     0,     0,     0,     0,     0,
    1184,   115,     0,     0,     0,     0,   119,   455,     0,   119,
       0,     0,     0,   153,     0,     0,     0,     0,     0,   259,
     115,     0,     0,     0,     0,   635,     0,     0,  1219,     0,
     869,     0,     0,     0,     0,  1225,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   423,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   115,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,   332,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   605,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,   115,
     115,     0,     0,     0,   115,   115,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     869,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,   119,     0,     0,   119,     0,     0,   869,   869,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,     0,     0,   259,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   423,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   336,   119,     0,     0,     0,     0,
     455,     0,     0,   267,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   456,     0,   109,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   605,   456,     0,     0,     0,     0,
       0,     0,     0,  1367,     0,     0,   119,     0,   119,     0,
       0,  1184,     0,   119,     0,     0,     0,     0,   605,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   605,     0,     0,     0,     0,     0,     0,     0,     0,
    1184,   114,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1415,   115,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   269,     0,   115,   604,     0,
       0,     0,     0,     0,     0,     0,     0,   527,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   869,   332,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,   456,
       0,     0,     0,   115,   115,     0,     0,   259,     0,     0,
     119,     0,     0,     0,     0,     0,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   869,   869,     0,     0,     0,
     456,     0,     0,   115,   119,     0,     0,     0,     0,   869,
     869,     0,     0,   458,   455,   455,     0,     0,     0,     0,
       0,     0,   336,   336,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   336,     0,   869,   869,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,  1367,  1367,
    1367,   153,   529,     0,     0,     0,     0,     0,     0,   336,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1595,  1595,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,   114,     0,     0,     0,   336,     0,   119,
     119,     0,     0,     0,     0,   120,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   605,     0,     0,   267,     0,
     336,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     332,     0,   607,     0,     0,   269,     0,     0,     0,     0,
       0,   119,     0,     0,     0,   119,     0,   119,     0,   607,
       0,     0,     0,   607,     0,   153,     0,     0,     0,     0,
     119,     0,     0,   120,     0,     0,     0,   456,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,   869,   869,
     336,     0,     0,     0,     0,     0,     0,   119,     0,     0,
     119,   119,     0,   119,   120,     0,     0,   336,   336,     0,
     120,   607,   119,   120,     0,   119,   119,   119,     0,     0,
    1735,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     869,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1752,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
     336,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1595,     0,     0,     0,     0,     0,     0,
       0,     0,   458,     0,   332,   119,     0,   153,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   869,     0,
       0,   109,     0,     0,     0,   120,     0,   120,     0,     0,
       0,     0,   120,   340,     0,     0,     0,     0,     0,     0,
       0,     0,   269,     0,   114,     0,     0,     0,     0,     0,
     109,     0,     0,   869,     0,   458,     0,   114,   869,   869,
       0,     0,     0,   455,   455,   120,     0,     0,   267,     0,
       0,     0,     0,   607,   458,     0,     0,     0,     0,     0,
       0,  1840,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   607,   605,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     607,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,   336,   456,  1595,     0,
       0,     0,     0,     0,     0,   119,   119,     0,     0,     0,
       0,     0,     0,   124,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   336,   336,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   336,
     336,     0,     0,   120,   336,   336,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,   458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   336,   336,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,  1963,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   109,
       0,   340,   340,     0,     0,     0,   455,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,   124,     0,
     340,   124,     0,     0,  1595,     0,     0,     0,   120,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   340,     0,
     456,     0,  1595,  1963,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,   124,     0,   120,     0,   120,     0,     0,   114,
       0,     0,     0,     0,     0,     0,   340,     0,     0,   120,
       0,     0,  1595,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   607,   119,     0,   269,     0,   340,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,  2069,     0,   124,     0,   124,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,   869,
       0,     0,   119,     0,     0,     0,     0,     0,   336,   336,
       0,     0,     0,   120,     0,     0,   458,     0,     0,     0,
     119,     0,     0,   124,     0,     0,   120,     0,     0,   120,
     120,     0,   120,     0,     0,     0,     0,     0,     0,     0,
     119,   120,   124,     0,   120,   120,   120,     0,     0,     0,
     336,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   267,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   340,
     212,     0,     0,   119,     0,     0,   223,   224,     0,     0,
       0,     0,     0,     0,     0,     0,   340,   340,     0,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   336,     0,     0,   124,     0,     0,
     285,     0,     0,     0,   120,     0,     0,     0,   336,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   340,
       0,   124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   336,     0,     0,     0,     0,   336,   336,
       0,     0,     0,   336,   336,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   119,   119,   119,   119,   119,   119,     0,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   119,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,   120,     0,     0,     0,     0,     0,   269,     0,     0,
       0,     0,     0,     0,   120,   120,   124,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     397,     0,     0,     0,     0,     0,     0,   607,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   577,     0,     0,     0,   119,   124,     0,
       0,     0,   124,     0,   124,   340,   458,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,   124,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -457,  -457,     0,  -457,    46,
       0,    47,     0,  -457,   340,   340,     0,     0,   605,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   340,   340,
      58,     0,     0,   340,   340,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,   336,     0,     0,     0,
       0,     0,     0,     0,   124,     0,   119,   124,   124,     0,
     124,     0,   340,   340,   109,     0,     0,     0,     0,   124,
       0,     0,   124,   124,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   109,   605,     0,     0,     0,     0,     0,     0,
       0,   778,     0,   779,     0,     0,     0,   114,   114,     0,
       0,     0,   795,   796,     0,     0,     0,     0,     0,   663,
       0,     0,     0,   397,   669,   119,     0,     0,     0,     0,
       0,     0,   109,   678,   679,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   397,   397,
       0,     0,   124,     0,   120,     0,     0,     0,     0,   458,
       0,     0,   120,     0,     0,     0,     0,     0,     0,   397,
     119,     0,     0,   369,     0,     0,   370,     0,   371,   336,
     372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,   373,     0,   397,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     879,     0,     0,     0,     0,   374,   375,     0,   472,   120,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,   340,   340,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,   120,     0,     0,     0,   385,    75,     0,   473,
     474,     0,   124,   124,   475,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,     0,   340,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     0,   269,   146,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   340,     0,   120,
     120,   120,   120,   120,   120,   120,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     120,     0,   340,     0,     0,     0,     0,   340,   340,     0,
       0,     0,   340,   340,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1060,     0,     0,     0,
       0,     0,   291,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   397,
     397,   397,     0,     0,     0,     0,   120,   114,    14,    15,
      16,    17,    18,     0,   119,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   124,  1131,  1132,     0,    46,     0,    47,     0,
     124,     0,     0,     0,  1190,  1191,  1192,     0,     0,  1194,
       0,     0,     0,   397,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,   291,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,   530,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   291,     0,
       0,     0,     0,     0,     0,   120,     0,     0,   291,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,   561,   565,     0,     0,     0,   607,     0,   572,
     573,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,  1264,     0,     0,     0,   583,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   340,     0,     0,     0,     0,
     124,     0,     0,     0,     0,   602,     0,     0,     0,     0,
       0,     0,     0,   114,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1285,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,   607,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     397,     0,   695,     0,     0,   397,     0,     0,     0,     0,
       0,     0,     0,     0,  1309,     0,   397,     0,     0,     0,
       0,   114,     0,  1313,  1314,  1315,  1316,     0,     0,     0,
       0,  1321,  1322,   736,     0,     0,     0,     0,     0,     0,
       0,  1330,     0,     0,     0,     0,     0,   124,   124,   124,
     124,   124,   124,   124,     0,     0,     0,     0,   397,   774,
       0,     0,  1346,   777,     0,  1349,     0,  1350,   340,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   124,     0,
       0,     0,   799,     0,     0,     0,   800,   801,     0,   120,
     804,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   818,   819,   820,   821,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1407,     0,     0,     0,   843,     0,     0,     0,     0,     0,
       0,     0,   846,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1421,     0,     0,     0,
       0,     0,     0,  1425,   124,  1427,  1429,     0,     0,     0,
     291,     0,     0,     0,     0,     0,     0,  1438,     0,  1439,
       0,  1440,     0,  1442,     0,     0,     0,     0,  1450,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   397,     0,
       0,   884,     0,     0,     0,     0,     0,     0,   561,     0,
       0,     0,     0,     0,   890,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   907,   912,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1495,     0,     0,     0,     0,     0,     0,  1502,  1503,     0,
       0,     0,     0,   124,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,     0,     0,     0,     0,
       0,  1526,   167,     0,     0,     0,     0,     0,  1531,     0,
       0,     0,  1532,   120,   397,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   167,   954,
       0,   397,   397,   397,     0,     0,     0,     0,   397,   397,
       0,     0,     0,     0,     0,     0,     0,     0,   223,     0,
       0,     0,   124,   120,     0,     0,     0,     0,     0,     0,
       0,     0,   397,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,   167,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,  1631,   167,
       0,     0,     0,     0,     0,     0,     0,   124,  1017,   397,
     397,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1034,     0,     0,     0,  1035,     0,     0,
     360,     0,     0,     0,  1653,     0,   907,     0,     0,     0,
       0,     0,  1658,     0,  1660,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,  1075,     0,
       0,     0,     0,     0,     0,     0,     0,  1084,     0,     0,
       0,     0,     0,  1087,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,  1691,  1692,     0,   167,   124,     0,   167,
     167,     0,     0,   167,     0,     0,   167,   167,  1697,  1698,
       1,  1699,     0,     0,  1224,     0,     0,     1,     0,     0,
    1703,     0,    14,    15,    16,    17,    18,     0,     0,     0,
    1708,  1709,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   370,     0,   371,     0,   372,     0,   167,     0,     0,
     167,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,   167,   167,     0,     0,     0,     0,     0,     0,     0,
    1244,     0,     0,     0,     0,     0,     0,   167,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,  1797,
    1798,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,  1806,   124,    76,   386,     0,     0,     0,     0,
     347,   387,   448,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,  1292,     0,     0,     0,  1293,     0,     0,
       0,   124,     0,     0,   907,     0,     0,     0,     0,  1830,
    1831,     0,   167,     0,  1306,     0,   445,   347,     0,     0,
       0,  1307,     0,     0,     0,     0,     0,     0,     0,     0,
    1311,     0,  1312,     0,     0,   397,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,   511,     0,
       0,     0,     0,     0,     0,   511,     0,     0,     0,     0,
       0,     0,     0,     0,  1340,     0,     0,   360,  1341,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,     0,     0,     0,     0,   146,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1900,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1910,     0,   511,  1911,  1912,     0,     0,     0,     0,     0,
    1914,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   347,   617,     0,     0,
       0,     0,     0,   360,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   638,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,     0,  1437,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,   167,   167,     0,     0,     0,     0,
       0,     0,     0,     0,  1462,     0,     0,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,   511,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,   397,     0,   511,   770,     0,   511,   773,     0,
       0,     0,     0,     0,  2026,   347,     0,     0,     0,   617,
       0,   385,  1260,     0,    76,   386,     0,     0,     0,  1261,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,   397,     0,     0,     0,     0,     0,  1535,     0,
       0,     0,  1536,     0,     0,     0,     0,     0,     0,     0,
     511,     0,     0,     0,   511,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2066,     0,
     167,   167,     0,     0,     0,     0,   167,     0,  1577,     0,
       0,     0,     0,     0,     0,     0,   347,     0,     0,     0,
       0,     0,     0,  2084,     0,     0,     0,   167,     0,     0,
     167,   167,     0,   167,     0,   167,   167,     0,  2093,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2106,     0,     0,     0,  1641,     0,
       0,  1644,     0,   397,     0,   397,   511,     0,     0,   347,
       0,     0,     0,     0,   167,     0,     0,     0,   167,  1655,
       0,     0,     0,     0,     0,     0,     0,   902,   347,     0,
       0,     0,   689,     0,     0,     0,     0,     0,   617,     0,
       0,     0,   617,     0,   397,     0,     0,     0,     0,   920,
       0,   347,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1690,     0,     0,     0,     0,   397,     0,     0,
       0,  1695,     0,   167,   167,  1696,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,     0,  1700,
    1701,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,   397,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,     0,  -458,    46,     0,    47,     0,
    -458,     0,   347,     0,     0,     0,     0,     0,   209,     0,
       0,   849,   851,     0,     0,     0,     0,    58,   511,   511,
       0,     0,     0,     0,   263,     0,     0,     0,   511,  1030,
       0,   511,  1033,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,   617,     0,   617,   617,
       0,     0,     0,     0,     0,   617,     0,     0,     0,     0,
       0,  1791,  1792,     0,     0,   347,   347,     0,   167,     0,
       0,     0,     0,     0,   209,     0,     0,     0,   311,     0,
       0,     0,     0,     0,   347,     0,     0,     0,   511,   352,
       0,     0,   511,     0,     0,     0,   511,  1101,     0,     0,
     511,  1105,     0,     0,     0,     0,     0,     0,  1108,   167,
       0,     0,     0,   209,     0,     0,   167,     0,     0,   167,
       0,     0,     0,     0,     0,     0,   465,     0,     0,   469,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   689,
     347,   511,     0,     0,     0,   689,     0,     0,     0,     0,
       0,     0,     0,     0,   689,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,   617,     0,   689,     0,     0,     0,     0,     0,     0,
       0,   263,     0,     0,   165,  1885,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1009,
     347,     0,  1644,     0,     0,     0,     0,   469,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,  1913,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,   167,   167,   610,     0,   627,     0,     0,
       0,     0,     0,     0,     0,     0,   283,     0,     0,     0,
    1931,     0,     0,     0,     0,     0,     0,     0,     0,   289,
       0,   290,     0,     0,     0,     0,     0,   511,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1959,     0,     0,
    1960,     0,     0,     0,   617,   617,     0,     0,     0,     0,
     693,   617,   167,     0,     0,     0,     0,     0,     0,     0,
       0,   167,     0,     0,   167,     0,   167,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,     0,     0,   511,  1335,
       0,   511,  1339,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,     0,   610,     0,     0,     0,     0,     0,
     794,   512,   513,     0,     0,   517,     0,     0,   520,   521,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  2046,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
     167,     0,     0,     0,     0,     0,     0,     0,     0,   209,
     209,     0,     0,     0,     0,   465,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   599,   600,     0,     0,     0,     0,     0,
       0,   347,     0,     0,     0,     0,     0,   617,  1446,   632,
     671,     0,     0,   672,   673,     0,   421,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   352,   450,
     347,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   478,     0,   478,     0,     0,   465,   167,   906,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   -16,     0,     0,     0,     0,
     610,     0,     0,     0,   511,  1499,     0,   167,     0,     0,
       0,     0,     0,   511,  1508,     0,   617,     0,     0,     0,
       0,     0,     0,     0,   209,     0,     0,   347,   347,     0,
       0,     0,     0,     0,   768,     0,     0,   693,     0,   167,
     693,   693,     0,   693,     0,   167,     0,     0,     0,     0,
       0,     0,   693,     0,     0,   693,   693,   693,     0,   578,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -458,  -458,     0,  -458,
      46,   839,    47,     0,  -458,     0,     0,     0,     0,     0,
       0,   465,     0,     0,   167,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   465,   347,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   465,   465,     0,     0,     0,     0,
     617,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   465,     0,     0,     0,     0,   167,   167,
       0,     0,     0,     0,    76,   309,   360,     0,     0,     0,
     167,     0,    78,    79,     0,     0,   918,   919,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   926,
       0,     0,     0,   478,     0,     0,     0,     0,     0,   478,
       0,     0,     0,     0,   815,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,     0,     0,   209,     0,     0,     0,
       0,   511,     0,     0,     0,     0,     0,     0,     0,   794,
       0,     0,     0,     0,     0,     0,     0,   511,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   167,     0,     0,  1586,
       0,     0,  1589,  1603,     0,     0,     0,     0,  1610,   352,
       0,   883,  1614,     0,  1616,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1023,  1024,     0,     0,     0,   347,  1028,     0,
     450,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   914,     0,     0,     0,     0,     0,  1049,
       0,     0,  1052,  1053,     0,  1056,     0,  1058,  1059,     0,
       0,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   347,   347,     0,     0,
       0,     0,     0,     0,     0,     0,  1099,     0,   948,     0,
    1103,     0,     0,   511,   511,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   511,
       0,     0,   815,   968,     0,     0,   970,     0,   972,     0,
       0,     0,   465,     0,   981,     0,   986,   981,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1710,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1014,  1209,  1210,     0,     0,     0,
       0,   167,     0,     0,     0,   693,     0,  1016,     0,  1226,
       0,     0,     0,     0,     0,     0,  1745,     0,  1025,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1751,     0,
       0,     0,   450,     0,     0,  1014,     0,     0,     0,     0,
       0,     0,     0,  1766,  1768,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   511,     0,     0,   263,
       0,     0,  1078,     0,   511,   478,     0,  1589,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     209,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     610,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   352,
       0,     0,     0,   693,     0,     0,     0,     0,     0,   347,
       0,     0,     0,   511,  1991,     0,     0,   511,     0,     0,
    1226,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1216,  1218,     0,     0,     0,     0,     0,     0,
     450,     0,     0,     0,     0,     0,     0,     0,   511,     0,
       0,  1326,     0,     0,     0,  1859,   465,   465,  1333,     0,
       0,  1337,     0,     0,  1862,     0,  1864,     0,   981,  1869,
    1873,     0,  1603,     0,     0,     0,     0,  1879,     0,     0,
       0,     0,  1014,     0,     0,     0,     0,     0,     0,     0,
    1257,     0,     0,     0,     0,     0,     0,   981,     0,     0,
     693,   693,   693,     0,     0,   693,   693,     0,     0,     0,
       0,     0,   469,   511,   511,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   478,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   511,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1948,     0,
       0,     0,   352,  1953,  1955,     0,     0,  1444,     0,     0,
       0,     0,     0,     0,     0,  1453,  1454,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     478,     0,  1325,     0,  1328,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2005,     0,  2008,     0,
       0,  2010,  2012,     0,  1497,     0,  2015,  2017,     0,     0,
       0,     0,     0,  1506,     0,     0,  1510,     0,  1513,  1514,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1399,  1399,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   209,     0,     0,  1540,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2060,  2062,  2064,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,     0,  1441,  2077,     0,
       0,     0,     0,  1451,     0,     0,     0,     0,     0,     0,
       0,     0,  2088,  2090,  2092,     0,     0,     0,     0,     0,
       0,     0,   450,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1638,     0,     0,     0,   352,     0,     0,   478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   981,     0,     0,   815,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     693,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   465,   465,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1534,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1510,     0,     0,     0,   369,  1542,  1543,   370,     0,   371,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   263,   373,  1705,
       0,     0,     0,   981,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   478,     0,     0,   815,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,     0,  2107,    73,     0,     0,
       0,     0,     0,     0,     0,   968,     0,     0,     0,     0,
       0,     0,     0,  1383,     0,  1656,  1657,   385,   977,  1579,
      76,   386,     0,     0,     0,   478,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   478,   369,   815,  1789,   370,     0,   371,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1137,     0,   373,    -2,
     693,  1139,  -243,  -243,  1140,  1141,  1142,  1143,  1144,  1145,
    1146,  1147,  1148,  1149,  1150,  1151,  -337,     0,  1152,  1153,
    1154,  1155,  1156,     0,  1157,     0,   374,   375,   465,   472,
       0,   377,  1158,  1159,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,  1160,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,     0,     0,    73,     0,   421,
    1844,  1845,     0,     0,  1734,     0,     0,     0,     0,     0,
       0,     0,  1849,     0,     0,   693,  -243,   385,   469,     0,
      76,   386,     0,     0,     0,   287,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,  -184,     0,     0,     0,     0,     0,     0,     0,
    2107,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1780,     0,     0,  1383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,     0,     0,
       0,  1808,     0,     0,  1810,     0,     0,     0,  1921,     0,
    1137,     0,   373,    -2,     0,  1139,  -244,  -244,  1140,  1141,
    1142,  1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,
    -337,     0,  1152,  1153,  1154,  1155,  1156,     0,  1157,  1835,
     374,   375,     0,   472,     0,   377,  1158,  1159,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,  1160,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,  1787,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1989,     0,     0,  1383,     0,     0,     0,
    -244,   385,     0,     0,    76,   386,     0,     0,     0,   287,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,  -184,   369,     0,     0,
     370,     0,   371,     0,   372,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1137,
       0,   373,    -2,     0,  1139,     0,     0,  1140,  1141,  1142,
    1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  -337,
       0,  1152,  1153,  1154,  1155,  1156,     0,  1157,     0,   374,
     375,     0,   472,     0,   377,  1158,  1159,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,  1160,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,   981,     0,    76,   386,     0,     0,     0,   287,     0,
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
    1148,  1149,  1150,  1151,  -337,     0,  1152,  1153,  1154,  1155,
    1156,     0,  1157,     0,   374,   375,    61,   472,     0,   377,
    1158,  1159,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,  1160,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,   385,     0,     0,    76,   417,
       0,     0,     0,   287,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,     0,     0,
    -184,     4,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1136,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
     370,    47,   371,     0,   372,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1137,
      58,  1138,    -2,     0,  1139,     0,     0,  1140,  1141,  1142,
    1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  -337,
       0,  1152,  1153,  1154,  1155,  1156,     0,  1157,     0,   374,
     375,    61,   472,     0,   377,  1158,  1159,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,  1160,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,    76,   417,     0,     0,     0,   287,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,  -184,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,   370,    47,   371,     0,   372,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,    61,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1597,
    1598,  1599,     0,     0,     0,   385,  1600,  1601,    76,   417,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,     0,     0,
    1602,     4,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
     370,    47,   371,     0,   372,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,    61,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1597,  1598,  1599,     0,     0,     0,
     385,  1600,     0,    76,   417,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,  1602,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,   370,    47,   371,     0,   372,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,    61,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,  1588,    76,   417,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     4,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,    61,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   417,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,    76,
     447,     0,     0,     0,     0,     0,   387,   448,    79,   388,
     389,   390,   391,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,  1213,     0,     0,     0,     0,
       0,   387,  1214,    79,   388,   389,   390,   391,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   447,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,  1930,     0,
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
      -2,    -2,  1958,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,  1448,    -2,     0,     0,     0,     0,    -2,    -2,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,   369,     0,     0,   370,
       0,   371,     0,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
    1449,    79,   388,   389,   390,   391,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    61,    62,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,    77,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   252,     0,     0,     0,  -783,     0,     0,    78,    79,
       4,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -390,  -390,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -390,     0,
       0,     0,    76,    77,     0,     0,     0,     0,     0,     0,
      78,    79,     4,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,     0,     0,     0,     0,  -391,  -391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -391,     0,     0,     0,    76,    77,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,   252,     0,  1358,
       0,  1359,     0,     0,    78,    79,  1360,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1361,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1362,     0,     0,
       0,    76,   944,     0,  1358,     0,  1359,     0,     0,    78,
      79,  1360,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1551,     0,     0,     0,    76,   944,     0,  1358,
       0,  1359,     0,     0,    78,    79,  1360,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1361,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1552,     0,     0,
       0,    76,   944,     0,  1358,     0,  1359,     0,     0,    78,
      79,  1360,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1553,     0,     0,     0,    76,   944,     0,     0,
       0,     0,     0,     0,    78,    79,   251,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,   251,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   252,
       0,    63,    64,     0,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     309,     0,     0,     0,     0,     0,     0,    78,    79,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,     0,  -458,    46,     0,    47,     0,
    -458,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   252,     0,     0,     0,  -787,     0,     0,    78,    79,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -458,  -458,     0,  -458,    46,     0,    47,
       0,  -458,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   252,     0,     0,     0,     0,     0,     0,    78,
      79,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   327,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,  1069,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -649,    76,   329,     0,     0,     0,    63,    64,     0,
      78,    79,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    76,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   327,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   328,    76,   329,     0,     0,     0,    63,    64,
       0,    78,    79,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    76,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,  1826,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   329,     0,     0,     0,     0,
       0,     0,    78,    79,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1828,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   309,     0,     0,
       0,     0,     0,     0,    78,    79,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
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
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,     0,     0,  1383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,   370,
       0,   371,     0,   372,     0,     0,     0,     0,    76,   252,
     182,     0,     0,   183,   184,     0,    78,    79,  1137,     0,
     373,     0,     0,  1139,  1852,  1853,  1140,  1141,  1142,  1143,
    1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  -337,     0,
    1152,  1153,  1154,  1155,  1156,     0,  1157,     0,   374,   375,
       0,   472,     0,   377,  1158,  1159,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,  1160,   380,   381,   382,
    1383,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,   369,    76,   386,   370,     0,   371,   287,   372,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,     0,  1137,  -184,   373,     0,     0,  1139,     0,
       0,  1140,  1141,  1142,  1143,  1144,  1145,  1146,  1147,  1148,
    1149,  1150,  1151,  -337,     0,  1152,  1153,  1154,  1155,  1156,
       0,  1157,     0,   374,   375,     0,   472,     0,   377,  1158,
    1159,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,  1160,   380,   381,   382,     0,   383,   384,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,     0,     0,    76,   386,     0,
       0,     0,   287,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
      14,    15,    16,    17,    18,    19,   680,    20,   681,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   682,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   683,     0,     0,     0,   287,     0,   387,
      78,    79,   684,   685,   390,   391,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,   370,    47,   371,     0,   372,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,   416,    76,   417,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   683,     0,     0,     0,   287,
       0,   387,    78,    79,   388,   389,   390,   391,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   369,     0,    46,   370,    47,   371,
       0,   372,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
      76,   417,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,   370,    47,   371,     0,   372,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   373,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,   566,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   251,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      63,    64,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -458,
    -458,     0,  -458,    46,     0,    47,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,     0,     0,     0,    58,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    63,
      64,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,    77,     0,
       0,     0,  -785,     0,     0,    78,    79,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
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
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   868,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -662,    76,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1742,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    76,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,   309,
       0,     0,     0,     0,     0,     0,    78,    79,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   327,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,    63,    64,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,    76,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1466,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   950,    76,   944,     0,     0,     0,    63,    64,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   944,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   294,     0,
       0,     0,    63,    64,     0,    78,    79,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,     0,     0,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   443,     0,     0,     0,    63,    64,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   329,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,    63,
      64,     0,   327,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   294,     0,
       0,     0,    63,    64,     0,    78,    79,     0,     0,     0,
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
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   309,     0,     0,     0,    63,    64,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   944,     0,     0,     0,
       0,     0,     0,    78,    79,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -458,  -458,
       0,  -458,    46,     0,    47,     0,  -458,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,    63,    64,
       0,   327,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,    76,     0,    46,     0,
      47,    63,    64,     0,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     944,     0,     0,     0,    63,    64,     0,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,    14,    15,    16,    17,    18,
      78,    79,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -458,
    -458,     0,  -458,    46,     0,    47,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,     0,     0,
      63,    64,     0,     0,     0,    78,    79,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,     0,     0,     0,     0,    76,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
     448,    79,   388,   389,   390,   391,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   369,     0,    46,   370,    47,
     371,     0,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,   369,     0,
       0,   370,     0,   371,    58,   372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,   369,     0,     0,   370,     0,
     371,    73,   372,     0,     0,     0,     0,    76,     0,     0,
       0,     0,     0,     0,     0,  1597,  1598,  1599,     0,   373,
       0,   385,  1767,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,  1867,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,   369,     0,     0,   370,     0,   371,    73,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1597,  1598,  1599,     0,   373,     0,   385,  1868,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,   475,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
     814,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,     0,     0,    76,   386,     0,     0,
       0,   287,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,   977,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,  1008,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  1327,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,  1393,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,     0,     0,    76,   386,     0,     0,
       0,  1457,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,  1858,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  1863,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  1872,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    1952,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  1954,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2007,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  2009,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  2011,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    2014,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  2016,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2059,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  2061,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  2063,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    2087,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  2089,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2091,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   662,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   668,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   677,     0,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,     0,
      76,   386,     0,     0,     0,     0,     0,   387,   882,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,   448,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
    1947,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   671,     0,     0,   672,   673
};

static const yytype_int16 yycheck[] =
{
       1,    74,   250,     4,     1,   385,   228,   291,   175,   332,
     228,     1,   164,   264,    74,   891,    74,    76,   152,   228,
       4,   625,     1,   983,   228,     4,    74,   709,   612,   180,
     228,   164,  1717,   524,   525,   877,   475,  1717,     1,   164,
      96,   216,   359,   694,   330,  1717,   228,   347,   228,  1786,
      83,   351,  1012,   783,   140,    56,    57,   612,    59,   789,
     231,   347,    59,    59,   612,   351,   753,   782,  1856,    59,
     165,   387,   229,    74,   898,  1718,  1385,     1,   687,  1146,
      59,   303,    83,   782,   616,   303,  1311,  1312,     1,     1,
      91,  1852,    74,    83,   303,    96,    59,    72,    99,   303,
     618,    99,   103,    74,   861,   303,    96,    10,   535,    99,
     934,   245,  1607,   103,   200,  1075,   149,   190,   545,   103,
     780,   303,    75,   303,   103,   799,   800,   298,   299,    89,
     190,   888,   455,     1,   780,    59,     4,    74,   237,    75,
     141,   240,   190,   144,   818,   146,    59,   304,   780,   146,
     146,   152,   780,     1,   152,   228,   146,   158,   649,   586,
     133,   123,   261,   449,   165,    72,    89,   146,   228,    89,
     228,     1,   271,   229,  1385,  1386,  1130,   152,     1,   118,
     228,   877,   134,   146,   160,   385,    84,   146,   189,   190,
     160,    59,     0,   155,   155,   273,   780,  1840,   158,   189,
     160,   177,   175,   204,  1603,   330,   284,   177,   190,   168,
     884,    59,     0,   214,   204,   176,   168,   156,   219,   190,
     253,    98,   146,   282,   177,   780,    83,   228,   229,    59,
     303,    99,   780,   146,  1995,   103,   179,   160,     0,   229,
     160,   177,   800,   303,   245,   303,   228,   159,   304,   156,
     782,    99,   253,   190,   152,   303,   507,   228,   940,   162,
     818,  2049,   263,   253,   167,   266,   784,  1762,   118,   760,
     788,   493,   273,   103,   152,   493,   266,   310,   146,   797,
     798,   604,   283,   284,   493,   286,  1269,   969,   572,   493,
     152,   228,   149,    72,   291,   493,  1138,  2085,   146,   180,
     177,   601,   303,   304,   158,   628,   481,  1130,    89,  1039,
     311,   493,   635,   493,   304,   601,   146,   318,   319,  1962,
     238,  1630,   323,   177,   449,  1167,   884,   707,   291,   646,
    1045,   188,   158,   324,  1021,   158,   159,   132,   638,   625,
     152,  2078,  2027,    62,   262,   502,  1045,  2027,   664,   958,
     134,   177,   638,   152,   272,  2027,     4,   579,   453,   445,
     361,   579,    20,   364,   365,  1122,    10,   291,  1767,  1768,
     579,  1120,   363,   152,  1998,   579,  1125,   156,  1038,   160,
      99,   579,   152,   915,   168,  2028,   419,   152,   522,    75,
     160,   110,  1038,   112,   528,   114,   253,   579,   266,   579,
    2024,  1355,  1356,  1357,  1228,    91,  1038,    75,    56,    57,
    1038,   176,  1236,  1087,   473,    72,   515,   154,   266,  1630,
     493,   158,    90,   109,   581,   220,  2069,  2051,   429,    61,
      62,   858,   361,   493,   153,   364,   155,   156,   537,   429,
     107,   108,  1138,    91,   543,   493,   502,  1196,   547,   134,
     583,   452,   453,   310,  1038,  1130,   154,    72,   583,    72,
     158,   291,   662,   464,   465,   665,   666,   158,   668,  1868,
    1869,  1167,   473,   330,   475,  1700,  1701,   677,   163,   164,
     680,   681,   682,  1038,   332,   204,   177,   160,   155,    72,
    1038,  1221,   493,   141,   167,   152,   144,   292,   158,   156,
     625,   502,  1485,  1486,  1487,   600,   579,   151,   168,   152,
     158,   493,   502,  1045,    77,    78,    72,   165,   162,   579,
    1480,   522,   493,   167,    72,   581,   152,   528,   160,  1087,
      72,   579,  1355,  1356,  1357,  1602,   157,   152,   158,   152,
    1607,   156,   177,   156,  1062,   177,   869,   266,    83,   268,
     269,    77,    78,    72,  1765,   149,   493,   177,   158,   583,
     158,    96,   419,    72,    99,   566,   160,   568,   103,   152,
    1300,   219,   152,   156,   154,   572,  1015,   296,   579,   177,
     581,   904,   176,   302,   585,   128,   129,   177,   585,   585,
     385,   581,   449,    72,   595,   585,   152,   160,   599,   600,
     156,   158,   902,  1352,   152,   158,   585,   455,   156,   572,
     152,  1215,   156,   332,   156,   263,   902,   161,   155,   338,
     177,   340,   585,    72,   177,   273,   544,   158,  1279,   172,
     173,   632,   807,   152,   160,   283,   284,   156,   286,   602,
     154,   155,   793,   152,   645,   158,   177,   156,   572,    13,
      14,    15,    16,    17,   189,   158,    58,   376,   163,    61,
      62,   585,    64,   311,   177,   170,   171,   524,   525,   204,
     318,   319,   585,   152,   177,   323,   735,   156,   107,   108,
    1355,  1356,  1357,    72,    62,  1561,   820,   782,  1913,  1385,
    1386,   158,   158,  1375,   229,  1762,     1,   201,   699,     4,
     701,   168,   703,   152,     3,   839,   707,   156,    72,   710,
     429,   177,   158,   361,  1205,    72,   364,   585,   253,   514,
    1931,   569,   168,  1683,   519,  1685,   104,   146,   147,   148,
     108,   266,   154,   111,   735,   113,   455,   585,   457,   458,
     570,   536,   572,   165,   166,  1396,   107,   108,  1959,   168,
     152,   546,   158,   472,    59,   585,    13,    14,    15,    16,
      17,   860,    72,   152,   158,    72,   132,   156,   625,   154,
     134,   177,   158,   158,   168,   799,   800,     3,    83,   780,
     628,   782,   168,   502,    72,   158,   152,   152,  1999,   160,
     156,   734,   649,   794,   818,   160,   167,   163,   164,  1866,
     801,   152,   167,   522,   452,   151,   807,   160,   527,   810,
     529,  1878,   158,   160,   777,    72,   464,   465,   819,   820,
     821,   954,  1261,  1074,  1648,  1649,  1477,   158,   176,   954,
     549,   926,   551,   552,   156,   140,   152,   168,   839,   161,
     153,   146,   152,   159,   149,   152,   156,   160,   160,   156,
     569,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     884,   154,   581,   152,   152,   151,   159,   662,   156,   146,
     147,   148,   158,   668,   875,   876,   877,   134,   256,  1946,
     877,   158,   677,   174,   154,   604,   152,   606,   607,   159,
     156,   168,  1209,  1210,   429,   200,  1219,   840,   877,   154,
     177,   696,   152,   760,   159,   152,   890,  1082,   154,   628,
     629,   890,   793,   159,   877,  1554,   635,   152,   566,   154,
    1559,   156,    74,   804,   152,   926,   154,   644,   156,  1215,
     954,   118,  1464,    58,  1630,  1106,    61,    62,   939,    64,
     152,   155,   156,   154,    96,   159,   251,   595,   253,  1044,
    1045,   599,   600,   877,  1393,   154,   334,   335,     3,   337,
     154,   339,   154,   158,   877,   160,   158,   502,    13,    14,
      15,    16,    17,   152,   975,   154,  1224,   156,   154,  1803,
    1360,   154,   983,   154,   632,   104,   291,   158,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   645,   376,   132,
     152,   154,   152,   154,   154,   310,   156,    47,    48,   877,
      50,  1012,   154,   154,  1015,    55,   158,   158,  1457,   152,
     152,   869,   890,   156,   177,    22,   530,    72,   158,   877,
     163,   164,   880,   154,   891,   156,  1475,  1038,   154,  1206,
    1207,   152,   158,  1044,  1045,   156,   152,   877,  1248,   152,
     845,   699,    99,   701,   158,   703,   904,   158,   177,   707,
     158,   856,   710,  1087,   859,  1199,   154,   152,   863,  1765,
     158,   156,   132,   152,  1075,   158,   228,   229,   152,  1903,
     152,   154,   156,  1234,   156,   158,   151,   735,   805,   152,
    1215,  1342,   152,   245,   472,     3,   156,  1130,   602,   165,
     166,  1385,   160,   163,   164,    13,    14,    15,    16,    17,
    1243,  1244,   160,   154,   419,   157,   176,   158,    89,  1244,
      13,    14,    15,    16,    17,   132,   154,  1008,   126,   127,
     158,   160,    13,    14,    15,    16,    17,  1454,   154,  1140,
     445,  1138,  1143,  1144,  1145,   152,   794,   130,   131,   156,
     869,   303,   304,   801,   157,   158,   163,   164,   132,  1138,
    1312,   880,   167,   541,    72,   176,  1167,   154,  1319,  1320,
    1167,   819,  1173,   821,   118,  1138,   152,   154,   152,    72,
    1181,   158,   156,  1184,  1185,   904,   152,  1188,  1167,   163,
     164,    72,   152,   154,  1184,  1185,   915,   158,  1199,   157,
     158,  1185,   152,  1084,  1167,   924,  1185,   158,   154,  1366,
     169,  1893,   158,   162,  1138,   146,   147,   148,   154,   154,
    1244,   164,   158,   158,   156,  1138,   154,   875,   876,   877,
     158,  1232,   736,   163,   164,  1931,   174,   168,   111,   112,
     113,   114,   115,  1167,  1245,   154,   177,   132,   154,   158,
     155,   132,   158,   154,  1167,   157,   158,   146,   147,   148,
    1261,   566,   154,  1959,   158,   159,   158,   572,  1269,   158,
    1138,   152,   154,   777,   154,   156,   158,   154,   926,   168,
     585,   158,   163,   164,   154,   154,   154,  1311,   177,   158,
    1138,   157,   158,   154,  2032,   799,   800,   158,  2036,  1167,
    1301,   453,  1021,  1999,  1437,   714,   715,   716,  1138,   154,
    1366,   154,  1437,   157,   818,   134,  1184,  1185,   134,  1167,
     157,   158,  1355,  1356,  1357,   159,  1045,  1360,  1361,  1124,
      13,    14,    15,    16,    17,    18,  1184,  1167,  1551,  1552,
    1553,   493,  1137,  1565,   159,  1662,  1630,  1565,  1205,    13,
     502,   157,   158,  1234,   158,  1185,  1565,   154,  1215,  1154,
     152,  1565,   157,   158,   154,  1366,   154,  1565,   154,  1370,
     522,  1219,  1373,   154,  1358,   154,   528,   157,   158,  1358,
     884,   157,   158,  1565,   152,  1565,   157,   158,  1385,  1386,
     157,    18,  1393,   721,   722,   723,   724,  1554,  1127,  1128,
    1129,  1282,  1559,   157,   158,   156,  1385,  1386,   157,   158,
    1567,    70,  1413,  1437,  1415,   160,   568,   157,   158,   157,
     158,   160,  1385,  1386,    88,  1415,   160,   579,   160,   581,
      57,    58,    59,    60,    61,    62,    63,    64,  1319,  1320,
     104,   157,   158,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   157,   158,   157,   158,  1457,   157,   158,   157,
     158,  1385,  1386,   158,   159,  1184,    13,    14,    15,    16,
      17,    18,   157,   158,  1475,  1675,   157,   158,  1462,  1480,
      77,    78,   157,  1462,  1485,  1486,  1487,   158,   159,  1641,
    1358,  1208,  1140,  1270,  1271,  1143,  1144,  1145,  1554,   152,
    1219,   717,   718,  1559,   719,   720,  1225,  1565,  1641,    78,
    1732,  1567,   725,   726,  1732,   157,  1641,  1385,  1386,  1167,
    1237,  1572,  1573,  1732,    18,  1173,  1206,  1207,  1732,  1455,
    1456,   176,   158,  1181,  1732,   160,   152,  1385,  1386,   177,
    1188,    13,    14,    15,    16,    17,    18,  1415,   154,  1701,
    1732,   154,  1732,  1554,   177,  1385,  1386,   157,  1559,    13,
      14,    15,    16,    17,  1565,   160,  1567,  1415,   160,  1286,
     157,    18,   877,  1574,   151,   154,   154,   160,  1735,   154,
     154,  1376,  1377,  1087,  1232,   890,   154,  1588,    13,    14,
      15,    16,    17,    18,  1462,   154,   154,  1245,   154,  1885,
    1601,   151,   154,   160,    70,    13,    14,    15,    16,    17,
     177,   160,  1596,   176,   154,   154,   151,  1596,    72,  1414,
     157,   154,   121,   176,   123,   124,   125,   154,   780,   160,
     782,   160,   154,  1630,   158,   158,   104,  1638,   154,   154,
    1963,   109,   110,   111,   112,   113,   114,   115,  1367,  1184,
    1185,  1630,   158,   152,   154,   157,   155,   156,  1792,   154,
    1644,   160,   161,   154,    72,  1644,   154,  1630,   820,   154,
     214,   154,   154,   154,  1960,   154,  1700,   154,   132,  1735,
     154,   154,  1683,   154,  1685,   153,  1629,   839,   156,   154,
     157,   154,  1849,   154,   151,   154,  1415,    14,   152,   151,
     176,  1852,   156,   158,  1561,   154,  1630,   239,   104,   163,
     164,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     158,   152,  1370,   152,   132,  1373,   152,  1595,  1596,   152,
     152,  1732,   152,    74,  1735,  1452,   177,    91,   159,   158,
     177,   159,  1459,   157,   152,  1746,   157,  1595,   156,  1750,
     177,   151,   154,   177,   158,   163,   164,   160,   157,  1476,
     177,  1913,  1630,  1764,   154,  1413,  1596,   158,  1765,   158,
     154,   158,   157,  1774,   154,   157,  1644,   154,   151,   151,
     177,   177,  1630,   152,   177,   152,  1765,    80,   152,   177,
    1791,  1792,   151,  1849,   177,   152,   177,   158,   177,   177,
    1630,   177,  1765,   177,   154,  2027,   104,  1311,  1312,  2027,
     158,   109,   110,   111,   112,   113,   114,   115,  2027,   151,
     151,   151,   160,  2027,   160,  1130,  1621,   151,  1962,  2027,
     157,   157,   157,  1138,   154,   373,   157,   159,  1781,   154,
     159,  1765,  1993,     1,  1995,  2027,     4,  2027,  1849,   121,
     151,   154,  1885,   157,   154,  1856,   151,   154,   154,  1860,
     398,   399,  1167,   157,  1865,   154,   177,   159,   158,   154,
     152,  2028,   158,   154,   152,   152,  1595,   110,   157,    75,
    1415,   419,   157,  2034,  1752,   151,  1038,   157,   151,  1890,
     422,   160,  1044,  1045,   154,  1612,   151,  1765,   154,   157,
     154,    59,   154,   561,  1752,   154,   438,   154,   154,   441,
      75,   449,  2069,   177,   151,   177,    74,  1765,   152,   154,
     177,    90,   154,   151,   160,    83,  1574,  1960,   157,   473,
     157,   475,   151,  1934,  1931,  1765,   151,  1938,    96,   156,
    1588,    99,   154,   154,   154,   103,  1889,   154,   154,  1666,
    1951,   154,  1931,  1601,  2027,    75,   155,   177,    75,   168,
    1677,  1962,  1959,  1964,   168,  2116,   498,  2027,  1931,  2027,
     159,  1852,  2028,   151,  1975,   177,  1977,  1978,   151,  2027,
    1959,   158,   140,   177,   154,   151,   154,   154,   146,   153,
    1638,   149,   154,   168,   152,   153,  1959,   168,   151,   159,
     104,  2002,  1999,   152,   158,    75,   164,  1931,   152,   151,
     153,   168,   168,  2069,   177,   110,  1735,   157,   177,   110,
    1999,   151,   153,   159,   151,   154,  2027,  2028,   154,   151,
     188,   189,   190,  1752,   152,  1959,  1999,  2038,  2028,   154,
     177,    75,   200,   201,   154,   154,   204,  1199,  2049,   177,
    1355,  1356,  1357,  1358,  1359,  1360,  1361,  1675,   177,  1276,
    1375,   727,   686,  1931,   729,   728,  1156,   418,  2069,   730,
     228,   229,  1167,  1577,   731,  1999,  1793,  1794,  2085,  2069,
    1385,  1386,  2083,  1931,  2085,  1630,  1995,   245,  1765,  2032,
    1773,  1959,  2024,  2036,  2037,   253,  2079,  1894,  1746,  2066,
    2078,  1931,  1750,  2104,  1622,  1622,  2037,  1960,   266,  2110,
    1959,  1959,  1993,  2101,  1995,    49,  1764,  1188,  1849,  2120,
     258,  1840,  1361,  2124,  1181,  1921,  1774,   930,  2071,  1959,
      78,  1999,  1573,  2134,   807,   482,   894,   595,   296,  1462,
       0,   939,  1644,  1791,   302,   303,   304,   752,   752,  1301,
     752,  1999,   310,  2034,  2097,  1543,   104,  1462,  2101,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,  1999,
      -1,    -1,   330,   331,   332,   713,    -1,    -1,    -1,    -1,
    2123,    -1,    -1,    -1,    -1,    -1,  2067,  1904,   846,   347,
      -1,    -1,   201,   351,    -1,    -1,  1700,  1701,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,  1856,    -1,
      -1,    -1,  1860,    -1,  1366,    -1,    -1,  1865,    -1,    -1,
     752,   753,    -1,    -1,    -1,    -1,    -1,   385,    -1,   177,
     762,    -1,    -1,   765,    -1,  2116,    -1,    -1,    72,    -1,
      -1,    -1,  1890,    -1,  1963,    -1,    -1,   104,    -1,   907,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    65,
      -1,   419,    62,   807,   422,  1982,   810,    -1,    -1,    -1,
     104,   429,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,  1934,   445,    18,    -1,
    1938,   449,    -1,    -1,   826,   453,    -1,   455,   830,    99,
      -1,  2096,   834,  1951,    -1,    -1,   163,    -1,    -1,    -1,
     110,    -1,   846,    -1,    -1,    -1,  1964,  2112,   152,   153,
      -1,   330,    -1,    -1,   333,  1630,    -1,  1975,    -1,  1977,
    1978,    61,    62,    63,    64,   493,    -1,    -1,   347,  1644,
      -1,    -1,   351,    -1,   502,    -1,    -1,    -1,    -1,    -1,
    2069,    -1,    -1,   153,  2002,    -1,    -1,    -1,    -1,  1017,
      -1,    -1,    -1,    -1,   522,    -1,   524,   525,    -1,    -1,
     528,    -1,   530,   907,   104,    -1,  1034,  1035,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,    -1,   104,
    2038,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  2049,  1554,    -1,   204,    -1,    -1,  1559,    -1,  1913,
     568,    -1,   950,  1565,    -1,  1567,    -1,   955,    -1,    -1,
      -1,   579,    -1,   581,    -1,   583,   156,   585,   966,    -1,
      -1,   975,    -1,    -1,    -1,  2083,    -1,  2085,    -1,   983,
     449,    -1,    -1,   601,   602,   160,   604,    13,    14,    15,
      16,    17,    -1,    -1,   612,    -1,  2104,    -1,   616,    -1,
    1765,    -1,  2110,    -1,    -1,    -1,   266,   625,  1012,    -1,
      -1,  1015,  2120,    -1,    -1,    -1,  2124,   635,    -1,    -1,
     638,    -1,    -1,  1017,    -1,    -1,  2134,    -1,    -1,  1021,
      -1,   649,    -1,    -1,    -1,    -1,   296,    -1,    -1,    -1,
    1034,  1035,   302,     3,   662,    -1,    72,   665,   666,    -1,
     668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,
      -1,   530,   680,   681,   682,    -1,    -1,    -1,    -1,    -1,
      -1,  1075,   332,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1092,    -1,    -1,  1095,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   583,    -1,   376,    -1,   736,    -1,
    1732,    -1,    -1,  1735,    -1,    -1,   152,   153,    -1,    -1,
     156,    -1,   601,   602,   752,   753,    -1,   163,   164,    -1,
    1138,    -1,   760,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   625,    -1,   149,   777,
      -1,    -1,   780,    -1,   782,    -1,  1931,    -1,    -1,   638,
      -1,    -1,   132,   164,  1292,  1293,    -1,    -1,    -1,    -1,
    1792,   799,   800,    -1,    -1,    -1,    -1,    -1,  1306,  1307,
      -1,    -1,   152,   153,  1959,   455,    -1,   188,    -1,   159,
     818,    -1,   820,   163,   164,    -1,    -1,    -1,    -1,    -1,
     201,    -1,   472,    -1,    -1,    -1,    -1,  1215,    -1,    -1,
      -1,   839,  1340,  1341,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,  1999,    -1,   104,  1849,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,  1251,  1252,  1253,    -1,  1261,    -1,   877,
    1258,  1259,   253,    -1,    -1,  1269,   884,   736,    -1,   529,
      -1,    -1,   890,   891,  1140,     4,     5,     6,     7,     8,
       9,    10,    11,    12,   902,    72,   904,   155,    -1,   549,
      -1,    -1,   160,    -1,    -1,    -1,  1288,   915,  1292,  1293,
      -1,    -1,    56,    57,    -1,  1297,    -1,    -1,   777,   569,
      -1,    -1,  1306,  1307,    -1,    -1,    -1,   104,    -1,   310,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   954,    91,    -1,   330,
      -1,    -1,    -1,    -1,   604,   132,  1340,  1341,    -1,    -1,
    1962,    -1,    65,    66,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   628,   156,
      -1,    -1,    -1,   104,    -1,   635,   163,   164,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   141,    -1,  1393,
     144,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1021,   158,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,  2027,  2028,  1535,  1536,    -1,
    1038,    -1,    -1,    -1,    -1,   156,    -1,  1045,   419,    -1,
      -1,    -1,    -1,   902,    -1,    -1,   905,    -1,    -1,    -1,
      -1,   104,    -1,   156,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1457,    -1,    -1,    -1,  2069,   449,  1577,
      -1,   174,    -1,    -1,    72,   219,    -1,    -1,    -1,  1087,
      -1,  1475,    -1,    -1,    -1,    -1,  1480,    -1,    -1,    -1,
      -1,  1485,  1486,  1487,   104,   954,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   104,   160,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   263,
      -1,    -1,  1130,    -1,    -1,    -1,    -1,    -1,    -1,   273,
    1138,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
     284,    -1,    -1,   524,   525,    -1,    -1,  1655,    -1,   530,
     160,  1535,  1536,    -1,   152,   153,    -1,  1413,    -1,  1167,
      -1,    -1,    -1,    -1,    -1,   163,   164,   311,    -1,    -1,
      -1,    72,    -1,  1555,   318,   319,  1184,  1185,    -1,   323,
      -1,    -1,  1690,    -1,    -1,    -1,    -1,  1695,  1696,    -1,
      -1,  1199,    -1,   843,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,   583,   104,    -1,    -1,    -1,  1215,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,   361,   104,   869,
     364,   602,    -1,   109,   110,   111,   112,   113,   114,   115,
     880,   132,    -1,    -1,    -1,     1,  1244,    -1,     4,    -1,
    1248,    -1,    -1,    -1,   625,    -1,   132,  1635,    -1,    -1,
     104,   152,   153,    -1,   904,   109,   110,   111,   112,   113,
     114,   115,   163,   164,    -1,    -1,   152,   153,   649,    -1,
     156,  1655,    -1,    -1,   104,    -1,    -1,   163,   164,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,    -1,  1683,
     120,  1685,   122,    59,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,  1311,  1312,    -1,  1690,    -1,   452,    -1,
      -1,  1695,  1696,    -1,    -1,    -1,    -1,    83,    -1,    -1,
     464,   465,    -1,   153,    -1,    -1,   156,    -1,    -1,    72,
      -1,    -1,  1588,    99,    -1,  1717,  1718,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,  1355,  1356,  1357,
    1358,    -1,  1360,  1361,    -1,   736,  1215,    -1,  1366,  1367,
      -1,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   140,    -1,    -1,  1385,  1386,   760,
     146,    -1,  1638,   149,    -1,  1244,    -1,   153,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,   777,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1415,    -1,   152,
     153,    -1,    -1,   156,    -1,    -1,    -1,    -1,   799,   800,
     163,   164,   188,    -1,    -1,    -1,    -1,    -1,    -1,  1437,
      -1,    72,    -1,    -1,   200,   201,   104,   818,   204,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,   595,    -1,    -1,  1462,    -1,  1464,    -1,  1840,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   251,    -1,   253,   156,    -1,
    1746,   132,    -1,    -1,  1750,    -1,    -1,    -1,    -1,    -1,
     266,   645,    -1,   884,  1892,    -1,    -1,    -1,  1764,    -1,
     891,   152,   153,    -1,    -1,    -1,    -1,   283,  1774,    -1,
      -1,    -1,   163,   164,    -1,   291,    -1,    -1,    -1,    -1,
     296,    -1,    -1,    -1,  1184,  1917,   302,    -1,  2046,  1921,
      -1,    -1,    -1,    -1,   310,    -1,  1554,  1555,    -1,    -1,
      -1,  1559,    -1,  1561,    -1,    -1,    -1,  1565,    -1,  1567,
      -1,    -1,    -1,    -1,   330,    -1,   332,   333,    -1,  1219,
      -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,  1437,    -1,
    1962,   347,    -1,    -1,    -1,   351,    -1,  1595,  1596,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1856,    -1,    -1,    -1,  1860,    -1,    -1,   104,    -1,  1865,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   385,
      -1,   104,  1630,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1641,  1890,    -1,  1644,    13,    14,    15,
      16,    17,    -1,    -1,    -1,  2027,  2028,    -1,    -1,   132,
     794,    -1,    -1,   419,    -1,    -1,   153,   801,    -1,   156,
      -1,    -1,  2046,    -1,    -1,    -1,    -1,  1675,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1934,   445,
     163,   164,  1938,   449,    -1,    -1,    -1,  2069,    -1,   455,
      -1,    -1,  1700,  1701,    -1,  1951,    72,    -1,  1348,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1087,    -1,    -1,  1717,
    1718,    -1,    -1,    -1,    -1,    -1,    -1,  1367,    -1,  1975,
      -1,  1977,  1978,    -1,  1732,    -1,    -1,  1735,   104,    -1,
      -1,    -1,   876,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,  1752,    -1,  2002,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,  1765,   524,   525,
      -1,    -1,   104,   529,   530,  1415,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   152,   153,   120,    -1,
     122,    -1,  2038,    -1,  1792,    -1,    -1,   163,   164,    -1,
      -1,    -1,    -1,  2049,    -1,   561,    -1,    -1,    -1,    -1,
     566,    -1,    -1,   569,   570,    -1,   572,    -1,    -1,    -1,
      -1,   153,    -1,    -1,   156,    -1,    -1,   583,    -1,   585,
      -1,    -1,    -1,    -1,  1205,    -1,    -1,  2083,    -1,  2085,
      -1,    -1,  1840,   599,  1215,   601,   602,    -1,   604,    -1,
      -1,  1849,    -1,    -1,    -1,    -1,    -1,    -1,  2104,    -1,
      -1,    -1,    -1,    -1,  2110,    -1,    -1,    -1,    -1,   625,
      -1,    -1,   628,  1244,  2120,    -1,   632,    -1,    -1,   635,
      -1,    -1,   638,    -1,   640,    -1,    -1,  1885,    -1,    -1,
      -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,   662,    -1,    -1,   665,
     666,    -1,   668,    -1,    -1,  1913,    -1,    -1,    -1,    -1,
      -1,   677,    -1,  1921,   680,   681,   682,    -1,    -1,    -1,
      -1,    -1,    -1,  1931,    -1,    -1,    -1,    -1,    -1,    -1,
    1311,  1312,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1595,    -1,    72,    -1,    -1,
      -1,  1959,  1960,    -1,  1962,  1963,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     736,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1999,    59,    -1,   760,    -1,    -1,    -1,    -1,  1143,
    1144,  1145,    -1,    -1,    -1,    -1,   152,   132,    -1,   155,
     156,   777,    -1,    -1,    -1,    -1,    83,    -1,    -1,  2027,
    2028,    -1,    -1,    -1,    -1,    -1,  1885,   152,   153,  1173,
      -1,    -1,    -1,   799,   800,    -1,   103,    -1,   163,   164,
      -1,    -1,    -1,    -1,  1188,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   818,    -1,    -1,    -1,  1437,    -1,    -1,    -1,
     104,  2069,    -1,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   140,    -1,    -1,   120,   843,   122,   146,
     846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1232,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,
      -1,  1960,  1752,   869,    13,    14,    15,    16,    17,   153,
      -1,   877,   156,    -1,   880,    -1,    -1,    -1,   884,    -1,
      -1,    -1,   189,    -1,   890,   891,    -1,    -1,    -1,    72,
      -1,    -1,    -1,   200,   201,    -1,   902,    -1,   904,   905,
     104,   907,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   229,    72,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,   245,    -1,
    1561,    -1,    -1,   250,   251,     1,   253,    -1,   954,   132,
      -1,   155,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   275,   152,
     153,   278,    -1,   280,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,    -1,   132,   291,    -1,  1370,    -1,    -1,  1373,
      -1,    -1,    -1,    49,    -1,    -1,    52,   304,    54,    -1,
      56,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,  1017,    -1,    -1,   163,   164,    -1,    73,    -1,    -1,
    1641,    -1,    -1,   330,    -1,    -1,   333,   104,  1034,  1035,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     347,    -1,    -1,    -1,   351,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,  1963,    -1,    -1,   132,   104,   155,  1700,
    1701,  1087,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,    -1,    -1,   120,    -1,   122,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1130,    -1,   153,    -1,    -1,    -1,
      -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,   445,    -1,
      -1,    -1,   449,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   104,
      -1,  1167,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   132,    -1,    -1,    -1,    -1,    -1,    -1,  1184,  1185,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
    1574,   152,   153,    -1,    -1,   156,    -1,    -1,    -1,  1205,
      -1,    -1,   163,   164,    -1,    -1,    -1,   152,   153,  1215,
      -1,   156,    -1,  1219,     1,   176,    -1,    -1,   163,   164,
      -1,    -1,    -1,   530,   104,  1231,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1243,  1244,    -1,
     104,    -1,  1248,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,   561,    -1,    -1,    -1,    -1,   566,
      -1,    -1,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,   583,    -1,   585,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1292,  1293,   152,    -1,
      -1,    -1,  1913,    -1,   601,   602,    -1,    -1,    -1,    -1,
    1306,  1307,    -1,    -1,    -1,  1311,  1312,    -1,    -1,   616,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,   625,    -1,
      -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   638,    -1,    -1,  1340,  1341,    -1,    -1,    -1,    -1,
      -1,    -1,  1348,    -1,    -1,    -1,    -1,    -1,    -1,  1355,
    1356,  1357,  1358,  1359,  1360,  1361,    -1,    -1,    -1,   146,
      -1,  1367,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,   164,   104,  1385,
    1386,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     132,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,   132,  1791,    -1,  1415,
     152,   153,    -1,    -1,   201,    -1,    -1,   159,    -1,   132,
      -1,   163,   164,    -1,    -1,    -1,   152,   153,    -1,   736,
      -1,  1437,    -1,    -1,    -1,    -1,    -1,   163,   164,   152,
     153,    -1,    -1,    -1,    -1,    -1,   753,    -1,    -1,    -1,
     163,   164,    -1,    -1,   104,    -1,  1462,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
     777,    -1,    -1,    -1,    -1,   782,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,    -1,   275,    -1,
     149,    -1,   799,   800,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,   291,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1535,
    1536,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,
      -1,    -1,    -1,   330,    -1,    -1,   333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,    -1,    -1,
     347,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,
     877,  1577,    -1,    -1,    -1,    -1,     1,   884,    -1,     4,
      -1,    -1,    -1,   890,    -1,    -1,    -1,    -1,    -1,  1595,
    1596,    -1,    -1,    -1,    -1,   902,    -1,    -1,   905,    -1,
     907,    -1,    -1,    -1,    -1,   912,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    59,  1641,    -1,    -1,  1644,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   954,    -1,  1655,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,   449,    -1,    -1,    -1,    -1,    -1,    -1,  1675,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,    -1,  1690,    -1,    -1,    -1,    -1,  1695,
    1696,    -1,    -1,    -1,  1700,  1701,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1017,    -1,    -1,    -1,    -1,   140,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,   149,    -1,    -1,  1034,  1035,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   530,    -1,    -1,  1752,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1765,
      -1,    -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   561,   200,    -1,    -1,    -1,    -1,
    1087,    -1,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   583,    -1,   585,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,   602,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1130,    -1,    -1,   251,    -1,   253,    -1,
      -1,  1138,    -1,   258,    -1,    -1,    -1,    -1,   625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1167,    59,    -1,    -1,    -1,    -1,   291,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1185,  1885,
      -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,  1913,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1931,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1243,  1244,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   736,
      -1,    -1,    -1,  1959,  1960,    -1,    -1,  1963,    -1,    -1,
     385,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1292,  1293,    -1,    -1,    -1,
     777,    -1,    -1,  1999,   419,    -1,    -1,    -1,    -1,  1306,
    1307,    -1,    -1,   201,  1311,  1312,    -1,    -1,    -1,    -1,
      -1,    -1,   799,   800,    -1,    -1,    -1,    -1,    -1,    -1,
     445,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,  1340,  1341,    -1,    -1,    -1,    -1,    -1,
    2046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1355,  1356,
    1357,  1358,  1359,    -1,    -1,    -1,    -1,    -1,    -1,   846,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1385,  1386,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     877,    -1,    -1,   291,    -1,    -1,    -1,   884,    -1,   524,
     525,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,   902,    -1,    -1,   905,    -1,
     907,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1437,    -1,   330,    -1,    -1,   333,    -1,    -1,    -1,    -1,
      -1,   566,    -1,    -1,    -1,   570,    -1,   572,    -1,   347,
      -1,    -1,    -1,   351,    -1,  1462,    -1,    -1,    -1,    -1,
     585,    -1,    -1,    59,    -1,    -1,    -1,   954,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   649,    -1,    -1,    -1,  1535,  1536,
    1017,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,
     665,   666,    -1,   668,   140,    -1,    -1,  1034,  1035,    -1,
     146,   449,   677,   149,    -1,   680,   681,   682,    -1,    -1,
    1567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1596,
      -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1087,    -1,    -1,    -1,   200,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   530,    -1,  1641,   760,    -1,  1644,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,
      -1,  1138,    -1,    -1,    -1,   251,    -1,   253,    -1,    -1,
      -1,    -1,   258,   561,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,    -1,
    1167,    -1,    -1,  1690,    -1,   583,    -1,   585,  1695,  1696,
      -1,    -1,    -1,  1700,  1701,   291,    -1,    -1,  1185,    -1,
      -1,    -1,    -1,   601,   602,    -1,    -1,    -1,    -1,    -1,
      -1,  1718,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   877,    -1,    -1,    -1,  1243,  1244,  1765,    -1,
      -1,    -1,    -1,    -1,    -1,   890,   891,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1292,  1293,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1306,
    1307,    -1,    -1,   419,  1311,  1312,    -1,    -1,    -1,    -1,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,   736,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   445,
      -1,    -1,    -1,  1340,  1341,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,  1885,   777,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1385,  1386,
      -1,   799,   800,    -1,    -1,    -1,  1913,    -1,    -1,    -1,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,
     818,   149,    -1,    -1,  1931,    -1,    -1,    -1,   524,   525,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,    -1,
    1437,    -1,  1959,  1960,    -1,    -1,    -1,    -1,    -1,    -1,
     188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     566,    -1,   200,    -1,   570,    -1,   572,    -1,    -1,   877,
      -1,    -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,   585,
      -1,    -1,  1999,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   902,  1130,    -1,   905,    -1,   907,
      -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2028,    -1,   251,    -1,   253,    -1,    -1,    -1,    -1,
     258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2046,
      -1,    -1,  1167,    -1,    -1,    -1,    -1,    -1,  1535,  1536,
      -1,    -1,    -1,   649,    -1,    -1,   954,    -1,    -1,    -1,
    1185,    -1,    -1,   291,    -1,    -1,   662,    -1,    -1,   665,
     666,    -1,   668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1205,   677,   310,    -1,   680,   681,   682,    -1,    -1,    -1,
    1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1596,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,
      87,    -1,    -1,  1248,    -1,    -1,    93,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1034,  1035,    -1,    -1,
      -1,    -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1641,    -1,    -1,   385,    -1,    -1,
     127,    -1,    -1,    -1,   760,    -1,    -1,    -1,  1655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1087,
      -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1690,    -1,    -1,    -1,    -1,  1695,  1696,
      -1,    -1,    -1,  1700,  1701,    -1,    -1,   445,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1355,  1356,  1357,  1358,  1359,  1360,  1361,    -1,    -1,    -1,
    1138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1385,  1386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1765,    -1,
      -1,   877,    -1,    -1,    -1,    -1,    -1,  1185,    -1,    -1,
      -1,    -1,    -1,    -1,   890,   891,   524,   525,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   300,    -1,    -1,    -1,  1462,   566,    -1,
      -1,    -1,   570,    -1,   572,  1243,  1244,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,   585,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,  1292,  1293,    -1,    -1,  1885,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1306,  1307,
      72,    -1,    -1,  1311,  1312,    -1,    -1,    -1,    -1,    -1,
      -1,   649,    -1,    -1,    -1,    -1,  1913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   662,    -1,  1561,   665,   666,    -1,
     668,    -1,  1340,  1341,  1931,    -1,    -1,    -1,    -1,   677,
      -1,    -1,   680,   681,   682,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1596,  1959,  1960,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   448,    -1,   450,    -1,    -1,    -1,  1385,  1386,    -1,
      -1,    -1,   459,   460,    -1,    -1,    -1,    -1,    -1,   369,
      -1,    -1,    -1,   373,   374,  1630,    -1,    -1,    -1,    -1,
      -1,    -1,  1999,   383,   384,    -1,    -1,    -1,    -1,  1644,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,   399,
      -1,    -1,   760,    -1,  1130,    -1,    -1,    -1,    -1,  1437,
      -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,    -1,   419,
    1675,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,  2046,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1167,    -1,    -1,    -1,    -1,    -1,    73,    -1,   449,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1185,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     567,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,  1205,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,  1535,  1536,    -1,
    1765,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,
      -1,    -1,  1248,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,   890,   891,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,  1577,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,  1596,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,  1355,
    1356,  1357,  1358,  1359,  1360,  1361,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1385,
    1386,    -1,  1690,    -1,    -1,    -1,    -1,  1695,  1696,    -1,
      -1,    -1,  1700,  1701,    -1,    -1,  1931,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,
      -1,    -1,   136,    -1,  1959,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   713,   714,   715,   716,   717,   718,   719,
     720,   721,   722,   723,   724,   725,   726,   727,   728,   729,
     730,   731,    -1,    -1,    -1,    -1,  1462,  1765,    13,    14,
      15,    16,    17,    -1,  1999,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,  1130,   870,   871,    -1,    51,    -1,    53,    -1,
    1138,    -1,    -1,    -1,   881,   882,   883,    -1,    -1,   886,
      -1,    -1,    -1,   793,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,   238,    -1,    -1,    -1,    -1,  1167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1185,   262,    -1,
      -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,   272,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,   286,   287,    -1,    -1,    -1,  1885,    -1,   293,
     294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1596,   968,    -1,    -1,    -1,   309,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1913,    -1,    -1,    -1,    -1,
    1248,    -1,    -1,    -1,    -1,   329,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1931,  1630,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1014,  1644,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1959,  1960,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1675,
     950,    -1,   386,    -1,    -1,   955,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1061,    -1,   966,    -1,    -1,    -1,
      -1,  1999,    -1,  1070,  1071,  1072,  1073,    -1,    -1,    -1,
      -1,  1078,  1079,   417,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1088,    -1,    -1,    -1,    -1,    -1,  1355,  1356,  1357,
    1358,  1359,  1360,  1361,    -1,    -1,    -1,    -1,  1008,   443,
      -1,    -1,  1109,   447,    -1,  1112,    -1,  1114,  2046,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1385,  1386,    -1,
      -1,    -1,   466,    -1,    -1,    -1,   470,   471,    -1,  1765,
     474,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   489,   490,   491,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1167,    -1,    -1,    -1,   508,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1193,    -1,    -1,    -1,
      -1,    -1,    -1,  1200,  1462,  1202,  1203,    -1,    -1,    -1,
     544,    -1,    -1,    -1,    -1,    -1,    -1,  1214,    -1,  1216,
      -1,  1218,    -1,  1220,    -1,    -1,    -1,    -1,  1225,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1138,    -1,
      -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,   582,    -1,
      -1,    -1,    -1,    -1,   588,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   612,   613,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1287,    -1,    -1,    -1,    -1,    -1,    -1,  1294,  1295,    -1,
      -1,    -1,    -1,  1561,    -1,  1931,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,    -1,    -1,    -1,    -1,
      -1,  1318,    48,    -1,    -1,    -1,    -1,    -1,  1325,    -1,
      -1,    -1,  1329,  1959,  1234,    -1,    -1,    -1,  1596,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,   683,
      -1,  1251,  1252,  1253,    -1,    -1,    -1,    -1,  1258,  1259,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1365,    -1,
      -1,    -1,  1630,  1999,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1282,    -1,    -1,    -1,  1644,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,  1405,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1675,   752,  1319,
    1320,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   767,    -1,    -1,    -1,   771,    -1,    -1,
     166,    -1,    -1,    -1,  1441,    -1,   780,    -1,    -1,    -1,
      -1,    -1,  1449,    -1,  1451,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,   802,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   811,    -1,    -1,
      -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   228,  1500,  1501,    -1,   232,  1765,    -1,   235,
     236,    -1,    -1,   239,    -1,    -1,   242,   243,  1515,  1516,
     854,  1518,    -1,    -1,     5,    -1,    -1,   861,    -1,    -1,
    1527,    -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,
    1537,  1538,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   888,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,   303,    -1,    -1,
     306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   327,   328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     944,    -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,
    1657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,  1669,  1931,   155,   156,    -1,    -1,    -1,    -1,
     164,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,  1027,    -1,    -1,    -1,  1031,    -1,    -1,
      -1,  1959,    -1,    -1,  1038,    -1,    -1,    -1,    -1,  1706,
    1707,    -1,   438,    -1,  1048,    -1,   200,   201,    -1,    -1,
      -1,  1055,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1064,    -1,  1066,    -1,    -1,  1635,    -1,    -1,    -1,    -1,
      -1,  1999,    -1,    -1,    -1,    -1,    -1,    -1,   232,    -1,
      -1,    -1,    -1,    -1,    -1,   239,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1098,    -1,    -1,   493,  1102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,
      -1,    -1,    -1,    -1,    -1,  1119,    -1,    -1,  1122,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1796,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1817,    -1,   306,  1820,  1821,    -1,    -1,    -1,    -1,    -1,
    1827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   330,   331,    -1,    -1,
      -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   351,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,  1213,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,   620,   621,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1238,    -1,    -1,   633,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   422,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,  1852,    -1,   438,   439,    -1,   441,   442,    -1,
      -1,    -1,    -1,    -1,  1961,   449,    -1,    -1,    -1,   453,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,  1892,    -1,    -1,    -1,    -1,    -1,  1332,    -1,
      -1,    -1,  1336,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     494,    -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2025,    -1,
     756,   757,    -1,    -1,    -1,    -1,   762,    -1,  1372,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,    -1,
      -1,    -1,    -1,  2050,    -1,    -1,    -1,   783,    -1,    -1,
     786,   787,    -1,   789,    -1,   791,   792,    -1,  2065,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2081,    -1,    -1,    -1,  1422,    -1,
      -1,  1425,    -1,  1993,    -1,  1995,   580,    -1,    -1,   583,
      -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,   834,  1443,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   601,   602,    -1,
      -1,    -1,   385,    -1,    -1,    -1,    -1,    -1,   612,    -1,
      -1,    -1,   616,    -1,  2034,    -1,    -1,    -1,    -1,   623,
      -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1496,    -1,    -1,    -1,    -1,  2067,    -1,    -1,
      -1,  1505,    -1,   899,   900,  1509,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   913,    -1,  1523,
    1524,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,  2116,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,   736,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,   524,   525,    -1,    -1,    -1,    -1,    72,   752,   753,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   762,   763,
      -1,   765,   766,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   777,    -1,    -1,   780,    -1,   782,   783,
      -1,    -1,    -1,    -1,    -1,   789,    -1,    -1,    -1,    -1,
      -1,  1645,  1646,    -1,    -1,   799,   800,    -1,  1044,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,    -1,
      -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,   822,   164,
      -1,    -1,   826,    -1,    -1,    -1,   830,   831,    -1,    -1,
     834,   835,    -1,    -1,    -1,    -1,    -1,    -1,   842,  1085,
      -1,    -1,    -1,   188,    -1,    -1,  1092,    -1,    -1,  1095,
      -1,    -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,   204,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,
     884,   885,    -1,    -1,    -1,   668,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,
      -1,   915,    -1,   696,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   266,    -1,    -1,    48,  1779,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   732,
     954,    -1,  1806,    -1,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,
      -1,  1825,    -1,    -1,    -1,  1221,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1229,  1230,   330,    -1,   332,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,
    1854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,
      -1,   135,    -1,    -1,    -1,    -1,    -1,  1021,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1881,    -1,    -1,
    1884,    -1,    -1,    -1,  1038,  1039,    -1,    -1,    -1,    -1,
     385,  1045,  1288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1297,    -1,    -1,  1300,    -1,  1302,  1303,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1087,    -1,    -1,    -1,    -1,  1092,  1093,
      -1,  1095,  1096,    -1,    -1,    -1,    -1,  1343,    -1,    -1,
      -1,    -1,    -1,    -1,   449,    -1,    -1,    -1,    -1,    -1,
     455,   235,   236,    -1,    -1,   239,    -1,    -1,   242,   243,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1988,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
    1416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   524,
     525,    -1,    -1,    -1,    -1,   530,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   327,   328,    -1,    -1,    -1,    -1,    -1,
      -1,  1215,    -1,    -1,    -1,    -1,    -1,  1221,  1222,   343,
     104,    -1,    -1,   107,   108,    -1,   188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,   201,
    1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   214,    -1,   216,    -1,    -1,   602,  1504,   604,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,    -1,
     625,    -1,    -1,    -1,  1288,  1289,    -1,  1533,    -1,    -1,
      -1,    -1,    -1,  1297,  1298,    -1,  1300,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   649,    -1,    -1,  1311,  1312,    -1,
      -1,    -1,    -1,    -1,   438,    -1,    -1,   662,    -1,  1565,
     665,   666,    -1,   668,    -1,  1571,    -1,    -1,    -1,    -1,
      -1,    -1,   677,    -1,    -1,   680,   681,   682,    -1,   301,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,   505,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,   736,    -1,    -1,  1640,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   777,  1437,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   799,   800,    -1,    -1,    -1,    -1,
    1464,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   818,    -1,    -1,    -1,    -1,  1724,  1725,
      -1,    -1,    -1,    -1,   155,   156,  1732,    -1,    -1,    -1,
    1736,    -1,   163,   164,    -1,    -1,   620,   621,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,   481,
      -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   884,
      -1,    -1,    -1,    -1,    -1,    -1,   891,    -1,    -1,    -1,
      -1,  1555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   904,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1571,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1842,    -1,    -1,  1382,
      -1,    -1,  1385,  1386,    -1,    -1,    -1,    -1,  1391,   954,
      -1,   573,  1395,    -1,  1397,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   756,   757,    -1,    -1,    -1,  1641,   762,    -1,
     602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   615,    -1,    -1,    -1,    -1,    -1,   783,
      -1,    -1,   786,   787,    -1,   789,    -1,   791,   792,    -1,
      -1,  1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1700,  1701,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,   670,    -1,
     834,    -1,    -1,  1717,  1718,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1733,
      -1,    -1,   694,   695,    -1,    -1,   698,    -1,   700,    -1,
      -1,    -1,  1087,    -1,   706,    -1,   708,   709,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1541,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   736,   899,   900,    -1,    -1,    -1,
      -1,  2027,    -1,    -1,    -1,  1130,    -1,   749,    -1,   913,
      -1,    -1,    -1,    -1,    -1,    -1,  1579,    -1,   760,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1591,    -1,
      -1,    -1,   774,    -1,    -1,   777,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1606,  1607,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1840,    -1,    -1,  1184,
      -1,    -1,   804,    -1,  1848,   807,    -1,  1630,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1244,
      -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,  1913,
      -1,    -1,    -1,  1917,  1918,    -1,    -1,  1921,    -1,    -1,
    1044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   891,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   904,   905,    -1,    -1,    -1,    -1,    -1,    -1,
     912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1962,    -1,
      -1,  1085,    -1,    -1,    -1,  1748,  1311,  1312,  1092,    -1,
      -1,  1095,    -1,    -1,  1757,    -1,  1759,    -1,   940,  1762,
    1763,    -1,  1765,    -1,    -1,    -1,    -1,  1770,    -1,    -1,
      -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     962,    -1,    -1,    -1,    -1,    -1,    -1,   969,    -1,    -1,
    1355,  1356,  1357,    -1,    -1,  1360,  1361,    -1,    -1,    -1,
      -1,    -1,  1367,  2027,  2028,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1015,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2069,    -1,    -1,    -1,    -1,
    1415,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1871,    -1,
      -1,    -1,  1437,  1876,  1877,    -1,    -1,  1221,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1229,  1230,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1082,    -1,  1084,    -1,  1086,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1939,    -1,  1941,    -1,
      -1,  1944,  1945,    -1,  1288,    -1,  1949,  1950,    -1,    -1,
      -1,    -1,    -1,  1297,    -1,    -1,  1300,    -1,  1302,  1303,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1155,  1156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,  1343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2020,  2021,  2022,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1595,    -1,    -1,    -1,    -1,    -1,    -1,  1219,  2041,    -1,
      -1,    -1,    -1,  1225,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2055,  2056,  2057,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1416,    -1,    -1,    -1,  1641,    -1,    -1,  1261,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1276,    -1,    -1,  1279,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1675,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1700,  1701,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1331,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1504,    -1,    -1,    -1,    49,  1347,  1348,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1752,    73,  1533,
      -1,    -1,    -1,  1375,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1393,    -1,    -1,  1396,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,     1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1437,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,  1447,  1448,   152,   153,   154,
     155,   156,    -1,    -1,    -1,  1457,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1475,    49,  1477,  1640,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,
    1885,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    -1,    93,    94,
      95,    96,    97,    -1,    99,    -1,   101,   102,  1913,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,  1561,
    1724,  1725,    -1,    -1,  1566,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1736,    -1,    -1,  1960,   151,   152,  1963,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1626,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,  1673,    -1,    -1,  1676,    -1,    -1,    -1,  1842,    -1,
      71,    -1,    73,    74,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    -1,    93,    94,    95,    96,    97,    -1,    99,  1711,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,     1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1917,    -1,    -1,    18,    -1,    -1,    -1,
     151,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,  1893,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,
      72,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    -1,    -1,   152,   153,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,   148,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,   106,
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
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
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
     163,   164,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,     3,
      -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,   163,
     164,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,     3,
      -1,     5,    -1,    -1,   163,   164,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,   163,
     164,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,     3,    55,    -1,
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
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,
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
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   155,    -1,    -1,    -1,    51,
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
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,   155,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
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
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,   156,
     104,    -1,    -1,   107,   108,    -1,   163,   164,    71,    -1,
      73,    -1,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    -1,
      93,    94,    95,    96,    97,    -1,    99,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
      18,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    49,   155,   156,    52,    -1,    54,   160,    56,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,   177,    73,    -1,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    93,    94,    95,    96,    97,
      -1,    99,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     107,   108,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    -1,    -1,    -1,    72,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,   107,
     108,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,    -1,   163,   164,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
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
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,   155,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
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
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,   107,   108,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,   155,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,    -1,
     163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    13,    14,
      15,    16,    17,    18,    72,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,   107,
     108,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,    -1,
     163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    13,    14,
      15,    16,    17,    18,    72,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,   107,
     108,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,    -1,
     163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    72,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,   107,   108,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      13,    14,    15,    16,    17,    18,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,   155,    -1,    51,    -1,
      53,   107,   108,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    13,    14,    15,    16,    17,
     163,   164,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,
     107,   108,    -1,    -1,    -1,   163,   164,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
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
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    49,    -1,
      -1,    52,    -1,    54,    72,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,
      54,   132,    56,    -1,    -1,    -1,    -1,   155,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,    73,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    49,    -1,    -1,    52,    -1,    54,   132,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,   148,    -1,    73,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   104,    -1,    -1,   107,   108
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   179,   392,   393,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      99,   103,   104,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   119,   132,   152,   153,   155,   156,   163,   164,
     182,   183,   184,   199,   282,   283,   284,   285,   286,   287,
     288,   289,   290,   291,   292,   293,   295,   297,   299,   300,
     301,   302,   303,   304,   305,   306,   307,   309,   311,   312,
     313,   315,   316,   320,   321,   322,   323,   324,   326,   332,
     333,   334,   335,   346,   350,   384,   387,   397,   403,   405,
     411,   415,   420,   421,   422,   423,   424,   425,   426,   427,
     449,   467,   468,   469,   470,     0,   179,   104,   183,   199,
     286,   288,   297,   300,   312,   316,   321,   118,   152,    58,
      61,    62,    64,   152,   152,   409,   410,   411,   308,   309,
     107,   108,   183,   364,   385,   386,   364,   152,   397,   152,
     152,     4,   104,   107,   108,   301,   306,   307,   152,   199,
     410,   415,   421,   422,   423,   425,   426,   427,   107,   323,
     157,   179,   289,   297,   300,   420,   424,   466,   467,   470,
     471,   177,   180,   149,   160,   176,   220,   367,    89,   158,
     404,   364,   180,   180,   180,   177,   107,   108,   152,   199,
     294,   406,   415,   416,   417,   418,   419,   420,   424,   428,
     429,   430,   431,   432,   438,     3,    47,    48,    50,    55,
     314,     3,   156,   199,   288,   301,   305,   307,   317,   322,
     400,   420,   424,   470,   286,   288,   300,   312,   316,   321,
     401,   420,   424,    65,   306,   306,   301,   307,   306,   301,
     306,   301,   155,   409,   158,   180,   152,   160,   228,   409,
     409,   179,   277,   278,   156,   297,   300,   468,   364,   364,
     397,   176,   300,   152,   199,   406,   415,   420,   429,   156,
     199,   470,   398,   399,    65,    66,    67,    68,   156,   174,
     364,   373,   375,   379,   381,   382,   322,    57,   154,   156,
     199,   296,   300,   304,   305,   311,   312,   318,   319,   320,
     321,   325,   332,   333,   350,   360,   362,   449,   462,   463,
     464,   465,   470,   471,   107,   108,   160,   183,   322,   438,
     411,   152,   380,   381,   152,   152,   118,   185,   186,    49,
      52,    54,    56,    73,   101,   102,   104,   106,   116,   117,
     120,   121,   122,   124,   125,   152,   156,   162,   165,   166,
     167,   168,   181,   182,   185,   187,   190,   198,   199,   200,
     201,   204,   205,   206,   207,   208,   209,   210,   211,   212,
     213,   214,   215,   216,   222,   322,   154,   156,   198,   199,
     215,   217,   297,   322,   365,   366,   383,   466,   471,   300,
     421,   422,   423,   425,   426,   427,   154,   154,   154,   154,
     154,   154,   154,   156,   297,   449,   468,   156,   163,   199,
     217,   288,   289,   296,   298,   300,   312,   319,   321,   355,
     356,   359,   360,   361,   462,   470,   152,   420,   424,   470,
     152,   158,   104,   155,   156,   160,   182,   184,   217,   368,
     369,   370,   371,   372,    22,   368,   152,   364,   228,   152,
     158,   158,   158,   410,   415,   417,   418,   419,   428,   430,
     431,   432,   300,   416,   429,   158,    99,   408,   156,   409,
     446,   449,   409,   409,   404,   277,   152,   409,   446,   404,
     409,   409,   300,   406,   152,   152,   299,   300,   297,   300,
     179,   297,   466,   471,   324,   160,   404,   277,   364,   367,
     288,   305,   402,   420,   424,   160,   404,   277,   385,   300,
     312,   300,   300,   107,   323,   107,   108,   183,   322,   327,
     385,   179,   183,   363,   151,   179,     3,   293,   295,   300,
     304,   228,   179,   179,   408,   152,   408,   180,   217,   410,
     415,   300,   152,   179,   364,   395,   160,   364,   160,   364,
     134,   163,   164,   378,   154,   158,   364,   382,   154,   409,
     409,   157,   179,   298,   300,   312,   319,   321,   461,   462,
     470,   471,   152,   156,   164,   176,   199,   449,   451,   452,
     453,   454,   455,   456,   473,   199,   325,   470,   300,   319,
     306,   301,   409,   154,   298,   300,   463,   298,   449,   463,
      10,   162,   167,   349,   351,   352,   347,   349,   373,   176,
     373,    13,    88,   104,   107,   108,   182,   412,   413,   414,
     154,   118,   152,   198,   152,   152,   152,   201,   152,   198,
     152,   104,   107,   108,   301,   306,   307,   152,   198,   198,
      19,    21,    85,   156,   165,   166,   202,   203,   217,   224,
     228,   335,   365,   470,   158,   179,   152,   187,   156,   161,
     156,   161,   121,   123,   124,   125,   152,   155,   156,   160,
     161,   201,   201,   169,   163,   170,   171,   165,   166,   126,
     127,   128,   129,   172,   173,   130,   131,   164,   162,   174,
     132,   133,   175,   154,   158,   155,   179,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   176,   219,
     220,   221,   152,   199,   442,   443,   444,   445,   446,   154,
     158,   154,   154,   154,   154,   154,   154,   152,   409,   446,
     449,   152,   446,   449,   179,   297,   468,   179,   180,   180,
     152,   164,   199,   415,   433,   434,   435,   436,   437,   438,
     439,   440,   441,   134,   470,   180,   180,   364,   364,   179,
     179,   179,   156,   184,   179,   369,   159,   158,   472,   368,
     155,   156,   159,   372,   153,   217,   223,   152,   179,   179,
     179,   179,   415,   417,   418,   419,   428,   430,   431,   432,
     154,   154,   154,   154,   154,   154,   154,   416,   429,   409,
     152,   367,   157,   179,   228,   404,   179,   228,   406,   224,
     366,   224,   366,   406,   395,   228,   404,   408,   160,   404,
     277,   395,   228,   404,   329,   330,   328,   160,   134,   300,
     357,   358,   361,   362,   154,   158,    70,   279,   280,   180,
     300,   293,   163,   217,   179,   415,   356,   397,   395,   157,
     179,   152,   377,   375,   376,    78,   310,   183,   160,   183,
     438,   298,   449,   463,   300,   304,   470,   179,   452,   453,
     454,   157,   179,    18,   217,   300,   451,   473,   409,   409,
     449,   298,   461,   471,   300,   183,   409,   298,   463,   322,
     158,   472,   364,   349,   160,   154,   366,   154,   154,   158,
     152,   177,   365,   187,   156,   365,   365,   365,   217,   365,
     154,   365,   365,   365,   179,   154,   165,   166,   203,    18,
     302,   154,   158,   154,   163,   164,   154,   223,   217,   160,
     217,   183,   217,   183,   116,   156,   183,   153,   191,   192,
     193,   217,   116,   156,   183,   335,   217,   191,   183,   201,
     204,   204,   204,   205,   205,   206,   206,   207,   207,   207,
     207,   208,   208,   209,   210,   211,   212,   213,   159,   224,
     177,   185,   156,   183,   217,   160,   217,   179,   443,   444,
     445,   300,   442,   409,   409,   217,   366,   152,   409,   446,
     449,   152,   446,   449,   179,   179,   157,   157,   152,   415,
     434,   435,   436,   439,    18,   300,   433,   437,   152,   409,
     455,   473,   409,   409,   473,   152,   409,   455,   409,   409,
     180,   216,   364,   157,   158,   157,   158,   473,   473,   134,
     354,   355,   356,   354,   364,   179,   215,   216,   217,   407,
     472,   368,   370,   151,   179,   154,   158,   179,   354,   183,
     406,   183,   154,   154,   154,   154,   154,   154,   152,   409,
     446,   449,   152,   409,   446,   449,   406,   185,   449,   217,
     307,   322,   447,   228,   357,   154,   154,   154,   154,   393,
     394,   228,   395,   228,   404,   394,   228,   160,   160,   160,
     336,   180,   180,   183,   281,   364,    18,    71,    73,    76,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    93,    94,    95,    96,    97,    99,   107,   108,
     119,   179,   224,   225,   226,   227,   228,   229,   230,   232,
     233,   243,   249,   250,   251,   252,   253,   254,   259,   260,
     266,   267,   268,   282,   300,   304,   364,   405,    70,   177,
     180,   180,   180,   354,   180,   396,   394,   286,   288,   297,
     388,   389,   390,   391,   383,   176,   374,   374,   351,   409,
     409,   298,   463,   156,   163,   199,   217,   322,   217,   300,
     357,   154,   154,   154,     5,   300,   409,   451,   160,   183,
     438,    10,   352,   151,   176,   353,   160,   351,   160,   154,
     413,   191,   154,   158,   179,   158,   154,   154,   158,   154,
     201,   154,   154,   154,   201,    18,   302,   217,   154,   154,
     153,   160,   201,   157,   180,   191,   157,   157,   116,   120,
     122,   184,   194,   195,   196,   154,   158,   194,   157,   158,
     151,   215,   159,   154,   194,   180,   369,   357,   154,   154,
     154,   442,   179,   179,   357,   357,   439,   154,   154,   154,
     154,   152,   415,   438,   433,   437,   179,   179,   157,   180,
     473,   179,   179,   180,   180,   180,   180,   367,   194,   134,
     168,   180,   180,   151,   368,   217,   409,   153,   217,   354,
     180,   176,   152,   409,   446,   449,   152,   409,   446,   449,
     179,   179,   408,   154,   146,   168,   180,   448,   158,   180,
     180,   396,   394,   228,   396,   336,   336,   336,     3,     5,
      10,    73,   151,   283,   290,   291,   297,   300,   337,   342,
     466,   154,   158,   158,   177,   152,    61,    62,   177,   228,
     282,   405,   152,    18,   226,   152,   152,   177,   364,   177,
     364,   163,   364,   160,   225,   152,   152,   152,   228,   217,
     218,   218,    14,   269,    74,   234,   177,   180,   230,    78,
     177,   364,    91,   255,   363,   300,   159,   281,   177,   157,
     157,   180,   158,   396,   406,   180,   177,   180,   177,   180,
     154,   366,   380,   380,   472,   349,   349,   179,   180,   180,
     180,   217,   180,   152,   409,   455,   449,   299,     5,   163,
     180,   217,   351,   409,   409,   322,   364,   160,   216,   351,
     472,   151,   179,   154,   296,   183,    78,   188,   189,   365,
     201,   201,   201,   201,   201,   160,   369,   158,   151,   197,
     156,   195,   197,   197,   157,   158,   123,   155,   193,   157,
     223,   215,   177,   157,   472,   180,   152,   409,   446,   449,
     357,   357,   180,   180,   154,   152,   409,   446,   449,   152,
     409,   455,   415,   409,   409,   357,   357,   157,   356,   359,
     359,   360,   154,   158,   158,   154,   180,   216,   216,   157,
     157,   180,   180,   154,   217,   179,   179,   357,   357,   367,
     409,   158,   217,   217,   307,   322,   157,   154,   151,   396,
     151,   151,   151,   151,   297,   297,   335,   343,   466,   297,
     342,   152,   331,   177,   177,   152,   159,   199,   338,   339,
     345,   415,   416,   429,   158,   177,   364,   179,   364,   154,
     191,   192,   177,   228,   177,   228,   224,    80,   154,   224,
     235,   282,   284,   287,   293,   300,   304,   146,   147,   148,
     153,   154,   177,   224,   244,   245,   246,   282,   177,   177,
     224,   177,   369,   177,   224,   223,   224,   111,   112,   113,
     114,   115,   261,   263,   264,   177,    98,   177,    84,   152,
     152,   180,   151,   177,   177,   152,   226,   228,   409,   177,
     154,   179,   151,   151,   179,   158,   158,   151,   160,   160,
     157,   157,   157,   180,   154,   179,   217,   217,   180,   157,
     180,   472,   348,   349,   353,   353,   369,   472,   151,   388,
     450,   451,   154,   159,   154,   158,   159,   369,   472,   223,
     121,   194,   195,   156,   195,   156,   195,   157,   151,   154,
     179,   180,   180,   154,   154,   179,   179,   180,   180,   180,
     179,   179,   157,   180,   154,   409,   357,   357,   180,   180,
     224,   448,   151,   331,   331,   331,   338,   152,   199,   340,
     341,   446,   457,   458,   459,   460,   177,   158,   177,   338,
     177,   383,   410,   415,   217,   300,   158,   177,   344,   345,
     344,   364,   134,   361,   362,   224,   154,   154,   152,   226,
     154,   224,   300,   146,   147,   148,   168,   177,   247,   248,
     226,   225,   177,   248,   154,   159,   224,   153,   224,   225,
     246,   177,   472,   154,   154,   154,   228,   263,   264,   152,
     217,   152,   185,   235,   201,   256,   110,     1,   226,   409,
     389,   179,   179,   351,   351,   157,   357,   180,   180,   157,
     157,   151,   349,   160,   472,   151,   180,   154,   217,   189,
     217,   472,   151,   157,   157,   194,   194,   357,   154,   154,
     357,   357,   154,   154,   157,   158,   134,   356,   134,   157,
     180,   180,   154,   154,   157,   217,   177,   458,   459,   460,
     300,   457,   158,   177,   409,   409,   177,   154,   415,   409,
     177,   226,    77,    78,   160,   238,   239,   240,   154,   224,
      75,   226,   224,   153,   224,    75,   177,   107,   153,   224,
     225,   246,   153,   224,   226,   245,   248,   248,   177,   224,
     151,   160,   240,   226,   152,   179,   177,   185,   154,   159,
     154,   154,   158,   159,   254,   258,   364,   406,   472,   472,
     180,   157,   157,   160,   351,   151,   151,   151,   157,   157,
     180,   180,   180,   179,   180,   154,   154,   154,   154,   154,
     457,   409,   339,     1,   216,   236,   237,   407,     1,   159,
       1,   179,   226,   238,    75,   177,   154,   226,    75,   177,
     168,   168,   226,   225,   248,   248,   177,   107,   224,   168,
     168,    75,   153,   224,   153,   224,   225,   177,     1,   179,
     179,   265,   298,   300,   466,   159,   177,   156,   185,   270,
     271,   272,   226,   201,   191,    75,   109,   255,   257,   151,
     151,   154,   351,   472,   154,   154,   154,   359,   152,   409,
     446,   449,   341,   134,     1,   158,   159,   151,   275,   276,
     282,   226,    75,   177,   226,   224,   153,   153,   224,   153,
     224,   153,   224,   225,   153,   224,   153,   224,   226,   168,
     168,   168,   168,   151,   275,   265,   180,   152,   199,   406,
     457,   183,   159,   104,   152,   154,   159,   158,    75,   154,
     226,   152,   226,   226,   472,   151,   179,   216,   236,   239,
     241,   242,   282,   226,   168,   168,   168,   168,   153,   153,
     224,   153,   224,   153,   224,   241,   180,   177,   262,   300,
     270,   157,   216,   177,   270,   272,   226,   224,   110,   110,
     151,   357,   226,   231,   180,   239,   153,   153,   224,   153,
     224,   153,   224,   180,   262,   215,   154,   159,   185,   154,
     154,   159,   154,   258,    75,   253,   180,     1,   226,   151,
     231,   151,   154,   228,   185,   273,   152,   177,   273,   226,
      75,   154,   228,   158,   159,   216,   154,   226,   185,   183,
     274,   154,   177,   154,   158,   177,   183
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
     231,   231,   231,   232,   232,   233,   233,   233,   233,   233,
     233,   233,   234,   234,   235,   235,   235,   235,   236,   236,
     236,   237,   237,   238,   238,   238,   238,   238,   239,   239,
     240,   241,   241,   242,   242,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   244,   244,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   246,   246,   246,   247,   247,   248,
     248,   248,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   250,   250,   251,   252,   253,   254,   254,   255,
     255,   256,   256,   257,   258,   258,   258,   258,   258,   258,
     259,   259,   260,   260,   260,   261,   261,   262,   262,   263,
     263,   263,   263,   264,   265,   265,   265,   265,   265,   266,
     267,   267,   268,   268,   268,   268,   268,   269,   269,   270,
     270,   271,   271,   272,   272,   273,   273,   273,   274,   274,
     275,   275,   276,   276,   277,   277,   278,   278,   279,   279,
     280,   280,   281,   281,   282,   282,   282,   283,   283,   284,
     284,   284,   284,   284,   285,   285,   285,   286,   286,   286,
     287,   287,   287,   287,   287,   288,   288,   289,   289,   290,
     290,   290,   291,   291,   291,   291,   291,   292,   292,   293,
     293,   293,   293,   294,   294,   295,   295,   295,   296,   296,
     296,   297,   297,   297,   298,   298,   298,   299,   299,   300,
     300,   301,   301,   302,   302,   302,   302,   302,   303,   304,
     304,   304,   305,   305,   306,   306,   306,   306,   306,   306,
     306,   306,   306,   307,   307,   307,   307,   307,   307,   307,
     307,   307,   307,   307,   307,   307,   307,   307,   307,   307,
     307,   307,   307,   307,   307,   307,   307,   307,   307,   307,
     307,   308,   308,   309,   310,   310,   311,   311,   311,   311,
     311,   312,   312,   313,   313,   313,   313,   314,   314,   314,
     314,   314,   314,   315,   315,   315,   315,   316,   317,   316,
     316,   318,   318,   318,   318,   319,   319,   319,   320,   320,
     320,   320,   321,   321,   321,   322,   322,   322,   322,   322,
     322,   323,   323,   323,   324,   324,   325,   325,   327,   326,
     328,   326,   329,   326,   330,   326,   326,   331,   331,   332,
     332,   333,   333,   334,   334,   334,   335,   335,   335,   335,
     335,   335,   335,   335,   336,   336,   337,   337,   337,   337,
     337,   337,   337,   337,   337,   337,   337,   338,   338,   338,
     339,   339,   339,   340,   340,   340,   341,   342,   342,   343,
     343,   344,   344,   345,   346,   347,   346,   346,   346,   346,
     348,   346,   346,   346,   346,   346,   349,   349,   350,   350,
     351,   351,   351,   351,   352,   352,   353,   353,   353,   354,
     354,   354,   354,   354,   354,   354,   355,   355,   355,   355,
     356,   356,   357,   357,   357,   357,   358,   358,   358,   358,
     359,   359,   359,   359,   359,   360,   360,   360,   360,   360,
     361,   361,   362,   362,   363,   363,   364,   364,   364,   365,
     365,   365,   366,   366,   367,   367,   367,   367,   368,   368,
     369,   369,   369,   369,   369,   370,   370,   371,   371,   372,
     372,   372,   372,   372,   373,   373,   374,   374,   376,   375,
     377,   375,   375,   375,   378,   378,   378,   378,   379,   379,
     379,   379,   380,   380,   381,   381,   382,   382,   383,   383,
     383,   383,   384,   384,   384,   385,   385,   386,   386,   387,
     387,   388,   388,   389,   389,   390,   390,   390,   391,   391,
     392,   392,   393,   393,   394,   394,   395,   396,   397,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   398,
     397,   399,   397,   400,   397,   401,   397,   402,   397,   403,
     403,   403,   404,   404,   405,   405,   405,   405,   405,   405,
     405,   405,   405,   405,   406,   406,   406,   407,   408,   408,
     409,   409,   410,   410,   411,   412,   412,   413,   413,   413,
     414,   414,   414,   414,   414,   414,   415,   415,   416,   416,
     416,   416,   417,   417,   417,   417,   418,   418,   418,   418,
     418,   418,   418,   419,   419,   419,   419,   420,   420,   420,
     421,   421,   421,   421,   421,   422,   422,   422,   422,   423,
     423,   423,   423,   423,   423,   424,   424,   424,   425,   425,
     425,   425,   425,   426,   426,   426,   426,   427,   427,   427,
     427,   427,   427,   428,   428,   429,   429,   429,   429,   430,
     430,   430,   430,   431,   431,   431,   431,   431,   431,   431,
     432,   432,   432,   432,   432,   433,   433,   433,   433,   433,
     434,   434,   434,   435,   435,   435,   435,   436,   436,   436,
     437,   437,   437,   437,   437,   438,   438,   439,   439,   439,
     440,   440,   441,   441,   442,   442,   442,   443,   443,   443,
     443,   443,   444,   444,   444,   444,   445,   445,   445,   446,
     446,   446,   446,   446,   447,   447,   447,   447,   447,   447,
     448,   448,   449,   449,   449,   449,   450,   450,   451,   451,
     451,   451,   452,   452,   452,   452,   452,   453,   453,   453,
     453,   454,   454,   454,   455,   455,   455,   456,   456,   456,
     456,   456,   456,   457,   457,   457,   458,   458,   458,   458,
     458,   459,   459,   459,   459,   460,   460,   461,   461,   461,
     462,   462,   463,   463,   463,   463,   463,   463,   464,   464,
     464,   464,   464,   464,   464,   464,   464,   464,   465,   465,
     465,   465,   466,   466,   466,   467,   467,   468,   468,   468,
     468,   468,   468,   469,   469,   469,   469,   469,   469,   470,
     470,   470,   471,   471,   472,   472,   473,   473
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
       1,     2,     1,     2,     0,     2,     3,     4,     4,     4,
       3,     2,     2,     3,     3,     2,     1,     0,     1,     4,
       1,     2,     2,     0,     1,     4,     1,     2,     3,     1,
       2,     0,     1,     2,     6,     0,     9,     8,     9,     8,
       0,    13,    11,    12,    11,     1,     0,     1,     3,     3,
       3,     2,     5,     5,     1,     1,     0,     2,     5,     0,
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
       2,     2,     2,     2,     2,     2,     1,     2,     5,     0,
       6,     0,     8,     0,     7,     0,     7,     0,     8,     1,
       2,     3,     0,     5,     3,     4,     4,     4,     4,     5,
       5,     5,     5,     6,     1,     1,     1,     3,     0,     5,
       0,     1,     1,     2,     6,     1,     3,     0,     1,     4,
       1,     1,     1,     1,     1,     1,     1,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     8,     9,     3,     4,     2,     1,     2,
       6,     8,     9,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     1,     1,     1,     4,     8,
       9,     3,     4,     2,     3,     3,     4,     4,     5,     4,
       5,     3,     4,     1,     3,     2,     1,     2,     2,     2,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     8,     9,     3,     4,     2,     4,     1,     2,     2,
       2,     3,     4,     2,     4,     4,     3,     6,     8,     3,
       2,     4,     1,     2,     2,     1,     1,     2,     3,     4,
       2,     4,     6,     8,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     5,     8,     3,     2,
       3,     7,     5,     1,     1,     1,     3,     3,     3,     5,
       1,     1,     5,     5,     6,     6,     0,     1,     1,     3,
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
#line 7554 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7560 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7566 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7578 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7584 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7590 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7596 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7602 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7608 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7618 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7642 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7648 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7654 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7660 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7670 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7676 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7682 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7688 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7694 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7700 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7706 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7717 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7726 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7732 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7738 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7744 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7750 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7756 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7766 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7772 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7779 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7785 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7791 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7803 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7809 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7815 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7821 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7827 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7833 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7839 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7845 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7851 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7857 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7863 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7869 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7879 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7885 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7891 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7897 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7903 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7909 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7915 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7959 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7965 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7974 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7986 "Parser/parser.cc"
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
#line 8006 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8012 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8018 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8030 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8036 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8042 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8048 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8054 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8063 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8069 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8075 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8081 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8087 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8093 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8099 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8105 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8111 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8117 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8123 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8129 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8135 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8141 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8147 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8153 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8159 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8165 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8171 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8177 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8183 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8189 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8195 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8201 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8213 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8219 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8237 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8279 "Parser/parser.cc"
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
#line 8291 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8297 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8303 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8309 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8315 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8321 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8327 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8333 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8339 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8345 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8351 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8357 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8363 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8369 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8375 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8381 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8387 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8393 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8399 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8405 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8411 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8417 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8423 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8429 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8452 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8458 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8464 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8470 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8476 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8482 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8488 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8494 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8500 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8506 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8512 "Parser/parser.cc"
    break;

  case 214:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8524 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8530 "Parser/parser.cc"
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
#line 8544 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8550 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 220:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8565 "Parser/parser.cc"
    break;

  case 221:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8571 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8595 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8601 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8613 "Parser/parser.cc"
    break;

  case 229:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8619 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8625 "Parser/parser.cc"
    break;

  case 232:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8631 "Parser/parser.cc"
    break;

  case 233:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8637 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8643 "Parser/parser.cc"
    break;

  case 235:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8649 "Parser/parser.cc"
    break;

  case 236:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8655 "Parser/parser.cc"
    break;

  case 237:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8661 "Parser/parser.cc"
    break;

  case 239:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8667 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8673 "Parser/parser.cc"
    break;

  case 241:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8679 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8685 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8691 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8697 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8706 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8718 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8724 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8733 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 254:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8760 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 256:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8772 "Parser/parser.cc"
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
#line 8791 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8797 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 261:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8809 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8815 "Parser/parser.cc"
    break;

  case 263:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8821 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8827 "Parser/parser.cc"
    break;

  case 265:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8833 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8839 "Parser/parser.cc"
    break;

  case 267:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8848 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8857 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8872 "Parser/parser.cc"
    break;

  case 271:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8881 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8887 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8893 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8899 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8905 "Parser/parser.cc"
    break;

  case 276:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8911 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8917 "Parser/parser.cc"
    break;

  case 278:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8923 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8929 "Parser/parser.cc"
    break;

  case 280:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8938 "Parser/parser.cc"
    break;

  case 281:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8948 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8954 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8960 "Parser/parser.cc"
    break;

  case 284:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8969 "Parser/parser.cc"
    break;

  case 285:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8979 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8985 "Parser/parser.cc"
    break;

  case 287:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8994 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9004 "Parser/parser.cc"
    break;

  case 289:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9010 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9016 "Parser/parser.cc"
    break;

  case 291:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9022 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9028 "Parser/parser.cc"
    break;

  case 293:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9037 "Parser/parser.cc"
    break;

  case 294:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9047 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 296:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9062 "Parser/parser.cc"
    break;

  case 297:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9072 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9078 "Parser/parser.cc"
    break;

  case 299:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9087 "Parser/parser.cc"
    break;

  case 300:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9097 "Parser/parser.cc"
    break;

  case 301:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9103 "Parser/parser.cc"
    break;

  case 302:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9112 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9121 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9127 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9133 "Parser/parser.cc"
    break;

  case 306:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9139 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9145 "Parser/parser.cc"
    break;

  case 308:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9151 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9157 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9163 "Parser/parser.cc"
    break;

  case 312:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 313:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9175 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9181 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9187 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9193 "Parser/parser.cc"
    break;

  case 317:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9199 "Parser/parser.cc"
    break;

  case 318:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9205 "Parser/parser.cc"
    break;

  case 319:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9211 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9217 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9223 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9229 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9235 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9241 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9247 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9253 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9259 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9265 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9271 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9277 "Parser/parser.cc"
    break;

  case 331:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9283 "Parser/parser.cc"
    break;

  case 334:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9289 "Parser/parser.cc"
    break;

  case 335:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9295 "Parser/parser.cc"
    break;

  case 336:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9301 "Parser/parser.cc"
    break;

  case 337:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9307 "Parser/parser.cc"
    break;

  case 339:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9313 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9319 "Parser/parser.cc"
    break;

  case 342:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9325 "Parser/parser.cc"
    break;

  case 343:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9331 "Parser/parser.cc"
    break;

  case 344:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9337 "Parser/parser.cc"
    break;

  case 345:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9343 "Parser/parser.cc"
    break;

  case 346:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9349 "Parser/parser.cc"
    break;

  case 347:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9355 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9361 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9367 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9373 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9379 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 353:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 354:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9397 "Parser/parser.cc"
    break;

  case 355:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 356:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 357:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9415 "Parser/parser.cc"
    break;

  case 358:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9421 "Parser/parser.cc"
    break;

  case 359:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9427 "Parser/parser.cc"
    break;

  case 360:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9433 "Parser/parser.cc"
    break;

  case 361:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9439 "Parser/parser.cc"
    break;

  case 362:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9445 "Parser/parser.cc"
    break;

  case 363:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 365:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 366:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 367:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 372:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9475 "Parser/parser.cc"
    break;

  case 373:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 375:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 376:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9499 "Parser/parser.cc"
    break;

  case 377:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9505 "Parser/parser.cc"
    break;

  case 378:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9511 "Parser/parser.cc"
    break;

  case 379:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9517 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9523 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9529 "Parser/parser.cc"
    break;

  case 384:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 385:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9541 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9553 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9562 "Parser/parser.cc"
    break;

  case 389:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9571 "Parser/parser.cc"
    break;

  case 390:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9577 "Parser/parser.cc"
    break;

  case 393:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 394:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9589 "Parser/parser.cc"
    break;

  case 396:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9595 "Parser/parser.cc"
    break;

  case 397:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 404:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9612 "Parser/parser.cc"
    break;

  case 407:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9618 "Parser/parser.cc"
    break;

  case 408:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9624 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9630 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9636 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9642 "Parser/parser.cc"
    break;

  case 416:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9648 "Parser/parser.cc"
    break;

  case 417:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9654 "Parser/parser.cc"
    break;

  case 418:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9660 "Parser/parser.cc"
    break;

  case 419:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 421:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9672 "Parser/parser.cc"
    break;

  case 422:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9678 "Parser/parser.cc"
    break;

  case 423:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9684 "Parser/parser.cc"
    break;

  case 424:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9695 "Parser/parser.cc"
    break;

  case 425:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9701 "Parser/parser.cc"
    break;

  case 426:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9707 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 428:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9719 "Parser/parser.cc"
    break;

  case 429:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9728 "Parser/parser.cc"
    break;

  case 430:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9737 "Parser/parser.cc"
    break;

  case 431:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9746 "Parser/parser.cc"
    break;

  case 432:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// if type_specifier is an anon aggregate => name 
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9756 "Parser/parser.cc"
    break;

  case 433:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9765 "Parser/parser.cc"
    break;

  case 434:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9774 "Parser/parser.cc"
    break;

  case 435:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9783 "Parser/parser.cc"
    break;

  case 436:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9792 "Parser/parser.cc"
    break;

  case 437:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9800 "Parser/parser.cc"
    break;

  case 438:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9808 "Parser/parser.cc"
    break;

  case 439:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9814 "Parser/parser.cc"
    break;

  case 443:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9820 "Parser/parser.cc"
    break;

  case 444:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9826 "Parser/parser.cc"
    break;

  case 452:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9837 "Parser/parser.cc"
    break;

  case 457:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9843 "Parser/parser.cc"
    break;

  case 460:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9849 "Parser/parser.cc"
    break;

  case 463:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9855 "Parser/parser.cc"
    break;

  case 464:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9861 "Parser/parser.cc"
    break;

  case 465:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9867 "Parser/parser.cc"
    break;

  case 466:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9873 "Parser/parser.cc"
    break;

  case 468:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9879 "Parser/parser.cc"
    break;

  case 470:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9885 "Parser/parser.cc"
    break;

  case 471:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9891 "Parser/parser.cc"
    break;

  case 473:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9897 "Parser/parser.cc"
    break;

  case 474:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9903 "Parser/parser.cc"
    break;

  case 475:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9909 "Parser/parser.cc"
    break;

  case 476:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9915 "Parser/parser.cc"
    break;

  case 477:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9921 "Parser/parser.cc"
    break;

  case 478:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9927 "Parser/parser.cc"
    break;

  case 479:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9933 "Parser/parser.cc"
    break;

  case 480:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9939 "Parser/parser.cc"
    break;

  case 481:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9945 "Parser/parser.cc"
    break;

  case 482:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9951 "Parser/parser.cc"
    break;

  case 483:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9957 "Parser/parser.cc"
    break;

  case 484:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9963 "Parser/parser.cc"
    break;

  case 485:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9969 "Parser/parser.cc"
    break;

  case 486:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9975 "Parser/parser.cc"
    break;

  case 487:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9981 "Parser/parser.cc"
    break;

  case 488:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9987 "Parser/parser.cc"
    break;

  case 489:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9993 "Parser/parser.cc"
    break;

  case 490:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9999 "Parser/parser.cc"
    break;

  case 491:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10005 "Parser/parser.cc"
    break;

  case 492:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10011 "Parser/parser.cc"
    break;

  case 493:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10017 "Parser/parser.cc"
    break;

  case 494:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10023 "Parser/parser.cc"
    break;

  case 495:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10029 "Parser/parser.cc"
    break;

  case 496:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10035 "Parser/parser.cc"
    break;

  case 497:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10041 "Parser/parser.cc"
    break;

  case 498:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10047 "Parser/parser.cc"
    break;

  case 499:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10053 "Parser/parser.cc"
    break;

  case 500:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10059 "Parser/parser.cc"
    break;

  case 501:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10065 "Parser/parser.cc"
    break;

  case 502:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10071 "Parser/parser.cc"
    break;

  case 503:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10077 "Parser/parser.cc"
    break;

  case 504:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10083 "Parser/parser.cc"
    break;

  case 505:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10089 "Parser/parser.cc"
    break;

  case 506:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10095 "Parser/parser.cc"
    break;

  case 507:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10101 "Parser/parser.cc"
    break;

  case 508:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10107 "Parser/parser.cc"
    break;

  case 509:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10113 "Parser/parser.cc"
    break;

  case 511:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10119 "Parser/parser.cc"
    break;

  case 513:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 514:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10131 "Parser/parser.cc"
    break;

  case 515:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10137 "Parser/parser.cc"
    break;

  case 517:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 518:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 519:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 520:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 522:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 524:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 525:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 526:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 527:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10191 "Parser/parser.cc"
    break;

  case 528:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 529:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10203 "Parser/parser.cc"
    break;

  case 530:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10209 "Parser/parser.cc"
    break;

  case 531:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10215 "Parser/parser.cc"
    break;

  case 532:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10221 "Parser/parser.cc"
    break;

  case 533:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10232 "Parser/parser.cc"
    break;

  case 534:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 535:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 536:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10250 "Parser/parser.cc"
    break;

  case 537:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10261 "Parser/parser.cc"
    break;

  case 538:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10267 "Parser/parser.cc"
    break;

  case 539:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 540:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10282 "Parser/parser.cc"
    break;

  case 542:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10288 "Parser/parser.cc"
    break;

  case 543:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10294 "Parser/parser.cc"
    break;

  case 544:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10300 "Parser/parser.cc"
    break;

  case 546:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10306 "Parser/parser.cc"
    break;

  case 547:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10312 "Parser/parser.cc"
    break;

  case 549:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10318 "Parser/parser.cc"
    break;

  case 550:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 551:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 553:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 554:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 555:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 556:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 557:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 559:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 560:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 561:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10378 "Parser/parser.cc"
    break;

  case 562:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10384 "Parser/parser.cc"
    break;

  case 563:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 564:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10401 "Parser/parser.cc"
    break;

  case 568:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10407 "Parser/parser.cc"
    break;

  case 569:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10413 "Parser/parser.cc"
    break;

  case 570:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10422 "Parser/parser.cc"
    break;

  case 571:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10439 "Parser/parser.cc"
    break;

  case 572:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10448 "Parser/parser.cc"
    break;

  case 573:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10458 "Parser/parser.cc"
    break;

  case 574:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10467 "Parser/parser.cc"
    break;

  case 575:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10477 "Parser/parser.cc"
    break;

  case 577:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10483 "Parser/parser.cc"
    break;

  case 578:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10489 "Parser/parser.cc"
    break;

  case 579:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10499 "Parser/parser.cc"
    break;

  case 580:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10514 "Parser/parser.cc"
    break;

  case 583:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10520 "Parser/parser.cc"
    break;

  case 584:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10526 "Parser/parser.cc"
    break;

  case 585:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10532 "Parser/parser.cc"
    break;

  case 586:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10538 "Parser/parser.cc"
    break;

  case 587:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10544 "Parser/parser.cc"
    break;

  case 588:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10550 "Parser/parser.cc"
    break;

  case 589:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10556 "Parser/parser.cc"
    break;

  case 590:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10562 "Parser/parser.cc"
    break;

  case 591:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10568 "Parser/parser.cc"
    break;

  case 592:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10574 "Parser/parser.cc"
    break;

  case 593:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10580 "Parser/parser.cc"
    break;

  case 594:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10586 "Parser/parser.cc"
    break;

  case 595:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10592 "Parser/parser.cc"
    break;

  case 596:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10605 "Parser/parser.cc"
    break;

  case 597:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10611 "Parser/parser.cc"
    break;

  case 598:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10617 "Parser/parser.cc"
    break;

  case 599:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10630 "Parser/parser.cc"
    break;

  case 600:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10636 "Parser/parser.cc"
    break;

  case 603:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10642 "Parser/parser.cc"
    break;

  case 604:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10648 "Parser/parser.cc"
    break;

  case 607:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10654 "Parser/parser.cc"
    break;

  case 609:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 610:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 611:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 612:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 613:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10684 "Parser/parser.cc"
    break;

  case 615:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 617:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 618:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 620:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 621:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10714 "Parser/parser.cc"
    break;

  case 623:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10720 "Parser/parser.cc"
    break;

  case 624:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10726 "Parser/parser.cc"
    break;

  case 625:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 626:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10738 "Parser/parser.cc"
    break;

  case 627:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10744 "Parser/parser.cc"
    break;

  case 628:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10755 "Parser/parser.cc"
    break;

  case 629:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10763 "Parser/parser.cc"
    break;

  case 630:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10772 "Parser/parser.cc"
    break;

  case 631:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10780 "Parser/parser.cc"
    break;

  case 632:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10788 "Parser/parser.cc"
    break;

  case 633:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10796 "Parser/parser.cc"
    break;

  case 634:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10804 "Parser/parser.cc"
    break;

  case 636:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10810 "Parser/parser.cc"
    break;

  case 637:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10816 "Parser/parser.cc"
    break;

  case 638:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10822 "Parser/parser.cc"
    break;

  case 639:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10828 "Parser/parser.cc"
    break;

  case 640:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10834 "Parser/parser.cc"
    break;

  case 641:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10840 "Parser/parser.cc"
    break;

  case 642:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10846 "Parser/parser.cc"
    break;

  case 643:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10852 "Parser/parser.cc"
    break;

  case 645:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10858 "Parser/parser.cc"
    break;

  case 646:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10864 "Parser/parser.cc"
    break;

  case 647:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10870 "Parser/parser.cc"
    break;

  case 648:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10876 "Parser/parser.cc"
    break;

  case 649:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10882 "Parser/parser.cc"
    break;

  case 650:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10888 "Parser/parser.cc"
    break;

  case 653:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10894 "Parser/parser.cc"
    break;

  case 654:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10900 "Parser/parser.cc"
    break;

  case 655:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10906 "Parser/parser.cc"
    break;

  case 657:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10912 "Parser/parser.cc"
    break;

  case 658:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10918 "Parser/parser.cc"
    break;

  case 659:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10924 "Parser/parser.cc"
    break;

  case 661:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10930 "Parser/parser.cc"
    break;

  case 662:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10936 "Parser/parser.cc"
    break;

  case 663:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10942 "Parser/parser.cc"
    break;

  case 665:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10948 "Parser/parser.cc"
    break;

  case 668:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10954 "Parser/parser.cc"
    break;

  case 669:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10960 "Parser/parser.cc"
    break;

  case 671:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10966 "Parser/parser.cc"
    break;

  case 672:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10972 "Parser/parser.cc"
    break;

  case 673:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 678:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10984 "Parser/parser.cc"
    break;

  case 680:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10990 "Parser/parser.cc"
    break;

  case 681:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10996 "Parser/parser.cc"
    break;

  case 682:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11002 "Parser/parser.cc"
    break;

  case 683:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11008 "Parser/parser.cc"
    break;

  case 684:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11014 "Parser/parser.cc"
    break;

  case 685:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11020 "Parser/parser.cc"
    break;

  case 691:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11026 "Parser/parser.cc"
    break;

  case 694:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11032 "Parser/parser.cc"
    break;

  case 695:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11038 "Parser/parser.cc"
    break;

  case 696:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11044 "Parser/parser.cc"
    break;

  case 697:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11050 "Parser/parser.cc"
    break;

  case 698:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11056 "Parser/parser.cc"
    break;

  case 699:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11062 "Parser/parser.cc"
    break;

  case 700:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11068 "Parser/parser.cc"
    break;

  case 702:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11074 "Parser/parser.cc"
    break;

  case 703:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11080 "Parser/parser.cc"
    break;

  case 704:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11086 "Parser/parser.cc"
    break;

  case 706:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11092 "Parser/parser.cc"
    break;

  case 708:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11098 "Parser/parser.cc"
    break;

  case 709:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11104 "Parser/parser.cc"
    break;

  case 710:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11110 "Parser/parser.cc"
    break;

  case 711:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11116 "Parser/parser.cc"
    break;

  case 712:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11122 "Parser/parser.cc"
    break;

  case 713:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11128 "Parser/parser.cc"
    break;

  case 715:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11134 "Parser/parser.cc"
    break;

  case 716:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11140 "Parser/parser.cc"
    break;

  case 717:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11146 "Parser/parser.cc"
    break;

  case 718:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11157 "Parser/parser.cc"
    break;

  case 719:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 720:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11169 "Parser/parser.cc"
    break;

  case 721:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11175 "Parser/parser.cc"
    break;

  case 722:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11184 "Parser/parser.cc"
    break;

  case 723:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 724:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11196 "Parser/parser.cc"
    break;

  case 725:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11202 "Parser/parser.cc"
    break;

  case 726:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11208 "Parser/parser.cc"
    break;

  case 727:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11214 "Parser/parser.cc"
    break;

  case 728:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11220 "Parser/parser.cc"
    break;

  case 729:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11226 "Parser/parser.cc"
    break;

  case 730:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11232 "Parser/parser.cc"
    break;

  case 731:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11238 "Parser/parser.cc"
    break;

  case 732:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11244 "Parser/parser.cc"
    break;

  case 735:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 736:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 737:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11262 "Parser/parser.cc"
    break;

  case 738:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 740:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11274 "Parser/parser.cc"
    break;

  case 741:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11280 "Parser/parser.cc"
    break;

  case 742:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11286 "Parser/parser.cc"
    break;

  case 743:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 744:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 745:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 746:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 747:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11319 "Parser/parser.cc"
    break;

  case 748:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11328 "Parser/parser.cc"
    break;

  case 749:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11334 "Parser/parser.cc"
    break;

  case 750:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 752:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 757:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 758:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 759:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 761:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11370 "Parser/parser.cc"
    break;

  case 762:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11376 "Parser/parser.cc"
    break;

  case 763:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11382 "Parser/parser.cc"
    break;

  case 764:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11388 "Parser/parser.cc"
    break;

  case 766:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11394 "Parser/parser.cc"
    break;

  case 767:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11400 "Parser/parser.cc"
    break;

  case 768:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11406 "Parser/parser.cc"
    break;

  case 770:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11412 "Parser/parser.cc"
    break;

  case 771:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11418 "Parser/parser.cc"
    break;

  case 772:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11424 "Parser/parser.cc"
    break;

  case 773:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11430 "Parser/parser.cc"
    break;

  case 774:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11436 "Parser/parser.cc"
    break;

  case 775:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11442 "Parser/parser.cc"
    break;

  case 777:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11451 "Parser/parser.cc"
    break;

  case 778:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 779:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11466 "Parser/parser.cc"
    break;

  case 780:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11476 "Parser/parser.cc"
    break;

  case 781:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11485 "Parser/parser.cc"
    break;

  case 782:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11495 "Parser/parser.cc"
    break;

  case 783:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11504 "Parser/parser.cc"
    break;

  case 784:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11514 "Parser/parser.cc"
    break;

  case 785:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11523 "Parser/parser.cc"
    break;

  case 786:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11533 "Parser/parser.cc"
    break;

  case 787:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11542 "Parser/parser.cc"
    break;

  case 788:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11552 "Parser/parser.cc"
    break;

  case 790:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 791:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 792:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11570 "Parser/parser.cc"
    break;

  case 793:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11582 "Parser/parser.cc"
    break;

  case 794:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11593 "Parser/parser.cc"
    break;

  case 795:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11602 "Parser/parser.cc"
    break;

  case 796:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11611 "Parser/parser.cc"
    break;

  case 797:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11617 "Parser/parser.cc"
    break;

  case 798:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 799:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11629 "Parser/parser.cc"
    break;

  case 800:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11638 "Parser/parser.cc"
    break;

  case 801:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11644 "Parser/parser.cc"
    break;

  case 802:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11650 "Parser/parser.cc"
    break;

  case 803:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11656 "Parser/parser.cc"
    break;

  case 807:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11662 "Parser/parser.cc"
    break;

  case 808:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11668 "Parser/parser.cc"
    break;

  case 809:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11678 "Parser/parser.cc"
    break;

  case 810:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11684 "Parser/parser.cc"
    break;

  case 813:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11690 "Parser/parser.cc"
    break;

  case 814:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11696 "Parser/parser.cc"
    break;

  case 816:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 817:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11708 "Parser/parser.cc"
    break;

  case 818:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 819:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11720 "Parser/parser.cc"
    break;

  case 824:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11726 "Parser/parser.cc"
    break;

  case 825:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11732 "Parser/parser.cc"
    break;

  case 826:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11738 "Parser/parser.cc"
    break;

  case 827:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11744 "Parser/parser.cc"
    break;

  case 828:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11750 "Parser/parser.cc"
    break;

  case 830:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11756 "Parser/parser.cc"
    break;

  case 831:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11762 "Parser/parser.cc"
    break;

  case 832:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11768 "Parser/parser.cc"
    break;

  case 833:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11774 "Parser/parser.cc"
    break;

  case 834:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11780 "Parser/parser.cc"
    break;

  case 835:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11786 "Parser/parser.cc"
    break;

  case 836:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11792 "Parser/parser.cc"
    break;

  case 837:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11798 "Parser/parser.cc"
    break;

  case 838:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11804 "Parser/parser.cc"
    break;

  case 839:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11810 "Parser/parser.cc"
    break;

  case 840:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11816 "Parser/parser.cc"
    break;

  case 841:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11822 "Parser/parser.cc"
    break;

  case 842:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11828 "Parser/parser.cc"
    break;

  case 843:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11834 "Parser/parser.cc"
    break;

  case 844:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 845:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11846 "Parser/parser.cc"
    break;

  case 846:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11852 "Parser/parser.cc"
    break;

  case 847:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11858 "Parser/parser.cc"
    break;

  case 849:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11864 "Parser/parser.cc"
    break;

  case 850:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11870 "Parser/parser.cc"
    break;

  case 851:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11876 "Parser/parser.cc"
    break;

  case 852:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11882 "Parser/parser.cc"
    break;

  case 853:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11888 "Parser/parser.cc"
    break;

  case 854:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11894 "Parser/parser.cc"
    break;

  case 855:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11900 "Parser/parser.cc"
    break;

  case 856:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11906 "Parser/parser.cc"
    break;

  case 857:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11912 "Parser/parser.cc"
    break;

  case 858:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11918 "Parser/parser.cc"
    break;

  case 859:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11924 "Parser/parser.cc"
    break;

  case 860:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11930 "Parser/parser.cc"
    break;

  case 861:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11936 "Parser/parser.cc"
    break;

  case 862:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11942 "Parser/parser.cc"
    break;

  case 863:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11948 "Parser/parser.cc"
    break;

  case 864:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11954 "Parser/parser.cc"
    break;

  case 868:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11960 "Parser/parser.cc"
    break;

  case 869:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11966 "Parser/parser.cc"
    break;

  case 870:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11972 "Parser/parser.cc"
    break;

  case 871:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11978 "Parser/parser.cc"
    break;

  case 872:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11984 "Parser/parser.cc"
    break;

  case 873:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11990 "Parser/parser.cc"
    break;

  case 874:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11996 "Parser/parser.cc"
    break;

  case 875:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12002 "Parser/parser.cc"
    break;

  case 876:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12008 "Parser/parser.cc"
    break;

  case 877:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12014 "Parser/parser.cc"
    break;

  case 878:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12020 "Parser/parser.cc"
    break;

  case 879:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12026 "Parser/parser.cc"
    break;

  case 880:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12032 "Parser/parser.cc"
    break;

  case 881:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12038 "Parser/parser.cc"
    break;

  case 882:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12044 "Parser/parser.cc"
    break;

  case 883:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12053 "Parser/parser.cc"
    break;

  case 884:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12059 "Parser/parser.cc"
    break;

  case 885:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 887:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 888:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 889:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 890:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 891:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 892:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 893:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 894:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 895:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 896:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 897:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 898:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12137 "Parser/parser.cc"
    break;

  case 899:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 900:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 901:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 902:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 903:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12167 "Parser/parser.cc"
    break;

  case 904:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 905:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 906:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 908:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 909:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12197 "Parser/parser.cc"
    break;

  case 910:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 911:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12209 "Parser/parser.cc"
    break;

  case 912:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 913:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 914:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 915:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12233 "Parser/parser.cc"
    break;

  case 916:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12239 "Parser/parser.cc"
    break;

  case 917:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 918:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 919:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12257 "Parser/parser.cc"
    break;

  case 920:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12263 "Parser/parser.cc"
    break;

  case 921:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 923:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12275 "Parser/parser.cc"
    break;

  case 924:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 925:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 926:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12293 "Parser/parser.cc"
    break;

  case 927:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 928:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 929:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 930:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 931:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 932:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 933:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 935:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 936:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 937:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 938:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 939:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 940:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 941:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 943:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 944:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 945:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12395 "Parser/parser.cc"
    break;

  case 946:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12401 "Parser/parser.cc"
    break;

  case 947:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 948:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12413 "Parser/parser.cc"
    break;

  case 949:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12419 "Parser/parser.cc"
    break;

  case 950:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 951:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 952:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12437 "Parser/parser.cc"
    break;

  case 954:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 955:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 957:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12455 "Parser/parser.cc"
    break;

  case 958:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12461 "Parser/parser.cc"
    break;

  case 960:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12467 "Parser/parser.cc"
    break;

  case 961:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12473 "Parser/parser.cc"
    break;

  case 962:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12479 "Parser/parser.cc"
    break;

  case 963:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12485 "Parser/parser.cc"
    break;

  case 964:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 965:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12497 "Parser/parser.cc"
    break;

  case 966:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12503 "Parser/parser.cc"
    break;

  case 969:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12509 "Parser/parser.cc"
    break;

  case 970:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 971:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 972:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 973:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 974:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 975:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 976:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12551 "Parser/parser.cc"
    break;

  case 978:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12557 "Parser/parser.cc"
    break;

  case 979:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12563 "Parser/parser.cc"
    break;

  case 980:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12569 "Parser/parser.cc"
    break;

  case 981:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12575 "Parser/parser.cc"
    break;

  case 982:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12581 "Parser/parser.cc"
    break;

  case 983:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12587 "Parser/parser.cc"
    break;

  case 985:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12593 "Parser/parser.cc"
    break;

  case 987:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12599 "Parser/parser.cc"
    break;

  case 988:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12605 "Parser/parser.cc"
    break;

  case 989:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12611 "Parser/parser.cc"
    break;

  case 990:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12617 "Parser/parser.cc"
    break;

  case 991:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12623 "Parser/parser.cc"
    break;

  case 992:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12629 "Parser/parser.cc"
    break;

  case 994:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12635 "Parser/parser.cc"
    break;

  case 995:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12641 "Parser/parser.cc"
    break;

  case 996:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12647 "Parser/parser.cc"
    break;

  case 997:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12653 "Parser/parser.cc"
    break;

  case 998:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12659 "Parser/parser.cc"
    break;

  case 999:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12665 "Parser/parser.cc"
    break;

  case 1000:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12671 "Parser/parser.cc"
    break;

  case 1002:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12677 "Parser/parser.cc"
    break;

  case 1003:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12683 "Parser/parser.cc"
    break;

  case 1004:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12689 "Parser/parser.cc"
    break;

  case 1005:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12695 "Parser/parser.cc"
    break;

  case 1006:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12701 "Parser/parser.cc"
    break;

  case 1009:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12707 "Parser/parser.cc"
    break;

  case 1012:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12713 "Parser/parser.cc"
    break;

  case 1013:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12719 "Parser/parser.cc"
    break;

  case 1014:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12725 "Parser/parser.cc"
    break;

  case 1015:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12731 "Parser/parser.cc"
    break;

  case 1016:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12737 "Parser/parser.cc"
    break;

  case 1017:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12743 "Parser/parser.cc"
    break;

  case 1018:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12749 "Parser/parser.cc"
    break;

  case 1019:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12755 "Parser/parser.cc"
    break;

  case 1020:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12761 "Parser/parser.cc"
    break;

  case 1021:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12767 "Parser/parser.cc"
    break;

  case 1022:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12773 "Parser/parser.cc"
    break;

  case 1023:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12779 "Parser/parser.cc"
    break;

  case 1024:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12785 "Parser/parser.cc"
    break;

  case 1025:
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12791 "Parser/parser.cc"
    break;

  case 1026:
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12797 "Parser/parser.cc"
    break;

  case 1027:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12803 "Parser/parser.cc"
    break;

  case 1028:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12809 "Parser/parser.cc"
    break;

  case 1029:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12815 "Parser/parser.cc"
    break;

  case 1030:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12821 "Parser/parser.cc"
    break;

  case 1031:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12827 "Parser/parser.cc"
    break;

  case 1033:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12833 "Parser/parser.cc"
    break;

  case 1037:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12839 "Parser/parser.cc"
    break;

  case 1038:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12845 "Parser/parser.cc"
    break;

  case 1039:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12851 "Parser/parser.cc"
    break;

  case 1040:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12857 "Parser/parser.cc"
    break;

  case 1041:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12863 "Parser/parser.cc"
    break;

  case 1042:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12869 "Parser/parser.cc"
    break;

  case 1043:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 1044:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 1045:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12887 "Parser/parser.cc"
    break;

  case 1046:
#line 3981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 1047:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 1048:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12905 "Parser/parser.cc"
    break;

  case 1049:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12911 "Parser/parser.cc"
    break;

  case 1050:
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12917 "Parser/parser.cc"
    break;

  case 1051:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12923 "Parser/parser.cc"
    break;

  case 1052:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12929 "Parser/parser.cc"
    break;

  case 1053:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12935 "Parser/parser.cc"
    break;

  case 1056:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12941 "Parser/parser.cc"
    break;

  case 1057:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12947 "Parser/parser.cc"
    break;


#line 12951 "Parser/parser.cc"

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
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
