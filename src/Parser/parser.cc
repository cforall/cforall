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
#define YYLAST   22766

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  297
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1063
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2153

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
    1982,  1983,  1984,  1990,  1992,  1997,  1998,  1999,  2000,  2011,
    2012,  2013,  2022,  2023,  2024,  2028,  2029,  2036,  2045,  2046,
    2047,  2052,  2053,  2062,  2063,  2068,  2069,  2073,  2075,  2077,
    2079,  2081,  2085,  2090,  2091,  2093,  2103,  2104,  2109,  2111,
    2113,  2115,  2117,  2119,  2122,  2124,  2126,  2131,  2133,  2135,
    2137,  2139,  2141,  2143,  2145,  2147,  2149,  2151,  2153,  2155,
    2157,  2159,  2161,  2163,  2165,  2167,  2169,  2171,  2173,  2175,
    2177,  2179,  2181,  2183,  2185,  2190,  2191,  2195,  2202,  2203,
    2209,  2210,  2212,  2214,  2216,  2221,  2223,  2228,  2229,  2231,
    2233,  2238,  2240,  2242,  2244,  2246,  2248,  2253,  2260,  2262,
    2264,  2269,  2277,  2276,  2280,  2288,  2289,  2291,  2293,  2298,
    2299,  2301,  2306,  2307,  2309,  2311,  2316,  2317,  2319,  2324,
    2326,  2328,  2330,  2331,  2333,  2338,  2340,  2342,  2347,  2354,
    2358,  2359,  2364,  2363,  2368,  2367,  2386,  2385,  2397,  2396,
    2407,  2412,  2413,  2418,  2424,  2438,  2439,  2443,  2445,  2447,
    2453,  2455,  2457,  2459,  2461,  2463,  2465,  2467,  2473,  2474,
    2479,  2488,  2490,  2492,  2501,  2503,  2504,  2505,  2507,  2509,
    2510,  2515,  2516,  2517,  2522,  2524,  2527,  2534,  2535,  2536,
    2542,  2547,  2549,  2555,  2556,  2562,  2563,  2567,  2572,  2575,
    2574,  2578,  2581,  2588,  2593,  2592,  2601,  2606,  2611,  2616,
    2621,  2622,  2627,  2629,  2634,  2636,  2638,  2640,  2645,  2646,
    2652,  2653,  2654,  2661,  2662,  2664,  2665,  2666,  2668,  2670,
    2677,  2678,  2680,  2682,  2687,  2688,  2694,  2695,  2697,  2698,
    2703,  2704,  2705,  2707,  2715,  2716,  2718,  2721,  2723,  2727,
    2728,  2729,  2731,  2733,  2738,  2740,  2745,  2747,  2756,  2758,
    2763,  2764,  2765,  2769,  2770,  2771,  2776,  2777,  2782,  2783,
    2784,  2785,  2789,  2790,  2795,  2796,  2797,  2798,  2799,  2813,
    2814,  2819,  2820,  2826,  2828,  2831,  2833,  2835,  2858,  2859,
    2865,  2866,  2872,  2871,  2881,  2880,  2884,  2890,  2896,  2897,
    2899,  2903,  2908,  2910,  2912,  2914,  2920,  2921,  2925,  2926,
    2931,  2933,  2940,  2942,  2943,  2945,  2950,  2952,  2954,  2959,
    2961,  2966,  2971,  2979,  2981,  2983,  2985,  2990,  2991,  2996,
    2997,  3001,  3002,  3003,  3008,  3010,  3016,  3018,  3023,  3025,
    3031,  3032,  3036,  3040,  3044,  3046,  3047,  3049,  3051,  3053,
    3055,  3057,  3059,  3060,  3065,  3068,  3067,  3079,  3078,  3091,
    3090,  3102,  3101,  3113,  3112,  3126,  3132,  3134,  3140,  3141,
    3152,  3159,  3164,  3170,  3173,  3176,  3180,  3186,  3189,  3192,
    3197,  3198,  3199,  3203,  3209,  3210,  3220,  3221,  3225,  3226,
    3231,  3236,  3237,  3243,  3244,  3246,  3251,  3252,  3253,  3254,
    3255,  3257,  3292,  3294,  3299,  3301,  3302,  3304,  3309,  3311,
    3313,  3315,  3320,  3322,  3324,  3326,  3328,  3330,  3332,  3337,
    3339,  3341,  3343,  3352,  3354,  3355,  3360,  3362,  3364,  3366,
    3368,  3373,  3375,  3377,  3379,  3384,  3386,  3388,  3390,  3392,
    3394,  3406,  3407,  3408,  3412,  3414,  3416,  3418,  3420,  3425,
    3427,  3429,  3431,  3436,  3438,  3440,  3442,  3444,  3446,  3461,
    3466,  3471,  3473,  3474,  3476,  3481,  3483,  3485,  3487,  3492,
    3494,  3496,  3498,  3500,  3502,  3504,  3509,  3511,  3513,  3515,
    3517,  3527,  3529,  3531,  3532,  3534,  3539,  3541,  3543,  3548,
    3550,  3552,  3554,  3559,  3561,  3563,  3577,  3579,  3581,  3582,
    3584,  3589,  3591,  3596,  3598,  3600,  3605,  3607,  3612,  3614,
    3631,  3632,  3634,  3639,  3641,  3643,  3645,  3647,  3652,  3653,
    3655,  3657,  3662,  3664,  3666,  3672,  3674,  3677,  3680,  3682,
    3686,  3688,  3690,  3691,  3693,  3695,  3699,  3701,  3706,  3708,
    3710,  3712,  3747,  3748,  3752,  3753,  3755,  3757,  3762,  3764,
    3766,  3768,  3770,  3775,  3776,  3778,  3780,  3785,  3787,  3789,
    3795,  3796,  3798,  3807,  3810,  3812,  3815,  3817,  3819,  3833,
    3834,  3836,  3841,  3843,  3845,  3847,  3849,  3854,  3855,  3857,
    3859,  3864,  3866,  3874,  3875,  3876,  3881,  3882,  3887,  3889,
    3891,  3893,  3895,  3897,  3904,  3906,  3908,  3910,  3912,  3915,
    3917,  3919,  3921,  3923,  3928,  3930,  3932,  3937,  3963,  3964,
    3966,  3970,  3971,  3975,  3977,  3979,  3981,  3983,  3985,  3992,
    3994,  3996,  3998,  4000,  4002,  4007,  4009,  4011,  4018,  4020,
    4038,  4040,  4045,  4046
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

#define YYPACT_NINF (-1760)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1062)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      74, 12834,    83,   139, 17582,   152, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,    -4,   856,
     151, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,   104,   306,
   -1760, -1760, -1760, -1760, -1760, -1760,  4837,  4837,   225, 12834,
     231,   369, 14317, -1760,   372, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760,  2748, -1760,   880,   380, -1760, -1760,
   -1760, -1760, -1760, 17430, -1760, -1760,   378,   428,   336,    67,
   -1760,  4837,   428,   428,   428,   421,  4467,   627,   788, 12996,
   -1760, -1760, -1760, 17278,  2639, -1760, -1760, -1760,  3527,   662,
    6433,  9452,   778,  3527,   871,   524, -1760, -1760, -1760, -1760,
     639, -1760, -1760, -1760, -1760,   538, -1760, -1760, -1760, -1760,
   -1760,   598,   601,   639, -1760,   639,   646, -1760, -1760, -1760,
   18448,  4837, -1760, -1760,  4837, -1760, 12834,   671, -1760,   665,
   18501, -1760, -1760,  4556, 19528, -1760,   757,   757,   697,  2824,
   -1760, -1760, -1760, -1760,    30, 14908,  2648,   639, -1760, -1760,
   -1760, -1760, -1760, -1760,   708, -1760,   696,   739,   753, -1760,
     789, 22043, -1760, -1760, -1760, -1760, -1760, -1760, -1760, 16233,
    4041,  2748,    43,   762,   769,   775,   787,   796,   809, -1760,
   -1760, 18653, 11845,   819, -1760, 18029, -1760, -1760, -1760, -1760,
     825, -1760, -1760,   822, -1760,  5441,   978,  8267, -1760,   838,
    4837,   601,   846,   853,   872,   877, -1760, -1760, -1760,  3299,
    3745,   892,   922,   105, -1760, -1760,   639,   639,   232,   243,
     133,   232, -1760,   639,   639, -1760,  3852, -1760, -1760,   905,
     916,   757, 14800, -1760, 17430, -1760, -1760,  3527, -1760,  2661,
     524,   959,  1005,   243,  4837,   336, -1760, 14425, -1760,   757,
     757,   965,  1005,   243,  4837, -1760, 22643, -1760, -1760, -1760,
     757, -1760, -1760, -1760, -1760,   757, -1760,   920,  3865,  4837,
   -1760,   930,   971, -1760, -1760, -1760, 17129,   601,   261, -1760,
   -1760, 19579, -1760,   922,    42, -1760,  2824, 22043, 19528,  3662,
    3852, -1760,   276, -1760, -1760, -1760, 18501,  4837, -1760,   999,
   -1760, -1760, -1760, -1760,  4837,  3050,   486,   458, -1760,  4837,
     696, -1760,  1016,   639,   639,  1007, 18706,   690, 15391, 14961,
    3527,  3527, -1760,  3527,   757,  3527,   757, -1760, -1760,   639,
   -1760,  1022, -1760, 18858, -1760, -1760, -1760, 18911,   825, -1760,
     477,   713,   229,   533,   524,  1012, -1760,  2824,  1014,   696,
    2824,  1620, -1760,  1043,  1101, 22117,  1051,  1069,  1072, 22043,
   22191,  1088, 15766, -1760, -1760, -1760, -1760, -1760, -1760, 22265,
   22265, 16077,  1079,  4214, -1760, -1760, -1760, -1760,   745, -1760,
     797, -1760,  1105, -1760, 22043, 22043, -1760,  1090,   613,  1103,
    1167,   545,  1177,  1109,  1128,  1125,  1179,    77, -1760,   522,
   -1760,  1150, -1760,  1158,  4787, 16545, -1760, -1760,   451,  1150,
   -1760, -1760,   624, -1760, -1760,  4041,  1163,  1181,  1187,  1194,
    1197,  1199, -1760, -1760,   299,  1165, -1760,   681,  1165, -1760,
   -1760, 18448, -1760,  1162,  1198, 16701, -1760, -1760,  4662,  4310,
    1228, 15391,  1230,   717,   743, -1760, -1760, -1760, -1760, -1760,
    4837,  4716, -1760, -1760, -1760, -1760, -1760, -1760, 17022,  3272,
    1079,  5441,  1211,  1218, -1760, -1760,  1210,  8267,   776, -1760,
   -1760, -1760, 20341,  1222, -1760, -1760, -1760, -1760, -1760,  3299,
     854,  1226,  1233,  1251,   863,  1267,  1269,  1275,  3745, -1760,
   -1760,   639,  1245,   336,  1278, -1760, -1760,  1282, -1760, -1760,
     601,  1005, -1760, -1760, -1760,   601, -1760, -1760,  3852, -1760,
   16545, 16545, -1760,   757,  4556, 20029, 15552, -1760, -1760, -1760,
   -1760, -1760,   601,  1005,    42, -1760, -1760,  3527,  1285,  1005,
     243, -1760,   601,  1005, -1760, 22694, -1760,   757,   757, -1760,
   -1760,  1289,   592,  1302,   524,  1321, -1760, 17743, -1760,   820,
   -1760,  1414,  8770, -1760,  4556, 17218, 14800, -1760, 17129, 22339,
   -1760, -1760, -1760, -1760,   858, -1760,  3662,   870,  3852, -1760,
   15552,   922, 12834, -1760,  1331, -1760,  1351, -1760, -1760, -1760,
   -1760,  1440,  2824, -1760, -1760,  1432,  4647,  3443, 18911, 11845,
   -1760, 19063, -1760,   757,   757, -1760, -1760,   825, -1760,   700,
    1355,  1495, 22043,  1170,  1282,  1340, -1760,   639,   639, -1760,
    1165, -1760, 18706, -1760, -1760, 18190,   757,   757, -1760,  4647,
     639, -1760, 19383, -1760, -1760, 18858, -1760,    30, -1760, -1760,
   -1760,  1361,  4837,  1012,  1360,   893, 18501,   933, -1760, -1760,
   -1760, -1760, -1760, -1760,   958, -1760,  1369,  1347, -1760, 16389,
   -1760,  4214, 19116, 19116, -1760, 16389, -1760, 22043, -1760, -1760,
   -1760, -1760, -1760, -1760, 16389, -1760, -1760, 18243, 19116, 19116,
    1158,  1127,  1232,   636,  1377, -1760,   963,  1371,  1160,  1373,
   -1760, 20341, 22043, 20415,  1368, 22043,   930, 22043,   930, -1760,
    2333, -1760, -1760, 20489,  2228, 22043, 20489,   930, -1760, -1760,
   22043, 22043, 22043, 22043, 22043, 22043, 22043, 22043, 22043, 22043,
   22043, 22043, 22043, 22043, 22043, 22043, 22043, 22043, 22043, 20563,
    1352,   789,  3590, 11845, -1760, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760,  1370, 22043, -1760, -1760,   451,
    1524, -1760, -1760,   639,   639, -1760, -1760, 16545, -1760,   352,
    1165, -1760,   968,  1165, -1760, -1760, -1760,  1282, -1760, -1760,
    1282, 22413, -1760, -1760, 11845,  1374,  1375,  4440,  1516,  3328,
     353,  1340, -1760,   639,   639,  1340,   367, -1760,   639,   639,
   22043,  4837,  1173,  1180,  1340,    48, 14747, 14747,  4837, -1760,
   -1760, 22043,  1210, -1760,  5441,  1384, -1760,  2557, -1760, -1760,
   -1760, -1760, -1760,   972, -1760, 14747,   930,  4556,   930,   975,
    1382,  1389,  1390,   976,  1393,  1394,  1398,   384,  1165, -1760,
   -1760,   401,  1165, -1760, -1760, -1760,  4556,   789, -1760,  1165,
   19724, -1760,   601, 17743, -1760, -1760,   979,  1400,   980,  1401,
   -1760,  1391, -1760,   601, -1760, -1760,   601,  1005,  1391, -1760,
     601,  1396,  1397,  1399, -1760, -1760, 18190, -1760,  1404, -1760,
   -1760, -1760,   930,  4837, 10989,  1491,  1385, 19277, -1760,  1198,
   -1760, 14747, -1760,   994, -1760, -1760,  1391, -1760, 18501, 16545,
    1395,  4837, -1760,  1395, -1760, -1760, -1760,   229,   639,   639,
   -1760, 18858, -1760, 12010, 16857, -1760, 17743,  1416,  1418,  1420,
   -1760,  9998,   639, -1760,  1170, -1760, -1760, -1760, -1760,  1282,
   -1760, -1760, -1760,   757, -1760,  3478, -1760, -1760,   524,   344,
    1425,  1402,  1421,   229, -1760, -1760,  1423,  1426,  1620, 20489,
   -1760,  1431,  1428,   380,  1430,  1438,  1441,  1439,  1445, 22043,
    1449,  1450,  1451, 11845, 22043, -1760, -1760,  1458, -1760, -1760,
   -1760, 22043, -1760,  1452,  1454, 20193,  1182, -1760, 20489,  1453,
   -1760,  1455, -1760, -1760,   962, -1760, -1760,  1003, -1760, -1760,
   -1760, -1760,   962, -1760, -1760,  1186,   660, -1760, -1760,  1090,
    1090,  1090,   613,   613,  1103,  1103,  1167,  1167,  1167,  1167,
     545,   545,  1177,  1109,  1128,  1125,  1179, 22043,  1169, -1760,
    1459,   962, -1760, -1760,  5441, -1760, 17743,  1460,  1462,  1463,
    1524, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,  1282,
   -1760, -1760,  1282, 17743, 17743, -1760, -1760,  4440,   934,  1464,
    1468,  1469,  1471,  2247,  3328, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,  1472,
   -1760,  1340, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,
    1474,  1478, -1760,   336,   962,  1188,    58, -1760, -1760,  1485,
   -1760,  8267, -1760, 22043,   639, 20637, 14747, -1760, -1760, -1760,
    1461,   409,  1165, -1760,   443,  1165, -1760, -1760, -1760, -1760,
    1282, -1760, -1760, -1760,  1282,   922,  1487,  1282,   396, -1760,
    1150,  1484, -1760, -1760, -1760, -1760, -1760, -1760,  1492, -1760,
   -1760,  1391, -1760,   601, -1760, -1760, -1760, -1760, -1760, 13637,
    1490,  1488, -1760,   432, -1760,   470,   240, 11680,  1493, 15912,
    1497,  1498,  1684,  2393,  3017, 20711,  1499, -1760, -1760,  1502,
    1503, -1760, -1760,   601, 22043, 22043,  1644,  1500,   719, -1760,
    1587,  1504,  1506, -1760, -1760, -1760, 10814, -1760, -1760, -1760,
   -1760, -1760,  2310, -1760, -1760, -1760,  1576, -1760, -1760, -1760,
     930, -1760, -1760, 13482, 17430,  1525, -1760,  4837, -1760,  1509,
    1532,  1533, -1760,  1200, -1760, -1760, -1760, -1760,  4556, -1760,
   -1760,  1514,  1521,  1004, 18501,   696,  1539,   696,  1361,  1012,
    1012, -1760, -1760,  1079,  1198, 16701, -1760,  1150, -1760, 12175,
   -1760,   479,  1165, -1760,   757, 10172, -1760, -1760,   229,   639,
     639,    30,  4837, -1760, 20785, -1760,   229,  1361,  1542, -1760,
   -1760,  1015,   736, 18190, 11845,   930, -1760,   736, 18296,   736,
   -1760, 22043, 22043, 22043, -1760, -1760, -1760, -1760, 22043, 22043,
    1541,  5441, -1760, -1760,  1544,   752, -1760, -1760, -1760,  3357,
   -1760, -1760,  1202, -1760,   207, -1760, 20489,  1209, -1760, 20341,
   -1760, -1760, 22043,  1529,  1215,  1243,  1210, -1760,   487,  1165,
   -1760, -1760, 17743, 17743, -1760, -1760,  1553,   490,  1165, -1760,
     512,  1560,   639,   639, -1760, -1760, 17743, 17743, -1760,  1554,
   -1760, 15552, 15552,  1563,  1556,  1561,  1564, -1760,  1562, 22043,
   22043,  1257,  1565, -1760, -1760, -1760, -1760, -1760, -1760, -1760,
    1567, 22043, -1760, -1760, -1760,  1282, -1760, -1760, -1760,  1282,
   17743, 17743,   336,   639, -1760, -1760,  1262, 22043, 19873,  1568,
    1572,  1585, -1760, -1760,  1586, 13792, 13947, 14102, 18501, 14800,
   19116, 19116,  1588, -1760,  1569,  1571,  2216,  7602, -1760,   444,
    4837, -1760, -1760,  4837, -1760, 20267,   284,   492, -1760, -1760,
   -1760, -1760, 22043,  1590,  1658, 11514, 11164, -1760,  1574, -1760,
    1577, 22043,  1578,  5441,  1579, 22043, 20341, 22043,   948, -1760,
    1580,   -11, -1760,   338,  1593, -1760, -1760,  1598, -1760,  1582,
   -1760,  1583,  1601, 15912,   654, 14586,   639,   446, -1760, -1760,
   -1760,  1609, -1760,  1614, -1760,  1615, -1760,  1581, -1760,  1611,
   -1760, -1760, -1760,  1616, -1760,  1619,  1613,  1621, 12340,  1618,
    1625,  1626, -1760,  1617, -1760, -1760, -1760,  1282, 22043, 22043,
    1198,  1630, -1760,  1361, -1760,  1012,   292,  1402,  5441, -1760,
    1361,  1638, -1760, 18501, -1760,   781,  1636,  1646,  1024, -1760,
    1647, -1760, -1760, -1760, -1760, -1760,  5441,  1210, 20341, -1760,
    1686,   962, -1760,  1686,  1686, -1760,   962,  3677,  3977, -1760,
   -1760,  1268, -1760, -1760, -1760,  1662,  1660, -1760, -1760, -1760,
    1282, -1760, -1760,  1663,  1664,   639, -1760, -1760, -1760,  1282,
   -1760, -1760, -1760,  1667, -1760, -1760, -1760, -1760, -1760, -1760,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760,  1665, -1760, -1760,
   -1760, -1760,  1666,  1670,   639, -1760, 17743, 17743, -1760, -1760,
   -1760, -1760, 22043, -1760,   396, -1760,  1150, -1760, -1760, -1760,
    1675, -1760,  1588,  1588,  1588,  4339,   804,  1650,   478, -1760,
    4339,   483, 16545, -1760, -1760, -1760,  4109, 22043,  3996,   505,
   -1760, -1760,    46,  1669,  1669,  4837, -1760, -1760, 17895, -1760,
   22043,  1672,  1677, -1760, -1760, -1760, -1760,  1041,  1680, 15912,
    1504,  1679, 22043,   378,  1676,   421, 14264, 18501, -1760, -1760,
   -1760,   818, 15912, 22043,   577,   800, -1760, 22043, 20039, -1760,
   -1760,   550, -1760,  1210, -1760,  1046,  1054,  1057, -1760, -1760,
   -1760, -1760,   601,   948,  1687, -1760, -1760, 22043, -1760,  1690,
     789, 11680, -1760, -1760, -1760, -1760, 22043,  1728, -1760, 10629,
   -1760,   639, 15552, -1760, -1760, 18501, -1760, -1760, -1760, 18501,
   -1760,   229,   229, -1760, -1760, -1760,  1688, -1760, 17743, -1760,
   -1760,  1691, -1760,  1692,  1693,  1012,  1695, -1760, -1760,  1210,
    1696, -1760, -1760,  1698, -1760, -1760, 22043, -1760, 18296, 22043,
    1210,  1702,  1270, -1760,  1274, -1760,   962, -1760,   962, -1760,
   -1760, -1760, -1760, 17743,  1703,  1705, -1760, -1760, 17743, 17743,
    1710,  1711,  1286, 15069, 15230, -1760,  1699, -1760, -1760, -1760,
   -1760,  1717,  1720,  1290, 22043, -1760, -1760, -1760, -1760,   552,
     804,  2151,   585, -1760, -1760, -1760, -1760,   639,   639, -1760,
   -1760, -1760,   608, -1760,  1062,  4109,   693, -1760,  3996,   639,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,   619, 15912,
     469, 20859,  1800, 15912,  1504, 15713, -1760, -1760, -1760, -1760,
   22043, -1760, 20933,  1802,  1704, 20116, 21007, 15912, 11339,  1504,
     544,   751,  1706, 22043, -1760,  1729,   498, 15912, -1760, -1760,
    1727, -1760, -1760,  1707,   789,   803,  1731,  1735,  1293,  1801,
   -1760, -1760, -1760, -1760,  4837,  4556, -1760,  1361,  1361, -1760,
   -1760,  1733,  1737, -1760, -1760, -1760,  1736,   229,  1744, -1760,
    1746, -1760, -1760, -1760, -1760,  1747, -1760, -1760, -1760,  1296,
    1303, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760, -1760,
   -1760,  1745, -1760, -1760,  1748,  1750, -1760, -1760, -1760, -1760,
   -1760,  1754,  1756,  1758,  2151, -1760,   639, -1760, -1760, -1760,
   -1760, -1760,  1757,  4339, -1760, -1760,  4738,   132, 12508, -1760,
   15807, -1760,    95,  1064, 15912,  1826,   621,  1752,    65, 15912,
   22043,  1760,   544,   751,  1739, 22487,  1753,   227,  1848, -1760,
   21081, 21155, 22043,  1504,  1749, 12672, -1760, -1760, -1760, 19330,
   -1760,  1766,  1751,   286, 15912, -1760, 22043, 20489,   435, -1760,
   -1760, -1760,  1778,  1779,  1780,  1781, -1760, -1760,   229,  1361,
   -1760, -1760, -1760, -1760, -1760,  1783,  1784,  1785, 15552,  1776,
   -1760, -1760,   525,  1165, -1760, -1760,   804, -1760, -1760,   325,
   -1760,   277, -1760, -1760, -1760,  1790, 13158, -1760, -1760, 15912,
   -1760,   103, -1760, 15912, 22043,  1787, 21229, -1760, -1760, 21303,
   21377, 22043,  1760,  1504, 21451, 21525, 15912,  1774,   390,  1777,
     504, -1760, -1760,  1793, 13158, 19330, -1760,  4406, 19063,   930,
    1791, -1760,  1847,  1803,   816,  1796, -1760,  1884, -1760,  1073,
   15912,  1809, 15912, 15912, -1760, -1760, -1760, -1760,  1361,  1811,
   -1760, -1760, -1760, -1760, -1760, -1760, -1760,  1282, -1760, 22043,
   -1760, 22043, -1760, -1760,  1388, 13320, -1760, -1760, 15912, -1760,
   -1760,  1504, -1760, -1760,  1504,  1795,   578,  1798,   641, -1760,
   -1760,  1504, -1760,  1504, -1760,  1814, 21599, 21673, 21747, -1760,
    1388, -1760,  1794,  2682,  3512, -1760, -1760, -1760,   286,  1807,
   22043,  1813,   286,   286, 15912, -1760, -1760, 22043,  1858,  1862,
    1829, -1760, 17743, -1760, -1760, 15807, -1760,  1388, -1760, -1760,
    1831, 21821, 21895, 21969, -1760, -1760,  1504, -1760,  1504, -1760,
    1504, -1760,  1794, 22043,  1834,  3512,  1835,   789,  1839, -1760,
     835, -1760, -1760,  1078,  1801,   424, -1760, -1760, -1760, 10380,
    1844, 15807, -1760, -1760,  1504, -1760,  1504, -1760,  1504,  1849,
    1845, -1760,   601,   789,  1846, -1760,  1825,   789, -1760, -1760,
   15912,  1928,  1852, -1760, -1760, -1760, 10507, -1760,   601, -1760,
   -1760,  1310, 22043, -1760,  1085, -1760, 15912, -1760, -1760,   789,
     930,  1853,  1833, -1760, -1760, -1760,  1097, -1760, -1760,  1837,
     930, -1760, -1760
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   461,     0,     2,   461,   478,   479,   480,   481,   482,
     483,   484,   485,   486,   467,   469,   468,   470,     0,     0,
       0,   487,   489,   510,   490,   511,   493,   494,   508,   509,
     488,   506,   507,   491,   492,   495,   496,   497,   498,   499,
     500,   501,   502,   503,   504,   505,   512,   513,   816,   515,
     588,   589,   592,   594,   590,   596,     0,     0,     0,   461,
       0,     0,    16,   559,   565,     9,    10,    11,    12,    13,
      14,    15,   774,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   832,   461,   775,   406,     0,   409,   698,   411,
     420,     0,   410,   440,   441,     0,     0,     0,     0,   542,
     463,   465,   471,   461,   473,   476,   527,   514,   445,   520,
     525,   447,   537,   446,   552,   556,   562,   541,   568,   580,
     816,   585,   586,   569,   639,   412,   413,     3,   782,   795,
     466,     0,     0,   816,   854,   816,     2,   871,   872,   873,
     461,     0,  1041,  1042,     0,     1,   461,     0,    16,     0,
     461,   429,   430,     0,   542,   455,   456,   457,   785,     0,
     591,   593,   595,   597,     0,   461,     0,   817,   818,   587,
     516,   691,   692,   690,   751,   746,   736,     0,     0,   783,
       0,     0,   478,   776,   780,   781,   777,   778,   779,   461,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   560,
     563,   461,   461,     0,  1043,   542,   861,   879,  1047,  1040,
    1038,  1045,   405,     0,   164,   704,   163,     0,   414,     0,
       0,     0,     0,     0,     0,     0,   404,   931,   932,     0,
       0,   439,   814,   816,   810,   835,   816,   816,   812,     2,
     816,   811,   892,   816,   816,   889,     0,   535,   536,     0,
       0,   461,   461,     2,   461,   421,   464,   474,   528,     0,
     557,     0,   798,     2,     0,   698,   422,   542,   521,   538,
     553,     0,   798,     2,     0,   477,   522,   529,   530,   448,
     539,   450,   451,   449,   544,   554,   558,     0,   572,     0,
     768,     2,     2,   796,   853,   855,   461,     0,     2,     2,
    1051,   542,  1054,   814,   814,     3,     0,     0,   542,     0,
       0,   432,   816,   812,   811,     2,   461,     0,   772,     0,
     732,   734,   733,   735,     0,     0,   728,     0,   718,     0,
     727,   738,     0,   816,   816,     2,   461,  1062,   462,   461,
     473,   452,   520,   453,   545,   454,   552,   549,   570,   816,
     571,     0,   679,   461,   680,  1016,  1017,   461,   681,   683,
     559,   565,   640,   642,   643,   640,   819,     0,   749,   737,
       0,   823,    21,     0,    20,     0,     0,     0,     0,     0,
       0,     0,    23,    25,     4,     8,     5,     6,     7,     0,
       0,   461,     2,     0,   103,   104,   105,   106,    87,    24,
      88,    42,    86,   107,     0,     0,   122,   124,   128,   131,
     134,   139,   142,   144,   146,   148,   150,   152,   155,     0,
      26,     0,   566,     2,   107,   461,   156,   743,   694,   556,
     696,   742,     0,   693,   697,     0,     0,     0,     0,     0,
       0,     0,   833,   859,   816,   869,   877,   881,   887,     2,
    1049,   461,  1052,     2,   100,   461,     3,   678,     0,  1062,
       0,   462,   520,   545,   552,     3,     3,   660,   664,   674,
     680,   681,     2,   862,   880,  1039,     2,     2,    23,     0,
       2,   704,    24,     0,   702,   705,  1060,     0,     0,   711,
     700,   699,     0,     0,   800,     2,     2,     2,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   838,
     895,   816,     0,   698,     2,   834,   842,   959,   836,   837,
       0,   798,     2,   891,   899,     0,   893,   894,     0,   435,
     461,   461,   526,   462,     0,   542,   461,  1044,  1048,  1046,
     543,   772,     0,   798,   814,   415,   423,   475,     0,   798,
       2,   772,     0,   798,   747,   523,   524,   540,   555,   561,
     564,   559,   565,   583,   584,     0,   748,   461,   688,     0,
     201,   398,   461,     3,     0,   542,   461,   797,   461,     0,
     417,     2,   418,   769,     0,   437,     0,     0,     0,     2,
     461,   814,   461,   772,     0,     2,     0,   731,   730,   729,
     724,   472,     0,   722,   739,   518,     0,     0,   461,   461,
    1018,   462,   458,   459,   460,  1022,  1013,  1014,  1020,     2,
       2,   101,     0,   978,   992,  1062,   974,   816,   816,   983,
     990,   686,   461,   550,   682,   462,   546,   547,   551,     0,
     816,  1028,   462,  1033,  1025,   461,  1030,     0,   649,   641,
     648,  1060,     0,   640,     0,     0,   461,     0,   831,   830,
     826,   828,   829,   827,     0,   821,   824,     0,    22,   461,
      94,     0,   461,   461,    89,   461,    96,     0,    32,    36,
      37,    33,    34,    35,   461,    92,    93,   461,   461,   461,
       2,   103,   104,     0,     0,   182,     0,     0,   586,     0,
    1038,     0,     0,     0,     0,     0,     0,     0,     0,    55,
       0,    61,    62,    66,     0,     0,    66,     0,    90,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   461,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   163,     0,   161,   162,     2,
     943,   695,   940,   816,   816,   948,   567,   461,   860,   816,
     870,   878,   882,   888,     2,   863,   865,   867,     2,   883,
     885,     0,  1050,  1053,   461,     0,     0,     2,   101,   978,
     816,  1062,   913,   816,   816,  1062,   816,   928,   816,   816,
       3,   682,     0,     0,  1062,  1062,   461,   461,     0,     2,
     713,     0,  1060,   710,  1061,     0,   706,     0,     2,   709,
     712,   179,   178,     0,     2,   461,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   816,   847,   851,
     890,   816,   904,   909,   839,   896,     0,     0,   443,   955,
       0,   801,     0,   461,   802,   436,     0,     0,     0,     0,
     434,     2,   803,     0,   419,   772,     0,   798,     2,   804,
       0,     0,     0,     0,   598,   667,   462,     3,     3,   671,
     670,   874,     0,     0,   461,   399,     0,   542,     3,   100,
       3,   461,   472,     0,     3,   773,     2,   726,   461,   461,
     720,     0,   719,   720,   519,   517,   642,   640,   816,   816,
    1024,   461,  1029,   462,   461,  1015,   461,     0,     0,     0,
     993,     0,   816,  1063,   979,   980,   687,   976,   977,   991,
    1019,  1023,  1021,   548,   583,     0,  1027,  1032,   645,   640,
       0,   650,     0,   640,   752,   750,     0,     0,   823,    66,
     784,     0,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   461,     0,   121,   120,     0,   117,   116,
      27,     0,    28,     0,     0,     0,     0,     3,    66,     0,
      51,     0,    52,    59,     0,    58,    70,     0,    67,    68,
      71,    54,     0,    53,    57,     0,     0,    50,   123,   125,
     126,   127,   129,   130,   132,   133,   137,   138,   135,   136,
     140,   141,   143,   145,   147,   149,   151,     0,     0,   408,
       0,     0,    29,     3,   704,   157,   461,     0,     0,     0,
     944,   945,   941,   942,   745,   744,     2,   864,   866,   868,
       2,   884,   886,   461,   461,   969,   968,     2,     0,     0,
       0,     0,     0,   816,   979,   916,   933,     2,   911,   919,
     684,   914,   915,   685,     2,   926,   936,   929,   930,     0,
       3,  1062,   427,     2,  1055,     2,   675,   676,   654,     3,
       3,     3,     3,   698,     0,   155,     0,     3,     3,     0,
     707,     0,   701,     0,   816,     0,   461,     3,   431,   433,
       0,   816,   848,   852,   816,   905,   910,     2,   840,   843,
     845,     2,   897,   900,   902,   814,     0,   956,     3,   960,
     961,     3,   806,     3,   532,   531,   534,   533,     2,   773,
     807,     2,   805,     0,   773,   808,   598,   598,   598,   461,
       0,     0,   689,     0,   402,     0,     0,   461,     0,     2,
       0,     0,     0,     0,     0,   184,     0,   332,   333,     0,
       0,   371,   370,     0,   159,   159,   377,   559,   565,   198,
       0,   185,     0,   209,   186,   187,   461,   203,   188,   189,
     190,   191,     0,   192,   193,   338,     0,   194,   195,   196,
       0,   197,   205,   542,   461,     0,   207,     0,   396,     0,
       0,     0,     3,     0,   786,   773,   761,   762,     0,     3,
     757,     3,     3,     0,   461,   736,     0,   736,  1060,   640,
     640,  1026,  1031,     2,   100,   461,     3,   557,     3,   462,
       3,   816,   986,   989,   461,     3,   975,   981,   640,   816,
     816,     0,     0,   628,     0,   644,   640,  1060,     2,   820,
     822,     0,    95,   461,   461,     0,    99,    97,   461,     0,
     111,     0,     0,     0,   115,   119,   118,   183,     0,     0,
       0,   704,   108,   176,     0,     0,    45,    46,    84,     0,
      84,    84,     0,    72,    74,    48,     0,     0,    44,     0,
      47,   154,     0,     0,     0,     0,  1060,     3,   816,   951,
     954,   946,   461,   461,     3,     3,     0,   816,   922,   925,
     816,     0,   816,   816,   917,   934,   461,   461,  1056,     0,
     677,   461,   461,     0,     0,     0,     0,   416,     3,     0,
       0,     0,     0,   703,   708,     3,   799,   181,   180,     3,
       0,     0,     2,   841,   844,   846,     2,   898,   901,   903,
     461,   461,   698,   816,   967,   966,     0,     0,     0,     0,
       0,     0,   773,   809,     0,   461,   461,   461,   461,   461,
     461,   461,   581,   610,     3,     3,   611,   542,   599,     0,
       0,   856,     2,     0,   400,    66,     0,     0,   323,   324,
     206,   208,     0,     0,     0,   461,   461,   319,     0,   317,
       0,     0,     0,   704,     0,     0,     0,     0,     0,   160,
       0,     0,   378,     0,     0,     3,   213,     0,   204,     0,
     314,     0,     0,     2,     0,   542,   816,     0,   397,   971,
     970,     0,     2,     0,   764,     2,   759,     0,   760,     0,
     740,   721,   725,     2,   723,     0,     0,     0,   461,     0,
       0,     0,     3,     0,     2,   982,   984,   985,     0,     0,
     100,     0,     3,  1060,   634,   640,   650,   650,   704,   651,
    1060,     0,   753,   461,   825,   972,     0,     0,     0,    38,
       0,   112,   114,   113,   110,   109,   704,  1060,     0,    65,
      81,     0,    75,    82,    83,    60,     0,     0,     0,    69,
      56,     0,   153,   407,    30,     0,     0,     2,   947,   949,
     950,     3,     3,     0,     0,   816,     2,   918,   920,   921,
       2,   935,   937,     0,   912,   927,     3,     3,  1057,     3,
     662,   661,   665,  1059,     2,     2,  1058,     0,     3,   813,
     714,   715,     0,     0,   816,   438,   461,   461,     3,     3,
     444,   815,     0,   962,     0,   963,   964,   958,   906,   790,
       0,   792,   581,   581,   581,   611,   617,   586,     0,   623,
     611,     0,   461,   573,   609,   605,     0,     0,     0,     0,
     612,   614,   816,   625,   625,     0,   606,   621,   461,   403,
       0,     0,    67,   327,   328,   325,   326,     0,     0,     2,
     224,     0,     0,   226,   411,   225,   542,   461,   305,   304,
     306,     0,     2,   184,   264,     0,   257,     0,   184,   320,
     318,     0,   312,  1060,   321,     0,     0,     0,   359,   360,
     361,   362,     0,   352,     0,   353,   329,     0,   330,     0,
       0,   461,   215,   202,   316,   315,     0,   350,   369,     0,
     401,   816,   461,   788,   741,   461,     2,     2,   754,   461,
     633,   640,   640,  1034,  1035,  1036,     0,   987,   461,     3,
       3,     0,   995,     0,     0,   640,     0,   647,   646,  1060,
       0,   631,     3,     0,   973,    98,     0,    31,   461,     0,
    1060,     0,     0,    85,     0,    73,     0,    79,     0,    77,
      43,   158,   952,   461,     0,     0,   857,   875,   461,   461,
       0,     0,     0,   461,   461,   717,     0,   424,   426,     3,
       3,     0,     0,     0,     0,   794,   577,   579,   575,     0,
       0,  1002,     0,   618,  1007,   620,   999,   816,   816,   604,
     624,   608,     0,   607,     0,     0,     0,   627,     0,   816,
     600,   615,   626,   616,   622,   669,   673,   672,     0,     2,
       0,     0,   245,     2,   227,   542,   310,   308,   311,   307,
       0,   309,     0,   253,     0,   184,     0,     2,   461,   265,
       0,   290,     0,     0,   313,     0,     0,     2,   336,   363,
       0,   354,     2,     0,     0,     0,     0,   341,     0,   337,
     200,   199,   425,   758,     0,     0,     3,  1060,  1060,  1037,
       3,     0,     0,   994,   996,   632,     0,   640,     0,   630,
       2,    49,    41,    39,    40,     0,    63,   177,    76,     0,
       0,     3,   858,   876,     3,     3,   923,   938,   428,     2,
     659,     3,   658,   716,     0,     0,   849,   907,   957,   965,
     602,     0,     0,     0,  1003,  1004,   816,   603,  1000,  1001,
     601,   582,     0,     0,   214,   335,     0,     0,     0,   238,
       2,   216,     0,     0,     2,   247,   262,   273,   267,     2,
     184,   302,     0,   277,     0,     0,   268,   266,   255,   258,
       0,     0,   184,   291,     0,     0,   219,   334,     2,   461,
     331,     0,     0,   379,     2,   339,     0,    66,     0,   351,
     763,   765,     2,     0,     0,     0,   997,   998,   640,  1060,
     652,   755,    64,    80,    78,     0,     0,     0,   461,     0,
     850,   908,   816,  1010,  1012,  1005,     0,   613,   233,   228,
     231,     0,   230,   237,   236,     0,   461,   240,   239,     2,
     249,     0,   246,     2,     0,     0,     0,   254,   259,     0,
       0,   184,   303,   278,     0,     0,     2,     0,   293,   294,
     292,   261,   322,     0,   461,   461,     3,   364,   462,   368,
       0,   372,     0,     0,     0,   380,   381,   222,   342,     0,
       2,     0,     2,     2,   756,   636,   638,   988,  1060,     0,
     953,   924,   939,   663,     2,  1006,  1008,  1009,   619,     0,
     235,     0,   234,   218,   241,   461,   392,   250,     2,   251,
     248,   263,   276,   274,   270,   282,   280,   281,   279,   260,
     275,   271,   272,   269,   256,     0,     0,     0,     0,   221,
     241,     3,   357,     0,  1002,   365,   366,   367,   379,     0,
       0,     0,   379,     0,     2,   340,   347,     0,   344,   346,
       0,   637,   461,   229,   232,     2,     3,   242,   393,   252,
       0,     0,     0,     0,   301,   299,   296,   300,   297,   298,
     295,     3,   357,     0,     0,  1003,     0,     0,     0,   373,
       0,   382,   223,     0,   337,     0,   635,     3,   210,     0,
       0,     2,   289,   287,   284,   288,   285,   286,   283,     0,
       0,   358,     0,   385,     0,   383,     0,   385,   343,   345,
       2,     0,     0,   212,   211,   217,     0,   220,     0,   355,
     386,     0,     0,   374,     0,   348,     2,  1011,   356,     0,
       0,     0,     0,   349,   387,   388,     0,   384,   375,     0,
       0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1760,  6707,  5853, -1760,    -1,   397,  1789,  -160, -1760,  -340,
   -1760,   323, -1760,  -701,   633,   726,  -956, -1014, -1760,   198,
    7064,  1942, -1760,   547, -1760,  1326,   379,   758,   760,   546,
     759,  1292,  1295,  1284,  1287,  1294, -1760,    20,  -174,  8962,
     862, -1760,  1604, -1760, -1760,  -678,  8005, -1127,  1116, -1760,
    -115, -1760,   857,   -64, -1760, -1760, -1760,   398,    29, -1760,
   -1690, -1588,   258,     7, -1760, -1760, -1760,   271, -1539, -1760,
   -1382, -1760, -1760, -1760, -1760,   -45, -1759,   143, -1760, -1760,
     -40, -1760, -1760, -1760,   -24,   426,   427,    87, -1760, -1760,
   -1760, -1760,  -790, -1760,    12,   -51, -1760,    94, -1760,  -127,
   -1760, -1760, -1760,   874,  -866, -1054, -1303, -1760,    50, -1276,
     195,  2463,  -977,  -953, -1760,  -272, -1760,    18, -1760,  -144,
      41,  -252,  -229,  3741,   828,  -633, -1760,   270,   205,  1527,
    1924, -1760,  2020, -1760,    62,  4136, -1760,  1961, -1760,   171,
   -1760, -1760,  1324,   283,  4676,  3039,   -38,  1816,  -268, -1760,
   -1760, -1760, -1760, -1760,  -280,  5222,  5038, -1760,  -389,   150,
   -1760,  -569,   210, -1760,   140,   709, -1760,   495,   -90, -1760,
   -1760, -1760,  -361,  5911,  -893,  1146,    33,  -676,  -656,  -257,
    1238, -1760, -1250,  -154,   -87,  1339,   896,  5371,   -95,  -499,
    -253,  -187,  -454,  1273, -1760,  1602,   -84,  1191,  1507, -1760,
   -1760, -1760, -1760,  -323,  -150,   108,  -889, -1760,   358, -1760,
   -1760, -1298,   434, -1760, -1760, -1760,  2097,  -752,  -463,  -955,
     -20, -1760, -1760, -1760, -1760, -1760, -1760,   146,  -808,  -152,
   -1732,  -207,  8517,   -58,  7099, -1760,  1151, -1760,  2165,    37,
    -200,  -192,  -180,     8,   -69,   -68,   -66,   725,    40,    53,
      68,  -177,   -63,  -136,  -126,  -121,  -714,  -698,  -688,  -632,
    -708,  -132,  -628, -1760, -1760,  -716,  1342,  1343,  1345,  2191,
   -1760,   551,  7677, -1760,  -580,  -571,  -553,  -550,  -728, -1760,
   -1596, -1683, -1679, -1671,  -597,  -119,  -293, -1760, -1760,   -70,
     109,   -54, -1760,  8433,   722,  -594,  -584
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1170,   223,   398,   399,    81,    82,   400,   374,   401,
    1478,  1479,   402,   987,   988,   989,  1282,  1283,  1284,  1490,
     424,   404,   405,   406,   693,   694,   407,   408,   409,   410,
     411,   412,   413,   414,   415,   416,   417,   426,  1086,   695,
    1410,   756,   217,   758,   420,   823,  1171,  1172,  1173,  1174,
    1175,  1176,  1177,  2099,  1178,  1179,  1415,  1601,  1940,  1941,
    1869,  1870,  1871,  2066,  2067,  1180,  1615,  1616,  1617,  1771,
    1772,  1181,  1182,  1183,  1184,  1185,  1186,  1423,  1798,  1993,
    1909,  1187,  1188,  1633,  2084,  1634,  1635,  1976,  1189,  1190,
    1191,  1413,  1984,  1985,  1986,  2131,  2146,  2014,  2015,   297,
     298,   884,   885,  1143,    84,    85,    86,    87,    88,    89,
     457,    91,    92,    93,    94,    95,   231,   574,   279,   459,
     428,   460,    98,   308,   100,   101,   102,   339,   340,   105,
     106,   169,   107,   905,   341,   155,   110,   251,   111,   156,
     259,   343,   344,   345,   157,   421,   116,   117,   347,   118,
     565,   873,   871,   872,  1573,   348,   349,   121,   122,  1139,
    1378,  1579,  1580,  1732,  1733,  1379,  1568,  1751,  1581,   123,
     653,  1675,   650,   350,   651,   652,  1245,  1079,   465,   466,
     877,   878,   467,   468,   879,   352,   569,  1195,   430,   431,
     218,   485,   486,   487,   488,   489,   327,  1215,   328,   903,
     900,   600,   329,   368,   330,   331,   432,   125,   175,   176,
     126,  1209,  1210,  1211,  1212,     2,  1128,  1129,   592,  1204,
     127,   318,   319,   261,   271,   548,   128,   221,   129,   232,
    1088,   864,   515,   167,   130,   664,   665,   666,   131,   234,
     235,   236,   237,   313,   133,   134,   135,   136,   137,   138,
     139,   240,   314,   242,   243,   244,   791,   792,   793,   794,
     795,   245,   797,   798,   799,   761,   762,   763,   764,   516,
    1121,  1357,   140,  1683,   625,   626,   627,   628,   629,   630,
    1735,  1736,  1737,  1738,   615,   470,   355,   356,   357,   433,
     209,   142,   143,   144,   359,   815,   631
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   311,   698,    80,   654,   193,   194,   419,   195,   132,
    1213,   351,   545,   208,  1218,   996,   191,   293,  1192,    96,
     373,   337,   532,   976,   573,   513,   369,   812,  1404,   501,
     491,   857,   859,   241,   365,   931,  1287,   502,   200,   179,
    1908,   926,    97,   925,  1031,   153,   354,  1851,   917,   503,
    1247,  1852,   504,   704,   151,    80,    80,   940,    80,  1853,
     643,   969,  1059,   108,   646,  1294,   918,   132,  1066,   919,
     633,  1530,  1531,    80,  -766,  1055,  1196,    96,   861,  1783,
     208,  1056,    80,   145,   610,  1373,   302,  1637,   868,  1049,
      80,   206,  1603,   505,   529,    80,   580,   582,    80,  1050,
      97,   641,    80,   506,   238,   644,   494,   262,   507,   501,
     141,   272,   521,   141,   196,   469,  1134,   502,    58,  1604,
    1604,   108,   436,   437,   204,   438,   305,   197,  1328,   503,
     896,  1082,   504,  1943,  1942,  1855,   543,    63,    64,  -767,
      80,   512,   198,    80,  1205,    80,   553,   452,   159,  1097,
    1080,  1080,    80,   265,   132,  1051,   219,   945,    80,  1052,
     193,   194,  1374,   195,    96,    80,  1638,   510,   141,  1080,
    1949,   499,   112,   505,  1361,  1682,    58,    58,  2018,  1364,
    1948,   300,   577,   506,   538,    76,  1375,    97,   507,    80,
      80,   204,  1329,   633,   581,   291,    90,   442,   206,   152,
     581,   418,   514,   610,    80,    58,   104,  1060,   108,   925,
     738,  1063,   141,   473,   482,  1202,   917,   616,  1089,    80,
    1076,  1077,   584,   971,   622,   220,  1330,  -798,    80,    80,
     112,   439,  1776,  1956,   918,  1080,  1885,   919,   206,   647,
     193,   194,   450,   195,   440,    80,   538,   510,  1251,   560,
    1433,   586,   739,    80,    90,   141,   165,   291,  1935,   441,
     848,   514,   206,    80,   104,  1492,    80,   509,  1035,   196,
     158,   103,  1950,    80,   269,   549,   697,  1275,  2010,  1942,
    2019,  1390,   197,   655,   113,   522,   657,    80,    80,   514,
      80,  1944,   942,   534,   255,   537,   699,   198,   266,   830,
     816,  1386,  1387,   164,   257,    80,   888,   831,    80,    80,
    1192,  1373,  1373,  1373,  1301,   912,    80,   112,   206,   832,
    1059,   219,   833,    80,    80,   994,    20,   796,    80,   103,
    1497,   952,  -394,   894,  1266,  1908,   616,    97,  1603,  1391,
    1314,    90,   113,   633,  1237,  1463,  1315,   509,    58,  1049,
    -395,   104,   937,  1470,  1241,   208,   910,   537,   108,  1050,
    1851,  1806,  1498,   834,  1852,  1604,    80,   633,  1196,    80,
     663,    58,  1853,   835,   633,  1948,   855,   178,   836,  1362,
     930,  2046,   860,   180,   520,   971,   830,   525,  1374,  1374,
    1374,   648,  -798,   936,   831,  1965,   649,   783,  1890,  1891,
     292,   149,  1131,  -394,   372,   851,   832,  1560,   542,   833,
     854,  1948,  1375,  1375,  1375,  1051,   103,  1388,   552,  1306,
    1339,  -395,  1639,   867,    58,    58,  2065,   862,   589,   113,
     436,   437,   514,   438,    80,  2011,  2012,   869,   604,    58,
    1080,   200,  1982,   473,   292,   845,   337,   287,  1855,   469,
     834,   774,  2065,   173,   173,   514,    58,    80,    80,  2009,
     835,  1593,   546,  1530,  1531,   836,   204,   112,  1244,    80,
      80,   354,   547,    58,  1237,   909,   917,   604,    80,  2101,
     482,    58,  1695,  1697,  1699,   214,  1774,  1320,   173,  1935,
    1640,  1782,   782,  1330,   918, -1061,   215,   919,    80,  2120,
    1959,  1960,  1604,  1456,  1036,  1057,   648,    80,   514,   620,
    1990,   649,   216,   616,  1160,    58,   473,   436,   437,  1064,
     438,   181,   469,   620,   189,   845,  1422,    80,   160,  1602,
    1618,   161,   162,    80,   163,  1694,  1107,   201,   173,   439,
     514,   173,  1354,   257,  1991,   844,  1866,  1867,   971,  1081,
    1081,    58,   440,  1111,   697,   212,   173,   514,  2036,    58,
     697,  1342,    58,   363,  1355,   514,   576,   441,  1081,   697,
    1296,    80,   895,    80,   951,  1866,  1867,   954,   955,   113,
     956,  1020,  1522,    73,    58,    80,    -3,    80,   697,   958,
    1383,    80,   960,   961,   962,  1346,   473,    58,   226,   514,
     132,    80,  1585,   759,  1383,    80,    80,   514,   633,  1384,
      96,  1501,   601,   153,    78,    79,   602,   173,  1222,    97,
     597,  1586,  1385,  1650,  1445,   844,  1070,  1090,   931,  1868,
     246,  1454,   554,    97,  1081,   620,  1740,  -931,    80,  1507,
     108,  1585,  1516,   514,  -931,   633,   514,   566,  1884,   598,
     599,    80,   292,  1471,   108,  1741,   469,   796,  1895,  1221,
    1743,   173,   971,  1749,  1520,  -455,   257,   852,   620,  1595,
    1604,   173,  2038,   728,   729,  1099,   740,  2004,  2003,   287,
     741,   514,  1750,  1744,  1591,   563,   173,  1116,   568,   863,
    1766,  1767,  1768,  -629,  1115,   866,   289,   469,  1604,   870,
    -629,   141,  1505,   173,   560,    80,   894,    80,   971,    80,
    1749,    58,  1769,    80,   173,  1441,    80,   730,   731,   469,
     469,   173,   173,  1766,  1767,  1768,   173,  1784,  1626,  1850,
      14,    15,    16,    17,   147,   971,   971,  1122,   469,  1604,
    1829,    80,  1830,  1856,   189,  1769,  2071,   269,  1130,   112,
     291,  1132,  -578,  1958,  1775,  1135,    14,    15,    16,    17,
     147,   292,  1857,   112,   173,  1971,  1749,   173,  1807,  1808,
      14,    15,    16,    17,   147,  1602,   721,   971,   766,   971,
     257,  -456,   767,   722,   723,  1860,    80,    90,    80,    58,
     964,    14,    15,    16,    17,   147,  1864,   104,  1954,   971,
      80,   965,   966,  1240,   469,   211,  -394,    80,   207,  2073,
    1692,  1290,   882,   482,   292,    58,    80,  1487,  1286,   337,
     418,   239,    73,   306,   263,    80,    80,    80,   273,    58,
    1327,  1085,    73,   778,  2029,   247,   248,   514,   249,  1081,
     547,   307,   619,   250,   354,    80,   620,   442,   576,   514,
      58,  -459,   619,    78,   621,   173,   620,  -787,  1446,  1447,
     367,   113,   103,    78,    79,   189,   622,   173,   173,  1674,
     325,   189,   211,  -932,  -457,   113,  1680,  -460,  -692,   914,
    -932,    80,    80,   482,    14,    15,    16,    17,   147,  1270,
     186,   370,  1442,  1691,  1444,  1684,  1271,  1766,  1767,  1768,
      80,   705,    96,  1489,  1334,   371,   706,   372,  1352,   971,
    1286,   434,  1618,    73,   160,   207,   443,   161,   162,  1769,
     163,  1313,   796,   444,  1919,    97,   674,   256,  -185,   445,
     474,   817,   818,   619,    80,   819,    73,   620,   277,  1208,
     284,   446,   286,    58,    78,   621,   108,   663,  1206,  1623,
     447,   718,   719,   707,  1777,   207,  1730,  1902,   708,  1778,
     514,   633,  1903,   448,  1766,  1767,  1768,    78,    79,   149,
    2051,   472,   718,   930,   881,  2052,   539,   476,   882,   207,
     477,  1567,   256,    80,   284,   286,  1769,   199,    64,  2116,
     492,    80,   550,   141,  2117,  1770,  1729,   894,   495,   173,
     490,  1742,   718,   906,   908,  1458,   291,   141,   442,   469,
     514,   496,   892,  1133,  1679,   522,   602,   840,  1363,   514,
      80,   512,   589,   482,   442,  1998,   514,   559,    64,  1785,
     497,  1389,  1690,   256,   148,   498,   934,  1291,   539,    65,
      66,    67,    68,    69,    70,    71,    80,   944,  1408,   173,
     511,   602,    80,    80,   914,   112,  1434,   530,   618,  1628,
    1629,  1630,  1631,  1632,  1529,   369,   148,   369,   531,  1380,
    1469,    65,    66,    67,    68,    69,    70,    71,  1278,    90,
    2016,   257,  1279,    80,  1280,  1818,  1057,   946,   442,   104,
     620,   602,   547,  1207,   219,   256,  1825,   284,   286,  1550,
     999,  1000,  1001,   980,  1676,   982,   616,   985,  2016,  1475,
     337,   993,   947,  1085,   997,    75,   948,   970,   257,   541,
    1040,   971,   570,  1605,   514,   551,  1094,   291,   522,   256,
    1095,   514,   514,  1124,  1126,   354,   256,   971,   971,  1022,
    -121,  -121,  -121,  -121,  -121,  -121,   589,   211,   482,  2068,
     514,    80,    80,    80,  1194,  1538,  1539,  1285,  1440,   593,
     474,  1286,   767,  1480,   608,    96,   256,   113,  1532,  1474,
     605,   287,   638,  1286,   286,   482,   640,   618,  1687,   649,
    1376,    80,  1688,    14,    15,    16,    17,   147,    97,    80,
     656,   894,    80,    80,    96,  1760,    80,   667,   173,   971,
    1786,   262,   272,   671,   971,   173,  1989,    80,  1787,   108,
     681,  1788,  1095,  1913,  1914,   971,  1861,    97,  1951,   668,
     767,   672,   971,  1098,   673,  1100,   709,  2055,   710,   711,
     712,  1286,  2118,   474,   469,   469,   971,   701,   108,  2142,
     677,    80,    58,  2139,   265,  -120,  -120,  -120,  -120,  -120,
    -120,  2149,   434,   434,    80,  2150,   141,   713,  2086,   720,
     714,   715,  2090,   256,   418,   716,   717,   998,   724,   725,
     482,  1594,  1596,   734,  1006,  1007,  1008,  1009,    80,  1142,
     173,   173,  1726,  1727,  1728,   141,  1365,  1366,  1367,   256,
     735,   638,   286,   726,   727,  1380,  1380,  1380,   173,   736,
    1569,  1380,    73,   141,   337,   742,   681,   732,   733,  1648,
      80,   737,  1502,  1584,  1816,   201,   701,   768,   112,   608,
     701,   449,   619,   973,   974,  1999,   620,   971,  1292,   354,
    1072,  1073,  1239,    78,   621,   769,   256,  1074,  1075,  1273,
    1095,   770,    90,  1288,  1289,  -156,  -156,   112,   771,   418,
     418,   772,   104,   773,   932,    -3,   256,  1074,  1432,  1495,
    1496,   256,   800,   256,  -458,   269,  1500,  1496,   814,  1605,
     -17,    90,  1504,  1496,   824,    80,   501,   813,   434,    80,
     837,   104,    80,   256,   502,   256,   256,   838,   255,   266,
      14,    15,    16,    17,   147,   968,   503,   847,   257,   504,
    1046,  1488,   482,   256,  2060,   839,  1376,  1376,  1376,   153,
    1565,  1566,  1570,  1583,  1540,  1488,   256,  1194,   151,  1046,
    1552,   841,   482,   842,    80,  1700,  1095,  1827,  1095,   843,
     113,  1828,  1496,   549,   547,   849,    97,    97,   299,   256,
     505,   638,   286,  1838,  1839,   865,  1194,  1848,   971,  -576,
     506,  1906,  1907,  1923,  1496,   507,  1841,   108,   108,   113,
    1924,  1496,  -574,   256,   638,  1866,  1867,   482,  2139,  2140,
     256,    14,    15,    16,    17,   147,  1265,   141,  1493,  1494,
    1795,   874,  1002,  1003,   883,   482,  1004,  1005,   897,   434,
      80,  1010,  1011,  1752,  1752,    80,    80,    80,  1532,  1677,
    1678,  1756,  1584,   899,   141,   141,  1260,  1584,   337,   901,
     904,  1264,   920,   922,  1208,   510,   622,  1789,  1745,   939,
     943,   949,  1272,  1206,   950,   972,   463,   975,   978,  1019,
    1024,  1045,  1046,   354,  1053,  1092,  1101,    14,    15,    16,
      17,   147,  -770,  1102,  1103,   830,   173,  1104,  1105,   173,
     173,   173,  1106,   831,  1125,  1127,  1136,  1137,  -668,  1138,
    1532,  1197,  1198,   152,    80,   832,   112,   112,   833,    80,
    1231,  1214,  1232,   173,  1233,    80,  1243,    80,  1244,   173,
    1249,  1246,   141,  1248,    80,  1252,  1253,   568,  1255,   187,
      90,    90,  1256,  1480,   173,  1257,    58,  1258,   482,  1259,
     104,   104,  1583,  1261,  1262,  1263,  1268,  1583,  1269,   834,
    1276,   482,  1277,  1293,  1298,   509,  1299,  1300,  1307,   835,
     546,   434,  1308,  1309,   836,  1310,   469,   469,  -656,  1318,
     547,   275,  -655,   658,  1901,   276,  1333,  1341,   280,   173,
     285,  1353,  1358,  -771,  1381,  1392,  1382,  1977,   482,  1395,
    1396,  1405,  1476,  1911,  1406,  1407,    73,   265,  1412,  -691,
     613,  1414,   971,   636,   148,  1607,  1607,  1422,  1207,    65,
      66,    67,    68,    69,    70,    71,   759,   613,   113,   113,
     514,   613,    97,  1416,  1426,   845,  1428,    78,    79,  1429,
    1430,  1436,  1939,  1472,   674,    80,  1208,    80,  1438,  1443,
    1208,  1486,  1488,   108,   256,  1206,  1503,  1515,   659,  1206,
     633,  1528,  1311,    75,  1534,   256,   141,  1533,  1536,  1535,
    1496,  1544,  1541,  1977,   660,  1557,  1558,   661,   662,    65,
      66,    67,    68,    69,    70,    71,  1559,  1561,  1598,  1656,
    1572,   256,  1385,  1983,    80,  1641,  1574,    80,  1575,  1643,
     141,  1619,   256,  1646,  1620,  1622,  1624,  1636,   482,  1644,
    1645,   256,   482,  1651,   141,  1653,  1654,  1658,   141,  1657,
    1660,  1667,   718,  1661,  1532,  1663,   482,   173,   269,   613,
     173,  1662,  1664,  1665,   275,   844,   482,  1672,   148,  1681,
    1685,   171,   172,    65,    66,    67,    68,    69,    70,    71,
    1584,   255,   266,    80,    80,  1686,  1689,  1693,  1481,  1482,
    1483,   257,   112,  1701,  1702,  1484,  1485,  1706,  1707,    97,
     173,   442,  1715,  1540,  1717,  2045,  1725,  1739,  1577,  1979,
    1286,  1759,  1761,  1763,   220,  2063,    90,  1939,  1799,  1792,
     108,   469,  1794,   501,  1815,  1809,   104,  1819,  1813,  1814,
    1207,   502,  1821,  1826,  1207,  1817,  1843,  1832,   256,  1833,
     463,  1397,    80,   503,  1836,  1837,   504,   275,   276,   482,
     637,  1846,   285,   482,  1847,  1874,  2088,  1879,   482,  1898,
    1894,  1880,   256,  1892,  1900,  1904,   418,   141,  1983,  1905,
    1916,  1160,  1983,  1983,  1917,  1920,  1918,  1921,  1922,  -657,
    1583,  1953,  1930,   482,  1931,  1979,   880,   505,  1932,   682,
    1933,  1607,  1934,   514,   463,  -559,  1961,   506,   263,   273,
    1955,  1964,   507,  1966,   113,  1980,  1972,  2114,  1981,  1994,
    1995,  1996,   613,   463,  1839,  1997,   434,  2000,  2001,  2002,
    2022,  2013,  2035,    83,  2039,  2037,   150,   932,   482,   112,
    2048,  2049,   482,  2130,  2053,  2050,   613,  2130,  2141,  2054,
     546,  2057,  2061,  2070,  2087,   482,  2072,  2074,  2094,   613,
     547,  2083,  2095,    90,   193,   194,    80,   195,    80,  2144,
    2096,   510,   173,   104,  2102,   586,   188,    97,  2112,   482,
    2089,   482,   482,  2115,  2113,  2125,   173,  2129,  2132,  2128,
    2127,    83,  2133,  2136,   483,   682,  2137,  2147,   108,   173,
    2148,  1823,  1499,  2138,  2151,    97,   190,   482,  1592,   967,
    1014,   256,   845,   258,  1015,    83,  1012,  1411,   757,   418,
    1013,   418,  1016,  1418,   278,   281,   108,  2126,   230,  1796,
    2064,   254,    80,    80,  1896,    83,   173,  2081,  1607,  1889,
    2121,  1992,   206,   482,  2119,   141,    97,   256,  2110,  1790,
    1791,   113,  2041,   256,   482,  2091,  2134,   463,  2040,   170,
     418,  1427,   282,  1937,   275,   540,  2008,   108,   258,  1753,
    1571,   509,   150,   141,    80,  1242,  1424,  1091,    83,  1803,
     820,  1123,   150,   473,  1217,   310,   316,     3,   482,  1250,
     482,  1027,  1028,  2111,  1029,  1724,     0,   336,   463,   902,
       0,     0,     0,     0,     0,     0,     0,   112,     0,   482,
       0,     0,   844,     0,   141,   482,     0,     0,     0,   258,
       0,   425,   190,   190,     0,   482,     0,     0,     0,    80,
       0,    90,     0,   150,   455,   112,     0,   254,     0,    80,
     550,   104,   418,     0,  1230,     0,   173,     0,     0,     0,
     173,     0,     0,     0,    14,    15,    16,    17,   147,    90,
       0,   230,   230,     0,   173,     0,     0,     0,     0,   104,
       0,     0,     0,     0,   173,     0,   112,     0,   310,     0,
       0,   258,   880,  1797,     0,     0,    83,     0,     0,     0,
       0,   173,     0,     0,     0,   256,     0,     0,     0,   254,
      90,     0,     0,     0,     0,     0,  1607,     0,     0,   246,
     104,     0,     0,    58,     0,   258,     0,     0,     0,   113,
       0,     0,   258,     0,     0,   613,     0,     0,   636,   192,
       0,     0,     0,   316,  1607,     0,    19,     0,     0,     0,
     316,   310,   310,   256,     0,   880,     0,   113,   150,     0,
       0,   233,   258,     0,  1297,  1394,     0,   173,   810,     0,
     483,   173,     0,     0,     0,     0,   173,     0,   336,   623,
     632,  1304,  1305,    73,     0,  1607,     0,   463,     0,    52,
      53,    54,    55,     0,   434,   336,     0,     0,   113,   336,
       0,   173,     0,  1730,     0,     0,   683,   514,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,   312,    58,
     148,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,   148,   425,     0,     0,     0,    65,    66,    67,
      68,    69,    70,    71,   991,     0,   173,     0,    73,     0,
     173,   148,     0,     0,   227,   228,    65,    66,    67,    68,
      69,    70,    71,   173,     0,   880,     0,   425,  1576,    75,
     760,     0,     0,     0,     0,  1577,  2047,   190,     0,    78,
      79,     0,   880,   880,   992,   258,     0,   173,  1419,   173,
     173,     0,     0,   150,   500,   233,     0,   455,     0,  1311,
      75,   789,   683,   632,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,   148,   173,     0,   171,   172,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,   524,     0,     0,   256,     0,     0,   148,     0,     0,
       0,   230,    65,    66,    67,    68,    69,    70,    71,   983,
     230,   173,     0,  1988,     0,     0,     0,     0,     0,   258,
       0,     0,   173,     0,     0,     0,     0,     0,     0,     0,
     310,     0,   425,   425,   587,   312,   310,     0,   336,   258,
       0,     0,     0,     0,     0,     0,     0,  1420,     0,   984,
       0,     0,     0,     0,     0,     0,   173,   148,   173,   258,
     171,   172,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,   310,   173,     0,     0,
       0,     0,     0,   173,     0,     0,     0,     0,   310,     0,
     310,     0,   336,   173,    83,   258,     0,  2145,     0,  1647,
    1511,  1512,     0,     0,     0,     0,   203,  2152,     0,   613,
     336,   455,     0,   632,  1526,  1527,     0,     0,     0,   258,
       0,   623,     0,     0,     0,   623,   258,     0,     0,     0,
    1399,     0,     0,     0,   336,     0,   256,     0,   463,     0,
       0,     0,     0,     0,   632,     0,     0,   336,  1548,  1549,
       0,     0,     0,   256,     0,     0,     0,     0,   150,     0,
       0,     0,     0,   483,     0,     0,   810,     0,     0,     0,
       0,   425,     0,   203,   150,   150,     0,   425,     0,   765,
       0,     0,     0,     0,   790,     0,   425,     0,   203,   150,
     150,   150,     0,     0,     0,   776,     0,     0,   779,     0,
       0,   880,   880,   182,     6,     7,     8,     9,    10,    11,
      12,    13,   203,     0,     0,   880,   880,     0,     0,     0,
       0,   148,     0,     0,   829,   458,    65,    66,    67,    68,
      69,    70,    71,   233,     0,     0,     0,     0,     0,    19,
       0,     0,   256,     0,     0,   455,     0,     0,     0,   880,
     880,     0,     0,   312,     0,   524,     0,     0,     0,   312,
       0,   760,   760,     0,   274,     0,     0,     0,     0,   425,
      75,     0,     0,   809,     0,  1762,     0,   203,    48,    49,
      50,    51,    52,    53,    54,    55,   455,     0,  1773,   789,
       0,   789,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,     0,     0,     0,     0,     0,     0,   336,   336,
       0,   893,   148,   312,    58,   360,   361,    65,    66,    67,
      68,    69,    70,    71,     0,  1801,     0,   336,     0,   310,
       0,     0,   463,  1281,  1119,     0,     0,     0,     0,   203,
       0,  1281,     0,     0,  1719,  1720,   148,     0,   310,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,   203,
     258,     0,     0,    76,     0,     0,   256,     0,   362,     0,
    1281,   258,     0,   483,    73,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,   425,     0,     0,     0,
       0,     0,     0,   336,  2043,    75,     0,   258,   514,     0,
     150,   425,     0,     0,     0,    78,    79,     0,     0,     0,
       0,     0,   148,   336,     0,  1225,     0,    65,    66,    67,
      68,    69,    70,    71,     0,     0,   623,     0,     0,     0,
       0,     0,     0,  1281,     0,  1865,     0,     0,     0,  1875,
      73,     0,     0,     0,     0,   880,   880,     0,   203,   320,
     321,   322,   323,  1888,     0,     0,     0,     0,     0,     0,
      74,    75,     0,  1897,     0,   455,  1810,     0,     0,     0,
       0,    78,    79,   256,     0,     0,     0,     0,   203,     0,
       0,     0,     0,     0,     0,     0,     0,  1757,   148,     0,
       0,   171,   172,    65,    66,    67,    68,    69,    70,    71,
       0,  1831,     0,     0,     0,     0,  1834,  1835,     0,     0,
     765,   765,  1048,     0,   790,     0,     0,     0,     0,     0,
    1038,     0,     0,  1041,     0,     0,     0,     0,     0,     0,
       0,     0,   760,     0,     0,     0,     0,     0,     0,     0,
     324,     0,     0,     0,     0,     0,  1947,     0,     0,   789,
    1952,     0,   312,   203,   203,  1957,   789,     0,   325,   458,
       0,     0,     0,     0,     0,     0,     0,   880,     0,     0,
       0,   312,     0,     0,     0,     0,     0,     0,     0,     0,
    1987,     0,     0,     0,   524,     0,     0,     0,  1109,     0,
       0,     0,  1113,     0,     0,     0,     0,     0,   336,     0,
     115,     0,   880,   115,     0,     0,     0,   880,   880,     0,
       0,     0,     0,   203,     0,     0,     0,     0,     0,     0,
     483,     0,     0,     0,     0,  2017,     0,     0,  1281,  2020,
       0,     0,   458,     0,     0,     0,     0,     0,     0,     0,
       0,   150,  2034,     0,     0,     0,     0,     0,     0,   425,
       0,     0,     0,     0,     0,   203,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,  2056,     0,  2058,  2059,
       0,     0,     0,     0,     0,     0,     0,   258,   425,   203,
       0,   148,   115,     0,   171,   172,    65,    66,    67,    68,
      69,    70,    71,     0,  2069,   254,    83,     0,   260,     0,
       0,     0,   115,     0,     0,     0,     0,     0,     0,     0,
     310,     0,     0,   258,   148,     0,   150,   171,   172,    65,
      66,    67,    68,    69,    70,    71,     0,   455,     0,     0,
    2092,     0,     0,     0,     0,     0,     0,     0,     0,   115,
    1401,  2098,     0,     0,     0,   115,     0,     0,     0,   115,
       0,     0,   483,   260,     0,     0,   455,     0,     0,     0,
     150,     0,     0,   332,   115,   364,   458,     0,     0,     0,
     595,     0,  1048,     0,     0,  2124,     0,  2098,  1312,   790,
       0,   765,     0,   613,     0,     0,     0,     0,   429,     0,
     203,     0,     0,     0,     0,     0,  2135,     0,     0,     0,
     115,   429,  2124,     0,   260,     0,     0,   458,     0,     0,
       0,     0,  2143,     0,     0,     0,     0,   483,     0,     0,
       0,     0,     0,   336,   336,     0,     0,     0,     0,   458,
     458,     0,     0,     0,     0,   483,     0,     0,     0,     0,
    1281,     0,  1555,     0,     0,  1281,  1281,  1281,   458,     0,
       0,   115,  1344,   115,     0,  1348,     0,     0,     0,   613,
    2097,   258,     0,     0,     0,     0,   260,   150,   150,   150,
     150,     0,   150,   150,     0,     0,     0,     0,  1578,   316,
       0,     0,     0,     0,     0,     0,     0,   564,     0,     0,
       0,     0,     0,     0,     0,   115,     0,   425,   425,     0,
     260,    14,    15,    16,    17,   147,     0,   260,     0,   258,
       0,     0,     0,     0,   458,   115,     0,     0,     0,     0,
       0,     0,   203,     0,     0,     0,     0,   254,     0,     0,
       0,    58,     0,   312,     0,   115,   148,   260,   115,   199,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     455,     0,   115,     0,     0,     0,   115,     0,     0,     0,
      58,   880,     0,   148,     0,     0,   227,   228,    65,    66,
      67,    68,    69,    70,    71,   150,     0,   623,     0,     0,
       0,     0,     0,     0,     0,    75,   203,     0,   809,     0,
     429,    73,   148,     0,     0,   227,   228,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,   229,    75,     0,     0,     0,     0,     0,     0,     0,
      73,   148,    78,    79,   429,     0,    65,    66,    67,    68,
      69,    70,    71,  1278,     0,     0,  1523,  1279,     0,  1280,
     787,    75,     0,     0,   620,  1281,     0,  1281,     0,  1509,
     115,    78,   788,     0,   429,     0,     0,     0,  1518,     0,
     260,     0,     0,     0,     0,     0,     0,  1578,  1731,     0,
      75,     0,  1578,  1491,   425,     0,     0,     0,  1578,     0,
    1578,     0,     0,     0,     0,    14,    15,    16,    17,   147,
     258,   182,     6,     7,     8,     9,    10,    11,    12,    13,
       0,  1582,     0,     0,     0,     0,     0,   148,   316,   150,
     227,   228,    65,    66,    67,    68,    69,    70,    71,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   429,
     429,     0,     0,     0,   260,   115,     0,     0,     0,     0,
       0,     0,   148,   425,    58,   227,   228,    65,    66,    67,
      68,    69,    70,    71,   336,     0,     0,   150,     0,     0,
       0,   150,     0,   907,     0,     0,   115,     0,     0,     0,
       0,   115,     0,     0,   260,   115,   148,   115,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,   115,
     150,   115,     0,     0,     0,     0,     0,     0,  1238,     0,
       0,     0,     0,     0,    73,   364,     0,   115,   429,     0,
     260,     0,     0,     0,     0,   336,   336,     0,     0,     0,
       0,     0,     0,     0,  2043,    75,     0,     0,   514,     0,
       0,   115,  1731,  1731,   260,    78,    79,   203,   564,     0,
       0,   260,     0,     0,   115,     0,   938,  1578,   203,   258,
    1578,     0,     0,     0,   148,   115,     0,   559,    64,    65,
      66,    67,    68,    69,    70,    71,     0,   316,   429,     0,
       0,   115,   115,     0,   429,     0,     0,   203,     0,     0,
     425,     0,     0,   429,     0,     0,   115,   115,   115,     0,
    1582,     0,     0,     0,    58,  1582,     0,     0,     0,     0,
       0,  1746,    99,  1582,     0,   154,  1021,   310,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1734,    14,    15,
      16,    17,   147,     0,     0,     0,   148,     0,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,   148,   429,     0,   458,   458,    65,    66,    67,    68,
      69,    70,    71,  1278,    73,     0,  1731,  1279,     0,  1280,
      99,     0,     0,     0,     0,  1578,   429,     0,     0,     0,
       0,     0,     0,     0,   309,    75,     0,    58,     0,     0,
       0,     0,     0,   429,   205,    78,    79,     0,     0,     0,
      75,     0,     0,  1696,     0,     0,     0,     0,     0,     0,
       0,   150,     0,     0,   267,   115,   115,     0,     0,   148,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,   115,    14,    15,    16,    17,   147,
     336,     0,     0,     0,     0,     0,     0,    73,  1731,     0,
       0,   301,     0,     0,     0,     0,     0,    99,   150,  1120,
       0,     0,   115,     0,     0,     0,     0,   229,    75,     0,
       0,     0,   258,     0,     0,     0,   338,     0,    78,    79,
    1862,   203,     0,  1582,     0,   260,   150,   150,     0,  2044,
     316,  1734,  1734,   429,    58,     0,   260,     0,     0,     0,
     115,   435,     0,     0,     0,     0,     0,   115,   429,     0,
       0,     0,   301,   461,     0,     0,     0,     0,     0,     0,
     115,     0,  1227,   429,     0,   115,   148,   150,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,   148,
     312,   508,   561,   562,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,    73,  2044,  2044,   528,     0,     0,
       0,     0,   533,   535,     0,   205,     0,     0,     0,     0,
       0,     0,   429,     0,   309,    75,     0,     0,     0,    14,
      15,    16,    17,   147,     0,    78,    79,   555,     0,     0,
      76,   557,     0,     0,     0,     0,   558,  2044,  1582,     0,
       0,     0,     0,     0,     0,   203,     0,   575,     0,     0,
       0,     0,     0,     0,     0,  1734,     0,     0,     0,     0,
       0,   588,     0,     0,    14,    15,    16,    17,   147,     0,
       0,     0,     0,     0,     0,   115,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,   611,     0,     0,
     635,   148,   115,   115,     0,     0,    65,    66,    67,    68,
      69,    70,    71,  1278,   642,     0,     0,  1279,   642,  1280,
     148,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,    58,     0,   203,     0,     0,     0,     0,
       0,     0,     0,  2006,     0,     0,     0,  1734,    73,     0,
      75,     0,     0,  1698,     0,   115,     0,   109,     0,     0,
       0,     0,   312,     0,     0,   148,     0,     0,  1576,    75,
      65,    66,    67,    68,    69,    70,    71,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,     0,  1734,     0,
       0,     0,     0,    73,     0,     0,   458,   458,   115,     0,
       0,    58,     0,     0,     0,     0,   429,     0,     0,     0,
       0,     0,   301,    74,    75,   109,   611,     0,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,   587,   312,
       0,     0,     0,   148,     0,   429,   227,   228,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,   260,   115,  1734,  1734,     0,     0,     0,   268,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
     312,     0,     0,   115,     0,     0,     0,     0,     0,     0,
       0,  1576,    75,     0,   429,     0,     0,     0,  1227,     0,
       0,     0,    78,    79,     0,     0,  1734,   461,     0,     0,
    1466,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,   429,     0,     0,     0,   115,     0,     0,
       0,   342,     0,     0,     0,     0,     0,     0,   876,     0,
       0,     0,     0,   535,     0,     0,   381,   887,   382,   575,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,   338,     0,    99,     0,     0,     0,     0,   462,     0,
       0,   115,   115,     0,     0,     0,     0,     0,     0,   642,
     913,     0,     0,     0,     0,   115,   115,     0,     0,     0,
     115,   115,     0,     0,   924,     0,   703,     0,     0,    76,
     392,     0,     0,   611,     0,     0,     0,     0,   933,     0,
       0,     0,     0,     0,     0,     0,   642,     0,     0,   115,
     115,   458,     0,     0,     0,     0,     0,  1556,     0,     0,
       0,     0,     0,     0,   115,   115,   115,   115,   115,   115,
     115,     0,   556,     0,   148,     0,   260,   227,   228,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,   109,     0,   429,   429,     0,     0,     0,     0,
       0,     0,    73,   148,     0,     0,   227,   228,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,   787,    75,   260,     0,   620,     0,     0,     0,
       0,    73,   612,    78,   788,   268,     0,     0,     0,     0,
       0,     0,     0,     0,   461,     0,   622,   429,     0,   612,
       0,  1576,    75,   612,     0,     0,     0,     0,  1577,     0,
       0,  1030,    78,    79,     0,     0,     0,     0,     0,     0,
     148,     0,   115,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,   913,     0,     0,     0,     0,
    1054,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,   148,     0,     0,   461,   461,    65,
      66,    67,    68,    69,    70,    71,     0,     0,  2043,    75,
       0,     0,   514,     0,     0,     0,   461,     0,     0,    78,
      79,   148,    73,     0,   227,   228,    65,    66,    67,    68,
      69,    70,    71,     0,     0,   115,   115,     0,     0,     0,
       0,   612,  1047,    75,   876,     0,   620,     0,     0,    73,
       0,     0,     0,    78,    79,     0,     0,     0,     0,     0,
       0,   429,     0,     0,     0,     0,     0,     0,     0,   229,
      75,     0,     0,     0,     0,  1193,     0,   115,     0,     0,
      78,    79,   461,     0,     0,     0,     0,     0,     0,   154,
       0,     0,     0,     0,     0,   260,   115,     0,     0,     0,
       0,     0,   642,     0,     0,  1229,     0,   876,     0,     0,
     148,     0,  1235,   227,   228,    65,    66,    67,    68,    69,
      70,    71,   462,     0,     0,     0,     0,   114,     0,     0,
     429,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,   115,     0,     0,   115,     0,     0,     0,   115,     0,
       0,     0,     0,   342,   338,     0,     0,   115,   309,    75,
       0,     0,   268,     0,   109,     0,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,   462,   115,   109,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,  1938,
       0,     0,   115,     0,   612,   462,     0,   115,   115,     0,
       0,   148,   115,   115,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,   148,   876,   612,   171,
     172,    65,    66,    67,    68,    69,    70,    71,     0,   270,
       0,   612,     0,     0,   876,   876,     0,   375,     0,     0,
     376,     0,   377,     0,   378,     0,     0,     0,     0,     0,
       0,     0,    76,     0,   260,     0,     0,     0,     0,     0,
       0,   379,     0,     0,   472,     0,     0,   429,     0,     0,
     148,     0,   114,   171,   172,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,   461,     0,   380,
     381,   346,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,   476,     0,
      73,     0,     0,     0,     0,     0,     0,     0,   464,   462,
    1377,     0,     0,     0,     0,     0,     0,     0,  1193,     0,
     391,     0,     0,    76,   392,     0,     0,     0,     0,     0,
     393,    78,    79,   394,   395,   396,   397,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1193,     0,     0,
     462,     0,   744,   745,   746,   747,   748,   749,   750,   751,
     752,   753,   754,     0,     0,  1425,   214,     0,   115,     0,
       0,   148,   342,   342,   171,   172,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,   342,     0,   755,     0,     0,   611,   115,     0,     0,
       0,     0,   114,     0,     0,   533,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   342,
       0,     0,     0,     0,   876,   338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   614,   115,   115,   270,     0,   260,     0,     0,
     109,     0,     0,     0,     0,     0,     0,   342,     0,   614,
       0,     0,     0,   614,     0,     0,     0,     0,     0,   120,
       0,     0,   120,   876,   876,     0,     0,   612,     0,     0,
     268,     0,   342,     0,   115,     0,     0,   876,   876,     0,
       0,     0,   461,   461,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   876,   876,     0,     0,     0,     0,   120,     0,   462,
       0,   115,     0,     0,     0,     0,  1377,  1377,  1377,   154,
     535,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   614,     0,     0,     0,     0,  1606,  1606,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   342,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   342,
     342,     0,     0,     0,   120,     0,     0,     0,   120,   338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   464,     0,   154,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   119,   120,     0,     0,
       0,     0,   342,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   346,     0,     0,     0,     0,     0,     0,
       0,     0,   270,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   464,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,   109,   614,   464,     0,   876,   876,     0,
     120,     0,   120,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   614,     0,
       0,     0,   109,     0,     0,     0,     0,     0,     0,  1748,
       0,   614,     0,     0,     0,   119,     0,     0,     0,   876,
     268,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1765,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,   612,   119,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1606,     0,     0,     0,     0,     0,     0,   342,
     462,     0,     0,   338,     0,     0,   154,     0,     0,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,   876,
       0,   119,     0,     0,     0,     0,     0,     0,     0,   464,
       0,     0,     0,   119,     0,     0,     0,   174,   177,   120,
       0,     0,     0,     0,     0,     0,     0,     0,   342,   342,
       0,     0,     0,     0,   876,     0,     0,     0,     0,   876,
     876,     0,   342,   342,   461,   461,     0,   342,   342,     0,
     464,     0,   222,   120,     0,     0,     0,     0,     0,     0,
       0,     0,  1854,     0,   119,     0,   119,     0,     0,     0,
       0,   119,   346,   346,     0,     0,   342,   342,     0,   120,
     375,     0,     0,   376,     0,   377,     0,   378,     0,     0,
       0,   346,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   303,     0,   379,   304,     0,     0,   119,  1606,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   346,
     326,   109,   109,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   380,   381,     0,   478,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
     114,   386,   387,   388,     0,   389,   390,   346,   120,   120,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   462,     0,     0,   614,     0,     0,
     270,   493,   346,   391,    75,     0,   479,   480,     0,     0,
       0,   481,     0,   393,    78,    79,   394,   395,   396,   397,
     120,     0,     0,   119,   120,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,     0,   544,     0,     0,     0,   464,
    1978,     0,     0,     0,     0,   174,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,     0,     0,     0,     0,     0,     0,     0,     0,   461,
       0,     0,     0,   119,     0,     0,     0,   326,     0,     0,
       0,     0,   342,   342,     0,     0,     0,  1606,   591,     0,
       0,     0,     0,     0,   120,   594,   596,     0,     0,     0,
     603,     0,   346,     0,     0,     0,     0,   120,     0,     0,
     120,   120,     0,   120,     0,  1606,  1978,     0,     0,   346,
     346,     0,   120,     0,   342,   120,   120,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   326,     0,
       0,   326,     0,   268,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   119,     0,     0,  1606,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   346,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,  2085,     0,     0,   342,     0,
       0,     0,     0,     0,   119,     0,     0,     0,   119,     0,
     119,     0,     0,   876,   342,   120,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,   222,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   342,
       0,   804,   805,     0,   342,   342,     0,     0,     0,   342,
     342,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     270,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,   119,   119,     0,   119,     0,     0,
       0,   614,     0,     0,     0,     0,   119,     0,     0,   119,
     119,   119,   124,     0,   109,   124,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,   346,
     464,     0,     0,     0,     0,     0,   120,   120,     0,     0,
     213,     0,     0,     0,     0,     0,   224,   225,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,   326,     0,     0,     0,     0,   346,   346,
     290,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   346,   346,   124,     0,     0,   346,   346,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,   941,     0,     0,   346,   346,     0,     0,
       0,     0,     0,     0,     0,   612,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,   124,     0,     0,
       0,   124,     0,     0,   342,     0,     0,     0,     0,     0,
       0,   114,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,   119,     0,     0,     0,
     109,   612,   124,     0,     0,     0,     0,     0,     0,     0,
     119,   119,     0,     0,   464,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,     0,   583,     0,
       0,     0,     0,   124,     0,   124,     0,     0,     0,     0,
     124,     0,  1071,     0,     0,     0,     0,   120,     0,  1083,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   342,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,   346,   346,     0,     0,     0,   124,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,  1144,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   346,     0,     0,     0,     0,     0,
       0,     0,  1216,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   270,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,   785,     0,   786,
       0,     0,     0,     0,     0,     0,     0,   114,   802,   803,
       0,     0,     0,     0,     0,     0,     0,     0,   346,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,   346,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   124,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   346,
       0,     0,     0,     0,   346,   346,     0,     0,     0,   346,
     346,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,   120,   120,   120,   120,   120,   120,   120,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   886,     0,     0,     0,
       0,     0,     0,   120,   120,     0,   119,     0,     0,     0,
       0,   124,   124,     0,     0,     0,    14,    15,    16,    17,
     147,     0,     0,    20,   114,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     119,     0,     0,   124,    46,     0,    47,   124,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,     0,    58,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1398,  1400,  1402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1421,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,  1144,     0,
       0,     0,     0,     0,     0,   614,     0,     0,     0,     0,
     124,     0,     0,   124,   124,     0,   124,   119,   119,   119,
     119,   119,   119,   119,     0,   124,     0,     0,   124,   124,
     124,     0,     0,     0,   346,     0,     0,     0,     0,     0,
     120,     0,     0,  1467,     0,     0,     0,   119,   119,     0,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
     114,   614,     0,  1069,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,     0,   120,     0,   119,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
     146,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
    1140,  1141,     0,     0,     0,     0,     0,     0,   346,     0,
       0,  1199,  1200,  1201,     0,     0,     0,  1203,     0,     0,
       0,  1587,     0,     0,  1589,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
     124,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
    1274,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1295,   119,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1319,     0,     0,     0,     0,     0,     0,
       0,     0,  1323,  1324,  1325,  1326,     0,     0,     0,     0,
    1331,  1332,     0,     0,     0,     0,   296,     0,     0,     0,
    1340,     0,     0,     0,     0,     0,  1754,     0,     0,     0,
     536,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     296,  1356,     0,     0,  1359,     0,  1360,     0,     0,     0,
     296,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   567,   571,
     119,     0,     0,     0,     0,   578,   579,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   590,     0,     0,     0,     0,     0,     0,  1417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   609,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,   120,     0,  1431,     0,     0,   124,     0,
       0,     0,  1435,     0,  1437,  1439,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1449,     0,  1450,
       0,  1451,     0,  1453,     0,     0,     0,   124,  1461,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   702,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
     743,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   168,     0,     0,
    1506,     0,     0,     0,     0,     0,   781,  1513,  1514,     0,
     784,     0,     0,     0,     0,     0,     0,     0,   119,   124,
       0,     0,     0,   168,     0,  1910,     0,     0,     0,   806,
       0,  1537,     0,   807,   808,     0,     0,   811,  1542,     0,
       0,     0,  1543,     0,     0,     0,   119,     0,     0,     0,
       0,     0,   825,   826,   827,   828,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   168,
       0,   850,     0,     0,     0,     0,     0,     0,   224,   853,
       0,     0,   168,     0,   168,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,   403,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   296,     0,     0,
       0,     0,     0,     0,     0,     0,   366,     0,  1642,     0,
       0,     0,     0,     0,     0,     0,   124,   124,   124,   124,
     124,   124,   124,     0,     0,     0,     0,     0,   891,     0,
     366,     0,     0,     0,     0,     0,   567,     0,     0,     0,
       0,     0,   898,     0,     0,  1666,   124,   124,     0,     0,
       0,     0,     0,  1671,     0,  1673,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   916,   921,   168,     0,
       0,     0,   168,     0,     0,   168,   168,     0,     0,   168,
       0,     0,   168,   168,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1704,  1705,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1710,
    1711,     0,  1712,     0,   124,     0,     0,     0,     0,     0,
       0,  1716,     0,     0,     0,     0,     0,   963,     0,     0,
       0,  1721,  1722,     0,     0,     0,     0,     0,   168,     0,
       0,   168,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   168,     0,     0,     0,     0,     0,   670,
       0,     0,     0,   403,   676,     0,     0,     0,   168,     0,
       0,     0,     0,   685,   686,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1026,     0,   403,   403,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1043,     0,   124,     0,  1044,     0,     0,     0,   403,
       0,     0,     0,     0,   916,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1084,     0,   124,   403,
       0,     0,  1811,  1812,     0,  1093,     0,     0,     0,     0,
       0,  1096,     0,     0,     0,  1820,     0,     0,     0,     0,
       0,     0,     0,   168,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     1,     0,
     124,     0,  1844,  1845,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,   124,
       0,     0,     0,     1,     0,   252,     0,     0,     0,     0,
     168,     0,     0,     0,     0,    14,    15,    16,    17,   147,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -462,
    -462,     0,  -462,    46,     0,    47,     0,  -462,     0,  1912,
    1254,     0,     0,  1915,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1925,   366,     0,  1926,  1927,   124,
       0,     0,     0,     0,  1929,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   168,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,   168,
       0,     0,     0,  1302,     0,     0,     0,  1303,     0,     0,
       0,     0,     0,     0,   916,     0,     0,    76,   315,     0,
       0,     0,     0,     0,  1316,    78,    79,     0,     0,     0,
       0,  1317,     0,     0,     0,     0,     0,     0,     0,     0,
    1321,     0,  1322,     0,   403,   403,   403,   403,   403,   403,
     403,   403,   403,   403,   403,   403,   403,   403,   403,   403,
     403,   403,   403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1350,     0,     0,     0,  1351,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2042,
       0,     0,     0,     0,     0,   146,     0,     0,     1,     0,
       0,     0,   353,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,   168,   168,   403,     0,     0,     0,   168,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   451,   353,
       0,     0,     0,     0,     0,   124,     0,     0,     0,   168,
       0,     0,   168,   168,  2082,   168,     0,   168,   168,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     517,     0,     0,     0,     0,     0,     0,   517,     0,  2100,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
    1448,     0,     0,     0,  2109,     0,   168,     0,     0,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2122,     0,     0,     0,     0,  1473,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   517,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   168,   168,     0,
       0,     0,     0,   353,   624,     0,     0,     0,     0,     0,
       0,   168,     0,   403,     0,     0,     0,     0,   403,     0,
       0,     0,     0,     0,   645,     0,     0,     0,     0,   403,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1546,
       0,     0,     0,  1547,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   403,     0,     0,     0,     0,     0,     0,     0,  1588,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   517,   777,     0,   517,   780,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,   624,     0,     0,  1652,
       0,     0,  1655,     0,     0,     0,     0,     0,     0,     0,
    1659,     0,   168,     0,     0,     0,     0,     0,     0,     0,
       0,  1668,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,     0,     0,
       0,   517,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   168,     0,     0,     0,     0,     0,     0,
     168,     0,     0,   168,     0,     0,     0,     0,     0,     0,
       0,   403,     0,   353,  1703,     0,     0,     0,     0,     0,
       0,     0,     0,  1708,     0,     0,     0,  1709,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1713,  1714,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   517,     0,     0,   353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   911,   353,     0,     0,   403,
       0,     0,     0,     0,     0,     0,   624,     0,     0,     0,
     624,     0,     0,     0,     0,     0,     0,   929,   403,   353,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   376,
       0,   377,     0,   378,     0,   403,   403,   403,     0,     0,
     168,     0,   403,   403,     0,     0,     0,     0,   168,   168,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   403,     0,     0,     0,
       0,     0,     0,  1804,  1805,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,   403,   403,     0,   696,   168,     0,    73,
       0,     0,     0,     0,     0,     0,   168,     0,     0,   168,
       0,   168,   168,     0,     0,     0,     0,     0,     0,   391,
     353,     0,    76,   392,     0,     0,     0,   481,     0,   393,
      78,    79,   394,   395,   396,   397,   517,   517,     0,     0,
       0,     0,     0,     0,     0,     0,   517,  1039,     0,   517,
    1042,     0,   168,     0,     0,     0,     0,     0,     0,     0,
       0,   353,     0,     0,   624,     0,   624,   624,     0,     0,
       0,     0,     0,   624,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,   353,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1899,
       0,     0,   353,     0,     0,     0,   517,     0,     0,     0,
     517,     0,     0,     0,   517,  1110,   210,     0,   517,  1114,
       0,     0,     0,     0,     0,   168,  1117,  1655,     0,     0,
       0,     0,   264,     0,     0,   856,   858,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1928,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   166,     0,     0,   353,     0,
     517,     0,     0,     0,     0,  1946,     0,     0,     0,     0,
       0,     0,     0,   210,     0,     0,     0,   317,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   358,     0,
       0,   624,  1974,     0,     0,  1975,     0,     0,     0,     0,
       0,     0,     0,     0,   168,     0,     0,     0,     0,  1655,
       0,     0,   210,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   471,     0,   288,   475,     0,
     353,     0,     0,   168,     0,     0,     0,     0,     0,     0,
     294,     0,   295,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   696,   168,     0,     0,     0,     0,
     696,   168,     0,     0,     0,     0,     0,   210,     0,   696,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     264,     0,     0,     0,     0,     0,     0,   517,   696,     0,
     403,  2062,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   624,   624,     0,     0,     0,     0,
       0,   624,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   475,     0,     0,  1018,     0,     0,     0,     0,   210,
     168,     0,     0,   518,   519,     0,     0,   523,     0,     0,
     526,   527,     0,     0,     0,     0,     0,     0,     0,   617,
       0,   634,     0,   353,     0,     0,     0,     0,   517,  1345,
       0,   517,  1349,    14,    15,    16,    17,   147,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,   700,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,   168,   168,     0,     0,
       0,     0,    58,     0,   366,     0,     0,     0,   168,     0,
     606,   607,     0,     0,     0,     0,     0,     0,   210,     0,
       0,     0,     0,     0,     0,     0,   639,     0,     0,     0,
       0,     0,     0,     0,   148,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,   617,     0,
       0,     0,     0,     0,   801,     0,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,     0,     0,   624,  1457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,     0,     0,     0,
     403,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,     0,     0,     0,     0,
       0,   775,     0,   210,   210,     0,     0,     0,     0,   471,
     403,     0,     0,     0,     0,   517,  1510,     0,     0,     0,
       0,     0,     0,     0,   517,  1519,     0,   624,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   353,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   358,     0,     0,     0,     0,   846,     0,
       0,   168,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   471,     0,   915,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   617,     0,     0,     0,     0,
       0,     0,     0,   403,     0,   403,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   210,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   700,     0,     0,   700,   700,     0,   700,     0,
       0,     0,     0,     0,   403,     0,     0,   700,     0,     0,
     700,   700,   700,     0,     0,   353,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,     0,   927,   928,     0,   403,     0,     0,
       0,   427,   624,     0,     0,     0,     0,   935,     0,     0,
       0,     0,     0,     0,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,   484,     0,   484,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   403,     0,     0,     0,
     210,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   471,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   471,
     471,     0,     0,   517,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   471,   517,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   585,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1032,  1033,     0,     0,     0,     0,  1037,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1058,     0,     0,
    1061,  1062,     0,  1065,     0,  1067,  1068,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,   353,
       0,     0,   210,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   801,     0,     0,     0,
       0,     0,     0,     0,  1108,     0,     0,     0,  1112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     353,   353,     0,     0,     0,     0,   358,  1597,     0,     0,
    1600,  1614,     0,     0,     0,     0,  1621,   517,   517,     0,
    1625,     0,  1627,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   517,     0,  1219,  1220,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1236,
       0,     0,     0,   484,     0,     0,     0,     0,     0,   484,
       0,     0,     0,     0,   822,     0,   182,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   247,
     248,     0,   249,    46,     0,    47,     0,   250,     0,     0,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   471,
       0,   517,     0,     0,     0,     0,     0,     0,     0,   517,
       0,   890,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1723,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1236,   456,   700,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   923,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1758,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   353,  -442,  1764,     0,   517,
    2007,  1336,     0,   517,     0,     0,     0,     0,  1343,     0,
       0,  1347,  1779,  1781,     0,     0,   264,     0,     0,  -442,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   957,
       0,     0,     0,     0,     0,     0,  1600,   210,     0,     0,
       0,     0,     0,     0,   517,     0,     0,     0,   617,     0,
       0,     0,     0,   822,   977,     0,     0,   979,     0,   981,
       0,     0,     0,     0,     0,   990,     0,   995,   990,     0,
       0,     0,     0,     0,     0,     0,     0,   358,     0,     0,
       0,   700,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1023,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1025,     0,
     517,   517,     0,     0,     0,     0,     0,     0,     0,  1034,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,     0,     0,  1023,     0,  1455,     0,
       0,     0,     0,     0,   471,   471,  1464,  1465,     0,     0,
       0,     0,   517,     0,     0,     0,  1873,     0,     0,     0,
       0,     0,     0,  1087,     0,  1876,   484,  1878,     0,     0,
    1883,  1887,     0,  1614,     0,     0,     0,     0,  1893,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   700,   700,
     700,     0,     0,   700,   700,     0,     0,     0,     0,     0,
     475,     0,  1118,     0,     0,  1508,     0,     0,     0,     0,
       0,     0,     0,     0,  1517,     0,     0,  1521,     0,  1524,
    1525,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   264,     0,
       0,   427,     0,     0,     0,     0,     0,     0,     0,     0,
    1551,     0,     0,     0,     0,  1226,  1228,     0,     0,     0,
       0,   358,     0,   456,     0,     0,     0,     0,     0,     0,
    1963,     0,     0,     0,     0,  1968,  1970,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   990,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1023,     0,     0,     0,     0,
       0,     0,     0,  1267,     0,     0,     0,     0,     0,     0,
     990,     0,     0,  1649,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2021,
       0,  2024,     0,     0,  2026,  2028,     0,     0,     0,  2031,
    2033,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   484,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1234,     0,   210,     0,     0,     0,     0,
       0,    14,    15,    16,    17,   147,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1521,     0,     0,     0,     0,     0,     0,   264,
       0,  2076,  2078,  2080,     0,     0,     0,   375,     0,     0,
     376,     0,   377,   484,   378,  1335,     0,  1338,     0,     0,
       0,  1718,  2093,     0,     0,     0,     0,     0,     0,     0,
      58,   379,     0,     0,     0,     0,  2104,  2106,  2108,     0,
       0,     0,     0,     0,     0,   358,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,     0,   382,     0,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,   700,   389,   390,     0,     0,  1409,  1409,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,   471,     0,     0,
     391,     0,     0,    76,   392,     0,     0,     0,     0,     0,
     393,   454,    79,   394,   395,   396,   397,     0,  1802,     0,
       0,     0,     0,     0,     0,     0,     0,  1459,     0,     0,
       0,     0,     0,     0,     0,    14,    15,    16,    17,   147,
       0,  1452,     0,     0,     0,     0,     0,  1462,   264,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   456,     0,     0,     0,
       0,   375,     0,     0,   376,     0,   377,     0,   378,     0,
       0,     0,     0,   484,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,   379,     0,     0,   990,     0,
       0,   822,     0,     0,  1858,  1859,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1863,     0,     0,     0,
       0,     0,     0,   380,   381,     0,   382,     0,   383,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,  1545,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1553,
    1554,     0,     0,     0,   391,     0,     0,    76,   392,     0,
       0,     0,   700,     0,   393,  1460,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,   990,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   471,     0,     0,     0,   484,     0,     0,   822,     0,
       0,     0,     0,  1936,     0,     0,     0,     0,     0,     0,
       0,  2123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1393,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   700,     0,
     977,   475,     0,     0,     0,     0,     0,     0,     0,     0,
    1669,  1670,     0,     0,     0,     0,     0,     0,     0,   375,
     484,     0,   376,     0,   377,     0,   378,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   484,  2005,
     822,  1146,     0,   379,    -2,     0,  1148,  -243,  -243,  1149,
    1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,  1159,
    1160,  -337,     0,  1161,  1162,  1163,  1164,  1165,     0,  1166,
       0,   380,   381,     0,   478,     0,   383,  1167,  1168,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,  1169,
     386,   387,   388,     0,   389,   390,     0,     0,  2123,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1393,     0,     0,     0,     0,
       0,  -243,   391,     0,   427,    76,   392,     0,     0,  1747,
     292,     0,   393,    78,    79,   394,   395,   396,   397,     0,
       0,     0,     0,     0,     0,     0,   375,  -184,     0,   376,
       0,   377,     0,   378,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1146,     0,
     379,    -2,     0,  1148,  -244,  -244,  1149,  1150,  1151,  1152,
    1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,  -337,  1793,
    1161,  1162,  1163,  1164,  1165,     0,  1166,     0,   380,   381,
       0,   478,     0,   383,  1167,  1168,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1169,   386,   387,   388,
    1800,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,  1393,  1822,     0,
       0,  1824,     0,     0,     0,     0,     0,     0,  -244,   391,
       0,     0,    76,   392,     0,     0,     0,   292,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,   375,     0,
       0,   376,     0,   377,  -184,   378,  1849,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1146,     0,   379,    -2,     0,  1148,     0,     0,  1149,  1150,
    1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,
    -337,     0,  1161,  1162,  1163,  1164,  1165,     0,  1166,     0,
     380,   381,     0,   478,     0,   383,  1167,  1168,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,  1169,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,   292,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,     0,     0,  -184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     4,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1145,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,   990,
     378,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,  1146,    58,  1147,    -2,     0,
    1148,     0,     0,  1149,  1150,  1151,  1152,  1153,  1154,  1155,
    1156,  1157,  1158,  1159,  1160,  -337,     0,  1161,  1162,  1163,
    1164,  1165,     0,  1166,     0,   380,   381,    61,   478,     0,
     383,  1167,  1168,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,  1169,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,   391,     0,     0,    76,
     423,     0,     0,     0,   292,     0,   393,    78,    79,   394,
     395,   396,   397,     0,     0,     0,     0,     0,     0,     0,
       0,  -184,     4,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1145,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1146,    58,  1147,    -2,     0,  1148,     0,     0,  1149,  1150,
    1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,
    -337,     0,  1161,  1162,  1163,  1164,  1165,     0,  1166,     0,
     380,   381,    61,   478,     0,   383,  1167,  1168,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,  1169,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   423,     0,     0,     0,   292,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,     0,     0,  -184,     4,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,    61,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1608,  1609,  1610,     0,     0,     0,   391,  1611,  1612,    76,
     423,     0,     0,     0,     0,     0,   393,    78,    79,   394,
     395,   396,   397,     0,     0,     0,     0,     0,     0,     0,
       0,  1613,     4,   182,     6,     7,     8,     9,    10,    11,
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
       0,     0,     0,     0,     0,  1608,  1609,  1610,     0,     0,
       0,   391,  1611,     0,    76,   423,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,     0,     0,  1613,     4,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,    61,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,  1599,    76,
     423,     0,     0,     0,     0,     0,   393,    78,    79,   394,
     395,   396,   397,     4,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   375,
       0,    46,   376,    47,   377,     0,   378,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   380,   381,    61,   382,     0,   383,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,     0,
     386,   387,   388,     0,   389,   390,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,    76,   423,     0,     0,     0,
       0,     0,   393,    78,    79,   394,   395,   396,   397,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   147,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   375,     0,    46,   376,    47,   377,
       0,   378,   333,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,     0,   389,
     390,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
      76,   453,     0,     0,     0,     0,     0,   393,   454,    79,
     394,   395,   396,   397,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,   147,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   375,
       0,    46,   376,    47,   377,     0,   378,   333,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   380,   381,     0,   382,     0,   383,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,     0,
     386,   387,   388,     0,   389,   390,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,    76,  1223,     0,     0,     0,
       0,     0,   393,  1224,    79,   394,   395,   396,   397,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   147,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   375,     0,    46,   376,    47,   377,
       0,   378,   333,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,     0,   389,
     390,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,   147,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   375,
       0,    46,   376,    47,   377,     0,   378,   333,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   380,   381,     0,   382,     0,   383,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,     0,
     386,   387,   388,     0,   389,   390,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,     0,     0,    76,   453,     0,     0,     0,
       0,     0,   393,    78,    79,   394,   395,   396,   397,  1945,
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
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,  1973,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,     4,     5,     6,
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
      15,    16,    17,   147,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -462,  -462,     0,  -462,    46,     0,    47,
       0,  -462,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   253,     0,     0,     0,  -789,     0,     0,    78,
      79,     4,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -390,  -390,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -390,
       0,     0,     0,    76,    77,     0,     0,     0,     0,     0,
       0,    78,    79,     4,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,     0,     0,     0,     0,  -391,  -391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -391,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,   252,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,   147,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -462,
    -462,     0,  -462,    46,     0,    47,     0,  -462,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   253,     0,
    1368,     0,  1369,     0,     0,    78,    79,  1370,     0,     0,
      14,    15,    16,    17,   147,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1372,     0,
       0,     0,    76,   953,     0,  1368,     0,  1369,     0,     0,
      78,    79,  1370,     0,     0,    14,    15,    16,    17,   147,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1562,     0,     0,     0,    76,   953,     0,
    1368,     0,  1369,     0,     0,    78,    79,  1370,     0,     0,
      14,    15,    16,    17,   147,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1563,     0,
       0,     0,    76,   953,     0,  1368,     0,  1369,     0,     0,
      78,    79,  1370,     0,     0,    14,    15,    16,    17,   147,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1564,     0,     0,     0,    76,   953,     0,
       0,     0,     0,     0,     0,    78,    79,   252,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,   147,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -462,  -462,     0,  -462,    46,     0,    47,     0,  -462,
       0,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,   147,     0,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     253,   183,     0,     0,   184,   185,     0,    78,    79,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   147,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -462,  -462,     0,  -462,    46,     0,    47,     0,
    -462,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   148,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   253,     0,     0,     0,  -793,     0,     0,    78,    79,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   147,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -462,  -462,     0,  -462,    46,     0,    47,
       0,  -462,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   253,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,   147,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   333,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,   147,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,  1078,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -653,    76,   335,     0,     0,     0,    63,    64,     0,
      78,    79,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,   147,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    76,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,   147,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   334,    76,   335,     0,     0,     0,    63,    64,
       0,    78,    79,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,   147,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    76,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   333,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,  1840,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   335,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,   147,    19,     0,
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
       0,     0,    73,     0,  1842,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   335,     0,     0,     0,
       0,     0,     0,    78,    79,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,   147,    19,
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
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   315,     0,     0,
       0,     0,     0,     0,    78,    79,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,   147,
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
       0,     0,     0,     0,     0,     0,     0,    76,   335,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     147,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -462,  -462,     0,  -462,    46,     0,    47,     0,  -462,     0,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   147,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,     0,     0,  1393,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   376,
       0,   377,     0,   378,     0,     0,     0,     0,    76,   253,
     678,     0,     0,   679,   680,     0,    78,    79,  1146,     0,
     379,     0,     0,  1148,  1866,  1867,  1149,  1150,  1151,  1152,
    1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,  -337,     0,
    1161,  1162,  1163,  1164,  1165,     0,  1166,     0,   380,   381,
       0,   478,     0,   383,  1167,  1168,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1169,   386,   387,   388,
    1393,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,   375,    76,   392,   376,     0,   377,   292,   378,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,  1146,  -184,   379,     0,     0,  1148,     0,
       0,  1149,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  -337,     0,  1161,  1162,  1163,  1164,  1165,
       0,  1166,     0,   380,   381,     0,   478,     0,   383,  1167,
    1168,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1169,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   392,     0,
       0,     0,   292,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
      14,    15,    16,    17,   147,    19,   687,    20,   688,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   375,     0,    46,   376,
      47,   377,     0,   378,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   689,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   690,     0,     0,     0,   292,     0,   393,
      78,    79,   691,   692,   396,   397,    14,    15,    16,    17,
     147,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   375,     0,    46,   376,    47,   377,     0,   378,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   379,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,   422,    76,   423,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,    14,    15,    16,    17,   147,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   375,     0,
      46,   376,    47,   377,     0,   378,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   391,     0,     0,    76,   690,     0,     0,     0,   292,
       0,   393,    78,    79,   394,   395,   396,   397,    14,    15,
      16,    17,   147,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   375,     0,    46,   376,    47,   377,
       0,   378,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,     0,   389,
     390,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   391,     0,     0,
      76,   423,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,    14,    15,    16,    17,   147,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     375,     0,    46,   376,    47,   377,     0,   378,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   379,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,     0,   389,   390,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,    76,   453,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
      14,    15,    16,    17,   147,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   375,     0,    46,   376,
      47,   377,     0,   378,   333,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,   147,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   678,     0,     0,   679,
     680,     0,   572,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,   147,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,   -16,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   252,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,   147,    63,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -462,  -462,     0,  -462,    46,
       0,    47,     0,  -462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
      58,    14,    15,    16,    17,   147,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,   148,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,    77,     0,     0,     0,  -791,     0,
       0,    78,    79,    14,    15,    16,    17,   147,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,   147,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     147,    19,     0,    20,     0,    21,    22,    23,    24,    25,
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
       0,     0,     0,     0,     0,     0,     0,   875,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -666,    76,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   147,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   333,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1755,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,   147,     0,     0,    20,
      76,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -462,  -462,     0,  -462,
      46,     0,    47,     0,  -462,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   148,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,   315,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,   147,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
     147,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,   147,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,    76,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1477,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   959,    76,   953,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   953,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,   147,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,   147,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   299,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
     147,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,   147,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,   333,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   449,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   335,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,   147,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,   147,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   299,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   449,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
     147,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     333,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,   147,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   315,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   953,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,   147,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -462,  -462,     0,  -462,    46,     0,
      47,     0,  -462,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,   147,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
     147,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,    76,     0,    46,     0,    47,    63,    64,     0,
     333,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   953,     0,     0,     0,
      63,    64,     0,    78,    79,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,    14,    15,    16,    17,   147,    78,    79,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -462,  -462,     0,  -462,    46,
       0,    47,     0,  -462,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,   147,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -462,  -462,     0,  -462,
      46,     0,    47,     0,  -462,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   315,     0,    63,    64,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
       0,     0,    78,    79,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   375,     0,    46,   376,    47,   377,     0,
     378,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   379,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   380,   381,     0,   382,     0,
     383,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     384,   385,   372,     0,   386,   387,   388,     0,   389,   390,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   391,     0,     0,    76,
     392,     0,     0,     0,     0,     0,   393,   454,    79,   394,
     395,   396,   397,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   375,     0,    46,   376,    47,   377,     0,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,    14,    15,    16,    17,   147,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -462,  -462,     0,  -462,
      46,     0,    47,     0,  -462,     0,     0,     0,   375,     0,
       0,   376,     0,   377,     0,   378,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,     0,   389,   390,   375,     0,     0,   376,     0,
     377,    73,   378,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,  1608,  1609,  1610,     0,   379,
       0,   391,  1780,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   380,   381,     0,
     382,     0,   383,  1881,    64,    65,    66,    67,    68,    69,
      70,    71,   384,   385,   372,     0,   386,   387,   388,     0,
     389,   390,   375,     0,     0,   376,     0,   377,    73,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1608,  1609,  1610,     0,   379,     0,   391,  1882,
       0,    76,   392,     0,     0,     0,     0,     0,   393,    78,
      79,   394,   395,   396,   397,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  1270,     0,    76,   392,
       0,     0,     0,  1271,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
     986,  1590,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,   821,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,     0,     0,
      76,   392,     0,     0,     0,   292,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,   986,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,  1017,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    1337,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,     0,     0,    76,   392,     0,     0,
       0,  1403,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,     0,     0,
      76,   392,     0,     0,     0,  1468,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,  1872,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  1877,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    1886,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  1967,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  1969,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2023,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  2025,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    2027,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2030,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2032,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2075,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  2077,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    2079,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2103,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2105,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2107,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   669,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   675,     0,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   684,     0,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,     0,
       0,   393,   889,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,   454,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,  1962,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,    14,    15,    16,    17,
     147,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -461,  -461,     0,  -461,    46,     0,    47,     0,  -461,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,   147,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -462,  -462,     0,  -462,    46,     0,    47,     0,  -462,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   153,   391,     4,   365,    74,    74,   181,    74,     1,
     899,   165,   265,    83,   907,   716,    74,   132,   884,     1,
     180,   165,   251,   701,   296,   232,   176,   481,  1155,   229,
     217,   530,   531,    96,   166,   632,   992,   229,    76,    59,
    1799,   625,     1,   623,   760,     4,   165,  1730,   619,   229,
     943,  1730,   229,   393,     4,    56,    57,   651,    59,  1730,
     353,   694,   790,     1,   357,  1021,   619,    59,   796,   619,
     338,  1321,  1322,    74,     0,   789,   884,    59,   541,  1618,
     150,   789,    83,     0,   336,  1139,   140,    98,   551,   787,
      91,    83,  1395,   229,   246,    96,   303,   304,    99,   787,
      59,   353,   103,   229,    96,   357,   221,    99,   229,   309,
       1,   103,   239,     4,    74,   202,   868,   309,    72,  1395,
    1396,    59,   191,   191,    83,   191,   146,    74,  1084,   309,
     593,   807,   309,     1,  1866,  1731,   263,   107,   108,     0,
     141,    99,    74,   144,   896,   146,   273,   201,   152,   825,
     806,   807,   153,   103,   146,   787,    89,   656,   159,   787,
     229,   229,  1139,   229,   146,   166,   177,   230,    59,   825,
      75,   229,     1,   309,  1129,  1473,    72,    72,    75,  1134,
    1870,   140,   297,   309,   254,   155,  1139,   146,   309,   190,
     191,   150,   134,   461,   152,   152,     1,   154,   190,     4,
     152,   181,   156,   455,   205,    72,     1,   791,   146,   789,
     133,   795,   103,   205,   215,   891,   787,   336,   812,   220,
     804,   805,   306,   158,   176,   158,   168,   160,   229,   230,
      59,   191,  1614,   168,   787,   891,  1775,   787,   230,    10,
     309,   309,   201,   309,   191,   246,   316,   310,   949,   287,
    1205,   309,   175,   254,    59,   146,   152,   152,  1854,   191,
     513,   156,   254,   264,    59,  1279,   267,   230,   767,   229,
     118,     1,   177,   274,   103,   267,   391,   978,     1,  2011,
     177,  1147,   229,   367,     1,   152,   370,   288,   289,   156,
     291,   159,   653,   252,    99,   254,   391,   229,   103,   499,
     487,    61,    62,   152,    99,   306,   578,   499,   309,   310,
    1176,  1365,  1366,  1367,  1030,   608,   317,   146,   310,   499,
    1048,    89,   499,   324,   325,   714,    20,   459,   329,    59,
     123,   671,    89,   590,   967,  2094,   455,   296,  1641,  1147,
    1054,   146,    59,   611,   924,  1238,  1054,   310,    72,  1047,
      89,   146,   645,  1246,    10,   425,   608,   316,   296,  1047,
    2043,  1659,   155,   499,  2043,  1641,   367,   635,  1176,   370,
     371,    72,  2043,   499,   642,  2065,   528,   152,   499,  1131,
     632,  1977,   534,   152,   238,   158,   586,   241,  1365,  1366,
    1367,   162,   160,   645,   586,   168,   167,   451,  1780,  1781,
     160,     4,   865,   160,   118,   520,   586,  1362,   262,   586,
     525,  2101,  1365,  1366,  1367,  1047,   146,   177,   272,  1047,
    1096,   160,    84,   550,    72,    72,  2014,   542,   152,   146,
     499,   499,   156,   499,   435,   158,   159,   552,   330,    72,
    1096,   479,   156,   435,   160,   508,   590,   155,  2044,   536,
     586,   152,  2040,    56,    57,   156,    72,   458,   459,   134,
     586,   177,   267,  1713,  1714,   586,   425,   296,   176,   470,
     471,   590,   267,    72,  1054,   607,  1047,   369,   479,  2067,
     481,    72,  1496,  1497,  1498,   149,  1613,  1071,    91,  2085,
     152,  1618,   451,   168,  1047,   151,   160,  1047,   499,    75,
    1882,  1883,  1778,  1231,   152,   152,   162,   508,   156,   156,
      75,   167,   176,   632,    90,    72,   508,   586,   586,   152,
     586,   152,   609,   156,   152,   588,    91,   528,    58,  1395,
    1396,    61,    62,   534,    64,  1491,   152,   157,   141,   499,
     156,   144,   146,   338,   109,   508,    77,    78,   158,   806,
     807,    72,   499,   152,   669,   177,   159,   156,   168,    72,
     675,   152,    72,   166,   168,   156,   296,   499,   825,   684,
    1024,   572,   592,   574,   669,    77,    78,   672,   673,   296,
     675,   741,  1310,   132,    72,   586,   158,   588,   703,   684,
     158,   592,   687,   688,   689,   152,   588,    72,   177,   156,
     592,   602,   158,   152,   158,   606,   607,   156,   876,   177,
     592,  1289,   154,   572,   163,   164,   158,   220,   911,   578,
     134,   177,   152,   177,  1218,   588,   800,   814,  1225,   160,
       3,   152,   274,   592,   891,   156,   158,   160,   639,   152,
     578,   158,   152,   156,   167,   913,   156,   289,  1775,   163,
     164,   652,   160,  1247,   592,   177,   743,   789,   160,   911,
     177,   264,   158,   158,   152,     3,   461,   521,   156,   177,
    1946,   274,   168,   128,   129,   827,   154,   152,  1928,   155,
     158,   156,   177,  1572,  1385,   288,   289,   847,   291,   543,
     146,   147,   148,   160,   846,   549,   158,   784,  1974,   553,
     167,   592,  1296,   306,   742,   706,   963,   708,   158,   710,
     158,    72,   168,   714,   317,  1214,   717,   172,   173,   806,
     807,   324,   325,   146,   147,   148,   329,   177,  1406,   177,
      13,    14,    15,    16,    17,   158,   158,   852,   825,  2015,
    1696,   742,  1698,   158,   152,   168,   168,   576,   863,   578,
     152,   866,   160,  1880,   177,   870,    13,    14,    15,    16,
      17,   160,   177,   592,   367,  1892,   158,   370,  1661,  1662,
      13,    14,    15,    16,    17,  1641,   163,   158,   154,   158,
     575,     3,   158,   170,   171,   177,   787,   592,   789,    72,
     154,    13,    14,    15,    16,    17,   177,   592,   177,   158,
     801,   165,   166,   935,   891,    83,   160,   808,    83,   168,
    1488,   151,   158,   814,   160,    72,   817,  1271,   158,   963,
     800,    96,   132,   152,    99,   826,   827,   828,   103,    72,
    1083,   811,   132,   152,  1961,    47,    48,   156,    50,  1096,
     635,   176,   152,    55,   963,   846,   156,   154,   578,   156,
      72,   134,   152,   163,   164,   458,   156,   160,  1219,  1220,
     152,   578,   592,   163,   164,   152,   176,   470,   471,  1463,
     174,   152,   150,   160,     3,   592,  1470,   134,   159,   609,
     167,   882,   883,   884,    13,    14,    15,    16,    17,   153,
      62,   152,  1215,  1487,  1217,  1475,   160,   146,   147,   148,
     901,   156,   884,   151,  1091,   152,   161,   118,  1115,   158,
     158,   189,  1778,   132,    58,   190,   154,    61,    62,   168,
      64,  1053,  1054,   154,  1817,   884,   379,    99,   177,   154,
     205,   155,   156,   152,   935,   159,   132,   156,   110,   898,
     112,   154,   114,    72,   163,   164,   884,   948,   898,  1403,
     154,   404,   405,   156,   154,   230,   152,   154,   161,   159,
     156,  1229,   159,   154,   146,   147,   148,   163,   164,   572,
     154,   152,   425,  1225,   154,   159,   254,   152,   158,   254,
     158,  1370,   154,   984,   156,   157,   168,   107,   108,   154,
     152,   992,   267,   884,   159,   177,  1565,  1254,   152,   602,
      22,  1570,   455,   606,   607,  1234,   152,   898,   154,  1096,
     156,   158,   154,   867,  1468,   152,   158,   154,  1133,   156,
    1021,    99,   152,  1024,   154,  1918,   156,   107,   108,  1623,
     158,  1146,  1486,   205,   104,   158,   639,  1017,   316,   109,
     110,   111,   112,   113,   114,   115,  1047,   154,  1163,   652,
     158,   158,  1053,  1054,   784,   884,  1208,   152,   336,   111,
     112,   113,   114,   115,  1321,  1215,   104,  1217,   152,  1139,
    1244,   109,   110,   111,   112,   113,   114,   115,   116,   884,
    1946,   876,   120,  1084,   122,  1679,   152,   154,   154,   884,
     156,   158,   887,   898,    89,   267,  1690,   269,   270,  1352,
     721,   722,   723,   706,  1465,   708,  1225,   710,  1974,  1253,
    1254,   714,   154,  1093,   717,   153,   158,   154,   913,   160,
     152,   158,   151,  1395,   156,   160,   154,   152,   152,   301,
     158,   156,   156,   154,   154,  1254,   308,   158,   158,   742,
      13,    14,    15,    16,    17,    18,   152,   425,  1149,  2015,
     156,  1152,  1153,  1154,   884,  1329,  1330,   154,   154,   160,
     435,   158,   158,  1258,   157,  1147,   338,   884,  1322,   154,
     154,   155,   344,   158,   346,  1176,   154,   455,   154,   167,
    1139,  1182,   158,    13,    14,    15,    16,    17,  1147,  1190,
     176,  1448,  1193,  1194,  1176,   154,  1197,   154,   801,   158,
     154,  1193,  1194,   152,   158,   808,  1907,  1208,   154,  1147,
     382,   154,   158,  1807,  1808,   158,   154,  1176,   154,   118,
     158,   152,   158,   826,   152,   828,   121,   154,   123,   124,
     125,   158,   154,   508,  1321,  1322,   158,   158,  1176,   154,
     152,  1242,    72,   158,  1194,    13,    14,    15,    16,    17,
      18,   154,   530,   531,  1255,   158,  1147,   152,  2048,   169,
     155,   156,  2052,   435,  1244,   160,   161,   720,   165,   166,
    1271,  1386,  1387,   164,   728,   729,   730,   731,  1279,   882,
     883,   884,  1562,  1563,  1564,  1176,  1136,  1137,  1138,   461,
     162,   463,   464,   126,   127,  1365,  1366,  1367,   901,   174,
    1370,  1371,   132,  1194,  1448,   155,   478,   130,   131,  1424,
    1311,   132,  1292,  1376,  1675,   157,   158,   154,  1147,   157,
     158,   156,   152,   163,   164,  1919,   156,   158,   159,  1448,
     157,   158,   935,   163,   164,   154,   508,   157,   158,   157,
     158,   154,  1147,   157,   158,   157,   158,  1176,   154,  1329,
    1330,   154,  1147,   154,   632,   157,   528,   157,   158,   157,
     158,   533,   134,   535,   134,  1194,   157,   158,   158,  1641,
     159,  1176,   157,   158,   152,  1376,  1576,   159,   656,  1380,
     154,  1176,  1383,   555,  1576,   557,   558,   154,  1193,  1194,
      13,    14,    15,    16,    17,    18,  1576,   152,  1193,  1576,
     157,   158,  1403,   575,  1998,   154,  1365,  1366,  1367,  1368,
    1369,  1370,  1371,  1376,   157,   158,   588,  1147,  1368,   157,
     158,   154,  1423,   154,  1425,   157,   158,   157,   158,   154,
    1147,   157,   158,  1425,  1229,   157,  1395,  1396,   156,   611,
    1576,   613,   614,   157,   158,   160,  1176,   157,   158,   160,
    1576,   158,   159,   157,   158,  1576,  1713,  1395,  1396,  1176,
     157,   158,   160,   635,   636,    77,    78,  1468,   158,   159,
     642,    13,    14,    15,    16,    17,    18,  1368,  1280,  1281,
    1640,   160,   724,   725,    70,  1486,   726,   727,   157,   767,
    1491,   732,   733,  1583,  1584,  1496,  1497,  1498,  1652,  1466,
    1467,  1588,  1565,   152,  1395,  1396,   959,  1570,  1652,    69,
      78,   964,   157,    18,  1473,  1578,   176,  1632,  1576,   158,
     160,   152,   975,  1473,   177,   154,   202,   154,   160,   177,
     160,   157,   157,  1652,    18,   151,   154,    13,    14,    15,
      16,    17,   151,   154,   154,  1745,  1149,   154,   154,  1152,
    1153,  1154,   154,  1745,   154,   154,   160,   160,   154,   160,
    1714,    70,   177,  1368,  1565,  1745,  1395,  1396,  1745,  1570,
     154,   176,   154,  1176,   154,  1576,   151,  1578,   176,  1182,
     154,   160,  1473,   160,  1585,   154,   158,  1190,   158,    62,
    1395,  1396,   154,  1688,  1197,   154,    72,   158,  1599,   154,
    1395,  1396,  1565,   154,   154,   154,   154,  1570,   154,  1745,
     157,  1612,   157,   154,   154,  1578,   154,   154,   154,  1745,
    1425,   899,   154,   154,  1745,   154,  1713,  1714,   154,   157,
    1425,   104,   154,    13,  1794,   108,   151,   176,   111,  1242,
     113,   154,   158,   151,   154,   152,   158,  1899,  1649,   152,
     152,   152,  1255,  1805,   152,   152,   132,  1607,    14,   159,
     336,    74,   158,   339,   104,  1395,  1396,    91,  1473,   109,
     110,   111,   112,   113,   114,   115,   152,   353,  1395,  1396,
     156,   357,  1641,   177,   159,  1748,   177,   163,   164,   157,
     157,   177,  1866,   151,  1147,  1696,  1655,  1698,   177,   160,
    1659,   160,   158,  1641,   876,  1655,   177,   154,    88,  1659,
    1978,   157,   152,   153,   158,   887,  1607,   154,   154,   158,
     158,   154,   157,  1975,   104,   157,   154,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   151,   151,    80,   158,
     152,   913,   152,  1903,  1745,   152,   177,  1748,   177,   151,
    1641,   177,   924,   152,   177,   177,   177,   177,  1759,   177,
     177,   933,  1763,   154,  1655,   151,   151,   151,  1659,   158,
     151,   154,  1225,   160,  1928,   157,  1777,  1380,  1607,   455,
    1383,   160,   157,   157,   257,  1748,  1787,   157,   104,   151,
     154,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1863,  1606,  1607,  1804,  1805,   159,   159,   121,  1261,  1262,
    1263,  1606,  1641,   151,   154,  1268,  1269,   154,   154,  1778,
    1423,   154,   157,   157,   154,  1977,   151,   177,   159,  1899,
     158,   154,   152,   154,   158,  2009,  1641,  2011,   110,   152,
    1778,  1928,   152,  2043,   151,   157,  1641,   151,   157,   157,
    1655,  2043,   154,   151,  1659,   160,   157,   154,  1030,   154,
     536,   177,  1863,  2043,   154,   154,  2043,   340,   341,  1870,
     343,   154,   345,  1874,   154,    75,  2050,    75,  1879,   152,
     151,   177,  1054,   177,   177,   154,  1866,  1778,  2048,   154,
     157,    90,  2052,  2053,   157,   151,   160,   151,   151,   154,
    1863,    75,   154,  1904,   154,  1975,   567,  2043,   154,   382,
     154,  1641,   154,   156,   590,   155,   177,  2043,  1193,  1194,
     168,   168,  2043,    75,  1641,   159,   177,  2087,   177,   151,
     151,   151,   608,   609,   158,   154,  1214,   154,   154,   154,
     153,   151,   168,     1,   151,   168,     4,  1225,  1949,  1778,
     159,   104,  1953,  2113,   158,   152,   632,  2117,  2132,    75,
    1765,   152,   151,   168,   157,  1966,   168,   153,   110,   645,
    1765,   177,   110,  1778,  2043,  2043,  1977,  2043,  1979,  2139,
     151,  2044,  1585,  1778,   153,  2043,    62,  1946,   154,  1990,
     177,  1992,  1993,   154,   159,   151,  1599,  2112,   152,   154,
     151,    59,   177,    75,   215,   478,   154,   154,  1946,  1612,
     177,  1688,  1286,  2128,   177,  1974,    74,  2018,  1385,   693,
     736,  1193,  2085,    99,   737,    83,   734,  1165,   424,  2009,
     735,  2011,   738,  1176,   110,   111,  1974,  2101,    96,  1641,
    2011,    99,  2043,  2044,  1786,   103,  1649,  2040,  1778,  1778,
    2095,  1908,  2044,  2054,  2094,  1946,  2015,  1229,  2082,  1633,
    1633,  1778,  1975,  1235,  2065,  2053,  2117,   743,  1974,    49,
    2050,  1197,   111,  1863,   547,   259,  1936,  2015,   154,  1584,
    1371,  2044,   140,  1974,  2085,   939,  1190,   814,   146,  1655,
     488,   853,   150,  2085,   903,   153,   154,     0,  2099,   948,
    2101,   759,   759,  2083,   759,  1554,    -1,   165,   784,   602,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1946,    -1,  2120,
      -1,    -1,  2085,    -1,  2015,  2126,    -1,    -1,    -1,   205,
      -1,   189,   190,   191,    -1,  2136,    -1,    -1,    -1,  2140,
      -1,  1946,    -1,   201,   202,  1974,    -1,   205,    -1,  2150,
    1425,  1946,  2132,    -1,   916,    -1,  1759,    -1,    -1,    -1,
    1763,    -1,    -1,    -1,    13,    14,    15,    16,    17,  1974,
      -1,   229,   230,    -1,  1777,    -1,    -1,    -1,    -1,  1974,
      -1,    -1,    -1,    -1,  1787,    -1,  2015,    -1,   246,    -1,
      -1,   267,   853,  1646,    -1,    -1,   254,    -1,    -1,    -1,
      -1,  1804,    -1,    -1,    -1,  1377,    -1,    -1,    -1,   267,
    2015,    -1,    -1,    -1,    -1,    -1,  1946,    -1,    -1,     3,
    2015,    -1,    -1,    72,    -1,   301,    -1,    -1,    -1,  1946,
      -1,    -1,   308,    -1,    -1,   911,    -1,    -1,   914,    74,
      -1,    -1,    -1,   301,  1974,    -1,    18,    -1,    -1,    -1,
     308,   309,   310,  1425,    -1,   916,    -1,  1974,   316,    -1,
      -1,    96,   338,    -1,  1026,  1149,    -1,  1870,   479,    -1,
     481,  1874,    -1,    -1,    -1,    -1,  1879,    -1,   336,   337,
     338,  1043,  1044,   132,    -1,  2015,    -1,   963,    -1,    61,
      62,    63,    64,    -1,  1572,   353,    -1,    -1,  2015,   357,
      -1,  1904,    -1,   152,    -1,    -1,   382,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,   153,    72,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   104,   391,    -1,    -1,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,  1949,    -1,   132,    -1,
    1953,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1966,    -1,  1026,    -1,   425,   152,   153,
     428,    -1,    -1,    -1,    -1,   159,  1979,   435,    -1,   163,
     164,    -1,  1043,  1044,   156,   461,    -1,  1990,    78,  1992,
    1993,    -1,    -1,   451,   229,   230,    -1,   455,    -1,   152,
     153,   459,   478,   461,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   246,    -1,    -1,   104,  2018,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,   240,    -1,    -1,  1606,    -1,    -1,   104,    -1,    -1,
      -1,   499,   109,   110,   111,   112,   113,   114,   115,   116,
     508,  2054,    -1,  1906,    -1,    -1,    -1,    -1,    -1,   535,
      -1,    -1,  2065,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     528,    -1,   530,   531,   309,   310,   534,    -1,   536,   555,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,   156,
      -1,    -1,    -1,    -1,    -1,    -1,  2099,   104,  2101,   575,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   574,  2120,    -1,    -1,
      -1,    -1,    -1,  2126,    -1,    -1,    -1,    -1,   586,    -1,
     588,    -1,   590,  2136,   592,   611,    -1,  2140,    -1,  1423,
    1302,  1303,    -1,    -1,    -1,    -1,    83,  2150,    -1,  1225,
     608,   609,    -1,   611,  1316,  1317,    -1,    -1,    -1,   635,
      -1,   619,    -1,    -1,    -1,   623,   642,    -1,    -1,    -1,
     177,    -1,    -1,    -1,   632,    -1,  1748,    -1,  1254,    -1,
      -1,    -1,    -1,    -1,   642,    -1,    -1,   645,  1350,  1351,
      -1,    -1,    -1,  1765,    -1,    -1,    -1,    -1,   656,    -1,
      -1,    -1,    -1,   814,    -1,    -1,   817,    -1,    -1,    -1,
      -1,   669,    -1,   150,   672,   673,    -1,   675,    -1,   428,
      -1,    -1,    -1,    -1,   459,    -1,   684,    -1,   165,   687,
     688,   689,    -1,    -1,    -1,   444,    -1,    -1,   447,    -1,
      -1,  1302,  1303,     4,     5,     6,     7,     8,     9,    10,
      11,    12,   189,    -1,    -1,  1316,  1317,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   499,   202,   109,   110,   111,   112,
     113,   114,   115,   508,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,  1854,    -1,    -1,   743,    -1,    -1,    -1,  1350,
    1351,    -1,    -1,   528,    -1,   504,    -1,    -1,    -1,   534,
      -1,   759,   760,    -1,    65,    -1,    -1,    -1,    -1,   767,
     153,    -1,    -1,   156,    -1,  1599,    -1,   254,    57,    58,
      59,    60,    61,    62,    63,    64,   784,    -1,  1612,   787,
      -1,   789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   574,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,   807,
      -1,   586,   104,   588,    72,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1649,    -1,   825,    -1,   827,
      -1,    -1,  1448,   984,   850,    -1,    -1,    -1,    -1,   316,
      -1,   992,    -1,    -1,  1546,  1547,   104,    -1,   846,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   336,
     876,    -1,    -1,   155,    -1,    -1,  1978,    -1,   160,    -1,
    1021,   887,    -1,  1024,   132,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,
      -1,    -1,    -1,   891,   152,   153,    -1,   913,   156,    -1,
     898,   899,    -1,    -1,    -1,   163,   164,    -1,    -1,    -1,
      -1,    -1,   104,   911,    -1,   913,    -1,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,   924,    -1,    -1,    -1,
      -1,    -1,    -1,  1084,    -1,  1759,    -1,    -1,    -1,  1763,
     132,    -1,    -1,    -1,    -1,  1546,  1547,    -1,   425,    65,
      66,    67,    68,  1777,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,  1787,    -1,   963,  1668,    -1,    -1,    -1,
      -1,   163,   164,  2085,    -1,    -1,    -1,    -1,   455,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1588,   104,    -1,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,  1703,    -1,    -1,    -1,    -1,  1708,  1709,    -1,    -1,
     759,   760,   787,    -1,   789,    -1,    -1,    -1,    -1,    -1,
     769,    -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1030,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,    -1,    -1,  1870,    -1,    -1,  1047,
    1874,    -1,   827,   530,   531,  1879,  1054,    -1,   174,   536,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1668,    -1,    -1,
      -1,   846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1904,    -1,    -1,    -1,   833,    -1,    -1,    -1,   837,    -1,
      -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,  1096,    -1,
       1,    -1,  1703,     4,    -1,    -1,    -1,  1708,  1709,    -1,
      -1,    -1,    -1,   590,    -1,    -1,    -1,    -1,    -1,    -1,
    1271,    -1,    -1,    -1,    -1,  1949,    -1,    -1,  1279,  1953,
      -1,    -1,   609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1139,  1966,    -1,    -1,    -1,    -1,    -1,    -1,  1147,
      -1,    -1,    -1,    -1,    -1,   632,    -1,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1990,    -1,  1992,  1993,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1193,  1176,   656,
      -1,   104,    83,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,  2018,  1193,  1194,    -1,    99,    -1,
      -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1208,    -1,    -1,  1229,   104,    -1,  1214,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1225,    -1,    -1,
    2054,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,
     163,  2065,    -1,    -1,    -1,   146,    -1,    -1,    -1,   150,
      -1,    -1,  1403,   154,    -1,    -1,  1254,    -1,    -1,    -1,
    1258,    -1,    -1,   164,   165,   166,   743,    -1,    -1,    -1,
     160,    -1,  1047,    -1,    -1,  2099,    -1,  2101,  1053,  1054,
      -1,  1030,    -1,  1899,    -1,    -1,    -1,    -1,   189,    -1,
     767,    -1,    -1,    -1,    -1,    -1,  2120,    -1,    -1,    -1,
     201,   202,  2126,    -1,   205,    -1,    -1,   784,    -1,    -1,
      -1,    -1,  2136,    -1,    -1,    -1,    -1,  1468,    -1,    -1,
      -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,   806,
     807,    -1,    -1,    -1,    -1,  1486,    -1,    -1,    -1,    -1,
    1491,    -1,  1358,    -1,    -1,  1496,  1497,  1498,   825,    -1,
      -1,   252,  1101,   254,    -1,  1104,    -1,    -1,    -1,  1975,
    2062,  1377,    -1,    -1,    -1,    -1,   267,  1365,  1366,  1367,
    1368,    -1,  1370,  1371,    -1,    -1,    -1,    -1,  1376,  1377,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   296,    -1,  1395,  1396,    -1,
     301,    13,    14,    15,    16,    17,    -1,   308,    -1,  1425,
      -1,    -1,    -1,    -1,   891,   316,    -1,    -1,    -1,    -1,
      -1,    -1,   899,    -1,    -1,    -1,    -1,  1425,    -1,    -1,
      -1,    72,    -1,  1208,    -1,   336,   104,   338,   339,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
    1448,    -1,   353,    -1,    -1,    -1,   357,    -1,    -1,    -1,
      72,  2062,    -1,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,  1473,    -1,  1475,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   963,    -1,   156,    -1,
     391,   132,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,   104,   163,   164,   425,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,    -1,  1311,   120,    -1,   122,
     152,   153,    -1,    -1,   156,  1696,    -1,  1698,    -1,  1298,
     451,   163,   164,    -1,   455,    -1,    -1,    -1,  1307,    -1,
     461,    -1,    -1,    -1,    -1,    -1,    -1,  1565,  1566,    -1,
     153,    -1,  1570,   156,  1572,    -1,    -1,    -1,  1576,    -1,
    1578,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
    1606,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,  1376,    -1,    -1,    -1,    -1,    -1,   104,  1606,  1607,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1096,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   530,
     531,    -1,    -1,    -1,   535,   536,    -1,    -1,    -1,    -1,
      -1,    -1,   104,  1641,    72,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1652,    -1,    -1,  1655,    -1,    -1,
      -1,  1659,    -1,   160,    -1,    -1,   567,    -1,    -1,    -1,
      -1,   572,    -1,    -1,   575,   576,   104,   578,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   590,
    1688,   592,    -1,    -1,    -1,    -1,    -1,    -1,   160,    -1,
      -1,    -1,    -1,    -1,   132,   606,    -1,   608,   609,    -1,
     611,    -1,    -1,    -1,    -1,  1713,  1714,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
      -1,   632,  1730,  1731,   635,   163,   164,  1214,   639,    -1,
      -1,   642,    -1,    -1,   645,    -1,   647,  1745,  1225,  1765,
    1748,    -1,    -1,    -1,   104,   656,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1765,   669,    -1,
      -1,   672,   673,    -1,   675,    -1,    -1,  1254,    -1,    -1,
    1778,    -1,    -1,   684,    -1,    -1,   687,   688,   689,    -1,
    1565,    -1,    -1,    -1,    72,  1570,    -1,    -1,    -1,    -1,
      -1,  1576,     1,  1578,    -1,     4,   156,  1805,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1566,    13,    14,
      15,    16,    17,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,   104,   743,    -1,  1321,  1322,   109,   110,   111,   112,
     113,   114,   115,   116,   132,    -1,  1854,   120,    -1,   122,
      59,    -1,    -1,    -1,    -1,  1863,   767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   784,    83,   163,   164,    -1,    -1,    -1,
     153,    -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1899,    -1,    -1,   103,   806,   807,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   825,    13,    14,    15,    16,    17,
    1928,    -1,    -1,    -1,    -1,    -1,    -1,   132,  1936,    -1,
      -1,   140,    -1,    -1,    -1,    -1,    -1,   146,  1946,   850,
      -1,    -1,   853,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,  1978,    -1,    -1,    -1,   165,    -1,   163,   164,
    1745,  1448,    -1,  1748,    -1,   876,  1974,  1975,    -1,  1977,
    1978,  1730,  1731,   884,    72,    -1,   887,    -1,    -1,    -1,
     891,   190,    -1,    -1,    -1,    -1,    -1,   898,   899,    -1,
      -1,    -1,   201,   202,    -1,    -1,    -1,    -1,    -1,    -1,
     911,    -1,   913,   914,    -1,   916,   104,  2015,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   104,
    1805,   230,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   132,  2043,  2044,   246,    -1,    -1,
      -1,    -1,   251,   252,    -1,   254,    -1,    -1,    -1,    -1,
      -1,    -1,   963,    -1,   152,   153,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   163,   164,   276,    -1,    -1,
     155,   280,    -1,    -1,    -1,    -1,   285,  2085,  1863,    -1,
      -1,    -1,    -1,    -1,    -1,  1572,    -1,   296,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1854,    -1,    -1,    -1,    -1,
      -1,   310,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,  1026,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,    -1,
     339,   104,  1043,  1044,    -1,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   353,    -1,    -1,   120,   357,   122,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    72,    -1,  1652,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1932,    -1,    -1,    -1,  1936,   132,    -1,
     153,    -1,    -1,   156,    -1,  1096,    -1,     1,    -1,    -1,
      -1,    -1,  1977,    -1,    -1,   104,    -1,    -1,   152,   153,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1977,    -1,
      -1,    -1,    -1,   132,    -1,    -1,  1713,  1714,  1139,    -1,
      -1,    72,    -1,    -1,    -1,    -1,  1147,    -1,    -1,    -1,
      -1,    -1,   451,   152,   153,    59,   455,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,  2043,  2044,
      -1,    -1,    -1,   104,    -1,  1176,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1193,  1194,  2043,  2044,    -1,    -1,    -1,   103,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2085,    -1,    -1,  1214,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,  1225,    -1,    -1,    -1,  1229,    -1,
      -1,    -1,   163,   164,    -1,    -1,  2085,   536,    -1,    -1,
    1241,    -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1253,  1254,    -1,    -1,    -1,  1258,    -1,    -1,
      -1,   165,    -1,    -1,    -1,    -1,    -1,    -1,   567,    -1,
      -1,    -1,    -1,   572,    -1,    -1,   102,   576,   104,   578,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   590,    -1,   592,    -1,    -1,    -1,    -1,   202,    -1,
      -1,  1302,  1303,    -1,    -1,    -1,    -1,    -1,    -1,   608,
     609,    -1,    -1,    -1,    -1,  1316,  1317,    -1,    -1,    -1,
    1321,  1322,    -1,    -1,   623,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,   632,    -1,    -1,    -1,    -1,   637,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,    -1,  1350,
    1351,  1928,    -1,    -1,    -1,    -1,    -1,  1358,    -1,    -1,
      -1,    -1,    -1,    -1,  1365,  1366,  1367,  1368,  1369,  1370,
    1371,    -1,   276,    -1,   104,    -1,  1377,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,   296,    -1,  1395,  1396,    -1,    -1,    -1,    -1,
      -1,    -1,   132,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,  1425,    -1,   156,    -1,    -1,    -1,
      -1,   132,   336,   163,   164,   339,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   743,    -1,   176,  1448,    -1,   353,
      -1,   152,   153,   357,    -1,    -1,    -1,    -1,   159,    -1,
      -1,   760,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,  1473,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,   784,    -1,    -1,    -1,    -1,
     789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   806,   807,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   152,   153,
      -1,    -1,   156,    -1,    -1,    -1,   825,    -1,    -1,   163,
     164,   104,   132,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,  1546,  1547,    -1,    -1,    -1,
      -1,   455,   152,   153,   853,    -1,   156,    -1,    -1,   132,
      -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
      -1,  1572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,   884,    -1,  1588,    -1,    -1,
     163,   164,   891,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,    -1,    -1,  1606,  1607,    -1,    -1,    -1,
      -1,    -1,   911,    -1,    -1,   914,    -1,   916,    -1,    -1,
     104,    -1,   921,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   536,    -1,    -1,    -1,    -1,     1,    -1,    -1,
    1641,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,  1652,    -1,    -1,  1655,    -1,    -1,    -1,  1659,    -1,
      -1,    -1,    -1,   567,   963,    -1,    -1,  1668,   152,   153,
      -1,    -1,   576,    -1,   578,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,   590,  1688,   592,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,     1,
      -1,    -1,  1703,    -1,   608,   609,    -1,  1708,  1709,    -1,
      -1,   104,  1713,  1714,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,   104,  1026,   632,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   103,
      -1,   645,    -1,    -1,  1043,  1044,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,  1765,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,   152,    -1,    -1,  1778,    -1,    -1,
     104,    -1,   146,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,  1096,    -1,   101,
     102,   165,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,   152,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,   743,
    1139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1147,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1176,    -1,    -1,
     784,    -1,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,    -1,    -1,  1194,   149,    -1,  1899,    -1,
      -1,   104,   806,   807,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   825,    -1,   176,    -1,    -1,  1225,  1928,    -1,    -1,
      -1,    -1,   296,    -1,    -1,  1234,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1946,    -1,    -1,    -1,   853,
      -1,    -1,    -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   336,  1974,  1975,   339,    -1,  1978,    -1,    -1,
     884,    -1,    -1,    -1,    -1,    -1,    -1,   891,    -1,   353,
      -1,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,  1302,  1303,    -1,    -1,   911,    -1,    -1,
     914,    -1,   916,    -1,  2015,    -1,    -1,  1316,  1317,    -1,
      -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1350,  1351,    -1,    -1,    -1,    -1,    59,    -1,   963,
      -1,  2062,    -1,    -1,    -1,    -1,  1365,  1366,  1367,  1368,
    1369,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   455,    -1,    -1,    -1,    -1,  1395,  1396,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1026,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,  1043,
    1044,    -1,    -1,    -1,   146,    -1,    -1,    -1,   150,  1448,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   536,    -1,  1473,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,   189,    -1,    -1,
      -1,    -1,  1096,    -1,    -1,    -1,    -1,    -1,    -1,   201,
      -1,    -1,    -1,   567,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   576,    -1,   578,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,   592,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    59,    -1,  1147,   608,   609,    -1,  1546,  1547,    -1,
     252,    -1,   254,    -1,    -1,    -1,    -1,   259,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,   632,    -1,
      -1,    -1,  1176,    -1,    -1,    -1,    -1,    -1,    -1,  1578,
      -1,   645,    -1,    -1,    -1,   103,    -1,    -1,    -1,  1588,
    1194,    -1,    -1,    -1,   296,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1607,    -1,
      -1,    -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,    -1,
      -1,  1225,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,
      -1,    -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,  1253,
    1254,    -1,    -1,  1652,    -1,    -1,  1655,    -1,    -1,    -1,
    1659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1668,
      -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   743,
      -1,    -1,    -1,   201,    -1,    -1,    -1,    56,    57,   391,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1302,  1303,
      -1,    -1,    -1,    -1,  1703,    -1,    -1,    -1,    -1,  1708,
    1709,    -1,  1316,  1317,  1713,  1714,    -1,  1321,  1322,    -1,
     784,    -1,    91,   425,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1731,    -1,   252,    -1,   254,    -1,    -1,    -1,
      -1,   259,   806,   807,    -1,    -1,  1350,  1351,    -1,   451,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    73,   144,    -1,    -1,   296,  1778,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   853,
     159,  1395,  1396,    -1,    -1,    -1,    -1,    -1,   316,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     884,   120,   121,   122,    -1,   124,   125,   891,   530,   531,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1448,    -1,    -1,   911,    -1,    -1,
     914,   220,   916,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,   162,   163,   164,   165,   166,   167,   168,
     572,    -1,    -1,   391,   576,    -1,   578,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     592,    -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,   963,
    1899,    -1,    -1,    -1,    -1,   274,    -1,   425,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     289,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1928,
      -1,    -1,    -1,   451,    -1,    -1,    -1,   306,    -1,    -1,
      -1,    -1,  1546,  1547,    -1,    -1,    -1,  1946,   317,    -1,
      -1,    -1,    -1,    -1,   656,   324,   325,    -1,    -1,    -1,
     329,    -1,  1026,    -1,    -1,    -1,    -1,   669,    -1,    -1,
     672,   673,    -1,   675,    -1,  1974,  1975,    -1,    -1,  1043,
    1044,    -1,   684,    -1,  1588,   687,   688,   689,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   367,    -1,
      -1,   370,    -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   530,   531,    -1,    -1,  2015,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1096,    -1,    -1,    -1,    -1,  1641,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2044,    -1,    -1,  1652,    -1,
      -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,   576,    -1,
     578,    -1,    -1,  2062,  1668,   767,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1147,    -1,    -1,    -1,    -1,    -1,   458,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1703,
      -1,   470,   471,    -1,  1708,  1709,    -1,    -1,    -1,  1713,
    1714,    -1,  1176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1194,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   669,    -1,    -1,   672,   673,    -1,   675,    -1,    -1,
      -1,  1225,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,
     688,   689,     1,    -1,  1778,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   884,    -1,    -1,    -1,    -1,    -1,    -1,  1253,
    1254,    -1,    -1,    -1,    -1,    -1,   898,   899,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    93,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    -1,   602,    -1,    -1,    -1,    -1,  1302,  1303,
     127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,
      -1,    -1,  1316,  1317,    83,    -1,    -1,  1321,  1322,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   652,    -1,    -1,  1350,  1351,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
      -1,   150,    -1,    -1,  1928,    -1,    -1,    -1,    -1,    -1,
      -1,  1395,  1396,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1946,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     189,    -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,
    1974,  1975,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     898,   899,    -1,    -1,  1448,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2015,    -1,    -1,    -1,    -1,    -1,    -1,   305,    -1,
      -1,    -1,    -1,   252,    -1,   254,    -1,    -1,    -1,    -1,
     259,    -1,   801,    -1,    -1,    -1,    -1,  1139,    -1,   808,
      -1,    -1,    -1,    -1,    -1,  1147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2062,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   296,    -1,    -1,
      -1,    -1,    -1,    -1,  1176,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1546,  1547,    -1,    -1,    -1,   316,    -1,    -1,
      -1,    -1,  1194,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1214,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1588,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1258,    -1,    -1,    -1,
      -1,    -1,   391,    -1,    -1,    -1,    -1,   454,    -1,   456,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1641,   465,   466,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1652,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1668,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1139,   451,    -1,    -1,    -1,    -1,    -1,    -1,  1147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1703,
      -1,    -1,    -1,    -1,  1708,  1709,    -1,    -1,    -1,  1713,
    1714,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1176,    -1,
      -1,    -1,    -1,  1365,  1366,  1367,  1368,  1369,  1370,  1371,
      -1,    -1,    -1,    -1,    -1,    -1,  1194,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   573,    -1,    -1,    -1,
      -1,    -1,    -1,  1395,  1396,    -1,  1214,    -1,    -1,    -1,
      -1,   530,   531,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,  1778,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    1258,    -1,    -1,   572,    51,    -1,    53,   576,    -1,   578,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   592,    -1,    72,    -1,    -1,    -1,    -1,
      -1,  1473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1152,  1153,  1154,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,  1197,    -1,
      -1,    -1,    -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,
     669,    -1,    -1,   672,   673,    -1,   675,  1365,  1366,  1367,
    1368,  1369,  1370,  1371,    -1,   684,    -1,    -1,   687,   688,
     689,    -1,    -1,    -1,  1928,    -1,    -1,    -1,    -1,    -1,
    1572,    -1,    -1,  1242,    -1,    -1,    -1,  1395,  1396,    -1,
      -1,    -1,  1946,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,
    1974,  1975,    -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,  1641,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2015,    -1,  1655,    -1,  1473,    -1,  1659,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1688,    -1,    -1,    -1,
     877,   878,    -1,    -1,    -1,    -1,    -1,    -1,  2062,    -1,
      -1,   888,   889,   890,    -1,    -1,    -1,   894,    -1,    -1,
      -1,  1380,    -1,    -1,  1383,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1572,   884,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   898,
     899,    -1,    -1,    -1,    -1,    -1,  1778,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1607,
     977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1023,  1655,    -1,    -1,
      -1,  1659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1688,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1070,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1079,  1080,  1081,  1082,    -1,    -1,    -1,    -1,
    1087,  1088,    -1,    -1,    -1,    -1,   239,    -1,    -1,    -1,
    1097,    -1,    -1,    -1,    -1,    -1,  1585,    -1,    -1,    -1,
     253,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     263,  1118,    -1,    -1,  1121,    -1,  1123,    -1,    -1,    -1,
     273,    -1,    -1,    -1,  1946,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   291,   292,
    1778,    -1,    -1,    -1,    -1,   298,   299,    -1,    -1,    -1,
      -1,    -1,  1974,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,  1176,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   335,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1139,    -1,    -1,  2015,    -1,  1202,    -1,    -1,  1147,    -1,
      -1,    -1,  1209,    -1,  1211,  1212,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,    -1,  1226,
      -1,  1228,    -1,  1230,    -1,    -1,    -1,  1176,  1235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   392,
      -1,    -1,    -1,    -1,    -1,  1194,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1214,    -1,    -1,    -1,    -1,
     423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
    1297,    -1,    -1,    -1,    -1,    -1,   449,  1304,  1305,    -1,
     453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1946,  1258,
      -1,    -1,    -1,    74,    -1,  1804,    -1,    -1,    -1,   472,
      -1,  1328,    -1,   476,   477,    -1,    -1,   480,  1335,    -1,
      -1,    -1,  1339,    -1,    -1,    -1,  1974,    -1,    -1,    -1,
      -1,    -1,   495,   496,   497,   498,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
      -1,   514,    -1,    -1,    -1,    -1,    -1,    -1,  1375,   522,
      -1,    -1,   133,    -1,   135,    -1,    -1,  2015,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,  1415,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1365,  1366,  1367,  1368,
    1369,  1370,  1371,    -1,    -1,    -1,    -1,    -1,   581,    -1,
     191,    -1,    -1,    -1,    -1,    -1,   589,    -1,    -1,    -1,
      -1,    -1,   595,    -1,    -1,  1452,  1395,  1396,    -1,    -1,
      -1,    -1,    -1,  1460,    -1,  1462,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   619,   620,   229,    -1,
      -1,    -1,   233,    -1,    -1,   236,   237,    -1,    -1,   240,
      -1,    -1,   243,   244,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1511,  1512,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1526,
    1527,    -1,  1529,    -1,  1473,    -1,    -1,    -1,    -1,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,    -1,   690,    -1,    -1,
      -1,  1548,  1549,    -1,    -1,    -1,    -1,    -1,   309,    -1,
      -1,   312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   333,   334,    -1,    -1,    -1,    -1,    -1,   375,
      -1,    -1,    -1,   379,   380,    -1,    -1,    -1,   349,    -1,
      -1,    -1,    -1,   389,   390,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   759,    -1,   404,   405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   774,    -1,  1572,    -1,   778,    -1,    -1,    -1,   425,
      -1,    -1,    -1,    -1,   787,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   809,    -1,  1607,   455,
      -1,    -1,  1669,  1670,    -1,   818,    -1,    -1,    -1,    -1,
      -1,   824,    -1,    -1,    -1,  1682,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   444,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,   861,    -1,
    1659,    -1,  1719,  1720,    -1,   868,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   499,  1688,
      -1,    -1,    -1,   896,    -1,     3,    -1,    -1,    -1,    -1,
     511,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,  1806,
     953,    -1,    -1,  1810,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1831,   586,    -1,  1834,  1835,  1778,
      -1,    -1,    -1,    -1,  1841,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   627,   628,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,   640,
      -1,    -1,    -1,  1036,    -1,    -1,    -1,  1040,    -1,    -1,
      -1,    -1,    -1,    -1,  1047,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,  1057,   163,   164,    -1,    -1,    -1,
      -1,  1064,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1073,    -1,  1075,    -1,   720,   721,   722,   723,   724,   725,
     726,   727,   728,   729,   730,   731,   732,   733,   734,   735,
     736,   737,   738,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,  1111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1976,
      -1,    -1,    -1,    -1,    -1,  1128,    -1,    -1,  1131,    -1,
      -1,    -1,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1946,    -1,    -1,
      -1,    -1,   763,   764,   800,    -1,    -1,    -1,   769,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,   202,
      -1,    -1,    -1,    -1,    -1,  1974,    -1,    -1,    -1,   790,
      -1,    -1,   793,   794,  2041,   796,    -1,   798,   799,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     233,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,  2066,
      -1,    -1,    -1,    -1,    -1,    -1,  2015,    -1,    -1,    -1,
    1223,    -1,    -1,    -1,  2081,    -1,   837,    -1,    -1,    -1,
     841,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2097,    -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   908,   909,    -1,
      -1,    -1,    -1,   336,   337,    -1,    -1,    -1,    -1,    -1,
      -1,   922,    -1,   959,    -1,    -1,    -1,    -1,   964,    -1,
      -1,    -1,    -1,    -1,   357,    -1,    -1,    -1,    -1,   975,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1342,
      -1,    -1,    -1,  1346,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1382,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   444,   445,    -1,   447,   448,    -1,    -1,    -1,    -1,
      -1,    -1,   455,    -1,    -1,    -1,   459,    -1,    -1,  1432,
      -1,    -1,  1435,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1443,    -1,  1053,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   500,    -1,    -1,
      -1,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1094,    -1,    -1,    -1,    -1,    -1,    -1,
    1101,    -1,    -1,  1104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1147,    -1,   536,  1507,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1516,    -1,    -1,    -1,  1520,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1534,  1535,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   587,    -1,    -1,   590,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   608,   609,    -1,    -1,  1225,
      -1,    -1,    -1,    -1,    -1,    -1,   619,    -1,    -1,    -1,
     623,    -1,    -1,    -1,    -1,    -1,    -1,   630,  1244,   632,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,  1261,  1262,  1263,    -1,    -1,
    1231,    -1,  1268,  1269,    -1,    -1,    -1,    -1,  1239,  1240,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1292,    -1,    -1,    -1,
      -1,    -1,    -1,  1656,  1657,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,  1329,  1330,    -1,   391,  1298,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,  1307,    -1,    -1,  1310,
      -1,  1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     743,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,   759,   760,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   769,   770,    -1,   772,
     773,    -1,  1353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   784,    -1,    -1,   787,    -1,   789,   790,    -1,    -1,
      -1,    -1,    -1,   796,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   806,   807,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1792,
      -1,    -1,   825,    -1,    -1,    -1,   829,    -1,    -1,    -1,
     833,    -1,    -1,    -1,   837,   838,    83,    -1,   841,   842,
      -1,    -1,    -1,    -1,    -1,  1426,   849,  1820,    -1,    -1,
      -1,    -1,    99,    -1,    -1,   530,   531,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1839,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,   891,    -1,
     893,    -1,    -1,    -1,    -1,  1868,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,    -1,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,    -1,
      -1,   924,  1895,    -1,    -1,  1898,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1515,    -1,    -1,    -1,    -1,  1912,
      -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   202,    -1,   120,   205,    -1,
     963,    -1,    -1,  1544,    -1,    -1,    -1,    -1,    -1,    -1,
     133,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   669,  1576,    -1,    -1,    -1,    -1,
     675,  1582,    -1,    -1,    -1,    -1,    -1,   254,    -1,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     267,    -1,    -1,    -1,    -1,    -1,    -1,  1030,   703,    -1,
    1646,  2004,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1047,  1048,    -1,    -1,    -1,    -1,
      -1,  1054,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   308,    -1,    -1,   739,    -1,    -1,    -1,    -1,   316,
    1651,    -1,    -1,   236,   237,    -1,    -1,   240,    -1,    -1,
     243,   244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   336,
      -1,   338,    -1,  1096,    -1,    -1,    -1,    -1,  1101,  1102,
      -1,  1104,  1105,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   391,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,  1737,  1738,    -1,    -1,
      -1,    -1,    72,    -1,  1745,    -1,    -1,    -1,  1749,    -1,
     333,   334,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   455,    -1,
      -1,    -1,    -1,    -1,   461,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1225,    -1,    -1,    -1,    -1,    -1,  1231,  1232,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,
    1866,  1254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1856,    -1,    -1,    -1,    -1,
      -1,   444,    -1,   530,   531,    -1,    -1,    -1,    -1,   536,
    1906,    -1,    -1,    -1,    -1,  1298,  1299,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1307,  1308,    -1,  1310,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1321,  1322,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   590,    -1,    -1,    -1,    -1,   511,    -1,
      -1,  1932,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   609,    -1,   611,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2009,    -1,  2011,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   669,    -1,    -1,   672,   673,    -1,   675,    -1,
      -1,    -1,    -1,    -1,  2050,    -1,    -1,   684,    -1,    -1,
     687,   688,   689,    -1,    -1,  1448,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2043,    -1,   627,   628,    -1,  2083,    -1,    -1,
      -1,   189,  1475,    -1,    -1,    -1,    -1,   640,    -1,    -1,
      -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   743,   215,    -1,   217,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2132,    -1,    -1,    -1,
     767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,
     807,    -1,    -1,  1566,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   825,  1582,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     763,   764,    -1,    -1,    -1,    -1,   769,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,
     793,   794,    -1,   796,    -1,   798,   799,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   891,    -1,    -1,    -1,    -1,  1652,
      -1,    -1,   899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   837,    -1,    -1,    -1,   841,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1713,  1714,    -1,    -1,    -1,    -1,   963,  1392,    -1,    -1,
    1395,  1396,    -1,    -1,    -1,    -1,  1401,  1730,  1731,    -1,
    1405,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1746,    -1,   908,   909,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   922,
      -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,   487,
      -1,    -1,    -1,    -1,   492,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1096,
      -1,  1854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1862,
      -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1552,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1053,   609,  1139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   622,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1928,   154,  1602,    -1,  1932,
    1933,  1094,    -1,  1936,    -1,    -1,    -1,    -1,  1101,    -1,
      -1,  1104,  1617,  1618,    -1,    -1,  1193,    -1,    -1,   177,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   677,
      -1,    -1,    -1,    -1,    -1,    -1,  1641,  1214,    -1,    -1,
      -1,    -1,    -1,    -1,  1977,    -1,    -1,    -1,  1225,    -1,
      -1,    -1,    -1,   701,   702,    -1,    -1,   705,    -1,   707,
      -1,    -1,    -1,    -1,    -1,   713,    -1,   715,   716,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1254,    -1,    -1,
      -1,  1258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   743,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   756,    -1,
    2043,  2044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   767,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   781,    -1,    -1,   784,    -1,  1231,    -1,
      -1,    -1,    -1,    -1,  1321,  1322,  1239,  1240,    -1,    -1,
      -1,    -1,  2085,    -1,    -1,    -1,  1761,    -1,    -1,    -1,
      -1,    -1,    -1,   811,    -1,  1770,   814,  1772,    -1,    -1,
    1775,  1776,    -1,  1778,    -1,    -1,    -1,    -1,  1783,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1365,  1366,
    1367,    -1,    -1,  1370,  1371,    -1,    -1,    -1,    -1,    -1,
    1377,    -1,   850,    -1,    -1,  1298,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1307,    -1,    -1,  1310,    -1,  1312,
    1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1425,    -1,
      -1,   899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1353,    -1,    -1,    -1,    -1,   913,   914,    -1,    -1,    -1,
      -1,  1448,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,
    1885,    -1,    -1,    -1,    -1,  1890,  1891,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   963,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   971,    -1,    -1,    -1,    -1,    -1,    -1,
     978,    -1,    -1,  1426,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1954,
      -1,  1956,    -1,    -1,  1959,  1960,    -1,    -1,    -1,  1964,
    1965,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1024,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,  1572,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1515,    -1,    -1,    -1,    -1,    -1,    -1,  1606,
      -1,  2036,  2037,  2038,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,  1091,    56,  1093,    -1,  1095,    -1,    -1,
      -1,  1544,  2057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,  2071,  2072,  2073,    -1,
      -1,    -1,    -1,    -1,    -1,  1652,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,  1688,   124,   125,    -1,    -1,  1164,  1165,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1713,  1714,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,  1651,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,  1229,    -1,    -1,    -1,    -1,    -1,  1235,  1765,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1254,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,  1271,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,  1286,    -1,
      -1,  1289,    -1,    -1,  1737,  1738,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1749,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,  1341,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1357,
    1358,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,  1899,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,  1385,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1928,    -1,    -1,    -1,  1403,    -1,    -1,  1406,    -1,
      -1,    -1,    -1,  1856,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1975,    -1,
    1448,  1978,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1458,  1459,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
    1468,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1486,  1932,
    1488,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    -1,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,     1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,  1572,   155,   156,    -1,    -1,  1577,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,   177,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      73,    74,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,  1637,
      93,    94,    95,    96,    97,    -1,    99,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
       1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,  1686,    -1,
      -1,  1689,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,   177,    56,  1724,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    -1,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,  1907,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    -1,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,
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
      91,    -1,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
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
      -1,    -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,
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
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
     165,   166,   167,   168,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
     165,   166,   167,   168,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     1,
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
      -1,   103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,     5,
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
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    72,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,   104,    -1,    -1,   107,   108,    -1,   163,   164,     4,
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
     163,   164,   165,   166,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   159,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,   107,   108,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      72,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   107,   108,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,     4,
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
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     155,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,   107,   108,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,   155,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    78,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,   156,
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
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,   155,    -1,    51,    -1,    53,   107,   108,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
     107,   108,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    13,    14,    15,    16,    17,   163,   164,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   107,   108,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,
      54,   132,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,   146,   147,   148,    -1,    73,
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
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
     153,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
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
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
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
      73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,   159,    -1,    -1,   162,   163,   164,   165,   166,
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
      -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    49,   124,   125,    52,    -1,    54,    -1,    56,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,
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
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
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
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
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
     450,   468,   469,   470,   471,     0,   179,    17,   104,   183,
     199,   286,   288,   298,   301,   313,   317,   322,   118,   152,
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
     323,   401,   421,   425,   471,   286,   288,   301,   313,   317,
     322,   402,   421,   425,    65,   307,   307,   302,   308,   296,
     307,   308,   315,   334,   302,   307,   302,   155,   410,   158,
     180,   152,   160,   228,   410,   410,   179,   277,   278,   156,
     298,   301,   469,   365,   365,   398,   152,   176,   301,   152,
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
     325,   160,   405,   277,   365,   368,   288,   306,   403,   421,
     425,   160,   405,   277,   386,   301,   313,   301,   301,   107,
     324,   107,   108,   183,   323,   328,   386,   179,   183,   364,
     151,   179,     3,   293,   295,   301,   305,   228,   179,   179,
     409,   152,   409,   180,   374,   217,   411,   416,   301,   152,
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
     407,   396,   228,   405,   409,   160,   405,   277,   396,   228,
     405,   330,   331,   329,   160,   134,   301,   358,   359,   362,
     363,   154,   158,    70,   279,   280,   180,   301,   293,   163,
     217,   179,   154,   416,   357,   398,   396,   157,   179,   152,
     378,    69,   376,   377,    78,   311,   183,   160,   183,   439,
     299,   450,   464,   301,   305,   471,   179,   453,   454,   455,
     157,   179,    18,   217,   301,   452,   474,   410,   410,   450,
     299,   462,   472,   301,   183,   410,   299,   464,   323,   158,
     473,   365,   350,   160,   154,   367,   154,   154,   158,   152,
     177,   366,   187,   156,   366,   366,   366,   217,   366,   154,
     366,   366,   366,   179,   154,   165,   166,   203,    18,   303,
     154,   158,   154,   163,   164,   154,   223,   217,   160,   217,
     183,   217,   183,   116,   156,   183,   153,   191,   192,   193,
     217,   116,   156,   183,   336,   217,   191,   183,   201,   204,
     204,   204,   205,   205,   206,   206,   207,   207,   207,   207,
     208,   208,   209,   210,   211,   212,   213,   159,   224,   177,
     185,   156,   183,   217,   160,   217,   179,   444,   445,   446,
     301,   443,   410,   410,   217,   367,   152,   410,   447,   450,
     152,   447,   450,   179,   179,   157,   157,   152,   416,   435,
     436,   437,   440,    18,   301,   434,   438,   152,   410,   456,
     474,   410,   410,   474,   152,   410,   456,   410,   410,   180,
     216,   365,   157,   158,   157,   158,   474,   474,   134,   355,
     356,   357,   355,   365,   179,   215,   216,   217,   408,   473,
     369,   371,   151,   179,   154,   158,   179,   355,   183,   407,
     183,   154,   154,   154,   154,   154,   154,   152,   410,   447,
     450,   152,   410,   447,   450,   407,   185,   450,   217,   308,
     323,   448,   228,   358,   154,   154,   154,   154,   394,   395,
     228,   396,   228,   405,   395,   228,   160,   160,   160,   337,
     180,   180,   183,   281,   365,    18,    71,    73,    76,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    93,    94,    95,    96,    97,    99,   107,   108,   119,
     179,   224,   225,   226,   227,   228,   229,   230,   232,   233,
     243,   249,   250,   251,   252,   253,   254,   259,   260,   266,
     267,   268,   282,   301,   305,   365,   406,    70,   177,   180,
     180,   180,   355,   180,   397,   395,   286,   288,   298,   389,
     390,   391,   392,   384,   176,   375,   365,   375,   352,   410,
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
     180,   397,   395,   228,   397,   337,   337,   337,     3,     5,
      10,    73,   151,   283,   290,   291,   298,   301,   338,   343,
     467,   154,   158,   158,   177,   152,    61,    62,   177,   228,
     282,   406,   152,    18,   226,   152,   152,   177,   365,   177,
     365,   163,   365,   160,   225,   152,   152,   152,   228,   217,
     218,   218,    14,   269,    74,   234,   177,   180,   230,    78,
     177,   365,    91,   255,   364,   301,   159,   281,   177,   157,
     157,   180,   158,   397,   407,   180,   177,   180,   177,   180,
     154,   367,   381,   160,   381,   473,   350,   350,   179,   180,
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
     397,   151,   151,   151,   151,   298,   298,   336,   344,   467,
     298,   343,   152,   332,   177,   177,   152,   159,   199,   339,
     340,   346,   416,   417,   430,   158,   177,   365,   179,   365,
     154,   191,   192,   177,   228,   177,   228,   224,    80,   154,
     224,   235,   282,   284,   287,   293,   301,   305,   146,   147,
     148,   153,   154,   177,   224,   244,   245,   246,   282,   177,
     177,   224,   177,   370,   177,   224,   223,   224,   111,   112,
     113,   114,   115,   261,   263,   264,   177,    98,   177,    84,
     152,   152,   180,   151,   177,   177,   152,   226,   228,   410,
     177,   154,   179,   151,   151,   179,   158,   158,   151,   179,
     151,   160,   160,   157,   157,   157,   180,   154,   179,   217,
     217,   180,   157,   180,   473,   349,   350,   354,   354,   370,
     473,   151,   389,   451,   452,   154,   159,   154,   158,   159,
     370,   473,   223,   121,   194,   195,   156,   195,   156,   195,
     157,   151,   154,   179,   180,   180,   154,   154,   179,   179,
     180,   180,   180,   179,   179,   157,   180,   154,   410,   358,
     358,   180,   180,   224,   449,   151,   332,   332,   332,   339,
     152,   199,   341,   342,   447,   458,   459,   460,   461,   177,
     158,   177,   339,   177,   384,   411,   416,   217,   301,   158,
     177,   345,   346,   345,   365,   134,   362,   363,   224,   154,
     154,   152,   226,   154,   224,   301,   146,   147,   148,   168,
     177,   247,   248,   226,   225,   177,   248,   154,   159,   224,
     153,   224,   225,   246,   177,   473,   154,   154,   154,   228,
     263,   264,   152,   217,   152,   185,   235,   201,   256,   110,
       1,   226,   410,   390,   179,   179,   389,   352,   352,   157,
     358,   180,   180,   157,   157,   151,   350,   160,   473,   151,
     180,   154,   217,   189,   217,   473,   151,   157,   157,   194,
     194,   358,   154,   154,   358,   358,   154,   154,   157,   158,
     134,   357,   134,   157,   180,   180,   154,   154,   157,   217,
     177,   459,   460,   461,   301,   458,   158,   177,   410,   410,
     177,   154,   416,   410,   177,   226,    77,    78,   160,   238,
     239,   240,   154,   224,    75,   226,   224,   153,   224,    75,
     177,   107,   153,   224,   225,   246,   153,   224,   226,   245,
     248,   248,   177,   224,   151,   160,   240,   226,   152,   179,
     177,   185,   154,   159,   154,   154,   158,   159,   254,   258,
     365,   407,   180,   473,   473,   180,   157,   157,   160,   352,
     151,   151,   151,   157,   157,   180,   180,   180,   179,   180,
     154,   154,   154,   154,   154,   458,   410,   340,     1,   216,
     236,   237,   408,     1,   159,     1,   179,   226,   238,    75,
     177,   154,   226,    75,   177,   168,   168,   226,   225,   248,
     248,   177,   107,   224,   168,   168,    75,   153,   224,   153,
     224,   225,   177,     1,   179,   179,   265,   299,   301,   467,
     159,   177,   156,   185,   270,   271,   272,   226,   201,   191,
      75,   109,   255,   257,   151,   151,   151,   154,   352,   473,
     154,   154,   154,   360,   152,   410,   447,   450,   342,   134,
       1,   158,   159,   151,   275,   276,   282,   226,    75,   177,
     226,   224,   153,   153,   224,   153,   224,   153,   224,   225,
     153,   224,   153,   224,   226,   168,   168,   168,   168,   151,
     275,   265,   180,   152,   199,   407,   458,   183,   159,   104,
     152,   154,   159,   158,    75,   154,   226,   152,   226,   226,
     473,   151,   179,   216,   236,   239,   241,   242,   282,   226,
     168,   168,   168,   168,   153,   153,   224,   153,   224,   153,
     224,   241,   180,   177,   262,   301,   270,   157,   216,   177,
     270,   272,   226,   224,   110,   110,   151,   358,   226,   231,
     180,   239,   153,   153,   224,   153,   224,   153,   224,   180,
     262,   215,   154,   159,   185,   154,   154,   159,   154,   258,
      75,   253,   180,     1,   226,   151,   231,   151,   154,   228,
     185,   273,   152,   177,   273,   226,    75,   154,   228,   158,
     159,   216,   154,   226,   185,   183,   274,   154,   177,   154,
     158,   177,   183
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
     293,   293,   293,   294,   294,   295,   295,   295,   295,   296,
     296,   296,   297,   297,   297,   298,   298,   298,   299,   299,
     299,   300,   300,   301,   301,   302,   302,   303,   303,   303,
     303,   303,   304,   305,   305,   305,   306,   306,   307,   307,
     307,   307,   307,   307,   307,   307,   307,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   309,   309,   310,   311,   311,
     312,   312,   312,   312,   312,   313,   313,   314,   314,   314,
     314,   315,   315,   315,   315,   315,   315,   316,   316,   316,
     316,   317,   318,   317,   317,   319,   319,   319,   319,   320,
     320,   320,   321,   321,   321,   321,   322,   322,   322,   323,
     323,   323,   323,   323,   323,   324,   324,   324,   325,   325,
     326,   326,   328,   327,   329,   327,   330,   327,   331,   327,
     327,   332,   332,   333,   333,   334,   334,   335,   335,   335,
     336,   336,   336,   336,   336,   336,   336,   336,   337,   337,
     338,   338,   338,   338,   338,   338,   338,   338,   338,   338,
     338,   339,   339,   339,   340,   340,   340,   341,   341,   341,
     342,   343,   343,   344,   344,   345,   345,   346,   347,   348,
     347,   347,   347,   347,   349,   347,   347,   347,   347,   347,
     350,   350,   351,   351,   352,   352,   352,   352,   353,   353,
     354,   354,   354,   355,   355,   355,   355,   355,   355,   355,
     356,   356,   356,   356,   357,   357,   358,   358,   358,   358,
     359,   359,   359,   359,   360,   360,   360,   360,   360,   361,
     361,   361,   361,   361,   362,   362,   363,   363,   364,   364,
     365,   365,   365,   366,   366,   366,   367,   367,   368,   368,
     368,   368,   369,   369,   370,   370,   370,   370,   370,   371,
     371,   372,   372,   373,   373,   373,   373,   373,   374,   374,
     375,   375,   377,   376,   378,   376,   376,   376,   379,   379,
     379,   379,   380,   380,   380,   380,   381,   381,   382,   382,
     383,   383,   384,   384,   384,   384,   385,   385,   385,   386,
     386,   387,   387,   388,   388,   388,   388,   389,   389,   390,
     390,   391,   391,   391,   392,   392,   393,   393,   394,   394,
     395,   395,   396,   397,   398,   398,   398,   398,   398,   398,
     398,   398,   398,   398,   398,   399,   398,   400,   398,   401,
     398,   402,   398,   403,   398,   404,   404,   404,   405,   405,
     406,   406,   406,   406,   406,   406,   406,   406,   406,   406,
     407,   407,   407,   408,   409,   409,   410,   410,   411,   411,
     412,   413,   413,   414,   414,   414,   415,   415,   415,   415,
     415,   415,   416,   416,   417,   417,   417,   417,   418,   418,
     418,   418,   419,   419,   419,   419,   419,   419,   419,   420,
     420,   420,   420,   421,   421,   421,   422,   422,   422,   422,
     422,   423,   423,   423,   423,   424,   424,   424,   424,   424,
     424,   425,   425,   425,   426,   426,   426,   426,   426,   427,
     427,   427,   427,   428,   428,   428,   428,   428,   428,   429,
     429,   430,   430,   430,   430,   431,   431,   431,   431,   432,
     432,   432,   432,   432,   432,   432,   433,   433,   433,   433,
     433,   434,   434,   434,   434,   434,   435,   435,   435,   436,
     436,   436,   436,   437,   437,   437,   438,   438,   438,   438,
     438,   439,   439,   440,   440,   440,   441,   441,   442,   442,
     443,   443,   443,   444,   444,   444,   444,   444,   445,   445,
     445,   445,   446,   446,   446,   447,   447,   447,   447,   447,
     448,   448,   448,   448,   448,   448,   449,   449,   450,   450,
     450,   450,   451,   451,   452,   452,   452,   452,   453,   453,
     453,   453,   453,   454,   454,   454,   454,   455,   455,   455,
     456,   456,   456,   457,   457,   457,   457,   457,   457,   458,
     458,   458,   459,   459,   459,   459,   459,   460,   460,   460,
     460,   461,   461,   462,   462,   462,   463,   463,   464,   464,
     464,   464,   464,   464,   465,   465,   465,   465,   465,   465,
     465,   465,   465,   465,   466,   466,   466,   466,   467,   467,
     467,   468,   468,   469,   469,   469,   469,   469,   469,   470,
     470,   470,   470,   470,   470,   471,   471,   471,   472,   472,
     473,   473,   474,   474
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
       1,     1,     1,     3,     6,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     2,     3,     1,     2,     1,     1,
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
       3,     4,     4,     4,     3,     2,     2,     3,     3,     2,
       1,     0,     1,     4,     1,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     6,     0,
       9,     8,     9,     8,     0,    13,    11,    12,    11,     1,
       0,     1,     3,     3,     3,     2,     5,     5,     1,     1,
       0,     2,     5,     0,     1,     1,     1,     5,     5,     5,
       1,     5,     5,     9,     1,     5,     0,     1,     1,     5,
       1,     1,     5,     5,     1,     3,     3,     4,     1,     1,
       1,     1,     2,     1,     3,     3,     2,     3,     1,     3,
       1,     1,     1,     1,     1,     2,     1,     1,     0,     2,
       2,     4,     1,     4,     0,     1,     2,     3,     4,     2,
       2,     1,     2,     2,     5,     5,     7,     6,     1,     3,
       0,     2,     0,     5,     0,     5,     3,     1,     0,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       5,     6,     1,     1,     3,     3,     2,     3,     3,     2,
       4,     1,     4,     7,     8,    10,    11,     1,     4,     2,
       2,     1,     1,     5,     2,     5,     0,     1,     3,     4,
       0,     1,     0,     0,     1,     1,     2,     2,     2,     2,
       2,     2,     1,     2,     5,     0,     6,     0,     8,     0,
       7,     0,     7,     0,     8,     1,     2,     3,     0,     5,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     3,     0,     5,     0,     1,     1,     2,
       6,     1,     3,     0,     1,     4,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     8,
       9,     3,     4,     2,     1,     2,     6,     8,     9,     3,
       4,     2,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     1,     1,     1,     4,     8,     9,     3,     4,     2,
       3,     3,     4,     4,     5,     4,     5,     3,     4,     1,
       3,     2,     1,     2,     2,     2,     3,     4,     5,     2,
       4,     5,     4,     5,     3,     4,     6,     8,     9,     3,
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
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7689 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7695 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7701 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7707 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7713 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7719 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7725 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7731 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7737 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7743 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7753 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7759 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7771 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7777 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7783 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7789 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7795 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7805 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7811 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7817 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7823 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7829 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7835 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7841 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7852 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7861 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7867 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7873 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7885 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7891 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7901 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7907 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7932 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7962 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7968 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7986 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8014 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8020 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8026 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8032 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8038 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8044 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8050 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8056 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8062 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8068 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8074 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8080 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8086 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8094 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8100 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8109 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8115 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8121 "Parser/parser.cc"
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
#line 8141 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8147 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8153 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8159 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8165 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8171 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8177 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8183 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8189 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8198 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8204 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8210 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8216 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8222 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8228 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8234 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8240 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8252 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8264 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8270 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8276 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8282 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8288 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8294 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8336 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8342 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8348 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8390 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8396 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8402 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8408 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8414 "Parser/parser.cc"
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
#line 8426 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8432 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8438 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8444 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8450 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8456 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8462 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8468 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8474 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8480 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8486 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8492 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8498 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8504 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8510 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8516 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8528 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8534 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8540 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8546 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8558 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8564 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8587 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8593 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8605 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8623 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8629 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8635 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8647 "Parser/parser.cc"
    break;

  case 214:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8659 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8665 "Parser/parser.cc"
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
#line 8679 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8685 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8691 "Parser/parser.cc"
    break;

  case 220:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8700 "Parser/parser.cc"
    break;

  case 221:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8706 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8718 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8724 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8730 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8736 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8742 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8748 "Parser/parser.cc"
    break;

  case 229:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8754 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8760 "Parser/parser.cc"
    break;

  case 232:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 233:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8772 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8778 "Parser/parser.cc"
    break;

  case 235:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8784 "Parser/parser.cc"
    break;

  case 236:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8790 "Parser/parser.cc"
    break;

  case 237:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8796 "Parser/parser.cc"
    break;

  case 239:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8802 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8808 "Parser/parser.cc"
    break;

  case 241:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8814 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8841 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8853 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8859 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8868 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8874 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8886 "Parser/parser.cc"
    break;

  case 254:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8895 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8901 "Parser/parser.cc"
    break;

  case 256:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8907 "Parser/parser.cc"
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
#line 8926 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 261:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8950 "Parser/parser.cc"
    break;

  case 263:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8962 "Parser/parser.cc"
    break;

  case 265:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8968 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8974 "Parser/parser.cc"
    break;

  case 267:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8983 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8992 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8998 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9007 "Parser/parser.cc"
    break;

  case 271:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9016 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9022 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9028 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9034 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9040 "Parser/parser.cc"
    break;

  case 276:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9046 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9052 "Parser/parser.cc"
    break;

  case 278:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9058 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9064 "Parser/parser.cc"
    break;

  case 280:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9073 "Parser/parser.cc"
    break;

  case 281:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9083 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9089 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9095 "Parser/parser.cc"
    break;

  case 284:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9104 "Parser/parser.cc"
    break;

  case 285:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9114 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9120 "Parser/parser.cc"
    break;

  case 287:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9129 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9139 "Parser/parser.cc"
    break;

  case 289:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9145 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9151 "Parser/parser.cc"
    break;

  case 291:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9157 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9163 "Parser/parser.cc"
    break;

  case 293:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9172 "Parser/parser.cc"
    break;

  case 294:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9182 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 296:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9197 "Parser/parser.cc"
    break;

  case 297:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9207 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9213 "Parser/parser.cc"
    break;

  case 299:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9222 "Parser/parser.cc"
    break;

  case 300:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9232 "Parser/parser.cc"
    break;

  case 301:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9238 "Parser/parser.cc"
    break;

  case 302:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9247 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9256 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9262 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9268 "Parser/parser.cc"
    break;

  case 306:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9274 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9280 "Parser/parser.cc"
    break;

  case 308:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9286 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9292 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9298 "Parser/parser.cc"
    break;

  case 312:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9304 "Parser/parser.cc"
    break;

  case 313:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9310 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9316 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9322 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9328 "Parser/parser.cc"
    break;

  case 317:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9334 "Parser/parser.cc"
    break;

  case 318:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9340 "Parser/parser.cc"
    break;

  case 319:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9346 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9352 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9358 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9364 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9370 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9376 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9382 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9388 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9394 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9400 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9406 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9412 "Parser/parser.cc"
    break;

  case 331:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9418 "Parser/parser.cc"
    break;

  case 334:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9424 "Parser/parser.cc"
    break;

  case 335:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9430 "Parser/parser.cc"
    break;

  case 336:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9436 "Parser/parser.cc"
    break;

  case 337:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9442 "Parser/parser.cc"
    break;

  case 339:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9448 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9454 "Parser/parser.cc"
    break;

  case 342:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9460 "Parser/parser.cc"
    break;

  case 343:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9466 "Parser/parser.cc"
    break;

  case 344:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 345:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 346:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 347:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9490 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9496 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9502 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9508 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9514 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9520 "Parser/parser.cc"
    break;

  case 353:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9526 "Parser/parser.cc"
    break;

  case 354:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9532 "Parser/parser.cc"
    break;

  case 355:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 356:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 357:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9550 "Parser/parser.cc"
    break;

  case 358:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9556 "Parser/parser.cc"
    break;

  case 359:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9562 "Parser/parser.cc"
    break;

  case 360:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9568 "Parser/parser.cc"
    break;

  case 361:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9574 "Parser/parser.cc"
    break;

  case 362:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9580 "Parser/parser.cc"
    break;

  case 363:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 365:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 366:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 367:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 372:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 373:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9616 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9622 "Parser/parser.cc"
    break;

  case 375:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9628 "Parser/parser.cc"
    break;

  case 376:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9634 "Parser/parser.cc"
    break;

  case 377:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9640 "Parser/parser.cc"
    break;

  case 378:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9646 "Parser/parser.cc"
    break;

  case 379:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9652 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9658 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 384:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 385:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9676 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9688 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9697 "Parser/parser.cc"
    break;

  case 389:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9706 "Parser/parser.cc"
    break;

  case 390:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9712 "Parser/parser.cc"
    break;

  case 393:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9718 "Parser/parser.cc"
    break;

  case 394:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9724 "Parser/parser.cc"
    break;

  case 396:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9730 "Parser/parser.cc"
    break;

  case 397:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9736 "Parser/parser.cc"
    break;

  case 404:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9747 "Parser/parser.cc"
    break;

  case 407:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9753 "Parser/parser.cc"
    break;

  case 408:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9759 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9765 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9771 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9777 "Parser/parser.cc"
    break;

  case 416:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9783 "Parser/parser.cc"
    break;

  case 417:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9789 "Parser/parser.cc"
    break;

  case 418:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9795 "Parser/parser.cc"
    break;

  case 419:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9801 "Parser/parser.cc"
    break;

  case 421:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9807 "Parser/parser.cc"
    break;

  case 422:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9813 "Parser/parser.cc"
    break;

  case 423:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9819 "Parser/parser.cc"
    break;

  case 424:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9830 "Parser/parser.cc"
    break;

  case 425:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9836 "Parser/parser.cc"
    break;

  case 426:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9842 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9848 "Parser/parser.cc"
    break;

  case 428:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9854 "Parser/parser.cc"
    break;

  case 429:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9863 "Parser/parser.cc"
    break;

  case 430:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9872 "Parser/parser.cc"
    break;

  case 431:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9881 "Parser/parser.cc"
    break;

  case 432:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// if type_specifier is an anon aggregate => name 
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9891 "Parser/parser.cc"
    break;

  case 433:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9900 "Parser/parser.cc"
    break;

  case 434:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9909 "Parser/parser.cc"
    break;

  case 435:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9918 "Parser/parser.cc"
    break;

  case 436:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9927 "Parser/parser.cc"
    break;

  case 437:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9935 "Parser/parser.cc"
    break;

  case 438:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9943 "Parser/parser.cc"
    break;

  case 439:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9949 "Parser/parser.cc"
    break;

  case 443:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 444:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 448:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc,
						  ::toString( "Missing ';' after end of ",
									  (yyvsp[-1].decl)->type->enumeration.name ? "enum" : AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
									  " declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 9973 "Parser/parser.cc"
    break;

  case 456:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9984 "Parser/parser.cc"
    break;

  case 461:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9990 "Parser/parser.cc"
    break;

  case 464:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9996 "Parser/parser.cc"
    break;

  case 467:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10002 "Parser/parser.cc"
    break;

  case 468:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10008 "Parser/parser.cc"
    break;

  case 469:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10014 "Parser/parser.cc"
    break;

  case 470:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10020 "Parser/parser.cc"
    break;

  case 472:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 10026 "Parser/parser.cc"
    break;

  case 474:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10032 "Parser/parser.cc"
    break;

  case 475:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10038 "Parser/parser.cc"
    break;

  case 477:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10044 "Parser/parser.cc"
    break;

  case 478:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10050 "Parser/parser.cc"
    break;

  case 479:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10056 "Parser/parser.cc"
    break;

  case 480:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10062 "Parser/parser.cc"
    break;

  case 481:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10068 "Parser/parser.cc"
    break;

  case 482:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10074 "Parser/parser.cc"
    break;

  case 483:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10080 "Parser/parser.cc"
    break;

  case 484:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10086 "Parser/parser.cc"
    break;

  case 485:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10092 "Parser/parser.cc"
    break;

  case 486:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10098 "Parser/parser.cc"
    break;

  case 487:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10104 "Parser/parser.cc"
    break;

  case 488:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10110 "Parser/parser.cc"
    break;

  case 489:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10116 "Parser/parser.cc"
    break;

  case 490:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10122 "Parser/parser.cc"
    break;

  case 491:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10128 "Parser/parser.cc"
    break;

  case 492:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10134 "Parser/parser.cc"
    break;

  case 493:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10140 "Parser/parser.cc"
    break;

  case 494:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10146 "Parser/parser.cc"
    break;

  case 495:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10152 "Parser/parser.cc"
    break;

  case 496:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10158 "Parser/parser.cc"
    break;

  case 497:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10164 "Parser/parser.cc"
    break;

  case 498:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10170 "Parser/parser.cc"
    break;

  case 499:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10176 "Parser/parser.cc"
    break;

  case 500:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10182 "Parser/parser.cc"
    break;

  case 501:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10188 "Parser/parser.cc"
    break;

  case 502:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10194 "Parser/parser.cc"
    break;

  case 503:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10200 "Parser/parser.cc"
    break;

  case 504:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10206 "Parser/parser.cc"
    break;

  case 505:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10212 "Parser/parser.cc"
    break;

  case 506:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10218 "Parser/parser.cc"
    break;

  case 507:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10224 "Parser/parser.cc"
    break;

  case 508:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10230 "Parser/parser.cc"
    break;

  case 509:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10236 "Parser/parser.cc"
    break;

  case 510:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10242 "Parser/parser.cc"
    break;

  case 511:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10248 "Parser/parser.cc"
    break;

  case 512:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10254 "Parser/parser.cc"
    break;

  case 513:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10260 "Parser/parser.cc"
    break;

  case 515:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10266 "Parser/parser.cc"
    break;

  case 517:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 518:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10278 "Parser/parser.cc"
    break;

  case 519:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10284 "Parser/parser.cc"
    break;

  case 521:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 522:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 523:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 524:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 526:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 528:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 529:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 530:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10332 "Parser/parser.cc"
    break;

  case 531:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10338 "Parser/parser.cc"
    break;

  case 532:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 533:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10350 "Parser/parser.cc"
    break;

  case 534:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10356 "Parser/parser.cc"
    break;

  case 535:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10362 "Parser/parser.cc"
    break;

  case 536:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10368 "Parser/parser.cc"
    break;

  case 537:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10379 "Parser/parser.cc"
    break;

  case 538:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 539:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 540:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 541:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10408 "Parser/parser.cc"
    break;

  case 542:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10414 "Parser/parser.cc"
    break;

  case 543:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 544:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10429 "Parser/parser.cc"
    break;

  case 546:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10435 "Parser/parser.cc"
    break;

  case 547:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10441 "Parser/parser.cc"
    break;

  case 548:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 550:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 551:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 553:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10465 "Parser/parser.cc"
    break;

  case 554:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 555:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10477 "Parser/parser.cc"
    break;

  case 557:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10483 "Parser/parser.cc"
    break;

  case 558:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10489 "Parser/parser.cc"
    break;

  case 559:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10495 "Parser/parser.cc"
    break;

  case 560:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 561:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 563:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10513 "Parser/parser.cc"
    break;

  case 564:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10519 "Parser/parser.cc"
    break;

  case 565:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10525 "Parser/parser.cc"
    break;

  case 566:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10531 "Parser/parser.cc"
    break;

  case 567:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10537 "Parser/parser.cc"
    break;

  case 568:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10548 "Parser/parser.cc"
    break;

  case 572:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10554 "Parser/parser.cc"
    break;

  case 573:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10560 "Parser/parser.cc"
    break;

  case 574:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10569 "Parser/parser.cc"
    break;

  case 575:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10586 "Parser/parser.cc"
    break;

  case 576:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10595 "Parser/parser.cc"
    break;

  case 577:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10605 "Parser/parser.cc"
    break;

  case 578:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10614 "Parser/parser.cc"
    break;

  case 579:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10624 "Parser/parser.cc"
    break;

  case 581:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10630 "Parser/parser.cc"
    break;

  case 582:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10636 "Parser/parser.cc"
    break;

  case 583:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10646 "Parser/parser.cc"
    break;

  case 584:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10661 "Parser/parser.cc"
    break;

  case 587:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10667 "Parser/parser.cc"
    break;

  case 588:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10673 "Parser/parser.cc"
    break;

  case 589:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10679 "Parser/parser.cc"
    break;

  case 590:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10685 "Parser/parser.cc"
    break;

  case 591:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10691 "Parser/parser.cc"
    break;

  case 592:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10697 "Parser/parser.cc"
    break;

  case 593:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10703 "Parser/parser.cc"
    break;

  case 594:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10709 "Parser/parser.cc"
    break;

  case 595:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10715 "Parser/parser.cc"
    break;

  case 596:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10721 "Parser/parser.cc"
    break;

  case 597:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10727 "Parser/parser.cc"
    break;

  case 598:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10733 "Parser/parser.cc"
    break;

  case 599:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10739 "Parser/parser.cc"
    break;

  case 600:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10752 "Parser/parser.cc"
    break;

  case 601:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 602:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10764 "Parser/parser.cc"
    break;

  case 603:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10777 "Parser/parser.cc"
    break;

  case 604:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10783 "Parser/parser.cc"
    break;

  case 607:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10789 "Parser/parser.cc"
    break;

  case 608:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10795 "Parser/parser.cc"
    break;

  case 611:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10801 "Parser/parser.cc"
    break;

  case 613:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10807 "Parser/parser.cc"
    break;

  case 614:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 615:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10819 "Parser/parser.cc"
    break;

  case 616:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10825 "Parser/parser.cc"
    break;

  case 617:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10831 "Parser/parser.cc"
    break;

  case 619:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10837 "Parser/parser.cc"
    break;

  case 621:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 622:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10849 "Parser/parser.cc"
    break;

  case 624:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 625:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10861 "Parser/parser.cc"
    break;

  case 627:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10867 "Parser/parser.cc"
    break;

  case 628:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 629:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 630:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10885 "Parser/parser.cc"
    break;

  case 631:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10891 "Parser/parser.cc"
    break;

  case 632:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10902 "Parser/parser.cc"
    break;

  case 633:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10910 "Parser/parser.cc"
    break;

  case 634:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10919 "Parser/parser.cc"
    break;

  case 635:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10927 "Parser/parser.cc"
    break;

  case 636:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10935 "Parser/parser.cc"
    break;

  case 637:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10943 "Parser/parser.cc"
    break;

  case 638:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10951 "Parser/parser.cc"
    break;

  case 640:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10957 "Parser/parser.cc"
    break;

  case 641:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10963 "Parser/parser.cc"
    break;

  case 642:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 643:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 644:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 645:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10987 "Parser/parser.cc"
    break;

  case 646:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 647:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 649:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11005 "Parser/parser.cc"
    break;

  case 650:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11011 "Parser/parser.cc"
    break;

  case 651:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 652:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11023 "Parser/parser.cc"
    break;

  case 653:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11029 "Parser/parser.cc"
    break;

  case 654:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11035 "Parser/parser.cc"
    break;

  case 657:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 658:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11047 "Parser/parser.cc"
    break;

  case 659:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11053 "Parser/parser.cc"
    break;

  case 661:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 662:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11065 "Parser/parser.cc"
    break;

  case 663:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11071 "Parser/parser.cc"
    break;

  case 665:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 666:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11083 "Parser/parser.cc"
    break;

  case 667:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11089 "Parser/parser.cc"
    break;

  case 669:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11095 "Parser/parser.cc"
    break;

  case 672:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 673:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 675:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 676:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 677:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 682:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 684:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11137 "Parser/parser.cc"
    break;

  case 685:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11143 "Parser/parser.cc"
    break;

  case 686:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11149 "Parser/parser.cc"
    break;

  case 687:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11155 "Parser/parser.cc"
    break;

  case 688:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 689:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 695:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11173 "Parser/parser.cc"
    break;

  case 698:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11179 "Parser/parser.cc"
    break;

  case 699:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11185 "Parser/parser.cc"
    break;

  case 700:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11191 "Parser/parser.cc"
    break;

  case 701:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11197 "Parser/parser.cc"
    break;

  case 702:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 703:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11209 "Parser/parser.cc"
    break;

  case 704:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11215 "Parser/parser.cc"
    break;

  case 706:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 707:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 708:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11233 "Parser/parser.cc"
    break;

  case 710:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 712:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11245 "Parser/parser.cc"
    break;

  case 713:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 714:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11257 "Parser/parser.cc"
    break;

  case 715:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11263 "Parser/parser.cc"
    break;

  case 716:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 717:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11275 "Parser/parser.cc"
    break;

  case 719:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 720:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11287 "Parser/parser.cc"
    break;

  case 721:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11293 "Parser/parser.cc"
    break;

  case 722:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11304 "Parser/parser.cc"
    break;

  case 723:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 724:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11316 "Parser/parser.cc"
    break;

  case 725:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 726:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11331 "Parser/parser.cc"
    break;

  case 727:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 728:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11343 "Parser/parser.cc"
    break;

  case 729:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11349 "Parser/parser.cc"
    break;

  case 730:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11355 "Parser/parser.cc"
    break;

  case 731:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11361 "Parser/parser.cc"
    break;

  case 732:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11367 "Parser/parser.cc"
    break;

  case 733:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11373 "Parser/parser.cc"
    break;

  case 734:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11379 "Parser/parser.cc"
    break;

  case 735:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11385 "Parser/parser.cc"
    break;

  case 736:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11391 "Parser/parser.cc"
    break;

  case 739:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 740:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 741:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11409 "Parser/parser.cc"
    break;

  case 742:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 744:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11421 "Parser/parser.cc"
    break;

  case 745:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11427 "Parser/parser.cc"
    break;

  case 746:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11433 "Parser/parser.cc"
    break;

  case 747:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 748:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 749:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 750:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 751:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11466 "Parser/parser.cc"
    break;

  case 752:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11475 "Parser/parser.cc"
    break;

  case 753:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr ); }
#line 11481 "Parser/parser.cc"
    break;

  case 754:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-5].decl), nullptr ); }
#line 11487 "Parser/parser.cc"
    break;

  case 755:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11493 "Parser/parser.cc"
    break;

  case 756:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-8].decl), (yyvsp[-2].decl) ); }
#line 11499 "Parser/parser.cc"
    break;

  case 758:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11505 "Parser/parser.cc"
    break;

  case 763:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11511 "Parser/parser.cc"
    break;

  case 764:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11517 "Parser/parser.cc"
    break;

  case 765:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11523 "Parser/parser.cc"
    break;

  case 767:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11529 "Parser/parser.cc"
    break;

  case 768:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11535 "Parser/parser.cc"
    break;

  case 769:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11541 "Parser/parser.cc"
    break;

  case 770:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11547 "Parser/parser.cc"
    break;

  case 772:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11553 "Parser/parser.cc"
    break;

  case 773:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11559 "Parser/parser.cc"
    break;

  case 774:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11565 "Parser/parser.cc"
    break;

  case 776:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11571 "Parser/parser.cc"
    break;

  case 777:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11577 "Parser/parser.cc"
    break;

  case 778:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11583 "Parser/parser.cc"
    break;

  case 779:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11589 "Parser/parser.cc"
    break;

  case 780:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11595 "Parser/parser.cc"
    break;

  case 781:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11601 "Parser/parser.cc"
    break;

  case 783:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11610 "Parser/parser.cc"
    break;

  case 784:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 11616 "Parser/parser.cc"
    break;

  case 785:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11625 "Parser/parser.cc"
    break;

  case 786:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11635 "Parser/parser.cc"
    break;

  case 787:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11644 "Parser/parser.cc"
    break;

  case 788:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11654 "Parser/parser.cc"
    break;

  case 789:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11663 "Parser/parser.cc"
    break;

  case 790:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11673 "Parser/parser.cc"
    break;

  case 791:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11682 "Parser/parser.cc"
    break;

  case 792:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11692 "Parser/parser.cc"
    break;

  case 793:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11701 "Parser/parser.cc"
    break;

  case 794:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11711 "Parser/parser.cc"
    break;

  case 796:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11717 "Parser/parser.cc"
    break;

  case 797:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 798:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11729 "Parser/parser.cc"
    break;

  case 799:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11741 "Parser/parser.cc"
    break;

  case 800:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11752 "Parser/parser.cc"
    break;

  case 801:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11761 "Parser/parser.cc"
    break;

  case 802:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11770 "Parser/parser.cc"
    break;

  case 803:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 804:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 805:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11788 "Parser/parser.cc"
    break;

  case 806:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11797 "Parser/parser.cc"
    break;

  case 807:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11803 "Parser/parser.cc"
    break;

  case 808:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11809 "Parser/parser.cc"
    break;

  case 809:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11815 "Parser/parser.cc"
    break;

  case 813:
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11821 "Parser/parser.cc"
    break;

  case 814:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11827 "Parser/parser.cc"
    break;

  case 815:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11837 "Parser/parser.cc"
    break;

  case 816:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11843 "Parser/parser.cc"
    break;

  case 819:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 820:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11855 "Parser/parser.cc"
    break;

  case 822:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 823:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11867 "Parser/parser.cc"
    break;

  case 824:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 825:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11879 "Parser/parser.cc"
    break;

  case 830:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11885 "Parser/parser.cc"
    break;

  case 831:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11891 "Parser/parser.cc"
    break;

  case 832:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 833:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11903 "Parser/parser.cc"
    break;

  case 834:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 836:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 837:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 838:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 839:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 840:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 841:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 842:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 843:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 844:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11963 "Parser/parser.cc"
    break;

  case 845:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 846:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 847:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11981 "Parser/parser.cc"
    break;

  case 848:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 849:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 850:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 851:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12005 "Parser/parser.cc"
    break;

  case 852:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 853:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 855:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 856:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 857:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 858:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 859:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12047 "Parser/parser.cc"
    break;

  case 860:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 861:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 862:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 863:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 864:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 865:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 866:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 867:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 868:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 869:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12107 "Parser/parser.cc"
    break;

  case 870:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 874:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 875:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 876:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 877:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12137 "Parser/parser.cc"
    break;

  case 878:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 879:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 880:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 881:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12161 "Parser/parser.cc"
    break;

  case 882:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 883:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 884:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 885:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 886:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 887:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12197 "Parser/parser.cc"
    break;

  case 888:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 889:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12212 "Parser/parser.cc"
    break;

  case 890:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12218 "Parser/parser.cc"
    break;

  case 891:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12224 "Parser/parser.cc"
    break;

  case 893:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12230 "Parser/parser.cc"
    break;

  case 894:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12236 "Parser/parser.cc"
    break;

  case 895:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12242 "Parser/parser.cc"
    break;

  case 896:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12248 "Parser/parser.cc"
    break;

  case 897:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12254 "Parser/parser.cc"
    break;

  case 898:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12260 "Parser/parser.cc"
    break;

  case 899:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12266 "Parser/parser.cc"
    break;

  case 900:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12272 "Parser/parser.cc"
    break;

  case 901:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12278 "Parser/parser.cc"
    break;

  case 902:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12284 "Parser/parser.cc"
    break;

  case 903:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12290 "Parser/parser.cc"
    break;

  case 904:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12296 "Parser/parser.cc"
    break;

  case 905:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12302 "Parser/parser.cc"
    break;

  case 906:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12308 "Parser/parser.cc"
    break;

  case 907:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12314 "Parser/parser.cc"
    break;

  case 908:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12320 "Parser/parser.cc"
    break;

  case 909:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12326 "Parser/parser.cc"
    break;

  case 910:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12332 "Parser/parser.cc"
    break;

  case 911:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12338 "Parser/parser.cc"
    break;

  case 912:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12344 "Parser/parser.cc"
    break;

  case 914:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12350 "Parser/parser.cc"
    break;

  case 915:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12356 "Parser/parser.cc"
    break;

  case 916:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12362 "Parser/parser.cc"
    break;

  case 917:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12368 "Parser/parser.cc"
    break;

  case 918:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12374 "Parser/parser.cc"
    break;

  case 919:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12380 "Parser/parser.cc"
    break;

  case 920:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12386 "Parser/parser.cc"
    break;

  case 921:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12392 "Parser/parser.cc"
    break;

  case 922:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12398 "Parser/parser.cc"
    break;

  case 923:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12404 "Parser/parser.cc"
    break;

  case 924:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12410 "Parser/parser.cc"
    break;

  case 925:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12416 "Parser/parser.cc"
    break;

  case 926:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12422 "Parser/parser.cc"
    break;

  case 927:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12428 "Parser/parser.cc"
    break;

  case 929:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12434 "Parser/parser.cc"
    break;

  case 930:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12440 "Parser/parser.cc"
    break;

  case 931:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12446 "Parser/parser.cc"
    break;

  case 932:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12452 "Parser/parser.cc"
    break;

  case 933:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12458 "Parser/parser.cc"
    break;

  case 934:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12464 "Parser/parser.cc"
    break;

  case 935:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12470 "Parser/parser.cc"
    break;

  case 936:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12476 "Parser/parser.cc"
    break;

  case 937:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12482 "Parser/parser.cc"
    break;

  case 938:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12488 "Parser/parser.cc"
    break;

  case 939:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12494 "Parser/parser.cc"
    break;

  case 941:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12500 "Parser/parser.cc"
    break;

  case 942:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12506 "Parser/parser.cc"
    break;

  case 943:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12512 "Parser/parser.cc"
    break;

  case 944:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12518 "Parser/parser.cc"
    break;

  case 945:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12524 "Parser/parser.cc"
    break;

  case 946:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12530 "Parser/parser.cc"
    break;

  case 947:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12536 "Parser/parser.cc"
    break;

  case 949:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12542 "Parser/parser.cc"
    break;

  case 950:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12548 "Parser/parser.cc"
    break;

  case 951:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12554 "Parser/parser.cc"
    break;

  case 952:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12560 "Parser/parser.cc"
    break;

  case 953:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12566 "Parser/parser.cc"
    break;

  case 954:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12572 "Parser/parser.cc"
    break;

  case 955:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12578 "Parser/parser.cc"
    break;

  case 956:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 12584 "Parser/parser.cc"
    break;

  case 957:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 12590 "Parser/parser.cc"
    break;

  case 958:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12596 "Parser/parser.cc"
    break;

  case 960:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12602 "Parser/parser.cc"
    break;

  case 961:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12608 "Parser/parser.cc"
    break;

  case 963:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12614 "Parser/parser.cc"
    break;

  case 964:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12620 "Parser/parser.cc"
    break;

  case 966:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12626 "Parser/parser.cc"
    break;

  case 967:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12632 "Parser/parser.cc"
    break;

  case 968:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 12638 "Parser/parser.cc"
    break;

  case 969:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12644 "Parser/parser.cc"
    break;

  case 970:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 12650 "Parser/parser.cc"
    break;

  case 971:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12656 "Parser/parser.cc"
    break;

  case 972:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12662 "Parser/parser.cc"
    break;

  case 975:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12668 "Parser/parser.cc"
    break;

  case 976:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12674 "Parser/parser.cc"
    break;

  case 977:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12680 "Parser/parser.cc"
    break;

  case 978:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12686 "Parser/parser.cc"
    break;

  case 979:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12692 "Parser/parser.cc"
    break;

  case 980:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12698 "Parser/parser.cc"
    break;

  case 981:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12704 "Parser/parser.cc"
    break;

  case 982:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12710 "Parser/parser.cc"
    break;

  case 984:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12716 "Parser/parser.cc"
    break;

  case 985:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12722 "Parser/parser.cc"
    break;

  case 986:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12728 "Parser/parser.cc"
    break;

  case 987:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12734 "Parser/parser.cc"
    break;

  case 988:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12740 "Parser/parser.cc"
    break;

  case 989:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12746 "Parser/parser.cc"
    break;

  case 991:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12752 "Parser/parser.cc"
    break;

  case 993:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12758 "Parser/parser.cc"
    break;

  case 994:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12764 "Parser/parser.cc"
    break;

  case 995:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 12770 "Parser/parser.cc"
    break;

  case 996:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12776 "Parser/parser.cc"
    break;

  case 997:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12782 "Parser/parser.cc"
    break;

  case 998:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12788 "Parser/parser.cc"
    break;

  case 1000:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12794 "Parser/parser.cc"
    break;

  case 1001:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12800 "Parser/parser.cc"
    break;

  case 1002:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12806 "Parser/parser.cc"
    break;

  case 1003:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12812 "Parser/parser.cc"
    break;

  case 1004:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12818 "Parser/parser.cc"
    break;

  case 1005:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 1006:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12830 "Parser/parser.cc"
    break;

  case 1008:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 1009:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 1010:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12848 "Parser/parser.cc"
    break;

  case 1011:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 1012:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12860 "Parser/parser.cc"
    break;

  case 1015:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 1018:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12872 "Parser/parser.cc"
    break;

  case 1019:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 1020:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12884 "Parser/parser.cc"
    break;

  case 1021:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12890 "Parser/parser.cc"
    break;

  case 1022:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12896 "Parser/parser.cc"
    break;

  case 1023:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12902 "Parser/parser.cc"
    break;

  case 1024:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12908 "Parser/parser.cc"
    break;

  case 1025:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 1026:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12920 "Parser/parser.cc"
    break;

  case 1027:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 1028:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12932 "Parser/parser.cc"
    break;

  case 1029:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 1030:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 1031:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 1032:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 1033:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 1034:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 1035:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12974 "Parser/parser.cc"
    break;

  case 1036:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12980 "Parser/parser.cc"
    break;

  case 1037:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12986 "Parser/parser.cc"
    break;

  case 1039:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 1043:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 1044:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 1045:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13010 "Parser/parser.cc"
    break;

  case 1046:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 1047:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 1048:
#line 3986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 1049:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13034 "Parser/parser.cc"
    break;

  case 1050:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13040 "Parser/parser.cc"
    break;

  case 1051:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13046 "Parser/parser.cc"
    break;

  case 1052:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13052 "Parser/parser.cc"
    break;

  case 1053:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13058 "Parser/parser.cc"
    break;

  case 1054:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13064 "Parser/parser.cc"
    break;

  case 1055:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 1056:
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13076 "Parser/parser.cc"
    break;

  case 1057:
#line 4012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13082 "Parser/parser.cc"
    break;

  case 1058:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13088 "Parser/parser.cc"
    break;

  case 1059:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13094 "Parser/parser.cc"
    break;

  case 1062:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13100 "Parser/parser.cc"
    break;

  case 1063:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13106 "Parser/parser.cc"
    break;


#line 13110 "Parser/parser.cc"

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
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
