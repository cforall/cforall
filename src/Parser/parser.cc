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
#define YYLAST   21849

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  297
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1063
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2150

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
    2079,  2081,  2086,  2091,  2092,  2094,  2104,  2105,  2110,  2112,
    2114,  2116,  2118,  2120,  2123,  2125,  2127,  2132,  2134,  2136,
    2138,  2140,  2142,  2144,  2146,  2148,  2150,  2152,  2154,  2156,
    2158,  2160,  2162,  2164,  2166,  2168,  2170,  2172,  2174,  2176,
    2178,  2180,  2182,  2184,  2186,  2191,  2192,  2196,  2203,  2204,
    2210,  2211,  2213,  2215,  2217,  2222,  2224,  2229,  2230,  2232,
    2234,  2239,  2241,  2243,  2245,  2247,  2249,  2254,  2261,  2263,
    2265,  2270,  2278,  2277,  2281,  2289,  2290,  2292,  2294,  2299,
    2300,  2302,  2307,  2308,  2310,  2312,  2317,  2318,  2320,  2325,
    2327,  2329,  2331,  2332,  2334,  2339,  2341,  2343,  2348,  2355,
    2359,  2360,  2365,  2364,  2369,  2368,  2387,  2386,  2398,  2397,
    2408,  2413,  2414,  2419,  2425,  2439,  2440,  2444,  2446,  2448,
    2454,  2456,  2458,  2460,  2462,  2464,  2466,  2468,  2474,  2475,
    2480,  2489,  2491,  2493,  2502,  2504,  2505,  2506,  2508,  2510,
    2511,  2516,  2517,  2518,  2523,  2525,  2528,  2535,  2536,  2537,
    2543,  2548,  2550,  2556,  2557,  2563,  2564,  2568,  2573,  2576,
    2575,  2579,  2582,  2589,  2594,  2593,  2602,  2607,  2612,  2617,
    2622,  2623,  2628,  2630,  2635,  2637,  2639,  2641,  2646,  2647,
    2653,  2654,  2655,  2662,  2663,  2665,  2666,  2667,  2669,  2671,
    2678,  2679,  2681,  2683,  2688,  2689,  2695,  2696,  2698,  2699,
    2704,  2705,  2706,  2708,  2716,  2717,  2719,  2722,  2724,  2728,
    2729,  2730,  2732,  2734,  2739,  2741,  2746,  2748,  2757,  2759,
    2764,  2765,  2766,  2770,  2771,  2772,  2777,  2778,  2783,  2784,
    2785,  2786,  2790,  2791,  2796,  2797,  2798,  2799,  2800,  2814,
    2815,  2820,  2821,  2827,  2829,  2832,  2834,  2836,  2859,  2860,
    2866,  2867,  2873,  2872,  2882,  2881,  2885,  2891,  2897,  2898,
    2900,  2904,  2909,  2911,  2913,  2915,  2921,  2922,  2926,  2927,
    2932,  2934,  2941,  2943,  2944,  2946,  2951,  2953,  2955,  2960,
    2962,  2967,  2972,  2980,  2985,  2987,  2992,  2997,  2998,  3003,
    3004,  3008,  3009,  3010,  3015,  3017,  3023,  3025,  3030,  3032,
    3038,  3039,  3043,  3047,  3051,  3053,  3054,  3056,  3058,  3060,
    3062,  3064,  3066,  3067,  3072,  3075,  3074,  3086,  3085,  3098,
    3097,  3109,  3108,  3120,  3119,  3133,  3139,  3141,  3147,  3148,
    3159,  3166,  3171,  3177,  3180,  3183,  3187,  3193,  3196,  3199,
    3204,  3205,  3206,  3210,  3216,  3217,  3227,  3228,  3232,  3233,
    3238,  3243,  3244,  3250,  3251,  3253,  3258,  3259,  3260,  3261,
    3262,  3264,  3299,  3301,  3306,  3308,  3309,  3311,  3316,  3318,
    3320,  3322,  3327,  3329,  3331,  3333,  3335,  3337,  3339,  3344,
    3346,  3348,  3350,  3359,  3361,  3362,  3367,  3369,  3371,  3373,
    3375,  3380,  3382,  3384,  3386,  3391,  3393,  3395,  3397,  3399,
    3401,  3413,  3414,  3415,  3419,  3421,  3423,  3425,  3427,  3432,
    3434,  3436,  3438,  3443,  3445,  3447,  3449,  3451,  3453,  3468,
    3473,  3478,  3480,  3481,  3483,  3488,  3490,  3492,  3494,  3499,
    3501,  3503,  3505,  3507,  3509,  3511,  3516,  3518,  3520,  3522,
    3524,  3534,  3536,  3538,  3539,  3541,  3546,  3548,  3550,  3555,
    3557,  3559,  3561,  3566,  3568,  3570,  3584,  3586,  3588,  3589,
    3591,  3596,  3598,  3603,  3605,  3607,  3612,  3614,  3619,  3621,
    3638,  3639,  3641,  3646,  3648,  3650,  3652,  3654,  3659,  3660,
    3662,  3664,  3669,  3671,  3673,  3679,  3681,  3684,  3687,  3689,
    3693,  3695,  3697,  3698,  3700,  3702,  3706,  3708,  3713,  3715,
    3717,  3719,  3754,  3755,  3759,  3760,  3762,  3764,  3769,  3771,
    3773,  3775,  3777,  3782,  3783,  3785,  3787,  3792,  3794,  3796,
    3802,  3803,  3805,  3814,  3817,  3819,  3822,  3824,  3826,  3840,
    3841,  3843,  3848,  3850,  3852,  3854,  3856,  3861,  3862,  3864,
    3866,  3871,  3873,  3881,  3882,  3883,  3888,  3889,  3894,  3896,
    3898,  3900,  3902,  3904,  3911,  3913,  3915,  3917,  3919,  3922,
    3924,  3926,  3928,  3930,  3935,  3937,  3939,  3944,  3970,  3971,
    3973,  3977,  3978,  3982,  3984,  3986,  3988,  3990,  3992,  3999,
    4001,  4003,  4005,  4007,  4009,  4014,  4016,  4018,  4025,  4027,
    4045,  4047,  4052,  4053
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

#define YYPACT_NINF (-1791)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1062)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     111, 12019,   129,   185, 16612,   172, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,   184,   881,
     215, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,   116,   353,
   -1791, -1791, -1791, -1791, -1791, -1791,  4827,  4827,   291, 12019,
     405,   418, 13347, -1791,   429, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791,  3082, -1791,   171,   348, -1791, -1791,
   -1791, -1791, -1791, 16460, -1791, -1791,   420,   454,   435,   384,
   -1791,  4827,   454,   454,   454,   497,  4583,   688,   852, 12181,
   -1791, -1791,   625, 16308,  1506, -1791, -1791, -1791,  2348,   709,
   14152, 10436,   743,  2348,  1013,   560, -1791, -1791, -1791, -1791,
     660, -1791, -1791, -1791, -1791,   566, -1791, -1791, -1791, -1791,
   -1791,   592,   637,   660, -1791,   660,   654, -1791, -1791, -1791,
   17478,  4827, -1791, -1791,  4827, -1791, 12019, -1791,   693, 17531,
   -1791, -1791,  4664,  8689, -1791, -1791,   911,   911,   729,  2600,
   -1791, -1791, -1791, -1791,   447, 13938,  2830,   660, -1791, -1791,
   -1791, -1791, -1791, -1791,   745, -1791,   748,   778,   785, -1791,
     826, 21177, -1791, -1791, -1791, -1791, -1791, -1791, -1791, 15263,
    3978,  3082,   288,   797,   800,   806,   831,   834,   840, -1791,
   -1791, 17683, 10839,   859, -1791, 17059, -1791, -1791, -1791, -1791,
     879, -1791, -1791,   883, -1791, 19179,  1025, 19401, -1791,   898,
    4827,   637,   908,   915,   954,   977, -1791, -1791, -1791,  3175,
    3491,  1001,  1007,    55, -1791, -1791,   660,   660,    98,   157,
      82,    98, -1791,   660,   660, -1791,  4405, -1791, -1791,  1012,
    1022,   911, 13830, -1791, 16460, -1791, -1791,  2348, -1791,  1352,
     560,   966,  1120,   157,  4827,  4827,   435, -1791, 13455, -1791,
     911,   911,  1052,  1120,   157,  4827, -1791, 18609, -1791, -1791,
   -1791,   911, -1791, -1791, -1791, -1791,   911, -1791,   666,  3298,
    4827, -1791,  1342,  1063, -1791, -1791, -1791, 16159,   637,   198,
   -1791, -1791, 18558, -1791,  1007,   181, -1791, 21177,  8689,  3681,
    4405, -1791,   109, -1791, -1791, -1791, 17531,  4827, -1791,  1064,
   -1791, -1791, -1791, -1791,  4827,  2897,   460,   478, -1791,  4827,
     748, -1791,   750,   660,   660,  1070, 17736,   571, 14421, 13991,
    2348,  2348, -1791,  2348,   911,  2348,   911, -1791, -1791,   660,
   -1791,  1075, -1791, 17888, -1791, -1791, -1791, 17941,   879, -1791,
     -18,   652,   134,   419,   560,  1067, -1791,  2600,  1069,   748,
    2600,  2083, -1791,  1094,  1148, 21251,  1108,  1119,  1125, 21177,
   21325,  1127, 14796, -1791, -1791, -1791, -1791, -1791, -1791, 21399,
   21399, 15107,  1135,  4009, -1791, -1791, -1791, -1791,   262, -1791,
     524, -1791,  1097, -1791, 21177, 21177, -1791,  1126,   639,   678,
     727,   656,   887,  1137,  1141,  1138,  1193,   202, -1791,   499,
   -1791,  1179, -1791,   876,  4747, 15575, -1791, -1791,   619,  1179,
   -1791, -1791,   712, -1791, -1791,  3978,  1186,  1202,  1204,  1206,
    1209,  1224, -1791, -1791,   277,  1228, -1791,   815,  1228, -1791,
   -1791, 17478, -1791,   900,  1236, 15731, -1791, -1791,  4235,  4161,
    1265, 14421,  1267,   776,   960, -1791, -1791, -1791, -1791, -1791,
    4827,  4763, -1791, -1791, -1791, -1791, -1791, -1791, 16052,  4745,
    1135, 19179,  1269,  1281, -1791, -1791,  1264, 19401,   809, -1791,
   -1791, -1791, 19475,  1280, -1791, -1791, -1791, -1791, -1791,  3175,
     782,  1293,  1296,  1309,   803,  1312,  1325,  1340,  3491, -1791,
   -1791,   660,  1346,   435,  1344, -1791, -1791,  1349, -1791, -1791,
     637,  1120, -1791, -1791, -1791,   637, -1791, -1791,  4405, -1791,
   15575, 15575, -1791,   911,  4664,  5885, 14582, -1791, -1791, -1791,
   -1791, -1791,   637,  1120,   181,  1347, -1791, -1791,  2348,  1360,
    1120,   157, -1791,   637,  1120, -1791, 21777, -1791,   911,   911,
   -1791, -1791,  1361,   311,  1378,   560,  1380, -1791, 16773, -1791,
     842, -1791,  1472, 19008, -1791,  4664, 16248, 13830, -1791, 16159,
   21473, -1791, -1791, -1791, -1791, -1791,  3681,   837,  4405, -1791,
   14582,  1007, 12019, -1791,  1386, -1791,  1392, -1791, -1791, -1791,
   -1791, -1791,  2600, -1791, -1791,  1471,  3538,  3271, 17941, 10839,
   -1791, 18093, -1791,   911,   911, -1791, -1791,   879, -1791,   849,
    1396,  1538, 21177,  1692,  1349,  1381, -1791,   660,   660, -1791,
    1228, -1791, 17736, -1791, -1791, 17220,   911,   911, -1791,  3538,
     660, -1791, 18413, -1791, -1791, 17888, -1791,   447, -1791, -1791,
   -1791,  1401,  4827,  1067,  1400,   848, 17531,   856, -1791, -1791,
   -1791, -1791, -1791, -1791,   901, -1791,  1409,  1389, -1791, 15419,
   -1791,  4009, 18146, 18146, -1791, 15419, -1791, 21177, -1791, -1791,
   -1791, -1791, -1791, -1791, 15419, -1791, -1791, 17273, 18146, 18146,
     876,  1329,  1518,   684,  1576, -1791,   922,  1415,   974,  1418,
   -1791, 19475, 21177, 19549,  1417, 21177,  1342, 21177,  1342, -1791,
    1205, -1791, -1791, 19623,  2952, 21177, 19623,  1342, -1791, -1791,
   21177, 21177, 21177, 21177, 21177, 21177, 21177, 21177, 21177, 21177,
   21177, 21177, 21177, 21177, 21177, 21177, 21177, 21177, 21177, 19697,
    1402,   826,  3950, 10839, -1791, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791,  1421, 21177, -1791, -1791,   619,
    1792, -1791, -1791,   660,   660, -1791, -1791, 15575, -1791,   394,
    1228, -1791,   934,  1228, -1791, -1791, -1791,  1349, -1791, -1791,
    1349, 21547, -1791, -1791, 10839,  1425,  1426,  3145,  1566,  3112,
     421,  1381, -1791,   660,   660,  1381,   448, -1791,   660,   660,
   21177,  4827,   993,  1030,  1381,   265, 13777, 13777,  4827, -1791,
   -1791, 21177,  1264, -1791, 19179,  1435, -1791,  2304, -1791, -1791,
   -1791, -1791, -1791,   939, -1791, 13777,  1342,  4664,  1342,   946,
    1442,  1444,  1446,   948,  1447,  1450,  1451,   463,  1228, -1791,
   -1791,   536,  1228, -1791, -1791, -1791,  4664,   826, -1791,  1228,
   18703, -1791,   637, 16773, -1791, -1791,   961,  1452,   964,  1453,
   -1791,  1458, -1791,   637, -1791,  1459, -1791,   637,  1120,  1458,
   -1791,   637,  1454,  1455,  1456, -1791, -1791, 17220, -1791,  1457,
   -1791, -1791, -1791,  1342,  4827,  9912,  1547,  1443, 18307, -1791,
    1236, -1791, 13777,   968, -1791, -1791,  1458, -1791, 17531, 15575,
    1445, -1791,  1445, -1791, -1791, -1791,   134,   660,   660, -1791,
   17888, -1791, 11004, 15887, -1791, 16773,  1465,  1468,  1469, -1791,
   11732,   660, -1791,  1692, -1791, -1791, -1791, -1791,  1349, -1791,
   -1791, -1791,   911, -1791,  3405, -1791, -1791,   560,   409,  1474,
    1460,  1466,   134, -1791, -1791,  1470,  1475,  2083, 19623, -1791,
    1478,  1476,   348,  1477,  1483,  1484,  1482,  1487, 21177,  1488,
    1490,  1491, 10839, 21177, -1791, -1791,  1640, -1791, -1791, -1791,
   21177, -1791,  1492,  1493, 19253,  1130, -1791, 19623,  1494, -1791,
    1504, -1791, -1791,  2225, -1791, -1791,   971, -1791, -1791, -1791,
   -1791,  2225, -1791, -1791,  1134,   689, -1791, -1791,  1126,  1126,
    1126,   639,   639,   678,   678,   727,   727,   727,   727,   656,
     656,   887,  1137,  1141,  1138,  1193, 21177,   933, -1791,  1496,
    2225, -1791, -1791, 19179, -1791, 16773,  1509,  1512,  1513,  1792,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,  1349, -1791,
   -1791,  1349, 16773, 16773, -1791, -1791,  3145,   892,  1515,  1516,
    1519,  1520,  3386,  3112, -1791, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,  1521, -1791,
    1381, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,  1522,
    1526, -1791,   435,  2225,  1142,    33, -1791, -1791,  1497, -1791,
   19401, -1791, 21177,   660, 19771, 13777, -1791, -1791, -1791,  1507,
     546,  1228, -1791,   548,  1228, -1791, -1791, -1791, -1791,  1349,
   -1791, -1791, -1791,  1349,  1007,  1532,  1349,   269, -1791,  1179,
    1529, -1791, -1791, -1791, -1791, -1791, -1791,  1537, -1791, -1791,
   -1791, 17531,  1458, -1791,   637, -1791, -1791, -1791, -1791, -1791,
    9193,  1539,  1533, -1791,   -20, -1791,   108,   363, 10674,  1548,
   14942,  1551,  1561,  2574,  2683,  2513, 19845,  1563, -1791, -1791,
    1564,  1565, -1791, -1791,   637, 21177, 21177,  1685,  1559,   683,
   -1791,  1646,  1567,  1545, -1791, -1791, -1791,  9737, -1791, -1791,
   -1791, -1791, -1791,   957, -1791, -1791, -1791,  1632, -1791, -1791,
   -1791,  1342, -1791, -1791, 12667, 16460,  1568, -1791,  4827, -1791,
    1553,  1571,  1575, -1791,  1150, -1791, -1791, -1791, -1791,  4664,
   -1791, -1791,  1560,  1569,   973, 17531,   748,   748,  1401,  1067,
    1067, -1791, -1791,  1135,  1236, 15731, -1791,  1179, -1791, 11169,
   -1791,   554,  1228, -1791,   911, 11853, -1791, -1791,   134,   660,
     660,   447,  4827, -1791, 19919, -1791,   134,  1401,  1573, -1791,
   -1791,  1002,   699, 17220, 10839,  1342, -1791,   699, 17326,   699,
   -1791, 21177, 21177, 21177, -1791, -1791, -1791, -1791, 21177, 21177,
    1578, 19179, -1791, -1791,  1582,   705, -1791, -1791, -1791,  2365,
   -1791, -1791,  1172, -1791,   148, -1791, 19623,  1175, -1791, 19475,
   -1791, -1791, 21177,  1572,  1195,  1208,  1264, -1791,   573,  1228,
   -1791, -1791, 16773, 16773, -1791, -1791,  1579,   584,  1228, -1791,
     586,  4086,   660,   660, -1791, -1791, 16773, 16773, -1791,  1584,
   -1791, 14582, 14582,  1588,  1585,  1587,  1594, -1791,  1592, 21177,
   21177,  1210,  1595, -1791, -1791, -1791, -1791, -1791, -1791, -1791,
    1600, 21177, -1791, -1791, -1791,  1349, -1791, -1791, -1791,  1349,
   16773, 16773,   435,   660, -1791, -1791,  1215, 21177, 18852,  1598,
    1606,  1612, -1791, -1791, -1791,  1614, 12822, 12977, 13132, 17531,
   13830, 18146, 18146,  1617, -1791,  1593,  1597,  2586,  8293, -1791,
     218,  4827, -1791, -1791,  4827, -1791, 19327,    17,   205, -1791,
   -1791, -1791, -1791, 21177,  1620,  1656, 10508, 10087, -1791,  1603,
   -1791,  1607, 21177,  1608, 19179,  1610, 21177, 19475, 21177,  1031,
   -1791,  1611,    75, -1791,   175,  1625, -1791, -1791,  1638, -1791,
    1615, -1791,  1616,  1631, 14942,   603, 13616,   660,   319, -1791,
   -1791, -1791,  1636, -1791,  1643, -1791,  1647, -1791,  1633, -1791,
    1639, -1791, -1791, -1791, -1791,  1650,  1651,  1652, 11334,  1653,
    1658,  1659, -1791,  1665, -1791, -1791, -1791,  1349, 21177, 21177,
    1236,  1666, -1791,  1401, -1791,  1067,   272,  1460, 19179, -1791,
    1401,  1662, -1791, 17531, -1791,  1016,  1668,  1667,  1011, -1791,
    1669, -1791, -1791, -1791, -1791, -1791, 19179,  1264, 19475, -1791,
    1704,  2225, -1791,  1704,  1704, -1791,  2225,  3616,  4420, -1791,
   -1791,  1217, -1791, -1791, -1791,  1676,  1675, -1791, -1791, -1791,
    1349, -1791, -1791,  1677,  1679,   660, -1791, -1791, -1791,  1349,
   -1791, -1791, -1791,  1681, -1791, -1791, -1791, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791,  1680, -1791, -1791,
   -1791, -1791,  1684,  1682,   660, -1791, 16773, 16773, -1791, -1791,
   -1791, -1791, 21177, -1791,   269, -1791,  1179, -1791, -1791, -1791,
    1691,  1695, -1791,  1617,  1617,  1617,  4252,  1074,  1670,   326,
   -1791,  4252,   339, 15575, -1791, -1791, -1791,  3746, 21177,  4491,
     341, -1791, -1791,    69,  1693,  1693,  4827, -1791, -1791, 16925,
   -1791, 21177,  1699,  1696, -1791, -1791, -1791, -1791,  1023,  1706,
   14942,  1567,  1708, 21177,   420,  1702,   497, 13294, 17531, -1791,
   -1791, -1791,   726, 14942, 21177,   931,   736, -1791, 21177, 19025,
   -1791, -1791,   385, -1791,  1264, -1791,  1049,  1082,  1088, -1791,
   -1791, -1791, -1791,   637,  1031,  1714, -1791, -1791, 21177, -1791,
    1715,   826, 10674, -1791, -1791, -1791, -1791, 21177,  1765, -1791,
    9562, -1791,   660, 14582, -1791, -1791, 17531, -1791, -1791, -1791,
     134,   134, -1791, -1791, -1791,  1719, -1791, 16773, -1791, -1791,
    1721, -1791,  1722,  1730,  1067,  1723, -1791, -1791,  1264,  1731,
   -1791, -1791,  1733, -1791, -1791, 21177, -1791, 17326, 21177,  1264,
    1734,  1229, -1791,  1231, -1791,  2225, -1791,  2225, -1791, -1791,
   -1791, -1791, 16773,  1735,  1736, -1791, -1791, 16773, 16773,  1737,
    1738,  1234, 14099, 14260, -1791,  1739, -1791, -1791, -1791, -1791,
    1740,  1741,  1240, 21177, -1791, -1791, -1791, -1791, -1791,   488,
    1074,  2095,   490, -1791, -1791, -1791, -1791,   660,   660, -1791,
   -1791, -1791,   610, -1791,  1093,  3746,   722, -1791,  4491,   660,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,   618, 14942,
     115, 19993,  1813, 14942,  1567, 14743, -1791, -1791, -1791, -1791,
   21177, -1791, 20067,  1822,  1726, 19102, 20141, 14942, 10262,  1567,
     313,  1015,  1727, 21177, -1791,  1747,   121, 14942, -1791, -1791,
    1753, -1791, -1791,  1732,   826,   737,  1745,  1763,  1259,  1817,
   -1791, -1791, -1791, -1791,  4827,  4664,  1401,  1401, -1791, -1791,
    1761,  1762, -1791, -1791, -1791,  1760,   134,  1770, -1791,  1771,
   -1791, -1791, -1791, -1791,  1772, -1791, -1791, -1791,  1263,  1273,
   -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791, -1791,
    1774, -1791, -1791,  1777,  1778, -1791, -1791, -1791, -1791, -1791,
    1779,  1785,  1788,  2095, -1791,   660, -1791, -1791, -1791, -1791,
   -1791,  1769,  4252, -1791, -1791,  8867,    91, 11502, -1791, 14837,
   -1791,    19,  1101, 14942,  1851,   647,  1783,   334, 14942, 21177,
    1790,   313,  1015,  1775, 21621,  1786,   481,  1878, -1791, 20215,
   20289, 21177,  1567,  1780, 11666, -1791, -1791, -1791, 18360, -1791,
    1799,  1782,   -25, 14942, -1791, 21177, 19623,   323, -1791, -1791,
   -1791,  1811,  1814,  1815, -1791, -1791,   134,  1401, -1791, -1791,
   -1791, -1791, -1791,  1816,  1818,  1819, 14582,  1810, -1791, -1791,
     587,  1228, -1791, -1791,  1074, -1791, -1791,   132, -1791,   220,
   -1791, -1791, -1791,  1826, 12343, -1791, -1791, 14942, -1791,    50,
   -1791, 14942, 21177,  1828, 20363, -1791, -1791, 20437, 20511, 21177,
    1790,  1567, 20585, 20659, 14942,  1820,   482,  1823,   502, -1791,
   -1791,  1831, 12343, 18360, -1791,  4557, 18093,  1342,  1825, -1791,
    1882,  1840,   761,  1836, -1791,  1920, -1791,  1109, 14942,  1844,
   14942, 14942, -1791, -1791, -1791,  1401,  1847, -1791, -1791, -1791,
   -1791, -1791, -1791, -1791,  1349, -1791, 21177, -1791, 21177, -1791,
   -1791,  1357, 12505, -1791, -1791, 14942, -1791, -1791,  1567, -1791,
   -1791,  1567,  1832,   515,  1833,   550, -1791, -1791,  1567, -1791,
    1567, -1791,  1846, 20733, 20807, 20881, -1791,  1357, -1791,  1827,
    2885,  3465, -1791, -1791, -1791,   -25,  1848, 21177,  1830,   -25,
     -25, 14942, -1791, -1791, 21177,  1893,  1898,  1858, -1791, 16773,
   -1791, -1791, 14837, -1791,  1357, -1791, -1791,  1859, 20955, 21029,
   21103, -1791, -1791,  1567, -1791,  1567, -1791,  1567, -1791,  1827,
   21177,  1861,  3465,  1857,   826,  1864, -1791,   799, -1791, -1791,
    1115,  1817,   474, -1791, -1791, -1791,  9318,  1868, 14837, -1791,
   -1791,  1567, -1791,  1567, -1791,  1567,  1869,  1867, -1791,   637,
     826,  1870, -1791,  1853,   826, -1791, -1791, 14942,  1949,  1871,
   -1791, -1791, -1791,  9440, -1791,   637, -1791, -1791,  1278, 21177,
   -1791,  1118, -1791, 14942, -1791, -1791,   826,  1342,  1874,  1855,
   -1791, -1791, -1791,  1132, -1791, -1791,  1866,  1342, -1791, -1791
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
     461,     0,  1041,  1042,     0,     1,   461,    16,     0,   461,
     429,   430,     0,   542,   471,   455,   456,   457,   785,     0,
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
     557,     0,   798,     2,     0,     0,   698,   422,   542,   521,
     538,   553,     0,   798,     2,     0,   477,   522,   529,   530,
     448,   539,   450,   451,   449,   544,   554,   558,     0,   572,
       0,   768,     2,     2,   796,   853,   855,   461,     0,     2,
       2,  1051,   542,  1054,   814,   814,     3,     0,   542,     0,
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
     543,   772,     0,   798,   814,     0,   415,   423,   475,     0,
     798,     2,   772,     0,   798,   747,   523,   524,   540,   555,
     561,   564,   559,   565,   583,   584,     0,   748,   461,   688,
       0,   201,   398,   461,     3,     0,   542,   461,   797,   461,
       0,   417,     2,   418,   769,   437,     0,     0,     0,     2,
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
     434,     2,   803,     0,   419,     2,   772,     0,   798,     2,
     804,     0,     0,     0,     0,   598,   667,   462,     3,     3,
     671,   670,   874,     0,     0,   461,   399,     0,   542,     3,
     100,     3,   461,     0,     3,   773,     2,   726,   461,   461,
     720,   719,   720,   519,   517,   642,   640,   816,   816,  1024,
     461,  1029,   462,   461,  1015,   461,     0,     0,     0,   993,
       0,   816,  1063,   979,   980,   687,   976,   977,   991,  1019,
    1023,  1021,   548,   583,     0,  1027,  1032,   645,   640,     0,
     650,     0,   640,   752,   750,     0,     0,   823,    66,   784,
       0,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   461,     0,   121,   120,     0,   117,   116,    27,
       0,    28,     0,     0,     0,     0,     3,    66,     0,    51,
       0,    52,    59,     0,    58,    70,     0,    67,    68,    71,
      54,     0,    53,    57,     0,     0,    50,   123,   125,   126,
     127,   129,   130,   132,   133,   137,   138,   135,   136,   140,
     141,   143,   145,   147,   149,   151,     0,     0,   408,     0,
       0,    29,     3,   704,   157,   461,     0,     0,     0,   944,
     945,   941,   942,   745,   744,     2,   864,   866,   868,     2,
     884,   886,   461,   461,   969,   968,     2,     0,     0,     0,
       0,     0,   816,   979,   916,   933,     2,   911,   919,   684,
     914,   915,   685,     2,   926,   936,   929,   930,     0,     3,
    1062,   427,     2,  1055,     2,   675,   676,   654,     3,     3,
       3,     3,   698,     0,   155,     0,     3,     3,     0,   707,
       0,   701,     0,   816,     0,   461,     3,   431,   433,     0,
     816,   848,   852,   816,   905,   910,     2,   840,   843,   845,
       2,   897,   900,   902,   814,     0,   956,     3,   960,   961,
       3,   806,     3,   532,   531,   534,   533,     2,   773,   807,
     754,   461,     2,   805,     0,   773,   808,   598,   598,   598,
     461,     0,     0,   689,     0,   402,     0,     0,   461,     0,
       2,     0,     0,     0,     0,     0,   184,     0,   332,   333,
       0,     0,   371,   370,     0,   159,   159,   377,   559,   565,
     198,     0,   185,     0,   209,   186,   187,   461,   203,   188,
     189,   190,   191,     0,   192,   193,   338,     0,   194,   195,
     196,     0,   197,   205,   542,   461,     0,   207,     0,   396,
       0,     0,     0,     3,     0,   786,   773,   761,   762,     0,
       3,   757,     3,     3,     0,   461,   736,   736,  1060,   640,
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
       0,     0,     3,   773,   809,     0,   461,   461,   461,   461,
     461,   461,   461,   581,   610,     3,     3,   611,   542,   599,
       0,     0,   856,     2,     0,   400,    66,     0,     0,   323,
     324,   206,   208,     0,     0,     0,   461,   461,   319,     0,
     317,     0,     0,     0,   704,     0,     0,     0,     0,     0,
     160,     0,     0,   378,     0,     0,     3,   213,     0,   204,
       0,   314,     0,     0,     2,     0,   542,   816,     0,   397,
     971,   970,     0,     2,     0,   764,     2,   759,     0,   760,
       0,   740,   721,   725,   723,     0,     0,     0,   461,     0,
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
       2,     0,   792,   581,   581,   581,   611,   617,   586,     0,
     623,   611,     0,   461,   573,   609,   605,     0,     0,     0,
       0,   612,   614,   816,   625,   625,     0,   606,   621,   461,
     403,     0,     0,    67,   327,   328,   325,   326,     0,     0,
       2,   224,     0,     0,   226,   411,   225,   542,   461,   305,
     304,   306,     0,     2,   184,   264,     0,   257,     0,   184,
     320,   318,     0,   312,  1060,   321,     0,     0,     0,   359,
     360,   361,   362,     0,   352,     0,   353,   329,     0,   330,
       0,     0,   461,   215,   202,   316,   315,     0,   350,   369,
       0,   401,   816,   461,   788,   741,   461,     2,     2,   633,
     640,   640,  1034,  1035,  1036,     0,   987,   461,     3,     3,
       0,   995,     0,     0,   640,     0,   647,   646,  1060,     0,
     631,     3,     0,   973,    98,     0,    31,   461,     0,  1060,
       0,     0,    85,     0,    73,     0,    79,     0,    77,    43,
     158,   952,   461,     0,     0,   857,   875,   461,   461,     0,
       0,     0,   461,   461,   717,     0,   424,   426,     3,     3,
       0,     0,     0,     0,   756,   794,   577,   579,   575,     0,
       0,  1002,     0,   618,  1007,   620,   999,   816,   816,   604,
     624,   608,     0,   607,     0,     0,     0,   627,     0,   816,
     600,   615,   626,   616,   622,   669,   673,   672,     0,     2,
       0,     0,   245,     2,   227,   542,   310,   308,   311,   307,
       0,   309,     0,   253,     0,   184,     0,     2,   461,   265,
       0,   290,     0,     0,   313,     0,     0,     2,   336,   363,
       0,   354,     2,     0,     0,     0,     0,   341,     0,   337,
     200,   199,   425,   758,     0,     0,  1060,  1060,  1037,     3,
       0,     0,   994,   996,   632,     0,   640,     0,   630,     2,
      49,    41,    39,    40,     0,    63,   177,    76,     0,     0,
       3,   858,   876,     3,     3,   923,   938,   428,     2,   659,
       3,   658,   716,     0,     0,   849,   907,   957,   965,   602,
       0,     0,     0,  1003,  1004,   816,   603,  1000,  1001,   601,
     582,     0,     0,   214,   335,     0,     0,     0,   238,     2,
     216,     0,     0,     2,   247,   262,   273,   267,     2,   184,
     302,     0,   277,     0,     0,   268,   266,   255,   258,     0,
       0,   184,   291,     0,     0,   219,   334,     2,   461,   331,
       0,     0,   379,     2,   339,     0,    66,     0,   351,   763,
     765,     0,     0,     0,   997,   998,   640,  1060,   652,   755,
      64,    80,    78,     0,     0,     0,   461,     0,   850,   908,
     816,  1010,  1012,  1005,     0,   613,   233,   228,   231,     0,
     230,   237,   236,     0,   461,   240,   239,     2,   249,     0,
     246,     2,     0,     0,     0,   254,   259,     0,     0,   184,
     303,   278,     0,     0,     2,     0,   293,   294,   292,   261,
     322,     0,   461,   461,     3,   364,   462,   368,     0,   372,
       0,     0,     0,   380,   381,   222,   342,     0,     2,     0,
       2,     2,   636,   638,   988,  1060,     0,   953,   924,   939,
     663,     2,  1006,  1008,  1009,   619,     0,   235,     0,   234,
     218,   241,   461,   392,   250,     2,   251,   248,   263,   276,
     274,   270,   282,   280,   281,   279,   260,   275,   271,   272,
     269,   256,     0,     0,     0,     0,   221,   241,     3,   357,
       0,  1002,   365,   366,   367,   379,     0,     0,     0,   379,
       0,     2,   340,   347,     0,   344,   346,     0,   637,   461,
     229,   232,     2,     3,   242,   393,   252,     0,     0,     0,
       0,   301,   299,   296,   300,   297,   298,   295,     3,   357,
       0,     0,  1003,     0,     0,     0,   373,     0,   382,   223,
       0,   337,     0,   635,     3,   210,     0,     0,     2,   289,
     287,   284,   288,   285,   286,   283,     0,     0,   358,     0,
     385,     0,   383,     0,   385,   343,   345,     2,     0,     0,
     212,   211,   217,     0,   220,     0,   355,   386,     0,     0,
     374,     0,   348,     2,  1011,   356,     0,     0,     0,     0,
     349,   387,   388,     0,   384,   375,     0,     0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1791,  5864,  5003, -1791,    -1,   397,  1556,  -176, -1791,  -332,
   -1791,   349, -1791,  -704,   655,   752,  -967, -1047, -1791,   180,
    6158,  1974, -1791,  1306, -1791,  1351,   190,   744,   746,   608,
     742,  1311,  1314,  1310,  1315,  1313, -1791,    21,  -163,  7670,
     888, -1791,  1629, -1791, -1791,  -655,  7237, -1080,  2290, -1791,
      -6, -1791,   878,   -42, -1791, -1791, -1791,   422,    52, -1791,
   -1694, -1505,   276,    28, -1791, -1791, -1791,   289, -1549, -1791,
   -1406, -1791, -1791, -1791, -1791,   -26, -1750,   162, -1791, -1791,
     -19, -1791, -1791, -1791,    -4,   442,   444,   112, -1791, -1791,
   -1791, -1791,  -860, -1791,    32,   -30, -1791,   114, -1791,  -196,
   -1791, -1791, -1791,   889,  -399,  -856, -1305, -1791,    16, -1114,
     293,  7453,  -815,  -800, -1791,  -280, -1791,     7, -1791,  -144,
     158,  -141,  -242,  3674,   373,  -609,    62,    87,  1203,  2268,
    1295, -1791,  2040, -1791,   301,  3878, -1791,  1979, -1791,   285,
   -1791, -1791,  2248,   371,  4041,  2782,   -66,  1834,  -281, -1791,
   -1791, -1791, -1791, -1791,  -585,  4822,   544, -1791,  -364,  -114,
   -1791,  -765,   230, -1791,   160,   728, -1791,   513,  -103, -1791,
   -1791, -1791,  -333,  5373,  -895,  1166,    26,  -708,  -692,   -53,
    1613, -1791, -1291,  -151,   928,  -263,   914,  2588,   -59,  -421,
    -251,  -172,  -448,  1292, -1791,  1619,  -128,  1211,  1523, -1791,
   -1791, -1791, -1791,   287,  -154,  -217,  -874, -1791,   317, -1791,
   -1791, -1090,   462, -1791, -1791, -1791,  2124,  -732,  -469,  -957,
     -23, -1791, -1791, -1791, -1791, -1791, -1791,   227,  -835,  -126,
   -1790,  -198,  7111,   -69,  6237, -1791,  1181, -1791,  3174,  -202,
    -223,  -222,  -206,     1,   -73,   -61,   -55,    80,    -9,    -5,
      22,  -187,   -80,  -181,  -177,  -170,  -715,  -653,  -652,  -639,
    -700,  -112,  -626, -1791, -1791,  -698,  1370,  1372,  1374,  1993,
   -1791,   580,  6824, -1791,  -552,  -590,  -584,  -582,  -752, -1791,
   -1630, -1651, -1650, -1649,  -592,   -10,  -289, -1791, -1791,   -44,
     816,   -89, -1791,  7174,  2318,  -330,  -548
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1171,   223,   398,   399,    81,    82,   400,   374,   401,
    1478,  1479,   402,   986,   987,   988,  1282,  1283,  1284,  1490,
     424,   404,   405,   406,   693,   694,   407,   408,   409,   410,
     411,   412,   413,   414,   415,   416,   417,   426,  1085,   695,
    1411,   756,   217,   758,   420,   823,  1172,  1173,  1174,  1175,
    1176,  1177,  1178,  2096,  1179,  1180,  1416,  1602,  1938,  1939,
    1868,  1869,  1870,  2063,  2064,  1181,  1616,  1617,  1618,  1771,
    1772,  1182,  1183,  1184,  1185,  1186,  1187,  1424,  1798,  1991,
    1908,  1188,  1189,  1634,  2081,  1635,  1636,  1974,  1190,  1191,
    1192,  1414,  1982,  1983,  1984,  2128,  2143,  2011,  2012,   298,
     299,   885,   886,  1144,    84,    85,    86,    87,    88,    89,
     457,    91,    92,    93,    94,    95,   231,   575,   280,   459,
     428,   460,    98,   308,   100,   101,   154,   339,   340,   105,
     106,   169,   107,   904,   341,   155,   110,   251,   111,   156,
     259,   343,   344,   345,   157,   421,   116,   117,   347,   118,
     566,   874,   872,   873,  1574,   348,   349,   121,   122,  1140,
    1379,  1580,  1581,  1732,  1733,  1380,  1569,  1751,  1582,   123,
     653,  1674,   650,   350,   651,   652,  1245,  1078,   465,   466,
     878,   879,   467,   468,   880,   352,   570,  1196,   430,   431,
     218,   485,   486,   487,   488,   489,   327,  1216,   328,   902,
     900,   600,   329,   368,   330,   331,   432,   125,   175,   176,
     126,  1210,  1211,  1212,  1213,     2,  1127,  1128,   592,  1205,
     127,   318,   319,   261,   272,   549,   128,   221,   129,   232,
    1087,   864,   515,   167,   130,   664,   665,   666,   131,   234,
     235,   236,   237,   313,   133,   134,   135,   136,   137,   138,
     139,   240,   314,   242,   243,   244,   791,   792,   793,   794,
     795,   245,   797,   798,   799,   761,   762,   763,   764,   516,
    1120,  1357,   140,  1682,   625,   626,   627,   628,   629,   630,
    1735,  1736,  1737,  1738,   615,   470,   355,   356,   357,   433,
     209,   142,   143,   144,   359,   815,   631
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   193,   132,    80,   373,   191,   501,   502,    96,   532,
     200,  1218,   995,   194,   351,   546,   241,   574,   419,   195,
     150,   337,   369,   503,  1287,  1214,   311,   698,   509,   916,
    1530,  1531,   654,   812,   513,   917,   179,   918,  1058,   208,
     930,  1362,   504,   521,  1065,   491,   975,  1247,   505,  1907,
    1197,   303,   506,  1294,   365,    80,    80,   633,    80,   507,
     132,   704,  1030,   102,   643,   196,    96,   543,   646,   197,
    1783,   924,   861,    80,  1054,  1940,  1405,   925,   554,  1850,
    1851,  1852,    80,   869,   206,   968,   501,   502,   103,  1055,
      80,  1604,  1941,   372,  1947,    80,   198,   238,    80,  1081,
     262,  1854,    80,   503,   273,   208,   581,   583,   509,   857,
     859,  -766,   452,   604,  1079,  1079,  1328,  1096,   436,   266,
     529,   102,   504,   306,   896,  2015,   294,    58,   505,   145,
     437,  1980,   506,  1079,  1048,  1049,   438,  1135,  1384,   507,
      80,    58,  -931,    80,   647,    80,   103,   132,  1050,  -931,
     510,    80,   604,    96,    58,   354,   193,  1385,    80,    97,
     499,  1051,   152,   207,  1206,    80,   160,  1329,   194,   161,
     162,  1361,   163,  1638,   195,  1946,   239,   293,  1365,   263,
     633,    58,   439,   274,  1203,  -767,   440,   219,    58,    80,
      80,   206,  1865,  1866,  1594,   610,  1948,   916,  1865,  1866,
    1079,  1330,   418,   917,    80,   918,   473,   292,   102,  1776,
     538,   514,   641,   441,   482,   494,   644,    97,  1940,    80,
     196,  2007,   561,  1933,   197,   514,  1884,  2016,    80,    80,
     510,   206,  1492,   103,   522,   944,   193,   924,   514,   655,
     586,   204,   657,  1059,  1251,    80,  -394,  1062,   194,  1434,
    1942,   198,  1639,    80,   195,   206,  1075,  1076,  -798,  1640,
    1386,   589,   848,    80,    80,   514,  2006,    80,   165,   550,
     207,  1497,   538,  1275,    80,  1867,   830,   831,   199,    64,
     512,  1894,  1605,  1605,  1374,   474,   112,  -395,    80,    80,
     158,    80,   578,   832,    90,  1058,   648,   151,   301,   889,
    1330,   649,   108,  1498,    97,   881,   844,   204,    80,    80,
     207,   206,   833,  1392,   610,   816,    80,  -394,   834,   911,
     941,   939,   835,    80,    80,  1375,   616,  1641,    80,   836,
     633,  1301,   699,   582,   207,   738,   159,  1604,  1314,   951,
    1376,  1907,  1197,  1463,   112,  2043,  1034,   796,   551,    58,
     993,  1470,    90,  1315,   633,   868,   936,  1266,  -395,   450,
     108,   633,   783,   830,   831,   293,    80,   164,  1946,    80,
     663,  1237,   113,    20,  1889,  1890,  1586,   739,  2008,  2009,
     832,   208,  1596,  1681,   577,   697,   844,  1339,   270,  1850,
    1851,  1852,   255,  1048,  1049,  1587,   267,  1132,  1988,   833,
    1363,   148,   855,  1079,  1946,   834,  1561,  1050,   860,   835,
     534,  1854,   537,   200,  1423,  1354,   836,   582,   705,  1241,
    1306,  1530,  1531,   706,  1387,  1388,   436,   288,   845,   774,
     113,   112,  1989,   514,    80,   186,   473,  1355,   437,    90,
     292,   622,   442,   178,   438,   616,   337,   108,  1244,  1694,
    1696,  1698,  1933,   173,   173,    97,   916,    80,    80,  1766,
    1767,  1768,   917,   189,   918,   520,    58,   909,   525,    80,
      80,  -578,   256,   219,   537,  1957,  1958,  1384,    80,  1456,
     482,  1769,  1088,   278,  1740,   285,  1193,   287,   173,   542,
     439,   929,   970,    58,   440,   908,  1651,  1586,    80,  1749,
     553,  1237,  1954,  1741,   935,   201,  2062,    80,   845,   473,
    1374,  1374,  1374,   436,   851,   474,  1743,   113,  1750,   854,
      58,   441,  1320,   293,  1693,   437,   256,    80,  1605,   285,
     287,   438,  2062,    80,  1774,    58,   862,   894,   173,  1782,
    1389,   173,   220,   970,  -798,   120,  1035,   870,   120,  2117,
     514,  1375,  1375,  1375,    63,    64,   173,   180,  1522,  2098,
   -1061,   547,  1784,   363,  1161,  1019,  1376,  1376,  1376,   895,
     181,   648,    80,  1056,    80,  1296,   649,   620,   256,  -629,
     354,   189,   112,   204,   214,    80,  -629,    80,   474,   473,
     881,    80,   555,   132,   597,   215,   633,   212,   108,    96,
    1063,    80,    76,   120,   620,    80,    80,   567,    58,   782,
     950,   216,    -3,   953,   954,  1106,   955,   173,    58,   514,
      58,  1222,   616,   598,   599,   957,    58,   120,   959,   960,
     961,   633,   601,   930,  1501,  2000,   602,  1069,    80,   970,
     970,   256,  1089,   285,   287,    58,  1749,   120,  1855,  1963,
    2033,    80,   881,   740,   102,   284,    58,   741,    58,    58,
     970,   173,   173,   697,  1605,  1849,   577,  1856,   113,   697,
    2035,  1115,   173,   970,   226,   256,   561,   796,   697,   103,
     707,   256,  1592,  2068,   120,   708,   564,   173,  1110,   569,
     120,   246,   514,   120,   265,  1883,   913,   697,  1342,  1744,
    1346,  1098,   514,    73,   514,    80,  1454,    80,   970,    80,
     620,   256,  -455,    80,   173,   288,    80,   638,  2070,   287,
    1114,   173,   173,   619,   290,  1507,   173,   620,  1828,   514,
    1829,   152,    58,   120,    78,   621,  1516,    97,  1520,  2001,
     514,    80,   620,   514,   292,   120,  -456,   622,   852,  1391,
      97,    73,  1627,  1080,  1080,   681,    14,    15,    16,    17,
      18,   883,   881,   293,   173,  1806,  1807,   173,  1749,  1221,
     863,   759,  1080,   560,    64,   514,   970,   867,  1193,   881,
     881,   871,    78,    79,   728,   729,    80,  1859,    80,    14,
      15,    16,    17,    18,  1442,  1863,   120,   293,   120,  1956,
      80,  1729,   721,   120,   189,   970,  1742,    80,   256,   722,
     723,  1969,  -932,   482,  -394,    58,    80,   141,   337,  -932,
     141,   418,  1240,  1487,  1952,    80,    80,    80,   730,   731,
    1605,  1327,  1084,  1691,   256,   189,   638,   287,   963,  1080,
    1290,   120,  -692,   724,   725,    80,  1121,  1286,    58,   964,
     965,   681,  1270,   726,   727,   173,  1489,  1129,  1605,  1271,
     120,  1133,   270,  1286,   112,  1136,   766,   173,   173,   307,
     767,   913,  1766,  1767,  1768,   141,   442,   112,   514,  2026,
     108,   256,    80,    80,   482,    90,  1446,  1447,  1445,  -787,
    1777,  1901,    96,   108,  1769,  1778,  1902,   367,  1605,   247,
     248,   256,   249,  1770,   605,   288,   256,   250,   256,   894,
    -459,   998,   999,  1000,  1207,  2048,  1352,  1471,  1334,   141,
    2049,  1917,   325,  1683,    14,    15,    16,    17,    18,   256,
     370,   256,   256,    80,   292,   120,   442,   371,   514,   160,
    1313,   796,   161,   162,   372,   163,   663,   102,   633,   256,
     113,   443,   354,  2113,   444,   522,  1624,   840,  2114,   514,
     445,   256,   141,   113,   817,   818,  1505,   778,   819,   120,
     148,   514,  1195,    14,    15,    16,    17,    18,  1726,  1727,
    1728,    73,    80,    58,   256,   446,   638,   287,   447,   589,
      80,   442,  1458,   514,   448,   120,   882,  1603,  1619,   173,
     883,   619,   943,   905,   907,   620,   602,  1568,   256,   638,
     945,   472,    78,    79,   602,   256,  -457,   732,   733,    80,
    1678,  1995,   482,  1366,  1367,  1368,    14,    15,    16,    17,
      18,   476,    58,   201,   701,  1420,   933,  1291,  1689,   881,
     881,   477,  1080,    97,  1056,    80,   442,   490,   620,   173,
     492,    80,    80,   881,   881,   946,  1209,   608,   701,   947,
     495,   147,   369,   369,   171,   172,    65,    66,    67,    68,
      69,    70,    71,   496,   120,   120,   969,  1766,  1767,  1768,
     970,  1469,    80,  1435,   929,    58,  1039,   881,   881,   970,
     514,   970,  1292,  1093,  -460,  1134,  1381,  1094,   292,  1769,
     522,  1550,   514,   979,   514,   981,   512,   984,  1775,  1475,
     337,   992,   497,  1084,   996,  1123,  1606,   120,  1125,   970,
     589,   120,   970,   120,   514,  1285,   541,  1441,  1364,  1286,
     469,   767,  1675,  1673,  1421,   498,   120,   972,   973,  1021,
    1679,  1390,  1629,  1630,  1631,  1632,  1633,  1207,    73,   482,
    1071,  1072,    80,    80,    80,    96,  1474,  1690,  1409,   511,
    1286,  1766,  1767,  1768,   530,  1686,  1538,  1539,   619,  1687,
     112,  1532,   620,   970,   531,  1584,   482,  1760,    90,    78,
     621,   970,    80,  1769,    96,  2083,   108,  1073,  1074,  2087,
      80,  1208,  -185,    80,    80,   262,   273,    80,   173,  1480,
     120,   894,  1987,  1786,   104,   173,    73,   970,    80,   219,
     102,   266,   552,   120,   571,   616,   120,   120,   709,   120,
     710,   711,   712,  1097,   593,  1099,  1730,   608,   120,   640,
     514,   120,   120,   120,   649,  1195,  1787,    78,    79,   102,
    1094,    80,  1788,  1603,   354,   656,   970,  1860,   667,   713,
     256,   767,   714,   715,    80,  1949,   113,   716,   717,   970,
     671,   256,   104,  2052,  1195,   418,   668,  1286,  1529,  2115,
     482,   672,  2139,   970,   263,   274,  2136,   673,    80,   677,
    1143,   173,   173,   881,   881,   256,  2146,  1273,  1094,  1209,
    2147,  1288,  1289,   701,  1785,   720,   256,  1585,  1377,  -156,
    -156,   734,   257,   735,   337,   256,    97,  1073,  1433,   147,
      80,   120,   736,  1502,    65,    66,    67,    68,    69,    70,
      71,   982,  1381,  1381,  1381,   737,  1757,  1570,  1381,  1495,
    1496,  1239,  1500,  1496,   742,    97,  1005,  1006,  1007,  1008,
     768,  1815,  -121,  -121,  -121,  -121,  -121,  -121,  1817,   104,
     418,   418,  1504,  1496,   501,   502,   769,   188,   770,  1824,
     771,   983,  1606,   772,  1584,  1045,  1488,  1540,  1488,  1584,
      19,   503,  1045,  1552,  1699,  1094,    80,   509,   773,  1619,
      80,  1595,  1597,    80,   449,   150,  1826,  1094,  1827,  1496,
     504,  1837,  1838,    -3,   258,   894,   505,  1847,   970,   800,
     506,  -458,   256,   482,   881,   279,   282,   507,   141,    48,
      49,    50,    51,    52,    53,    54,    55,  1905,  1906,  1649,
    1921,  1496,   814,   482,  1208,    80,   256,   550,   -17,   120,
    1922,  1496,   824,   112,  1865,  1866,  2136,  2137,   354,   881,
     813,    90,   120,   120,   881,   881,   147,   837,   258,   108,
     838,    65,    66,    67,    68,    69,    70,    71,   102,   102,
    1493,  1494,   112,   839,   469,  1795,   841,   482,  1001,  1002,
      90,   548,  1003,  1004,  1009,  1010,  1911,  1912,   108,   842,
     270,  1752,  1752,  1608,  1608,   482,  1585,   255,   267,  1207,
      80,  1585,  1676,  1677,   843,    80,    80,    80,   847,   510,
     258,   849,  1532,  1443,  1444,   300,   551,   865,  1745,   337,
     182,     6,     7,     8,     9,    10,    11,    12,    13,   113,
     866,  -576,   830,   831,  1377,  1377,  1377,   152,  1566,  1567,
    1571,  -120,  -120,  -120,  -120,  -120,  -120,   469,  -574,   832,
     875,   257,   884,   897,   899,  2013,   844,   173,   113,   903,
     173,   173,   173,   919,    97,    97,   921,   622,   833,   938,
     942,   948,  1532,   258,   834,    80,   949,   256,   835,   971,
      80,   275,   974,  2013,   173,   836,    80,   977,    80,  1018,
     173,  1023,  1044,  1045,  1052,    80,  1091,  1996,   569,    14,
      15,    16,    17,    18,   967,   173,  1100,   258,  1101,   482,
    1102,  1103,   256,   258,  1104,  1105,  1124,  1126,   256,  -770,
    1130,  -668,   482,  2065,  1137,  1138,  1139,  1198,  1900,  1231,
    1199,  1215,  1232,  1233,   266,  1243,  1246,  1789,  1480,  1249,
    1248,  1209,  1252,   258,  1253,  1255,  1244,  1256,  1257,   173,
    1258,  1259,  1261,   354,  1262,  1263,  1268,  1269,  1333,   482,
    1293,  1276,  1476,    14,    15,    16,    17,    18,  1265,  1840,
    1584,  1277,   151,  1298,   257,  2057,  1299,  1300,   845,  1307,
    1308,   469,  1207,  1309,  1310,   120,  -656,   683,  1318,  1910,
    -655,   112,   112,  1341,   120,   674,  1353,  1358,  -771,    90,
      90,  1383,   120,  1382,    80,   633,    80,   108,   108,  1413,
    1393,   141,  1937,  1396,   102,    14,    15,    16,    17,    18,
     718,   719,   469,  1397,   141,  1406,  1407,  1408,  -691,   547,
    1415,   120,  1417,  1423,  1472,   970,  1981,  1427,  1430,  1608,
    1429,   718,  1431,  1515,   469,   469,  1599,  1437,  1486,   120,
    1488,  1528,  1533,  1534,    80,  1535,  1439,    80,  1536,  1503,
    1496,   256,  1541,   469,  1544,  1557,   258,  1975,   482,   120,
    1558,   718,   482,  1559,    58,  1562,  1208,   113,   113,  1573,
    1575,   483,  1386,   683,  1576,  1532,   482,  1642,   173,   257,
    1620,   173,  1585,  1647,  1621,  1623,   482,  1625,  1637,  1644,
    1652,  1657,  1645,  1646,  1654,   104,   881,  1658,  1655,   256,
      97,  1659,   120,    80,    80,    14,    15,    16,    17,    18,
    1662,  1660,  1661,  1680,  1209,  1663,  1664,   501,   502,  1666,
     469,   173,  1684,  1671,    73,  1692,  1685,  1700,  1688,  1701,
     258,  1705,  1975,  1706,   503,   442,  1716,  1714,   548,   509,
     102,  1540,  1724,  2060,   619,  1937,  1725,  1739,   620,  2042,
    1759,   258,  1578,   504,  1977,    78,   621,  1286,  1761,   505,
     220,    80,  1763,   506,    58,  1608,  1792,  1794,   482,  1981,
     507,   258,   482,  1981,  1981,  1799,  1808,   482,  1812,  1813,
     844,  1814,  1818,  1816,  2085,  1825,   418,  1820,  1873,  1831,
    1832,  1835,  1836,   270,  1845,  1846,  1842,  1878,  1893,  1903,
     255,   267,   482,  1879,  1891,  1897,   258,  1161,  2111,  1899,
     120,   120,   120,   120,   120,   120,   120,  1904,  1914,  1915,
    1916,  1918,  1919,  1920,    73,   514,  1951,   112,  -657,  1977,
     258,  1928,  1929,  1930,  2127,    90,    97,   258,  2127,  1931,
     120,   120,  1932,   108,   759,  -559,   482,   141,   514,  1208,
     482,  1953,  1959,  1964,  1962,    78,    79,  1970,  1978,  1979,
    2141,   510,  1992,   482,   141,  1993,  2138,   193,  1838,  1994,
    1997,   586,  1998,  1999,    80,    83,    80,  2010,   149,   194,
     256,  2019,  2036,   173,  2045,   195,  2046,   482,  2032,   482,
     482,  2034,  2047,   141,  2050,  2051,  2054,   173,  2058,  2071,
    2067,  2069,   845,  2091,  2080,  2084,   102,  2086,  2092,  2093,
     173,   141,  2099,   113,   482,  2109,  2110,   120,  2112,  2122,
    2124,  2125,  2129,   469,  2133,  2134,   997,   418,  2144,   418,
    2130,  1608,  2145,    83,   102,   810,  1822,   483,  1499,    80,
      80,  1593,   206,  2148,   966,  1011,  1013,   173,   190,  1012,
     482,  1015,  1014,   757,  1412,  1419,  2123,    83,   547,  1608,
    2061,   482,  1895,   112,  1796,  2078,  2118,  1888,   418,  1990,
     230,    90,  2116,   254,   102,  2107,  1790,    83,  1791,   108,
     257,    80,  2088,   473,  2131,  2038,  2037,  1428,   104,   170,
     283,   548,  1935,   540,  2005,   482,   658,   482,  1753,  1608,
    1572,  2108,    97,  2126,  1242,  1425,  1090,   820,    14,    15,
      16,    17,    18,  1217,   149,   257,   482,   120,  1803,  2135,
      83,   256,   482,   149,     3,   901,   310,   316,  1250,  1026,
      97,  1027,   482,  1028,  1723,     0,    80,     0,   256,   336,
       0,     0,     0,     0,     0,  1118,    80,     0,     0,   113,
     418,     0,   120,     0,     0,     0,   173,     0,     0,     0,
     173,     0,     0,   425,   190,   190,     0,    58,     0,     0,
      97,   659,   258,     0,   173,   149,   455,     0,     0,   254,
       0,     0,     0,   258,   173,   141,   120,   660,     0,     0,
     661,   662,    65,    66,    67,    68,    69,    70,    71,     0,
     120,   173,     0,   230,   230,     0,     0,   258,     0,     0,
       0,     0,   141,   141,     0,     0,     0,     0,     0,     0,
     310,     0,     0,     0,     0,     0,   256,    73,    83,   112,
       0,   120,     0,   524,     0,     0,     0,    90,     0,     0,
       0,     0,   254,     0,     0,   108,     0,  1730,     0,   469,
     469,   514,     0,     0,     0,     0,     0,   112,    78,    79,
       0,     0,     0,     0,  1260,    90,   173,     0,     0,  1264,
     173,     0,     0,   108,     0,   173,   316,     0,     0,     0,
    1272,     0,   316,   310,   310,     0,     0,     0,     0,   141,
     149,     0,     0,     0,     0,     0,     0,   112,     0,     0,
     173,     0,     0,     0,     0,    90,     0,     0,     0,     0,
     336,   623,   632,   108,     0,   113,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,   336,     0,   147,
     187,   336,     0,     0,    65,    66,    67,    68,    69,    70,
      71,  1278,     0,   113,   173,  1279,     0,  1280,   173,   256,
       0,   104,   182,     6,     7,     8,     9,    10,    11,    12,
      13,   173,     0,     0,     0,   425,     0,     0,     0,     0,
     483,     0,   276,   810,  2044,     0,   277,     0,    75,   281,
     104,   286,     0,   113,     0,   173,     0,   173,   173,     0,
       0,     0,     0,     0,     0,     0,     0,   257,     0,   425,
       0,   211,   760,     0,     0,     0,     0,     0,   147,   190,
       0,     0,   173,    65,    66,    67,    68,    69,    70,    71,
       0,   765,     0,     0,   141,   149,     0,     0,     0,   455,
       0,     0,   548,   789,     0,   632,     0,   776,     0,     0,
     779,     0,     0,     0,     0,     0,     0,     0,   173,     0,
     463,     0,     0,     0,   674,   256,     0,    75,   141,   173,
     809,     0,     0,     0,     0,     0,  1122,   211,     0,   147,
       0,     0,   141,   230,    65,    66,    67,    68,    69,    70,
      71,  1278,   230,     0,     0,  1279,     0,  1280,   120,   258,
       0,     0,     0,   173,     0,   173,     0,   524,     0,     0,
       0,     0,   310,     0,   425,   425,     0,   434,   310,     0,
     336,     0,     0,     0,   173,     0,   120,  1756,    75,     0,
     173,  1491,     0,     0,   258,   276,     0,     0,  1230,     0,
     173,   718,     0,     0,  2142,     0,     0,     0,     0,  1281,
       0,     0,     0,     0,  2149,     0,     0,  1281,     0,   310,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
     310,     0,   310,     0,   336,     0,    83,  1481,  1482,  1483,
       0,     0,   539,     0,  1484,  1485,  1281,     0,     0,   483,
       0,     0,   336,   455,   613,   632,     0,   636,     0,   246,
       0,     0,     0,   623,   141,     0,     0,   623,     0,   104,
     104,   613,     0,     0,     0,   613,   336,     0,   276,   277,
       0,   637,     0,   286,     0,     0,   632,   147,     0,   336,
     171,   172,    65,    66,    67,    68,    69,    70,    71,   548,
     149,     0,     0,     0,   539,     0,     0,     0,  1297,  1281,
     469,   469,     0,   425,   174,   177,   149,   149,     0,   425,
     682,     0,     0,  1555,   618,  1304,  1305,     0,   425,     0,
       0,   149,   149,   149,     0,   320,   321,   322,   323,     0,
       0,     0,     0,   258,     0,     0,  1402,     0,   147,   222,
       0,   171,   172,    65,    66,    67,    68,    69,    70,    71,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,   613,   147,     0,     0,   171,   172,    65,
      66,    67,    68,    69,    70,    71,     0,   455,    73,     0,
       0,   258,     0,     0,     0,     0,     0,     0,     0,   304,
       0,     0,   305,   760,   760,     0,     0,     0,  1577,    75,
       0,   425,     0,   211,     0,  1578,   682,   326,     0,    78,
      79,  1398,   765,   765,     0,     0,   324,     0,   455,     0,
     141,   789,  1037,   789,     0,  1040,     0,     0,     0,     0,
       0,     0,     0,   618,   325,     0,     0,     0,     0,     0,
     336,   336,     0,   115,   463,     0,   115,   147,   141,     0,
     171,   172,    65,    66,    67,    68,    69,    70,    71,   336,
       0,   310,     0,     0,     0,     0,     0,     0,   493,     0,
     257,     0,     0,     0,     0,     0,   276,     0,     0,     0,
     310,     0,     0,     0,     0,     0,   524,   483,   141,     0,
    1108,     0,     0,     0,  1112,  1281,     0,     0,   463,     0,
       0,   115,     0,     0,     0,   104,     0,     0,   434,   434,
       0,     0,   544,   545,   469,     0,   613,   463,     0,   425,
    1400,     0,     0,   174,     0,   115,   336,     0,     0,     0,
       0,     0,   149,   425,     0,     0,     0,     0,   174,     0,
     613,   260,     0,     0,   336,   115,  1225,     0,     0,     0,
       0,     0,     0,   613,     0,     0,     0,   623,     0,     0,
       0,     0,   258,     0,     0,   591,     0,     0,     0,     0,
       0,     0,   594,   596,     0,  1511,  1512,   603,     0,     0,
       0,     0,   115,     0,     0,     0,     0,     0,   115,  1526,
    1527,   115,     0,     0,   147,   260,   455,   360,   361,    65,
      66,    67,    68,    69,    70,    71,   332,   115,   364,     0,
     931,     0,     0,  1797,     0,   326,     0,    58,   326,     0,
     483,     0,     0,  1548,  1549,     0,     0,     0,   548,     0,
      19,   429,     0,     0,   434,     0,     0,     0,     0,     0,
       0,   104,     0,   115,   429,    76,     0,   260,     0,   147,
     362,   463,   227,   228,    65,    66,    67,    68,    69,    70,
      71,   147,     0,   760,   171,   172,    65,    66,    67,    68,
      69,    70,    71,    52,    53,    54,    55,    73,     0,     0,
     789,     0,   765,     0,   483,     0,     0,   789,     0,     0,
       0,     0,   463,     0,   115,     0,   115,  2040,    75,     0,
       0,   514,   483,     0,     0,     0,   222,  1281,    78,    79,
     260,     0,  1281,  1281,  1281,     0,   147,   595,   804,   805,
     258,    65,    66,    67,    68,    69,    70,    71,   990,   336,
       0,   565,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,   260,   434,     0,     0,     0,     0,
     260,     0,     0,  1344,     0,     0,  1348,     0,   115,     0,
       0,     0,     0,     0,     0,   149,     0,     0,   991,     0,
       0,     0,     0,     0,   149,     0,     0,     0,   115,     0,
     260,   115,   425,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,   104,     0,     0,
       0,   425,     0,     0,    58,     0,     0,     0,   613,  1718,
    1719,   636,     0,     0,     0,     0,     0,     0,   254,    83,
       0,     0,     0,   429,     0,   104,     0,     0,     0,     0,
       0,     0,     0,   310,    58,     0,   147,     0,     0,   149,
     326,    65,    66,    67,    68,    69,    70,    71,     0,   455,
       0,     0,     0,     0,     0,     0,     0,   429,     0,     0,
     463,  1986,     0,     0,    73,   104,   147,   434,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,   455,     0,
       0,     0,   149,   115,    74,    75,     0,   429,     0,     0,
     940,     0,     0,   260,    73,    78,    79,    58,   192,   147,
       0,  1281,     0,  1281,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,   787,    75,     0,     0,   620,     0,
     233,   258,     0,     0,     0,    78,   788,    73,     0,   147,
    1809,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,  1509,     0,     0,     0,   336,   336,  1046,    75,     0,
    1518,   620,     0,     0,     0,     0,     0,    73,    78,    79,
       0,     0,   429,   429,     0,  1830,     0,   260,   115,     0,
    1833,  1834,     0,     0,     0,     0,   312,   229,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,    79,
     149,   149,   149,   149,     0,   149,   149,     0,     0,     0,
     115,  1579,   316,     0,     0,   115,     0,     0,   260,   115,
       0,   115,     0,     0,     0,     0,     0,     0,     0,     0,
     425,   425,   115,     0,   115,   147,     0,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,   364,  1070,
     115,   429,     0,   260,     0,     0,  1082,     0,     0,     0,
     254,     0,   147,   500,   233,   562,   563,    65,    66,    67,
      68,    69,    70,    71,   115,     0,     0,   260,     0,     0,
     312,   565,   455,     0,   260,     0,     0,   115,     0,   937,
       0,   906,     0,     0,     0,     0,     0,     0,   115,     0,
    1395,     0,     0,     0,     0,     0,     0,   149,     0,   623,
       0,   429,     0,    76,   115,   115,     0,   429,    58,     0,
       0,     0,     0,     0,     0,     0,   429,     0,     0,   115,
     115,   115,  1145,   613,     0,     0,     0,     0,    14,    15,
      16,    17,    18,   587,   312,     0,     0,     0,     0,     0,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,   463,     0,    14,    15,    16,    17,    18,   147,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,   429,     0,     0,     0,     0,
       0,     0,     0,   434,     0,     0,     0,    58,  1311,    75,
    1579,  1731,     0,   931,     0,  1579,     0,   425,     0,   429,
       0,  1579,     0,  1579,     0,     0,     0,     0,     0,     0,
    1734,     0,     0,    58,     0,  1238,   429,     0,     0,   147,
       0,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,   316,   149,     0,     0,     0,     0,     0,   115,   115,
       0,     0,     0,     0,     0,   147,     0,    73,   227,   228,
      65,    66,    67,    68,    69,    70,    71,   115,     0,     0,
       0,     0,     0,     0,     0,     0,   425,  2040,    75,     0,
       0,   514,     0,    73,     0,     0,     0,   336,    78,    79,
     149,     0,  1119,   790,     0,   115,     0,     0,     0,     0,
       0,     0,   147,   229,    75,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    78,    79,     0,     0,     0,   260,
       0,   149,     0,     0,     0,     0,     0,   429,     0,     0,
     260,     0,  2094,   829,   115,    99,     0,     0,   153,     0,
     115,   429,   233,     0,     0,     0,   336,   336,     0,     0,
       0,     0,   115,    76,  1227,   429,   463,   115,     0,     0,
       0,     0,   312,     0,  1731,  1731,     0,     0,   312,     0,
       0,     0,     0,     0,  1648,     0,     0,     0,     0,  1579,
     147,     0,  1579,  1734,  1734,    65,    66,    67,    68,    69,
      70,    71,  1278,    99,     0,     0,  1279,     0,  1280,   316,
       0,  1399,  1401,  1403,   429,     0,     0,     0,     0,   312,
       0,     0,   425,    58,     0,     0,     0,   205,     0,     0,
     893,     0,   312,     0,     0,     0,     0,     0,     0,    75,
       0,  1422,  1695,     0,     0,     0,     0,   268,     0,   310,
       0,     0,     0,     0,     0,   147,  1145,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,    73,   302,     0,     0,     0,    58,     0,
      99,     0,     0,     0,   115,   115,     0,  1731,     0,     0,
    1467,     0,     0,   309,    75,     0,  1579,     0,     0,   338,
       0,     0,     0,     0,    78,    79,  1734,     0,     0,     0,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   435,     0,     0,     0,     0,     0,
       0,     0,   149,     0,     0,   302,   461,   115,    73,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1762,   434,     0,     0,     0,     0,     0,     0,  1577,    75,
     336,     0,     0,  1773,   508,     0,     0,     0,  1731,    78,
      79,     0,     0,   115,     0,     0,     0,     0,   149,     0,
     528,     0,   115,  2003,     0,   533,   535,  1734,   205,     0,
     429,     0,     0,     0,     0,     0,     0,   109,     0,     0,
    1801,     0,     0,     0,     0,     0,   149,   149,     0,  2041,
     316,   556,     0,     0,     0,   558,     0,     0,     0,   429,
     559,  1047,     0,   790,     0,     0,     0,     0,  1734,  1588,
       0,   576,  1590,     0,     0,     0,   260,   115,     0,     0,
       0,   269,     0,     0,   588,     0,   149,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,   115,     0,     0,
       0,   312,     0,     0,     0,     0,     0,   429,     0,     0,
     611,  1227,     0,   635,  2041,  2041,     0,     0,     0,     0,
     312,     0,     0,  1466,   109,     0,     0,   642,     0,     0,
       0,   642,     0,  1734,  1734,   115,   429,     0,     0,     0,
     115,     0,   114,   342,     0,     0,     0,     0,     0,  1864,
      58,     0,     0,  1874,   147,     0,  2041,   560,    64,    65,
      66,    67,    68,    69,    70,    71,     0,  1887,     0,     0,
       0,     0,     0,     0,     0,  1734,     0,  1896,     0,     0,
     462,     0,   147,     0,   115,   115,     0,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,   115,   115,
     114,     0,     0,   115,   115,     0,  1020,     0,     0,     0,
      73,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   302,     0,     0,     0,   611,
      74,    75,   115,   115,     0,     0,     0,     0,     0,     0,
    1556,    78,    79,     0,   271,     0,   613,     0,   115,   115,
     115,   115,   115,   115,   115,   557,     0,     0,     0,  1945,
     260,   703,     0,  1950,    76,   392,     0,     0,  1955,     0,
       0,     0,     0,     0,  1754,   109,     0,     0,   429,   429,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
     147,     0,     0,  1985,     0,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,   346,     0,   260,     0,
     461,     0,     0,     0,   612,     0,     0,   269,     0,     0,
    1047,   613,     0,     0,     0,     0,  1312,   790,     0,     0,
     429,   612,     0,     0,     0,   612,     0,  2014,  1311,    75,
       0,  2017,   877,   464,     0,     0,     0,   535,     0,     0,
       0,   888,     0,   576,  2031,   115,     0,     0,     0,     0,
       0,     0,     0,     0,   338,   147,    99,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,  2053,     0,
    2055,  2056,   642,   912,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,   923,     0,     0,
       0,     0,     0,     0,     0,  2066,   611,     0,     0,     0,
       0,   932,     0,   787,    75,     0,     0,   620,     0,   642,
       0,     0,     0,     0,    78,   788,     0,     0,   115,   115,
       0,     0,     0,   612,     0,     0,     0,   622,   114,   147,
       0,  2089,   171,   172,    65,    66,    67,    68,    69,    70,
      71,     0,  2095,     0,     0,   429,   147,     0,     0,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,   115,     0,     0,     0,     0,     0,   614,     0,     0,
     271,     0,     0,   312,    73,     0,  2121,   472,  2095,   260,
     115,     0,  1909,     0,   614,     0,     0,     0,   614,     0,
       0,     0,     0,     0,  1577,    75,     0,  2132,     0,     0,
       0,  1578,     0,  2121,   462,    78,    79,   461,    14,    15,
      16,    17,    18,  2140,   429,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1029,   115,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,   342,     0,     0,   115,
       0,     0,     0,     0,     0,   269,     0,   109,   912,     0,
       0,     0,     0,  1053,     0,     0,     0,     0,   462,   115,
     109,     0,     0,     0,     0,     0,     0,    58,     0,     0,
     461,   461,     0,     0,   115,  1523,   612,   462,     0,   115,
     115,     0,     0,     0,   115,   115,   614,     0,     0,   461,
       0,     0,     0,     0,    14,    15,    16,    17,    18,   147,
     612,     0,   227,   228,    65,    66,    67,    68,    69,    70,
      71,     0,     0,   612,   147,     0,     0,   877,     0,    65,
      66,    67,    68,    69,    70,    71,  1278,    73,     0,     0,
    1279,     0,  1280,     0,     0,     0,     0,   260,     0,     0,
       0,  1583,     0,     0,     0,     0,     0,   309,    75,  1194,
     429,     0,     0,    58,     0,     0,   461,     0,    78,    79,
       0,     0,   153,    75,     0,     0,  1697,   464,     0,     0,
       0,     0,     0,     0,   642,     0,     0,  1229,     0,   877,
       0,     0,     0,     0,  1235,   147,     0,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,     0,   346,
       0,     0,     0,     0,     0,     0,     0,     0,   271,     0,
     114,   462,     0,    73,     0,     0,     0,     0,     0,     0,
       0,   464,     0,   114,     0,     0,   338,     0,     0,     0,
       0,     0,     0,  1577,    75,     0,     0,     0,     0,   614,
     464,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,   147,   462,     0,   227,   228,    65,    66,    67,    68,
      69,    70,    71,   614,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   342,   342,   614,   147,     0,    73,
     227,   228,    65,    66,    67,    68,    69,    70,    71,   877,
       0,     0,     0,   342,     0,     0,     0,     0,   115,  2040,
      75,     0,     0,   514,     0,    73,   877,   877,     0,     0,
      78,    79,     0,     0,     0,     0,   115,     0,     0,     0,
       0,   342,     0,     0,     0,   229,    75,     0,     0,     0,
    1583,     0,     0,     0,     0,  1583,    78,    79,     0,     0,
       0,  1746,     0,  1583,   115,   115,     0,     0,   260,     0,
       0,     0,     0,   109,     0,     0,     0,     0,   147,   461,
     342,   227,   228,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,   464,     0,     0,     0,   612,     0,
       0,   269,     0,   342,   115,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,   153,     0,     0,     0,     0,
       0,     0,     0,     0,  1378,     0,   309,    75,     0,     0,
       0,     0,  1194,   119,     0,   464,   119,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     462,   115,     0,     0,     0,     0,     0,   346,   346,   147,
       0,  1194,   199,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,   346,   147,     0,  1426,
     171,   172,    65,    66,    67,    68,    69,    70,    71,     0,
       0,   119,   744,   745,   746,   747,   748,   749,   750,   751,
     752,   753,   754,     0,   346,     0,   214,     0,    75,   611,
       0,   809,     0,   342,     0,   119,     0,     0,   533,     0,
       0,     0,     0,     0,     0,   476,     0,     0,     0,  1861,
     342,   342,  1583,   755,     0,   119,   114,   877,   338,     0,
       0,   147,     0,   346,   171,   172,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,   614,     0,     0,   271,     0,   346,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,   119,     0,
       0,   119,     0,   342,     0,     0,   877,   877,     0,   312,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     877,   877,     0,     0,     0,   461,   461,     0,     0,     0,
       0,     0,     0,   464,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   877,   877,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1583,     0,     0,     0,
    1378,  1378,  1378,   153,   535,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   346,     0,     0,     0,
    1607,  1607,     0,   269,   119,     0,   119,     0,     0,     0,
       0,   119,     0,   346,   346,     0,     0,     0,     0,     0,
     213,     0,     0,     0,     0,     0,   224,   225,     0,     0,
       0,     0,     0,   612,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   338,     0,     0,     0,     0,     0,     0,     0,
     291,   342,   462,     0,     0,     0,   346,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   153,     0,   312,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     342,   342,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,   342,   342,     0,     0,     0,   342,
     342,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   587,   312,     0,     0,   114,     0,
     877,   877,     0,     0,     0,     0,     0,     0,   342,   342,
       0,     0,     0,     0,     0,     0,   271,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,  1748,     0,     0,   312,     0,     0,     0,
       0,     0,     0,   877,     0,     0,   614,     0,     0,     0,
       0,     0,     0,   119,   109,   109,     0,     0,     0,     0,
       0,     0,  1765,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   346,   464,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   584,
       0,     0,     0,     0,     0,     0,  1607,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   462,   338,     0,     0,
     153,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   877,     0,   346,   346,     0,     0,     0,     0,     0,
       0,     0,   119,   119,     0,     0,     0,   346,   346,     0,
       0,     0,   346,   346,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,   877,   124,     0,     0,
       0,   877,   877,     0,     0,     0,   461,   461,     0,     0,
       0,   346,   346,     0,     0,   119,     0,     0,     0,   119,
       0,   119,     0,     0,     0,  1853,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   342,   342,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,   114,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1607,     0,     0,     0,   124,   785,     0,   786,
       0,     0,     0,     0,     0,     0,     0,   342,   802,   803,
       0,     0,     0,     0,     0,     0,   124,     0,   119,     0,
       0,     0,     0,     0,     0,     0,   269,     0,     0,   464,
       0,   119,     0,     0,   119,   119,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,   119,
     119,   119,     0,   124,     0,     0,     0,     0,     0,   124,
     109,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,   342,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   342,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1976,     0,   124,     0,     0,   887,     0,     0,
     342,     0,     0,     0,     0,   342,   342,   346,   346,   119,
     342,   342,     0,     0,     0,     0,     0,     0,     0,     0,
     461,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1607,     0,
       0,     0,     0,     0,     0,   124,     0,   124,     0,     0,
     346,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1607,  1976,     0,   271,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,  1607,     0,     0,   124,
       0,     0,     0,     0,   346,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,   346,     0,
       0,     0,     0,     0,     0,  2082,     0,     0,     0,     0,
     119,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   877,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   346,     0,     0,     0,     0,   346,   346,
       0,     0,     0,   346,   346,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   612,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,  1068,   342,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,   109,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   612,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,   146,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1141,  1142,     0,     0,     0,     0,     0,     0,     0,
     109,     0,  1200,  1201,  1202,     0,     0,  1204,    14,    15,
      16,    17,    18,   124,   124,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -462,  -462,     0,  -462,    46,   342,    47,   614,
    -462,   202,     0,     0,     0,     0,   124,     0,     0,     0,
     124,     0,   124,   119,     0,     0,     0,    58,     0,     0,
       0,     0,   119,     0,     0,   124,     0,   346,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,  1274,
       0,     0,     0,     0,     0,   114,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,   119,
     297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,   614,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,  1295,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
      76,     0,   124,     0,     0,   124,   124,     0,   124,     0,
       0,     0,     0,   114,     0,     0,     0,   124,     0,     0,
     124,   124,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1319,     0,     0,     0,     0,     0,     0,     0,
     119,  1323,  1324,  1325,  1326,     0,     0,     0,     0,  1331,
    1332,     0,     0,     0,     0,     0,     0,     0,     0,  1340,
     346,     0,     0,   297,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   536,     0,     0,
    1356,     0,     0,  1359,     0,  1360,     0,   297,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   297,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   568,   572,     0,     0,
       0,     0,     0,   579,   580,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   590,
    1418,     0,     0,     0,     0,     0,     0,     0,   119,   119,
     119,   119,   119,   119,   119,     0,     0,     0,     0,   609,
       0,     0,     0,     0,     0,     0,  1432,     0,     0,     0,
       0,     0,     0,  1436,     0,  1438,  1440,     0,   119,   119,
       0,     0,     0,     0,     0,     0,     0,  1449,     0,  1450,
       0,  1451,     0,  1453,     0,     0,     0,     0,  1461,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   702,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   124,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,     0,   743,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
    1506,     0,     0,     0,     0,     0,     0,  1513,  1514,     0,
       0,   168,     0,   781,     0,     0,     0,   784,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1537,     0,     0,     0,     0,   806,     0,  1542,   403,
     807,   808,  1543,     0,   811,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   168,     0,   825,
     826,   827,   828,     0,     0,  1560,     0,     0,     0,     0,
     168,     0,   168,     0,     0,     0,     0,     0,   850,   224,
       0,     0,     0,     0,     0,     0,   853,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   297,     0,     0,     0,  1643,
       0,     0,     0,     0,     0,     0,     0,     0,   366,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   892,     0,     0,     0,
       0,     0,     0,   568,     0,  1665,     0,     0,     0,   898,
       0,     0,     0,  1670,   119,  1672,   168,     0,     0,     0,
     168,     0,     0,   168,   168,     0,     0,   168,   119,     0,
     168,   168,     0,   915,   920,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,   119,
       0,     0,     0,   124,  1703,  1704,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,     0,  1709,
    1710,     0,  1711,   670,     0,     0,     0,   403,   676,     0,
       0,  1715,     0,     0,     0,     0,   168,   685,   686,   168,
     124,  1720,  1721,     0,   962,     0,     0,     0,     0,     0,
       0,     0,   403,   403,     0,     0,     0,     0,   124,     0,
     168,   168,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,   168,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   403,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1025,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,  1042,     0,
       0,     0,  1043,     0,     0,     0,     0,     0,     0,     0,
       0,   915,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1810,  1811,  1083,     0,     0,     0,     0,     0,     0,
       0,   168,  1092,     0,  1819,     0,     0,     0,  1095,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1843,  1844,     0,     0,     1,     0,     0,     0,  1131,
       0,     0,     0,     1,     0,     0,   366,     0,     0,   124,
     124,   124,   124,   124,   124,   124,     0,     0,   168,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,   119,     0,     0,   124,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1913,     0,     0,     0,  1254,     0,     0,     0,
       0,     0,     0,   366,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1923,   119,     0,  1924,  1925,     0,     0,
       0,     0,     0,  1927,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,   168,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   168,   403,   403,
     403,   403,   403,   403,   403,   403,   403,   403,   403,   403,
     403,   403,   403,   403,   403,   403,   403,     0,     0,  1302,
       0,     0,     0,  1303,     0,     0,     0,     0,     0,     0,
     915,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1316,     0,     0,     0,     0,     0,     0,  1317,     0,     0,
       0,     0,     0,     0,     0,     0,  1321,     0,  1322,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   403,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1350,     0,     0,     0,  1351,     0,     0,  2039,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,     0,   353,
       0,   146,     0,     0,     0,     0,     1,     0,     0,     0,
     168,   168,     0,     0,     0,     0,   168,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   451,   353,   168,     0,   124,
     168,   168,     0,   168,     0,   168,   168,     0,     0,     0,
       0,  2079,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   517,     0,     0,
     124,     0,     0,     0,   517,     0,  2097,     0,     0,     0,
       0,     0,     0,     0,   168,     0,     0,     0,   168,     0,
       0,  2106,     0,     0,     0,     0,     0,  1448,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1473,     0,     0,     0,   403,     0,     0,     0,
       0,   403,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   403,     0,     0,     0,   517,     0,     0,     0,
       0,     0,     0,     0,   168,   168,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,   168,   166,
     353,   624,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   403,     0,     0,     0,     0,     0,
       0,   645,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1546,     0,     0,     0,
    1547,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   289,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   295,     0,   296,  1589,     0,     0,
       0,     0,   517,     0,     0,     0,     0,   210,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   517,   777,
       0,   517,   780,   264,     0,     0,     0,     0,     0,   353,
       0,     0,     0,   624,     0,     0,     0,     0,     0,   168,
       0,     0,     0,     0,     0,     0,     0,  1653,     0,     0,
    1656,     0,     0,     0,     0,     0,   403,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,  1667,     0,
       0,     0,     0,   210,   517,     0,     0,   317,   517,     0,
     168,     0,     0,     0,     0,     0,     0,   168,     0,   358,
     168,     0,     0,     0,     0,   124,     0,   518,   519,     0,
       0,   523,     0,     0,   526,   527,     0,     0,     0,     0,
     353,     0,     0,   210,     0,     0,     0,     0,     0,     0,
       0,  1702,     0,     0,     0,     0,   471,     0,     0,   475,
    1707,     0,     0,   403,  1708,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1712,  1713,
       0,     0,   403,     0,     0,     0,     0,     0,     0,     0,
       0,   517,     0,     0,   353,     0,     0,     0,     0,   403,
     403,   403,     0,     0,  1656,     0,   403,   403,   210,     0,
       0,     0,   910,   353,     0,     0,     0,     0,     0,     0,
       0,     0,   264,   624,   606,   607,     0,   624,     0,     0,
     403,     0,     0,     0,   928,     0,   353,     0,     0,     0,
     639,     0,     0,     0,     0,     0,     0,     0,   168,     0,
       0,     0,     0,     0,     0,     0,   168,   168,     0,     0,
       0,     0,   475,     0,     0,     0,     0,   403,   403,     0,
     210,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     617,     0,   634,     0,     0,     0,     0,     0,     0,     0,
       0,  1804,  1805,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,   203,     0,     0,     0,
       0,     0,     0,     0,   168,     0,     0,   168,     0,   168,
     168,     0,     0,     0,     0,   775,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   700,     0,   353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   517,   517,     0,     0,     0,     0,     0,
     168,     0,     0,   517,  1038,     0,   517,  1041,     0,   210,
       0,     0,   203,     0,     0,     0,     0,     0,   353,     0,
       0,   624,     0,   624,   624,     0,     0,     0,   203,     0,
     624,     0,   846,     0,     0,     0,     0,     0,   696,   617,
     353,   353,     0,     0,     0,   801,     0,     0,     0,     0,
       0,     0,   203,     0,     0,     0,     0,     0,     0,   353,
       0,     0,     0,   517,     0,   458,  1898,   517,     0,     0,
       0,   517,  1109,     0,   168,   517,  1113,     0,     0,     0,
       0,     0,     0,  1116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1656,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1926,     0,   210,   210,     0,   203,     0,     0,
     471,     0,     0,     0,     0,     0,   353,   517,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1944,     0,     0,     0,     0,     0,     0,   926,   927,
       0,     0,     0,     0,     0,     0,     0,   624,     0,     0,
       0,   934,   168,     0,     0,     0,     0,     0,  1972,     0,
       0,  1973,     0,     0,   358,     0,     0,   856,   858,   203,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   168,     0,   471,     0,   914,   353,     0,     0,   203,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   403,   617,     0,     0,     0,
       0,     0,     0,     0,   168,     0,     0,     0,     0,     0,
     168,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     210,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   700,     0,     0,   700,   700,     0,   700,
       0,     0,     0,   517,     0,     0,     0,     0,   700,   427,
       0,   700,   700,   700,     0,  2059,     0,     0,     0,     0,
     624,   624,   456,     0,  1031,  1032,     0,   624,   203,     0,
    1036,     0,     0,     0,     0,   484,     0,   484,     0,   168,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1057,     0,     0,  1060,  1061,   696,  1064,   203,  1066,
    1067,     0,   696,     0,     0,     0,     0,   471,     0,   353,
       0,   696,     0,     0,   517,  1345,     0,   517,  1349,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     696,   210,     0,     0,     0,     0,     0,     0,  1107,     0,
       0,     0,  1111,     0,     0,     0,     0,     0,   471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,   168,  1017,   585,     0,     0,
     471,   471,   366,   203,   203,     0,   168,     0,     0,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1219,  1220,
       0,     0,     0,   403,     0,     0,     0,     0,     0,     0,
       0,     0,  1236,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   203,     0,     0,     0,     0,     0,   353,
       0,     0,     0,     0,     0,   624,  1457,     0,     0,     0,
       0,     0,   458,   403,     0,     0,   471,     0,     0,     0,
       0,     0,     0,   210,     0,     0,     0,     0,   353,     0,
       0,     0,     0,     0,     0,   203,   801,     0,     0,     0,
       0,     0,   168,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   203,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   517,  1510,     0,     0,     0,     0,     0,     0,
       0,   517,  1519,     0,   624,     0,   358,     0,     0,     0,
       0,     0,     0,     0,     0,   353,   353,     0,     0,     0,
       0,   484,     0,     0,     0,     0,     0,   484,     0,     0,
       0,     0,   822,  1236,   403,     0,   403,   168,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   458,     0,     0,     0,
       0,     0,     0,     0,  1336,   403,     0,     0,     0,     0,
       0,  1343,     0,     0,  1347,     0,     0,     0,     0,     0,
     203,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   458,   403,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     891,     0,     0,     0,     0,     0,     0,     0,     0,   458,
     458,     0,     0,     0,     0,     0,     0,     0,     0,   471,
       0,     0,   353,     0,     0,     0,     0,   168,   458,   456,
       0,     0,     0,     0,     0,     0,     0,   403,     0,     0,
       0,     0,   922,     0,     0,     0,   252,     0,     0,   624,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,   700,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -462,  -462,  1455,  -462,    46,   458,    47,   956,  -462,     0,
    1464,  1465,   203,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,   264,     0,
       0,   822,   976,     0,     0,   978,     0,   980,     0,     0,
       0,     0,     0,   989,     0,   994,   989,     0,     0,   210,
       0,   517,     0,     0,     0,     0,     0,     0,     0,   617,
      63,    64,     0,     0,     0,     0,     0,   517,     0,  1508,
       0,     0,     0,  1022,     0,   203,     0,     0,  1517,     0,
       0,  1521,     0,  1524,  1525,    73,  1024,     0,   358,     0,
       0,     0,   700,     0,     0,     0,     0,  1033,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   315,
       0,   456,     0,     0,  1022,     0,    78,    79,     0,     0,
       0,     0,     0,     0,  1551,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   353,     0,     0,
       0,  1086,     0,     0,   484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   471,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   353,   353,  1650,     0,
     700,   700,   700,     0,     0,   700,   700,     0,   458,     0,
       0,     0,   475,     0,   517,   517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   427,
     517,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1226,  1228,     0,     0,     0,     0,     0,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     264,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   989,     0,
       0,     0,   358,     0,     0,     0,  1521,     0,     0,     0,
    1598,     0,  1022,  1601,  1615,     0,     0,     0,     0,  1622,
    1267,     0,     0,  1626,     0,  1628,     0,   989,     0,     0,
       0,     0,     0,     0,     0,  1717,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   203,     0,
       0,     0,     0,     0,     0,     0,     0,   517,   203,     0,
       0,     0,     0,     0,     0,   517,     0,     0,     0,     0,
       0,     0,     0,   484,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,   203,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -462,  -462,     0,  -462,
      46,     0,    47,     0,  -462,     0,     0,   210,     0,     0,
     353,     0,     0,     0,   517,  2004,     0,     0,   517,     0,
     484,    58,  1335,  1802,  1338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   458,   458,     0,     0,     0,     0,
       0,   264,     0,     0,     0,     0,     0,     0,     0,  1722,
       0,     0,     0,     0,     0,     0,    63,    64,     0,   517,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,   358,  1758,     0,
       0,     0,     0,     0,     0,  1410,  1410,     0,     0,     0,
    1764,     0,     0,     0,    76,   315,     0,     0,  1857,  1858,
       0,     0,    78,    79,     0,  1779,  1781,     0,     0,     0,
    1862,   700,     0,     0,   517,   517,     0,     0,  1936,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1601,
       0,     0,     0,     0,     0,     0,   471,   471,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1452,
       0,   203,     0,     0,     0,  1462,   517,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   376,
       0,   377,     0,   378,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   264,
     379,   484,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   989,     0,     0,   822,
       0,     0,     0,     0,     0,     0,  1934,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,  1872,    73,
       0,     0,     0,     0,     0,     0,     0,  1875,     0,  1877,
       0,  1545,  1882,  1886,     0,  1615,     0,     0,     0,   391,
    1892,     0,    76,   392,     0,     0,   203,  1553,  1554,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,  2002,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   989,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   700,     0,   484,     0,     0,   822,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,     0,     0,     0,   203,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   976,     0,
       0,  1961,     0,     0,     0,     0,  1966,  1968,  1668,  1669,
       0,     0,     0,     0,     0,     0,     0,     0,   484,     0,
       0,     0,     0,     0,     0,     0,     0,   700,     0,     0,
     475,     0,     0,     0,     0,     0,   484,     0,   822,     0,
       0,     0,     0,     0,     0,   458,   458,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2018,
       0,  2021,     0,     0,  2023,  2025,  1369,     0,  1370,  2028,
    2030,     0,     0,  1371,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,   427,    46,     0,    47,     0,  1747,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1372,     0,     0,     0,
    2073,  2075,  2077,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2090,     0,     0,     0,     0,    61,     0,     0,     0,
      63,    64,     0,     0,     0,  2101,  2103,  2105,  1793,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2120,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1394,     0,     0,     0,
       0,     0,     0,     0,  1373,     0,     0,     0,    76,   952,
       0,     0,     0,     0,     0,  1821,    78,    79,  1823,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
     376,     0,   377,     0,   378,     0,     0,     0,     0,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1147,
       0,   379,    -2,  1848,  1149,  -243,  -243,  1150,  1151,  1152,
    1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -337,
       0,  1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,
     381,     0,   478,     0,   383,  1168,  1169,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,  1170,   386,   387,
     388,  2120,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,  1394,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -243,
     391,     0,     0,    76,   392,     0,     0,     0,   293,     0,
     393,    78,    79,   394,   395,   396,   397,     0,     0,   375,
       0,     0,   376,     0,   377,  -184,   378,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1147,     0,   379,    -2,     0,  1149,  -244,  -244,  1150,
    1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,  1159,  1160,
    1161,  -337,     0,  1162,  1163,  1164,  1165,  1166,     0,  1167,
       0,   380,   381,     0,   478,     0,   383,  1168,  1169,    65,
      66,    67,    68,    69,    70,    71,   384,   385,   372,  1170,
     386,   387,   388,  1800,   389,   390,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,   989,     0,     0,     0,
    1394,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -244,   391,     0,     0,    76,   392,     0,     0,     0,
     293,     0,   393,    78,    79,   394,   395,   396,   397,     0,
       0,   375,     0,     0,   376,     0,   377,  -184,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1147,     0,   379,    -2,     0,  1149,     0,
       0,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  -337,     0,  1162,  1163,  1164,  1165,  1166,
       0,  1167,     0,   380,   381,     0,   478,     0,   383,  1168,
    1169,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1170,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   392,     0,
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
    1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -337,     0,
    1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,   381,
      61,   478,     0,   383,  1168,  1169,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1170,   386,   387,   388,
       0,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -3,   391,
       0,     0,    76,   423,     0,     0,     0,   293,     0,   393,
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
    1159,  1160,  1161,  -337,     0,  1162,  1163,  1164,  1165,  1166,
       0,  1167,     0,   380,   381,    61,   478,     0,   383,  1168,
    1169,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1170,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   423,     0,
       0,     0,   293,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
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
       0,     0,     0,  1609,  1610,  1611,     0,     0,     0,   391,
    1612,  1613,    76,   423,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,     0,  1614,     4,   182,     6,     7,     8,
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
       0,     0,     0,     0,     0,     0,     0,     0,  1609,  1610,
    1611,     0,     0,     0,   391,  1612,     0,    76,   423,     0,
       0,     0,     0,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  1614,
     182,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   247,   248,     0,   249,    46,     0,    47,
       0,   250,     0,     0,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     4,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   375,     0,    46,
     376,    47,   377,     0,   378,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   379,     0,     0,     0,     0,     0,     0,     0,     0,
    -442,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   380,
     381,    61,   382,  -442,   383,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   384,   385,   372,     0,   386,   387,
     388,     0,   389,   390,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,  1600,    76,   423,     0,     0,     0,     0,     0,
     393,    78,    79,   394,   395,   396,   397,     4,   182,     6,
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
       0,     0,     0,     0,     0,     0,   391,     0,     0,    76,
     423,     0,     0,     0,     0,     0,   393,    78,    79,   394,
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
       0,   393,   454,    79,   394,   395,   396,   397,   182,     6,
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
    1223,     0,     0,     0,     0,     0,   393,  1224,    79,   394,
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
       0,   391,     0,     0,    76,   392,     0,     0,     0,     0,
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
     453,     0,     0,     0,     0,     0,   393,    78,    79,   394,
     395,   396,   397,  1943,     0,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,    -2,    -2,  1971,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,  1234,    -2,     0,
       0,     0,     0,    -2,    -2,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,   375,     0,     0,   376,     0,   377,     0,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,     0,     0,    58,   379,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,     0,     0,   380,   381,     0,   382,     0,   383,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,  1459,     0,
       0,     0,     0,     0,    73,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   392,     0,
       0,     0,     0,     0,   393,   454,    79,   394,   395,   396,
     397,     0,   375,     0,     0,   376,     0,   377,     0,   378,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,   379,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,     0,   389,   390,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,  1460,    79,   394,   395,
     396,   397,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,    59,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    61,    62,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,    77,     0,     0,     0,     0,
       0,     0,    78,    79,   252,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -462,  -462,
       0,  -462,    46,     0,    47,     0,  -462,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,   253,     0,     0,
       0,  -789,     0,     0,    78,    79,     4,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -390,  -390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -390,     0,     0,     0,    76,    77,
       0,     0,     0,     0,     0,     0,    78,    79,     4,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -391,  -391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -391,     0,     0,     0,
      76,    77,     0,     0,     0,     0,     0,     0,    78,    79,
     252,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -462,  -462,     0,  -462,    46,     0,
      47,     0,  -462,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   147,     0,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
      75,     0,    76,   253,     0,  1369,     0,  1370,     0,     0,
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
      45,  -462,  -462,     0,  -462,    46,     0,    47,     0,  -462,
       0,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    58,    20,     0,    21,
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
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -462,  -462,     0,  -462,    46,     0,    47,     0,
    -462,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   253,     0,     0,     0,  -793,     0,     0,    78,    79,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -462,  -462,     0,  -462,    46,     0,    47,
       0,  -462,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   253,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   333,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,  1077,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -653,    76,   335,     0,     0,     0,    63,    64,     0,
      78,    79,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    76,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
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
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    76,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   333,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
       0,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,    73,     0,  1839,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   335,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
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
       0,     0,    73,     0,  1841,     0,     0,     0,     0,     0,
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
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   315,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    76,   335,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -462,  -462,     0,  -462,    46,     0,    47,     0,  -462,     0,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,     0,     0,  1394,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,   376,
       0,   377,     0,   378,     0,     0,     0,     0,    76,   253,
     678,     0,     0,   679,   680,     0,    78,    79,  1147,     0,
     379,     0,     0,  1149,  1865,  1866,  1150,  1151,  1152,  1153,
    1154,  1155,  1156,  1157,  1158,  1159,  1160,  1161,  -337,     0,
    1162,  1163,  1164,  1165,  1166,     0,  1167,     0,   380,   381,
       0,   478,     0,   383,  1168,  1169,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,  1170,   386,   387,   388,
    1394,   389,   390,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   391,
       0,   375,    76,   392,   376,     0,   377,   293,   378,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,     0,  1147,  -184,   379,     0,     0,  1149,     0,
       0,  1150,  1151,  1152,  1153,  1154,  1155,  1156,  1157,  1158,
    1159,  1160,  1161,  -337,     0,  1162,  1163,  1164,  1165,  1166,
       0,  1167,     0,   380,   381,     0,   478,     0,   383,  1168,
    1169,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,  1170,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   392,     0,
       0,     0,   293,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
      14,    15,    16,    17,    18,    19,   687,    20,   688,    21,
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
       0,     0,    76,   690,     0,     0,     0,   293,     0,   393,
      78,    79,   691,   692,   396,   397,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
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
     396,   397,    14,    15,    16,    17,    18,    19,     0,    20,
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
       0,   391,     0,     0,    76,   690,     0,     0,     0,   293,
       0,   393,    78,    79,   394,   395,   396,   397,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
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
     394,   395,   396,   397,    14,    15,    16,    17,    18,    19,
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
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
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
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   678,     0,     0,   679,
     680,     0,   573,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,   -16,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   252,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    63,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -462,  -462,     0,  -462,    46,
       0,    47,     0,  -462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
      58,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,    77,     0,     0,     0,  -791,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   147,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
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
       0,     0,     0,     0,     0,     0,     0,   876,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -666,    76,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
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
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      76,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -462,  -462,     0,  -462,
      46,     0,    47,     0,  -462,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   147,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,   315,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,    76,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1477,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   958,    76,   952,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   952,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   300,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
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
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   333,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   333,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   300,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   449,     0,     0,
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
       0,    76,   952,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -462,  -462,     0,  -462,    46,     0,
      47,     0,  -462,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,   333,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,    76,     0,    46,     0,    47,    63,    64,     0,
     333,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   952,     0,     0,     0,
      63,    64,     0,    78,    79,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,    14,    15,    16,    17,    18,    78,    79,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -462,  -462,     0,  -462,    46,
       0,    47,     0,  -462,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -461,  -461,     0,  -461,
      46,     0,    47,     0,  -461,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,    78,    79,    20,     0,    21,    22,    23,    24,    25,
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
       0,     0,     0,     0,     0,   393,   454,    79,   394,   395,
     396,   397,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   375,     0,    46,   376,    47,   377,     0,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   379,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   380,   381,     0,   382,     0,   383,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   391,     0,     0,    76,   392,     0,
       0,     0,     0,     0,   393,    78,    79,   394,   395,   396,
     397,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,   375,     0,     0,   376,     0,   377,
      58,   378,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,     0,   389,
     390,   375,     0,     0,   376,     0,   377,    73,   378,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,  1609,  1610,  1611,     0,   379,     0,   391,  1780,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   380,   381,     0,   382,     0,   383,  1880,
      64,    65,    66,    67,    68,    69,    70,    71,   384,   385,
     372,     0,   386,   387,   388,     0,   389,   390,   375,     0,
       0,   376,     0,   377,    73,   378,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1609,  1610,
    1611,     0,   379,     0,   391,  1881,     0,    76,   392,     0,
       0,     0,     0,     0,   393,    78,    79,   394,   395,   396,
     397,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   478,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,    75,     0,   479,   480,     0,     0,     0,   481,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
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
     985,  1591,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,     0,     0,    76,   392,     0,     0,
       0,   481,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,   821,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,   293,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,   985,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,  1016,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  1337,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,     0,     0,
      76,   392,     0,     0,     0,  1404,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,     0,     0,    76,   392,     0,     0,     0,  1468,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,  1871,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
    1876,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  1885,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  1965,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  1967,     0,    76,   392,     0,     0,     0,     0,
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
    2022,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2024,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   391,  2027,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   391,  2029,     0,    76,   392,     0,     0,     0,     0,
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
    2074,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   391,  2076,     0,    76,   392,     0,     0,
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
       0,   391,  2102,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,  2104,     0,    76,   392,
       0,     0,     0,     0,     0,   393,    78,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
      78,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,   375,   389,   390,   376,     0,   377,
       0,   378,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,   669,     0,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
       0,     0,     0,     0,     0,     0,   380,   381,     0,   382,
       0,   383,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   384,   385,   372,     0,   386,   387,   388,   375,   389,
     390,   376,     0,   377,     0,   378,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,     0,     0,     0,   675,     0,     0,
      76,   392,     0,     0,     0,     0,     0,   393,    78,    79,
     394,   395,   396,   397,     0,     0,     0,     0,     0,     0,
     380,   381,     0,   382,     0,   383,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   384,   385,   372,     0,   386,
     387,   388,   375,   389,   390,   376,     0,   377,     0,   378,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   379,     0,     0,     0,
       0,   684,     0,     0,    76,   392,     0,     0,     0,     0,
       0,   393,    78,    79,   394,   395,   396,   397,     0,     0,
       0,     0,     0,     0,   380,   381,     0,   382,     0,   383,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   384,
     385,   372,     0,   386,   387,   388,   375,   389,   390,   376,
       0,   377,     0,   378,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     379,     0,     0,     0,     0,   391,     0,     0,    76,   392,
       0,     0,     0,     0,     0,   393,   890,    79,   394,   395,
     396,   397,     0,     0,     0,     0,     0,     0,   380,   381,
       0,   382,     0,   383,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   384,   385,   372,     0,   386,   387,   388,
     375,   389,   390,   376,     0,   377,     0,   378,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   379,     0,     0,     0,     0,   391,
       0,     0,    76,   392,     0,     0,     0,     0,     0,   393,
     454,    79,   394,   395,   396,   397,     0,     0,     0,     0,
       0,     0,   380,   381,     0,   382,     0,   383,  1960,    64,
      65,    66,    67,    68,    69,    70,    71,   384,   385,   372,
       0,   386,   387,   388,     0,   389,   390,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,    76,   392,     0,     0,
       0,     0,     0,   393,    78,    79,   394,   395,   396,   397,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -462,  -462,     0,  -462,    46,     0,
      47,     0,  -462,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    74,     1,     4,   180,    74,   229,   229,     1,   251,
      76,   906,   716,    74,   165,   266,    96,   297,   181,    74,
       4,   165,   176,   229,   991,   899,   152,   391,   230,   619,
    1321,  1322,   365,   481,   232,   619,    59,   619,   790,    83,
     632,  1131,   229,   239,   796,   217,   701,   942,   229,  1799,
     885,   140,   229,  1020,   166,    56,    57,   338,    59,   229,
      59,   393,   760,     1,   353,    74,    59,   263,   357,    74,
    1619,   623,   541,    74,   789,  1865,  1156,   625,   274,  1730,
    1730,  1730,    83,   552,    83,   694,   309,   309,     1,   789,
      91,  1396,     1,   118,    75,    96,    74,    96,    99,   807,
      99,  1731,   103,   309,   103,   149,   304,   305,   310,   530,
     531,     0,   201,   330,   806,   807,  1083,   825,   191,   103,
     246,    59,   309,   146,   593,    75,   132,    72,   309,     0,
     191,   156,   309,   825,   787,   787,   191,   869,   158,   309,
     141,    72,   160,   144,    10,   146,    59,   146,   787,   167,
     230,   152,   369,   146,    72,   165,   229,   177,   159,     1,
     229,   787,     4,    83,   896,   166,    58,   134,   229,    61,
      62,  1128,    64,    98,   229,  1869,    96,   160,  1135,    99,
     461,    72,   191,   103,   892,     0,   191,    89,    72,   190,
     191,   190,    77,    78,   177,   336,   177,   787,    77,    78,
     892,   168,   181,   787,   205,   787,   205,   152,   146,  1615,
     254,   156,   353,   191,   215,   221,   357,    59,  2008,   220,
     229,     1,   288,  1853,   229,   156,  1775,   177,   229,   230,
     310,   230,  1279,   146,   152,   656,   309,   789,   156,   367,
     309,    83,   370,   791,   948,   246,    89,   795,   309,  1206,
     159,   229,   177,   254,   309,   254,   804,   805,   160,    84,
     152,   152,   513,   264,   265,   156,   134,   268,   152,   268,
     190,   123,   316,   977,   275,   160,   499,   499,   107,   108,
      99,   160,  1396,  1397,  1140,   205,     1,    89,   289,   290,
     118,   292,   298,   499,     1,  1047,   162,     4,   140,   579,
     168,   167,     1,   155,   146,   568,   508,   149,   309,   310,
     230,   310,   499,  1148,   455,   487,   317,   160,   499,   608,
     653,   651,   499,   324,   325,  1140,   336,   152,   329,   499,
     611,  1029,   391,   152,   254,   133,   152,  1642,  1053,   671,
    1140,  2091,  1177,  1238,    59,  1975,   767,   459,   268,    72,
     714,  1246,    59,  1053,   635,   551,   645,   966,   160,   201,
      59,   642,   451,   586,   586,   160,   367,   152,  2062,   370,
     371,   923,     1,    20,  1780,  1781,   158,   175,   158,   159,
     586,   425,   177,  1473,   297,   391,   588,  1095,   103,  2040,
    2040,  2040,    99,  1046,  1046,   177,   103,   866,    75,   586,
    1132,     4,   528,  1095,  2098,   586,  1363,  1046,   534,   586,
     252,  2041,   254,   479,    91,   146,   586,   152,   156,    10,
    1046,  1712,  1713,   161,    61,    62,   499,   155,   508,   152,
      59,   146,   109,   156,   435,    62,   435,   168,   499,   146,
     152,   176,   154,   152,   499,   455,   590,   146,   176,  1496,
    1497,  1498,  2082,    56,    57,   297,  1046,   458,   459,   146,
     147,   148,  1046,   152,  1046,   238,    72,   608,   241,   470,
     471,   160,    99,    89,   316,  1881,  1882,   158,   479,  1231,
     481,   168,   812,   110,   158,   112,   885,   114,    91,   262,
     499,   632,   158,    72,   499,   607,   177,   158,   499,   158,
     273,  1053,   168,   177,   645,   157,  2011,   508,   588,   508,
    1366,  1367,  1368,   586,   520,   435,   177,   146,   177,   525,
      72,   499,  1070,   160,  1491,   586,   153,   528,  1642,   156,
     157,   586,  2037,   534,  1614,    72,   542,   590,   141,  1619,
     177,   144,   158,   158,   160,     1,   152,   553,     4,    75,
     156,  1366,  1367,  1368,   107,   108,   159,   152,  1310,  2064,
     151,   268,   177,   166,    90,   741,  1366,  1367,  1368,   592,
     152,   162,   573,   152,   575,  1023,   167,   156,   205,   160,
     590,   152,   297,   425,   149,   586,   167,   588,   508,   588,
     853,   592,   275,   592,   134,   160,   877,   177,   297,   592,
     152,   602,   155,    59,   156,   606,   607,   290,    72,   451,
     669,   176,   158,   672,   673,   152,   675,   220,    72,   156,
      72,   910,   632,   163,   164,   684,    72,    83,   687,   688,
     689,   912,   154,  1225,  1289,  1926,   158,   800,   639,   158,
     158,   268,   814,   270,   271,    72,   158,   103,   158,   168,
     168,   652,   915,   154,   592,   111,    72,   158,    72,    72,
     158,   264,   265,   669,  1778,   177,   579,   177,   297,   675,
     168,   847,   275,   158,   177,   302,   742,   789,   684,   592,
     156,   308,  1386,   168,   140,   161,   289,   290,   152,   292,
     146,     3,   156,   149,    69,  1775,   609,   703,   152,  1573,
     152,   827,   156,   132,   156,   706,   152,   708,   158,   710,
     156,   338,     3,   714,   317,   155,   717,   344,   168,   346,
     846,   324,   325,   152,   158,   152,   329,   156,  1695,   156,
    1697,   573,    72,   189,   163,   164,   152,   579,   152,   152,
     156,   742,   156,   156,   152,   201,     3,   176,   521,  1148,
     592,   132,  1407,   806,   807,   382,    13,    14,    15,    16,
      17,   158,  1025,   160,   367,  1660,  1661,   370,   158,   910,
     543,   152,   825,   107,   108,   156,   158,   550,  1177,  1042,
    1043,   554,   163,   164,   128,   129,   787,   177,   789,    13,
      14,    15,    16,    17,  1215,   177,   252,   160,   254,  1879,
     801,  1566,   163,   259,   152,   158,  1571,   808,   435,   170,
     171,  1891,   160,   814,   160,    72,   817,     1,   962,   167,
       4,   800,   934,  1271,   177,   826,   827,   828,   172,   173,
    1944,  1082,   811,  1488,   461,   152,   463,   464,   154,   892,
     151,   297,   159,   165,   166,   846,   852,   158,    72,   165,
     166,   478,   153,   126,   127,   458,   151,   863,  1972,   160,
     316,   867,   577,   158,   579,   871,   154,   470,   471,   176,
     158,   784,   146,   147,   148,    59,   154,   592,   156,  1959,
     579,   508,   883,   884,   885,   592,  1219,  1220,  1218,   160,
     154,   154,   885,   592,   168,   159,   159,   152,  2012,    47,
      48,   528,    50,   177,   154,   155,   533,    55,   535,   962,
     134,   721,   722,   723,   898,   154,  1114,  1247,  1090,   103,
     159,  1816,   174,  1475,    13,    14,    15,    16,    17,   556,
     152,   558,   559,   934,   152,   391,   154,   152,   156,    58,
    1052,  1053,    61,    62,   118,    64,   947,   885,  1229,   576,
     579,   154,   962,   154,   154,   152,  1404,   154,   159,   156,
     154,   588,   146,   592,   155,   156,  1296,   152,   159,   425,
     573,   156,   885,    13,    14,    15,    16,    17,  1563,  1564,
    1565,   132,   983,    72,   611,   154,   613,   614,   154,   152,
     991,   154,  1234,   156,   154,   451,   154,  1396,  1397,   602,
     158,   152,   154,   606,   607,   156,   158,  1371,   635,   636,
     154,   152,   163,   164,   158,   642,     3,   130,   131,  1020,
    1468,  1916,  1023,  1137,  1138,  1139,    13,    14,    15,    16,
      17,   152,    72,   157,   158,    78,   639,  1016,  1486,  1302,
    1303,   158,  1095,   885,   152,  1046,   154,    22,   156,   652,
     152,  1052,  1053,  1316,  1317,   154,   898,   157,   158,   158,
     152,   104,  1216,  1217,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   158,   530,   531,   154,   146,   147,   148,
     158,  1244,  1083,  1209,  1225,    72,   152,  1350,  1351,   158,
     156,   158,   159,   154,   134,   868,  1140,   158,   152,   168,
     152,  1352,   156,   706,   156,   708,    99,   710,   177,  1253,
    1254,   714,   158,  1092,   717,   154,  1396,   573,   154,   158,
     152,   577,   158,   579,   156,   154,   160,   154,  1134,   158,
     202,   158,  1465,  1463,   177,   158,   592,   163,   164,   742,
    1470,  1147,   111,   112,   113,   114,   115,  1131,   132,  1150,
     157,   158,  1153,  1154,  1155,  1148,   154,  1487,  1164,   158,
     158,   146,   147,   148,   152,   154,  1329,  1330,   152,   158,
     885,  1322,   156,   158,   152,  1377,  1177,   154,   885,   163,
     164,   158,  1183,   168,  1177,  2045,   885,   157,   158,  2049,
    1191,   898,   177,  1194,  1195,  1194,  1195,  1198,   801,  1258,
     656,  1254,  1906,   154,     1,   808,   132,   158,  1209,    89,
    1148,  1195,   160,   669,   151,  1225,   672,   673,   121,   675,
     123,   124,   125,   826,   160,   828,   152,   157,   684,   154,
     156,   687,   688,   689,   167,  1148,   154,   163,   164,  1177,
     158,  1242,   154,  1642,  1254,   176,   158,   154,   154,   152,
     877,   158,   155,   156,  1255,   154,   885,   160,   161,   158,
     152,   888,    59,   154,  1177,  1244,   118,   158,  1321,   154,
    1271,   152,   154,   158,  1194,  1195,   158,   152,  1279,   152,
     883,   884,   885,  1546,  1547,   912,   154,   157,   158,  1131,
     158,   157,   158,   158,  1624,   169,   923,  1377,  1140,   157,
     158,   164,    99,   162,  1448,   932,  1148,   157,   158,   104,
    1311,   767,   174,  1292,   109,   110,   111,   112,   113,   114,
     115,   116,  1366,  1367,  1368,   132,  1589,  1371,  1372,   157,
     158,   934,   157,   158,   155,  1177,   728,   729,   730,   731,
     154,  1674,    13,    14,    15,    16,    17,    18,  1678,   146,
    1329,  1330,   157,   158,  1577,  1577,   154,    62,   154,  1689,
     154,   156,  1642,   154,  1566,   157,   158,   157,   158,  1571,
      18,  1577,   157,   158,   157,   158,  1377,  1579,   154,  1778,
    1381,  1387,  1388,  1384,   156,  1369,   157,   158,   157,   158,
    1577,   157,   158,   157,    99,  1448,  1577,   157,   158,   134,
    1577,   134,  1029,  1404,  1667,   110,   111,  1577,   592,    57,
      58,    59,    60,    61,    62,    63,    64,   158,   159,  1425,
     157,   158,   158,  1424,  1131,  1426,  1053,  1426,   159,   885,
     157,   158,   152,  1148,    77,    78,   158,   159,  1448,  1702,
     159,  1148,   898,   899,  1707,  1708,   104,   154,   153,  1148,
     154,   109,   110,   111,   112,   113,   114,   115,  1396,  1397,
    1280,  1281,  1177,   154,   536,  1641,   154,  1468,   724,   725,
    1177,   268,   726,   727,   732,   733,  1806,  1807,  1177,   154,
    1195,  1584,  1585,  1396,  1397,  1486,  1566,  1194,  1195,  1473,
    1491,  1571,  1466,  1467,   154,  1496,  1497,  1498,   152,  1579,
     205,   157,  1653,  1216,  1217,   156,  1426,   160,  1577,  1653,
       4,     5,     6,     7,     8,     9,    10,    11,    12,  1148,
     160,   160,  1745,  1745,  1366,  1367,  1368,  1369,  1370,  1371,
    1372,    13,    14,    15,    16,    17,    18,   609,   160,  1745,
     160,   338,    70,   157,   152,  1944,  1748,  1150,  1177,    78,
    1153,  1154,  1155,   157,  1396,  1397,    18,   176,  1745,   158,
     160,   152,  1713,   268,  1745,  1566,   177,  1194,  1745,   154,
    1571,    65,   154,  1972,  1177,  1745,  1577,   160,  1579,   177,
    1183,   160,   157,   157,    18,  1586,   151,  1917,  1191,    13,
      14,    15,    16,    17,    18,  1198,   154,   302,   154,  1600,
     154,   154,  1229,   308,   154,   154,   154,   154,  1235,   151,
     151,   154,  1613,  2012,   160,   160,   160,    70,  1794,   154,
     177,   176,   154,   154,  1608,   151,   160,  1633,  1687,   154,
     160,  1473,   154,   338,   158,   158,   176,   154,   154,  1242,
     158,   154,   154,  1653,   154,   154,   154,   154,   151,  1650,
     154,   157,  1255,    13,    14,    15,    16,    17,    18,  1712,
    1862,   157,  1369,   154,   461,  1995,   154,   154,  1748,   154,
     154,   743,  1656,   154,   154,  1131,   154,   382,   157,  1805,
     154,  1396,  1397,   176,  1140,   379,   154,   158,   151,  1396,
    1397,   158,  1148,   154,  1695,  1976,  1697,  1396,  1397,    14,
     152,   885,  1865,   152,  1642,    13,    14,    15,    16,    17,
     404,   405,   784,   152,   898,   152,   152,   152,   159,  1426,
      74,  1177,   177,    91,   151,   158,  1902,   159,   157,  1642,
     177,   425,   157,   154,   806,   807,    80,   177,   160,  1195,
     158,   157,   154,   158,  1745,   158,   177,  1748,   154,   177,
     158,  1378,   157,   825,   154,   157,   461,  1898,  1759,  1215,
     154,   455,  1763,   151,    72,   151,  1473,  1396,  1397,   152,
     177,   215,   152,   478,   177,  1926,  1777,   152,  1381,   576,
     177,  1384,  1862,   152,   177,   177,  1787,   177,   177,   151,
     154,   158,   177,   177,   151,   592,  2059,   158,   151,  1426,
    1642,   151,  1258,  1804,  1805,    13,    14,    15,    16,    17,
     157,   160,   160,   151,  1656,   157,   157,  2040,  2040,   154,
     892,  1424,   154,   157,   132,   121,   159,   151,   159,   154,
     535,   154,  1973,   154,  2040,   154,   154,   157,   635,  2041,
    1778,   157,   151,  2006,   152,  2008,   151,   177,   156,  1975,
     154,   556,   159,  2040,  1898,   163,   164,   158,   152,  2040,
     158,  1862,   154,  2040,    72,  1778,   152,   152,  1869,  2045,
    2040,   576,  1873,  2049,  2050,   110,   157,  1878,   157,   157,
    2082,   151,   151,   160,  2047,   151,  1865,   154,    75,   154,
     154,   154,   154,  1608,   154,   154,   157,    75,   151,   154,
    1607,  1608,  1903,   177,   177,   152,   611,    90,  2084,   177,
    1366,  1367,  1368,  1369,  1370,  1371,  1372,   154,   157,   157,
     160,   151,   151,   151,   132,   156,    75,  1642,   154,  1973,
     635,   154,   154,   154,  2110,  1642,  1778,   642,  2114,   154,
    1396,  1397,   154,  1642,   152,   155,  1947,  1131,   156,  1656,
    1951,   168,   177,    75,   168,   163,   164,   177,   159,   177,
    2136,  2041,   151,  1964,  1148,   151,  2129,  2040,   158,   154,
     154,  2040,   154,   154,  1975,     1,  1977,   151,     4,  2040,
    1607,   153,   151,  1586,   159,  2040,   104,  1988,   168,  1990,
    1991,   168,   152,  1177,   158,    75,   152,  1600,   151,   153,
     168,   168,  2082,   110,   177,   157,  1944,   177,   110,   151,
    1613,  1195,   153,  1642,  2015,   154,   159,  1473,   154,   151,
     151,   154,   152,  1095,    75,   154,   720,  2006,   154,  2008,
     177,  1944,   177,    59,  1972,   479,  1687,   481,  1286,  2040,
    2041,  1386,  2041,   177,   693,   734,   736,  1650,    74,   735,
    2051,   738,   737,   424,  1166,  1177,  2098,    83,  1765,  1972,
    2008,  2062,  1786,  1778,  1642,  2037,  2092,  1778,  2047,  1907,
      96,  1778,  2091,    99,  2012,  2079,  1634,   103,  1634,  1778,
     877,  2082,  2050,  2082,  2114,  1973,  1972,  1198,   885,    49,
     111,   888,  1862,   259,  1934,  2096,    13,  2098,  1585,  2012,
    1372,  2080,  1944,  2109,   938,  1191,   814,   488,    13,    14,
      15,    16,    17,   902,   140,   912,  2117,  1573,  1656,  2125,
     146,  1748,  2123,   149,     0,   602,   152,   153,   947,   759,
    1972,   759,  2133,   759,  1554,    -1,  2137,    -1,  1765,   165,
      -1,    -1,    -1,    -1,    -1,   850,  2147,    -1,    -1,  1778,
    2129,    -1,  1608,    -1,    -1,    -1,  1759,    -1,    -1,    -1,
    1763,    -1,    -1,   189,   190,   191,    -1,    72,    -1,    -1,
    2012,    88,   877,    -1,  1777,   201,   202,    -1,    -1,   205,
      -1,    -1,    -1,   888,  1787,  1369,  1642,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1656,  1804,    -1,   229,   230,    -1,    -1,   912,    -1,    -1,
      -1,    -1,  1396,  1397,    -1,    -1,    -1,    -1,    -1,    -1,
     246,    -1,    -1,    -1,    -1,    -1,  1853,   132,   254,  1944,
      -1,  1687,    -1,   240,    -1,    -1,    -1,  1944,    -1,    -1,
      -1,    -1,   268,    -1,    -1,  1944,    -1,   152,    -1,  1321,
    1322,   156,    -1,    -1,    -1,    -1,    -1,  1972,   163,   164,
      -1,    -1,    -1,    -1,   958,  1972,  1869,    -1,    -1,   963,
    1873,    -1,    -1,  1972,    -1,  1878,   302,    -1,    -1,    -1,
     974,    -1,   308,   309,   310,    -1,    -1,    -1,    -1,  1473,
     316,    -1,    -1,    -1,    -1,    -1,    -1,  2012,    -1,    -1,
    1903,    -1,    -1,    -1,    -1,  2012,    -1,    -1,    -1,    -1,
     336,   337,   338,  2012,    -1,  1944,    -1,    -1,    -1,    -1,
      -1,    -1,  1778,    -1,    -1,    -1,    -1,   353,    -1,   104,
      62,   357,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,  1972,  1947,   120,    -1,   122,  1951,  1976,
      -1,  1148,     4,     5,     6,     7,     8,     9,    10,    11,
      12,  1964,    -1,    -1,    -1,   391,    -1,    -1,    -1,    -1,
     814,    -1,   104,   817,  1977,    -1,   108,    -1,   153,   111,
    1177,   113,    -1,  2012,    -1,  1988,    -1,  1990,  1991,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1194,    -1,   425,
      -1,    83,   428,    -1,    -1,    -1,    -1,    -1,   104,   435,
      -1,    -1,  2015,   109,   110,   111,   112,   113,   114,   115,
      -1,   428,    -1,    -1,  1608,   451,    -1,    -1,    -1,   455,
      -1,    -1,  1229,   459,    -1,   461,    -1,   444,    -1,    -1,
     447,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2051,    -1,
     202,    -1,    -1,    -1,  1148,  2082,    -1,   153,  1642,  2062,
     156,    -1,    -1,    -1,    -1,    -1,   853,   149,    -1,   104,
      -1,    -1,  1656,   499,   109,   110,   111,   112,   113,   114,
     115,   116,   508,    -1,    -1,   120,    -1,   122,  1944,  1194,
      -1,    -1,    -1,  2096,    -1,  2098,    -1,   504,    -1,    -1,
      -1,    -1,   528,    -1,   530,   531,    -1,   189,   534,    -1,
     536,    -1,    -1,    -1,  2117,    -1,  1972,  1589,   153,    -1,
    2123,   156,    -1,    -1,  1229,   257,    -1,    -1,   915,    -1,
    2133,  1225,    -1,    -1,  2137,    -1,    -1,    -1,    -1,   983,
      -1,    -1,    -1,    -1,  2147,    -1,    -1,   991,    -1,   575,
      -1,    -1,    -1,    -1,    -1,    -1,  2012,    -1,    -1,    -1,
     586,    -1,   588,    -1,   590,    -1,   592,  1261,  1262,  1263,
      -1,    -1,   254,    -1,  1268,  1269,  1020,    -1,    -1,  1023,
      -1,    -1,   608,   609,   336,   611,    -1,   339,    -1,     3,
      -1,    -1,    -1,   619,  1778,    -1,    -1,   623,    -1,  1396,
    1397,   353,    -1,    -1,    -1,   357,   632,    -1,   340,   341,
      -1,   343,    -1,   345,    -1,    -1,   642,   104,    -1,   645,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1426,
     656,    -1,    -1,    -1,   316,    -1,    -1,    -1,  1025,  1083,
    1712,  1713,    -1,   669,    56,    57,   672,   673,    -1,   675,
     382,    -1,    -1,  1358,   336,  1042,  1043,    -1,   684,    -1,
      -1,   687,   688,   689,    -1,    65,    66,    67,    68,    -1,
      -1,    -1,    -1,  1378,    -1,    -1,   163,    -1,   104,    91,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   455,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   743,   132,    -1,
      -1,  1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,   144,   759,   760,    -1,    -1,    -1,   152,   153,
      -1,   767,    -1,   425,    -1,   159,   478,   159,    -1,   163,
     164,   177,   759,   760,    -1,    -1,   156,    -1,   784,    -1,
    1944,   787,   769,   789,    -1,   772,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   455,   174,    -1,    -1,    -1,    -1,    -1,
     806,   807,    -1,     1,   536,    -1,     4,   104,  1972,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   825,
      -1,   827,    -1,    -1,    -1,    -1,    -1,    -1,   220,    -1,
    1607,    -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,
     846,    -1,    -1,    -1,    -1,    -1,   833,  1271,  2012,    -1,
     837,    -1,    -1,    -1,   841,  1279,    -1,    -1,   590,    -1,
      -1,    59,    -1,    -1,    -1,  1642,    -1,    -1,   530,   531,
      -1,    -1,   264,   265,  1926,    -1,   608,   609,    -1,   885,
     177,    -1,    -1,   275,    -1,    83,   892,    -1,    -1,    -1,
      -1,    -1,   898,   899,    -1,    -1,    -1,    -1,   290,    -1,
     632,    99,    -1,    -1,   910,   103,   912,    -1,    -1,    -1,
      -1,    -1,    -1,   645,    -1,    -1,    -1,   923,    -1,    -1,
      -1,    -1,  1607,    -1,    -1,   317,    -1,    -1,    -1,    -1,
      -1,    -1,   324,   325,    -1,  1302,  1303,   329,    -1,    -1,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,  1316,
    1317,   149,    -1,    -1,   104,   153,   962,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   164,   165,   166,    -1,
     632,    -1,    -1,  1647,    -1,   367,    -1,    72,   370,    -1,
    1404,    -1,    -1,  1350,  1351,    -1,    -1,    -1,  1765,    -1,
      18,   189,    -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,
      -1,  1778,    -1,   201,   202,   155,    -1,   205,    -1,   104,
     160,   743,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   104,    -1,  1029,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    61,    62,    63,    64,   132,    -1,    -1,
    1046,    -1,  1029,    -1,  1468,    -1,    -1,  1053,    -1,    -1,
      -1,    -1,   784,    -1,   252,    -1,   254,   152,   153,    -1,
      -1,   156,  1486,    -1,    -1,    -1,   458,  1491,   163,   164,
     268,    -1,  1496,  1497,  1498,    -1,   104,   160,   470,   471,
    1765,   109,   110,   111,   112,   113,   114,   115,   116,  1095,
      -1,   289,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   297,
      -1,    -1,    -1,    -1,   302,   767,    -1,    -1,    -1,    -1,
     308,    -1,    -1,  1100,    -1,    -1,  1103,    -1,   316,    -1,
      -1,    -1,    -1,    -1,    -1,  1131,    -1,    -1,   156,    -1,
      -1,    -1,    -1,    -1,  1140,    -1,    -1,    -1,   336,    -1,
     338,   339,  1148,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,   353,    -1,    -1,    -1,   357,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1944,    -1,    -1,
      -1,  1177,    -1,    -1,    72,    -1,    -1,    -1,   910,  1546,
    1547,   913,    -1,    -1,    -1,    -1,    -1,    -1,  1194,  1195,
      -1,    -1,    -1,   391,    -1,  1972,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1209,    72,    -1,   104,    -1,    -1,  1215,
     602,   109,   110,   111,   112,   113,   114,   115,    -1,  1225,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,
     962,  1905,    -1,    -1,   132,  2012,   104,   899,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,  1254,    -1,
      -1,    -1,  1258,   451,   152,   153,    -1,   455,    -1,    -1,
     652,    -1,    -1,   461,   132,   163,   164,    72,    74,   104,
      -1,  1695,    -1,  1697,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
      96,  1976,    -1,    -1,    -1,   163,   164,   132,    -1,   104,
    1667,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1298,    -1,    -1,    -1,  1321,  1322,   152,   153,    -1,
    1307,   156,    -1,    -1,    -1,    -1,    -1,   132,   163,   164,
      -1,    -1,   530,   531,    -1,  1702,    -1,   535,   536,    -1,
    1707,  1708,    -1,    -1,    -1,    -1,   152,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
    1366,  1367,  1368,  1369,    -1,  1371,  1372,    -1,    -1,    -1,
     568,  1377,  1378,    -1,    -1,   573,    -1,    -1,   576,   577,
      -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1396,  1397,   590,    -1,   592,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   606,   801,
     608,   609,    -1,   611,    -1,    -1,   808,    -1,    -1,    -1,
    1426,    -1,   104,   229,   230,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   632,    -1,    -1,   635,    -1,    -1,
     246,   639,  1448,    -1,   642,    -1,    -1,   645,    -1,   647,
      -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,
    1150,    -1,    -1,    -1,    -1,    -1,    -1,  1473,    -1,  1475,
      -1,   669,    -1,   155,   672,   673,    -1,   675,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,
     688,   689,   884,  1225,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,   309,   310,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  1254,    -1,    13,    14,    15,    16,    17,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,   743,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1215,    -1,    -1,    -1,    72,   152,   153,
    1566,  1567,    -1,  1225,    -1,  1571,    -1,  1573,    -1,   767,
      -1,  1577,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,
    1567,    -1,    -1,    72,    -1,   160,   784,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1607,  1608,    -1,    -1,    -1,    -1,    -1,   806,   807,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   132,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   825,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1642,   152,   153,    -1,
      -1,   156,    -1,   132,    -1,    -1,    -1,  1653,   163,   164,
    1656,    -1,   850,   459,    -1,   853,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   152,   153,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   163,   164,    -1,    -1,    -1,   877,
      -1,  1687,    -1,    -1,    -1,    -1,    -1,   885,    -1,    -1,
     888,    -1,  2059,   499,   892,     1,    -1,    -1,     4,    -1,
     898,   899,   508,    -1,    -1,    -1,  1712,  1713,    -1,    -1,
      -1,    -1,   910,   155,   912,   913,  1448,   915,    -1,    -1,
      -1,    -1,   528,    -1,  1730,  1731,    -1,    -1,   534,    -1,
      -1,    -1,    -1,    -1,  1424,    -1,    -1,    -1,    -1,  1745,
     104,    -1,  1748,  1730,  1731,   109,   110,   111,   112,   113,
     114,   115,   116,    59,    -1,    -1,   120,    -1,   122,  1765,
      -1,  1153,  1154,  1155,   962,    -1,    -1,    -1,    -1,   575,
      -1,    -1,  1778,    72,    -1,    -1,    -1,    83,    -1,    -1,
     586,    -1,   588,    -1,    -1,    -1,    -1,    -1,    -1,   153,
      -1,  1183,   156,    -1,    -1,    -1,    -1,   103,    -1,  1805,
      -1,    -1,    -1,    -1,    -1,   104,  1198,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1025,    -1,    -1,
      -1,    -1,    -1,   132,   140,    -1,    -1,    -1,    72,    -1,
     146,    -1,    -1,    -1,  1042,  1043,    -1,  1853,    -1,    -1,
    1242,    -1,    -1,   152,   153,    -1,  1862,    -1,    -1,   165,
      -1,    -1,    -1,    -1,   163,   164,  1853,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1898,    -1,    -1,   201,   202,  1095,   132,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1600,  1573,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
    1926,    -1,    -1,  1613,   230,    -1,    -1,    -1,  1934,   163,
     164,    -1,    -1,  1131,    -1,    -1,    -1,    -1,  1944,    -1,
     246,    -1,  1140,  1930,    -1,   251,   252,  1934,   254,    -1,
    1148,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,
    1650,    -1,    -1,    -1,    -1,    -1,  1972,  1973,    -1,  1975,
    1976,   277,    -1,    -1,    -1,   281,    -1,    -1,    -1,  1177,
     286,   787,    -1,   789,    -1,    -1,    -1,    -1,  1975,  1381,
      -1,   297,  1384,    -1,    -1,    -1,  1194,  1195,    -1,    -1,
      -1,   103,    -1,    -1,   310,    -1,  2012,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,  1215,    -1,    -1,
      -1,   827,    -1,    -1,    -1,    -1,    -1,  1225,    -1,    -1,
     336,  1229,    -1,   339,  2040,  2041,    -1,    -1,    -1,    -1,
     846,    -1,    -1,  1241,   146,    -1,    -1,   353,    -1,    -1,
      -1,   357,    -1,  2040,  2041,  1253,  1254,    -1,    -1,    -1,
    1258,    -1,     1,   165,    -1,    -1,    -1,    -1,    -1,  1759,
      72,    -1,    -1,  1763,   104,    -1,  2082,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1777,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2082,    -1,  1787,    -1,    -1,
     202,    -1,   104,    -1,  1302,  1303,    -1,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,  1316,  1317,
      59,    -1,    -1,  1321,  1322,    -1,   156,    -1,    -1,    -1,
     132,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   451,    -1,    -1,    -1,   455,
     152,   153,  1350,  1351,    -1,    -1,    -1,    -1,    -1,    -1,
    1358,   163,   164,    -1,   103,    -1,  1898,    -1,  1366,  1367,
    1368,  1369,  1370,  1371,  1372,   277,    -1,    -1,    -1,  1869,
    1378,   152,    -1,  1873,   155,   156,    -1,    -1,  1878,    -1,
      -1,    -1,    -1,    -1,  1586,   297,    -1,    -1,  1396,  1397,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     104,    -1,    -1,  1903,    -1,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,   165,    -1,  1426,    -1,
     536,    -1,    -1,    -1,   336,    -1,    -1,   339,    -1,    -1,
    1046,  1973,    -1,    -1,    -1,    -1,  1052,  1053,    -1,    -1,
    1448,   353,    -1,    -1,    -1,   357,    -1,  1947,   152,   153,
      -1,  1951,   568,   202,    -1,    -1,    -1,   573,    -1,    -1,
      -1,   577,    -1,   579,  1964,  1473,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   590,   104,   592,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,  1988,    -1,
    1990,  1991,   608,   609,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,   623,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2015,   632,    -1,    -1,    -1,
      -1,   637,    -1,   152,   153,    -1,    -1,   156,    -1,   645,
      -1,    -1,    -1,    -1,   163,   164,    -1,    -1,  1546,  1547,
      -1,    -1,    -1,   455,    -1,    -1,    -1,   176,   297,   104,
      -1,  2051,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  2062,    -1,    -1,  1573,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,  1589,    -1,    -1,    -1,    -1,    -1,   336,    -1,    -1,
     339,    -1,    -1,  1209,   132,    -1,  2096,   152,  2098,  1607,
    1608,    -1,  1804,    -1,   353,    -1,    -1,    -1,   357,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,  2117,    -1,    -1,
      -1,   159,    -1,  2123,   536,   163,   164,   743,    13,    14,
      15,    16,    17,  2133,  1642,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   760,  1653,    -1,    -1,  1656,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   568,    -1,    -1,  1667,
      -1,    -1,    -1,    -1,    -1,   577,    -1,   579,   784,    -1,
      -1,    -1,    -1,   789,    -1,    -1,    -1,    -1,   590,  1687,
     592,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
     806,   807,    -1,    -1,  1702,  1311,   608,   609,    -1,  1707,
    1708,    -1,    -1,    -1,  1712,  1713,   455,    -1,    -1,   825,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,   104,
     632,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,   645,   104,    -1,    -1,   853,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   132,    -1,    -1,
     120,    -1,   122,    -1,    -1,    -1,    -1,  1765,    -1,    -1,
      -1,  1377,    -1,    -1,    -1,    -1,    -1,   152,   153,   885,
    1778,    -1,    -1,    72,    -1,    -1,   892,    -1,   163,   164,
      -1,    -1,   898,   153,    -1,    -1,   156,   536,    -1,    -1,
      -1,    -1,    -1,    -1,   910,    -1,    -1,   913,    -1,   915,
      -1,    -1,    -1,    -1,   920,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,   568,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,    -1,
     579,   743,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   590,    -1,   592,    -1,    -1,   962,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,   608,
     609,    -1,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,
      -1,   104,   784,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   632,    -1,    -1,    -1,    -1,    -1,    -1,
    1898,    -1,    -1,    -1,   806,   807,   645,   104,    -1,   132,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1025,
      -1,    -1,    -1,   825,    -1,    -1,    -1,    -1,  1926,   152,
     153,    -1,    -1,   156,    -1,   132,  1042,  1043,    -1,    -1,
     163,   164,    -1,    -1,    -1,    -1,  1944,    -1,    -1,    -1,
      -1,   853,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
    1566,    -1,    -1,    -1,    -1,  1571,   163,   164,    -1,    -1,
      -1,  1577,    -1,  1579,  1972,  1973,    -1,    -1,  1976,    -1,
      -1,    -1,    -1,   885,    -1,    -1,    -1,    -1,   104,  1095,
     892,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,   743,    -1,    -1,    -1,   910,    -1,
      -1,   913,    -1,   915,  2012,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1140,    -1,   152,   153,    -1,    -1,
      -1,    -1,  1148,     1,    -1,   784,     4,   163,   164,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     962,  2059,    -1,    -1,    -1,    -1,    -1,   806,   807,   104,
      -1,  1177,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,   825,   104,    -1,  1195,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    59,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,    -1,   853,    -1,   149,    -1,   153,  1225,
      -1,   156,    -1,  1025,    -1,    83,    -1,    -1,  1234,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,  1745,
    1042,  1043,  1748,   176,    -1,   103,   885,  1253,  1254,    -1,
      -1,   104,    -1,   892,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   910,    -1,    -1,   913,    -1,   915,    -1,    -1,    -1,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,
      -1,   149,    -1,  1095,    -1,    -1,  1302,  1303,    -1,  1805,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1316,  1317,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,
      -1,    -1,    -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   201,  1350,  1351,  1148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1862,    -1,    -1,    -1,
    1366,  1367,  1368,  1369,  1370,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1177,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,
    1396,  1397,    -1,  1195,   252,    -1,   254,    -1,    -1,    -1,
      -1,   259,    -1,  1042,  1043,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    93,    94,    -1,    -1,
      -1,    -1,    -1,  1225,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   297,
      -1,    -1,  1448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     127,  1253,  1254,    -1,    -1,    -1,  1095,    -1,   316,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1473,    -1,  1975,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1302,  1303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1148,
      -1,    -1,    -1,    -1,  1316,  1317,    -1,    -1,    -1,  1321,
    1322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   391,  2040,  2041,    -1,    -1,  1177,    -1,
    1546,  1547,    -1,    -1,    -1,    -1,    -1,    -1,  1350,  1351,
      -1,    -1,    -1,    -1,    -1,    -1,  1195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,
      -1,    -1,    -1,  1579,    -1,    -1,  2082,    -1,    -1,    -1,
      -1,    -1,    -1,  1589,    -1,    -1,  1225,    -1,    -1,    -1,
      -1,    -1,    -1,   451,  1396,  1397,    -1,    -1,    -1,    -1,
      -1,    -1,  1608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   306,
      -1,    -1,    -1,    -1,    -1,    -1,  1642,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1448,  1653,    -1,    -1,
    1656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1667,    -1,  1302,  1303,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   530,   531,    -1,    -1,    -1,  1316,  1317,    -1,
      -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,  1702,     4,    -1,    -1,
      -1,  1707,  1708,    -1,    -1,    -1,  1712,  1713,    -1,    -1,
      -1,  1350,  1351,    -1,    -1,   573,    -1,    -1,    -1,   577,
      -1,   579,    -1,    -1,    -1,  1731,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1546,  1547,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,  1396,  1397,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1778,    -1,    -1,    -1,    83,   454,    -1,   456,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1589,   465,   466,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,   656,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1608,    -1,    -1,  1448,
      -1,   669,    -1,    -1,   672,   673,    -1,   675,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,
     688,   689,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,
    1642,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1653,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1667,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1898,    -1,   201,    -1,    -1,   574,    -1,    -1,
    1702,    -1,    -1,    -1,    -1,  1707,  1708,  1546,  1547,   767,
    1712,  1713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1944,    -1,
      -1,    -1,    -1,    -1,    -1,   252,    -1,   254,    -1,    -1,
    1589,    -1,   259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1972,  1973,    -1,  1608,
      -1,    -1,    -1,    -1,    -1,    -1,  1778,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     297,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1642,    -1,    -1,  2012,    -1,    -1,   316,
      -1,    -1,    -1,    -1,  1653,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   885,  1667,    -1,
      -1,    -1,    -1,    -1,    -1,  2041,    -1,    -1,    -1,    -1,
     898,   899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2059,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,  1707,  1708,
      -1,    -1,    -1,  1712,  1713,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   391,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1898,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,    -1,
      -1,    -1,    -1,   800,  1926,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1778,
      -1,    -1,  1944,    -1,   451,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1972,  1973,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     0,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   878,   879,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2012,    -1,   889,   890,   891,    -1,    -1,   894,    13,    14,
      15,    16,    17,   530,   531,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,  2059,    53,  1898,
      55,    77,    -1,    -1,    -1,    -1,   573,    -1,    -1,    -1,
     577,    -1,   579,  1131,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,  1140,    -1,    -1,   592,    -1,  1926,    -1,    -1,
    1148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   976,
      -1,    -1,    -1,    -1,    -1,  1944,    -1,    -1,    -1,    -1,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,  1177,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1972,  1973,    -1,    -1,  1195,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1022,    -1,    -1,    -1,   656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,    -1,
     155,    -1,   669,    -1,    -1,   672,   673,    -1,   675,    -1,
      -1,    -1,    -1,  2012,    -1,    -1,    -1,   684,    -1,    -1,
     687,   688,   689,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1069,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1258,  1078,  1079,  1080,  1081,    -1,    -1,    -1,    -1,  1086,
    1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1096,
    2059,    -1,    -1,   239,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,
    1117,    -1,    -1,  1120,    -1,  1122,    -1,   263,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,
     767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   292,   293,    -1,    -1,
      -1,    -1,    -1,   299,   300,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   315,
    1177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1366,  1367,
    1368,  1369,  1370,  1371,  1372,    -1,    -1,    -1,    -1,   335,
      -1,    -1,    -1,    -1,    -1,    -1,  1203,    -1,    -1,    -1,
      -1,    -1,    -1,  1210,    -1,  1212,  1213,    -1,  1396,  1397,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,    -1,  1226,
      -1,  1228,    -1,  1230,    -1,    -1,    -1,    -1,  1235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   392,    -1,   885,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   898,   899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,   423,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1473,    -1,    -1,    -1,    -1,
    1297,    -1,    -1,    -1,    -1,    -1,    -1,  1304,  1305,    -1,
      -1,    74,    -1,   449,    -1,    -1,    -1,   453,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1328,    -1,    -1,    -1,    -1,   472,    -1,  1335,   181,
     476,   477,  1339,    -1,   480,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   495,
     496,   497,   498,    -1,    -1,  1362,    -1,    -1,    -1,    -1,
     133,    -1,   135,    -1,    -1,    -1,    -1,    -1,   514,  1376,
      -1,    -1,    -1,    -1,    -1,    -1,   522,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   551,    -1,    -1,    -1,  1416,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,
    1608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   582,    -1,    -1,    -1,
      -1,    -1,    -1,   589,    -1,  1452,    -1,    -1,    -1,   595,
      -1,    -1,    -1,  1460,  1642,  1462,   229,    -1,    -1,    -1,
     233,    -1,    -1,   236,   237,    -1,    -1,   240,  1656,    -1,
     243,   244,    -1,   619,   620,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1131,    -1,    -1,    -1,    -1,  1687,
      -1,    -1,    -1,  1140,  1511,  1512,    -1,    -1,    -1,    -1,
      -1,  1148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1526,
    1527,    -1,  1529,   375,    -1,    -1,    -1,   379,   380,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,   309,   389,   390,   312,
    1177,  1548,  1549,    -1,   690,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   404,   405,    -1,    -1,    -1,    -1,  1195,    -1,
     333,   334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   425,    -1,    -1,   349,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   759,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1258,    -1,    -1,    -1,    -1,    -1,    -1,   774,    -1,
      -1,    -1,   778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   787,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1668,  1669,   809,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   444,   818,    -1,  1681,    -1,    -1,    -1,   824,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1718,  1719,    -1,    -1,   861,    -1,    -1,    -1,   865,
      -1,    -1,    -1,   869,    -1,    -1,   499,    -1,    -1,  1366,
    1367,  1368,  1369,  1370,  1371,  1372,    -1,    -1,   511,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     896,    -1,    -1,    -1,    -1,    -1,  1944,    -1,    -1,  1396,
    1397,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1972,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1809,    -1,    -1,    -1,   952,    -1,    -1,    -1,
      -1,    -1,    -1,   586,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1830,  2012,    -1,  1833,  1834,    -1,    -1,
      -1,    -1,    -1,  1840,    -1,    -1,  1473,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   627,   628,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   640,   720,   721,
     722,   723,   724,   725,   726,   727,   728,   729,   730,   731,
     732,   733,   734,   735,   736,   737,   738,    -1,    -1,  1035,
      -1,    -1,    -1,  1039,    -1,    -1,    -1,    -1,    -1,    -1,
    1046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1056,    -1,    -1,    -1,    -1,    -1,    -1,  1063,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1072,    -1,  1074,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   800,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1106,    -1,    -1,    -1,  1110,    -1,    -1,  1974,    -1,    -1,
      -1,  1608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   165,
      -1,  1127,    -1,    -1,    -1,    -1,  1132,    -1,    -1,    -1,
     763,   764,    -1,    -1,    -1,    -1,   769,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1642,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   201,   202,   790,    -1,  1656,
     793,   794,    -1,   796,    -1,   798,   799,    -1,    -1,    -1,
      -1,  2038,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   233,    -1,    -1,
    1687,    -1,    -1,    -1,   240,    -1,  2063,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   837,    -1,    -1,    -1,   841,    -1,
      -1,  2078,    -1,    -1,    -1,    -1,    -1,  1223,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2094,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1248,    -1,    -1,    -1,   958,    -1,    -1,    -1,
      -1,   963,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   974,    -1,    -1,    -1,   312,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   907,   908,    -1,    -1,    -1,    -1,
      -1,  1778,    -1,    -1,    -1,    -1,    -1,    -1,   921,    48,
     336,   337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1016,    -1,    -1,    -1,    -1,    -1,
      -1,   357,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,    -1,    -1,
    1346,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   133,    -1,   135,  1383,    -1,    -1,
      -1,    -1,   428,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   444,   445,
      -1,   447,   448,    99,    -1,    -1,    -1,    -1,    -1,   455,
      -1,    -1,    -1,   459,    -1,    -1,    -1,    -1,    -1,  1052,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1433,    -1,    -1,
    1436,    -1,    -1,    -1,    -1,    -1,  1148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1944,  1454,    -1,
      -1,    -1,    -1,   149,   500,    -1,    -1,   153,   504,    -1,
    1093,    -1,    -1,    -1,    -1,    -1,    -1,  1100,    -1,   165,
    1103,    -1,    -1,    -1,    -1,  1972,    -1,   236,   237,    -1,
      -1,   240,    -1,    -1,   243,   244,    -1,    -1,    -1,    -1,
     536,    -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1507,    -1,    -1,    -1,    -1,   202,    -1,    -1,   205,
    1516,    -1,    -1,  1225,  1520,  2012,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1534,  1535,
      -1,    -1,  1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   587,    -1,    -1,   590,    -1,    -1,    -1,    -1,  1261,
    1262,  1263,    -1,    -1,  1560,    -1,  1268,  1269,   254,    -1,
      -1,    -1,   608,   609,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   268,   619,   333,   334,    -1,   623,    -1,    -1,
    1292,    -1,    -1,    -1,   630,    -1,   632,    -1,    -1,    -1,
     349,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1231,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1239,  1240,    -1,    -1,
      -1,    -1,   308,    -1,    -1,    -1,    -1,  1329,  1330,    -1,
     316,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     336,    -1,   338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1657,  1658,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1307,    -1,    -1,  1310,    -1,  1312,
    1313,    -1,    -1,    -1,    -1,   444,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   391,    -1,   743,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   759,   760,    -1,    -1,    -1,    -1,    -1,
    1353,    -1,    -1,   769,   770,    -1,   772,   773,    -1,   425,
      -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,   784,    -1,
      -1,   787,    -1,   789,   790,    -1,    -1,    -1,   165,    -1,
     796,    -1,   511,    -1,    -1,    -1,    -1,    -1,   391,   455,
     806,   807,    -1,    -1,    -1,   461,    -1,    -1,    -1,    -1,
      -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,   825,
      -1,    -1,    -1,   829,    -1,   202,  1792,   833,    -1,    -1,
      -1,   837,   838,    -1,  1427,   841,   842,    -1,    -1,    -1,
      -1,    -1,    -1,   849,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1838,    -1,   530,   531,    -1,   254,    -1,    -1,
     536,    -1,    -1,    -1,    -1,    -1,   892,   893,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1867,    -1,    -1,    -1,    -1,    -1,    -1,   627,   628,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   923,    -1,    -1,
      -1,   640,  1515,    -1,    -1,    -1,    -1,    -1,  1894,    -1,
      -1,  1897,    -1,    -1,   590,    -1,    -1,   530,   531,   316,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1544,    -1,   609,    -1,   611,   962,    -1,    -1,   336,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1647,   632,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1577,    -1,    -1,    -1,    -1,    -1,
    1583,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   669,    -1,    -1,   672,   673,    -1,   675,
      -1,    -1,    -1,  1029,    -1,    -1,    -1,    -1,   684,   189,
      -1,   687,   688,   689,    -1,  2001,    -1,    -1,    -1,    -1,
    1046,  1047,   202,    -1,   763,   764,    -1,  1053,   425,    -1,
     769,    -1,    -1,    -1,    -1,   215,    -1,   217,    -1,  1652,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   790,    -1,    -1,   793,   794,   669,   796,   455,   798,
     799,    -1,   675,    -1,    -1,    -1,    -1,   743,    -1,  1095,
      -1,   684,    -1,    -1,  1100,  1101,    -1,  1103,  1104,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     703,   767,    -1,    -1,    -1,    -1,    -1,    -1,   837,    -1,
      -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,   784,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1737,  1738,   739,   307,    -1,    -1,
     806,   807,  1745,   530,   531,    -1,  1749,    -1,    -1,   536,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   825,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   907,   908,
      -1,    -1,    -1,  1865,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   590,    -1,    -1,    -1,    -1,    -1,  1225,
      -1,    -1,    -1,    -1,    -1,  1231,  1232,    -1,    -1,    -1,
      -1,    -1,   609,  1905,    -1,    -1,   892,    -1,    -1,    -1,
      -1,    -1,    -1,   899,    -1,    -1,    -1,    -1,  1254,    -1,
      -1,    -1,    -1,    -1,    -1,   632,   912,    -1,    -1,    -1,
      -1,    -1,  1855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1298,  1299,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1307,  1308,    -1,  1310,    -1,   962,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,
      -1,   481,    -1,    -1,    -1,    -1,    -1,   487,    -1,    -1,
      -1,    -1,   492,  1052,  2006,    -1,  2008,  1930,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   743,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1093,  2047,    -1,    -1,    -1,    -1,
      -1,  1100,    -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,
     767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,  2080,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   806,
     807,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1095,
      -1,    -1,  1448,    -1,    -1,    -1,    -1,  2040,   825,   609,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2129,    -1,    -1,
      -1,    -1,   622,    -1,    -1,    -1,     3,    -1,    -1,  1475,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,  1140,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,  1231,    50,    51,   892,    53,   677,    55,    -1,
    1239,  1240,   899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,  1194,    -1,
      -1,   701,   702,    -1,    -1,   705,    -1,   707,    -1,    -1,
      -1,    -1,    -1,   713,    -1,   715,   716,    -1,    -1,  1215,
      -1,  1567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,
     107,   108,    -1,    -1,    -1,    -1,    -1,  1583,    -1,  1298,
      -1,    -1,    -1,   743,    -1,   962,    -1,    -1,  1307,    -1,
      -1,  1310,    -1,  1312,  1313,   132,   756,    -1,  1254,    -1,
      -1,    -1,  1258,    -1,    -1,    -1,    -1,   767,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   781,    -1,    -1,   784,    -1,   163,   164,    -1,    -1,
      -1,    -1,    -1,    -1,  1353,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1653,    -1,    -1,
      -1,   811,    -1,    -1,   814,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1712,  1713,  1427,    -1,
    1366,  1367,  1368,    -1,    -1,  1371,  1372,    -1,  1095,    -1,
      -1,    -1,  1378,    -1,  1730,  1731,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   899,
    1746,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   912,   913,    -1,    -1,    -1,    -1,    -1,    -1,
     920,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   948,    -1,
      -1,    -1,  1448,    -1,    -1,    -1,  1515,    -1,    -1,    -1,
    1393,    -1,   962,  1396,  1397,    -1,    -1,    -1,    -1,  1402,
     970,    -1,    -1,  1406,    -1,  1408,    -1,   977,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1853,  1225,    -1,
      -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,  1254,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,  1573,    -1,    -1,
    1926,    -1,    -1,    -1,  1930,  1931,    -1,    -1,  1934,    -1,
    1090,    72,  1092,  1652,  1094,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,
      -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1552,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,  1975,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,  1653,  1591,    -1,
      -1,    -1,    -1,    -1,    -1,  1165,  1166,    -1,    -1,    -1,
    1603,    -1,    -1,    -1,   155,   156,    -1,    -1,  1737,  1738,
      -1,    -1,   163,   164,    -1,  1618,  1619,    -1,    -1,    -1,
    1749,  1687,    -1,    -1,  2040,  2041,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1642,
      -1,    -1,    -1,    -1,    -1,    -1,  1712,  1713,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1229,
      -1,  1448,    -1,    -1,    -1,  1235,  2082,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,  1254,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1765,
      73,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1286,    -1,    -1,  1289,
      -1,    -1,    -1,    -1,    -1,    -1,  1855,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,  1761,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1770,    -1,  1772,
      -1,  1341,  1775,  1776,    -1,  1778,    -1,    -1,    -1,   152,
    1783,    -1,   155,   156,    -1,    -1,  1573,  1357,  1358,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,  1930,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1898,    -1,  1404,    -1,    -1,  1407,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1926,    -1,    -1,    -1,    -1,    -1,  1653,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1448,    -1,
      -1,  1884,    -1,    -1,    -1,    -1,  1889,  1890,  1458,  1459,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1468,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,    -1,
    1976,    -1,    -1,    -1,    -1,    -1,  1486,    -1,  1488,    -1,
      -1,    -1,    -1,    -1,    -1,  1712,  1713,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1952,
      -1,  1954,    -1,    -1,  1957,  1958,     3,    -1,     5,  1962,
    1963,    -1,    -1,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,  1573,    51,    -1,    53,    -1,  1578,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
    2033,  2034,  2035,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2054,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,   108,    -1,    -1,    -1,  2068,  2069,  2070,  1638,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,  1685,   163,   164,  1688,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,  1926,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    73,    74,  1723,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,     1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,   177,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    -1,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,     1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,  1906,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,   177,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    73,    74,    -1,    76,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,
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
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
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
     153,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
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
     148,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,   103,   104,   177,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,     3,     4,     5,
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
     166,   167,   168,     1,    -1,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,   163,   164,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,     5,    72,    -1,
      -1,    -1,    -1,    77,    78,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,     5,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,    -1,   163,   164,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
     153,    -1,   155,   156,    -1,     3,    -1,     5,    -1,    -1,
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
      61,    62,    63,    64,    -1,    13,    14,    15,    16,    17,
      -1,    72,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    20,    -1,    22,    23,    24,    25,    26,
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
     167,   168,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
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
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    49,    -1,    -1,    52,    -1,    54,
      72,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    49,    -1,    -1,    52,    -1,    54,   132,    56,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,   147,   148,    -1,    73,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    49,    -1,
      -1,    52,    -1,    54,   132,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   147,
     148,    -1,    73,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
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
      -1,    -1,   155,   156,    -1,    -1,   159,    -1,    -1,   162,
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
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    49,   124,   125,    52,
      -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
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
      73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
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
      49,   124,   125,    52,    -1,    54,    -1,    56,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72
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
     365,   154,   191,   192,   177,   228,   177,   228,   224,    80,
     154,   224,   235,   282,   284,   287,   293,   301,   305,   146,
     147,   148,   153,   154,   177,   224,   244,   245,   246,   282,
     177,   177,   224,   177,   370,   177,   224,   223,   224,   111,
     112,   113,   114,   115,   261,   263,   264,   177,    98,   177,
      84,   152,   152,   180,   151,   177,   177,   152,   226,   228,
     410,   177,   154,   179,   151,   151,   179,   158,   158,   151,
     160,   160,   157,   157,   157,   180,   154,   179,   217,   217,
     180,   157,   180,   473,   349,   350,   354,   354,   370,   473,
     151,   389,   451,   452,   154,   159,   154,   158,   159,   370,
     473,   223,   121,   194,   195,   156,   195,   156,   195,   157,
     151,   154,   179,   180,   180,   154,   154,   179,   179,   180,
     180,   180,   179,   179,   157,   180,   154,   410,   358,   358,
     180,   180,   224,   449,   151,   151,   332,   332,   332,   339,
     152,   199,   341,   342,   447,   458,   459,   460,   461,   177,
     158,   177,   339,   177,   384,   411,   416,   217,   301,   158,
     177,   345,   346,   345,   365,   134,   362,   363,   224,   154,
     154,   152,   226,   154,   224,   301,   146,   147,   148,   168,
     177,   247,   248,   226,   225,   177,   248,   154,   159,   224,
     153,   224,   225,   246,   177,   473,   154,   154,   154,   228,
     263,   264,   152,   217,   152,   185,   235,   201,   256,   110,
       1,   226,   410,   390,   179,   179,   352,   352,   157,   358,
     180,   180,   157,   157,   151,   350,   160,   473,   151,   180,
     154,   217,   189,   217,   473,   151,   157,   157,   194,   194,
     358,   154,   154,   358,   358,   154,   154,   157,   158,   134,
     357,   134,   157,   180,   180,   154,   154,   157,   217,   177,
     459,   460,   461,   301,   458,   158,   177,   410,   410,   177,
     154,   416,   410,   177,   226,    77,    78,   160,   238,   239,
     240,   154,   224,    75,   226,   224,   153,   224,    75,   177,
     107,   153,   224,   225,   246,   153,   224,   226,   245,   248,
     248,   177,   224,   151,   160,   240,   226,   152,   179,   177,
     185,   154,   159,   154,   154,   158,   159,   254,   258,   365,
     407,   473,   473,   180,   157,   157,   160,   352,   151,   151,
     151,   157,   157,   180,   180,   180,   179,   180,   154,   154,
     154,   154,   154,   458,   410,   340,     1,   216,   236,   237,
     408,     1,   159,     1,   179,   226,   238,    75,   177,   154,
     226,    75,   177,   168,   168,   226,   225,   248,   248,   177,
     107,   224,   168,   168,    75,   153,   224,   153,   224,   225,
     177,     1,   179,   179,   265,   299,   301,   467,   159,   177,
     156,   185,   270,   271,   272,   226,   201,   191,    75,   109,
     255,   257,   151,   151,   154,   352,   473,   154,   154,   154,
     360,   152,   410,   447,   450,   342,   134,     1,   158,   159,
     151,   275,   276,   282,   226,    75,   177,   226,   224,   153,
     153,   224,   153,   224,   153,   224,   225,   153,   224,   153,
     224,   226,   168,   168,   168,   168,   151,   275,   265,   180,
     152,   199,   407,   458,   183,   159,   104,   152,   154,   159,
     158,    75,   154,   226,   152,   226,   226,   473,   151,   179,
     216,   236,   239,   241,   242,   282,   226,   168,   168,   168,
     168,   153,   153,   224,   153,   224,   153,   224,   241,   180,
     177,   262,   301,   270,   157,   216,   177,   270,   272,   226,
     224,   110,   110,   151,   358,   226,   231,   180,   239,   153,
     153,   224,   153,   224,   153,   224,   180,   262,   215,   154,
     159,   185,   154,   154,   159,   154,   258,    75,   253,   180,
       1,   226,   151,   231,   151,   154,   228,   185,   273,   152,
     177,   273,   226,    75,   154,   228,   158,   159,   216,   154,
     226,   185,   183,   274,   154,   177,   154,   158,   177,   183
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
       4,     1,     4,     7,     5,    10,     8,     1,     4,     2,
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
#line 7502 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7508 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7514 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7520 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7526 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7532 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7538 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7544 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7550 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7556 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7566 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7578 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7584 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7590 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7596 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7602 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7608 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7618 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7624 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7630 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7636 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7642 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7648 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7654 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7665 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7674 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7680 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7686 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7692 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7698 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7704 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7714 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7720 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7727 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7733 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7739 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7745 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7751 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7757 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7763 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7769 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7775 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7781 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7787 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7793 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7799 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7805 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7811 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7817 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7827 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7833 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7839 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7845 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7851 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7857 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7863 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7869 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7875 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7881 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7887 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7893 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7899 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7907 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7913 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7922 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7928 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7934 "Parser/parser.cc"
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
#line 7954 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7960 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7966 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7972 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7978 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7984 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7990 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7996 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8011 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8017 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8023 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8029 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8035 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8041 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8047 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8053 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8071 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8083 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8089 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8095 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8101 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8107 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8119 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8125 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8131 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8137 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8149 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8227 "Parser/parser.cc"
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
#line 8239 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8245 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8251 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8257 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8263 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8269 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8275 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8281 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8287 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8293 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8299 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8305 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8311 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8317 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8323 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8329 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8335 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8341 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8347 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8353 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8359 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8365 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8371 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8377 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8383 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8389 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8400 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8406 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8412 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8418 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8424 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8430 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8448 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8454 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8460 "Parser/parser.cc"
    break;

  case 214:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8466 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8472 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8478 "Parser/parser.cc"
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
#line 8492 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8498 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 220:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8513 "Parser/parser.cc"
    break;

  case 221:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8519 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8531 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8543 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8549 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8555 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8561 "Parser/parser.cc"
    break;

  case 229:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8573 "Parser/parser.cc"
    break;

  case 232:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8579 "Parser/parser.cc"
    break;

  case 233:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8585 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8591 "Parser/parser.cc"
    break;

  case 235:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8597 "Parser/parser.cc"
    break;

  case 236:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8603 "Parser/parser.cc"
    break;

  case 237:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8609 "Parser/parser.cc"
    break;

  case 239:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8615 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8621 "Parser/parser.cc"
    break;

  case 241:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8627 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8633 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8639 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8645 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8654 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8681 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8687 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 254:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8708 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8714 "Parser/parser.cc"
    break;

  case 256:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8720 "Parser/parser.cc"
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
#line 8739 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 261:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8763 "Parser/parser.cc"
    break;

  case 263:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8775 "Parser/parser.cc"
    break;

  case 265:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8781 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8787 "Parser/parser.cc"
    break;

  case 267:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8796 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8805 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8820 "Parser/parser.cc"
    break;

  case 271:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8829 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8835 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8841 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8847 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8853 "Parser/parser.cc"
    break;

  case 276:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8859 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8865 "Parser/parser.cc"
    break;

  case 278:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8871 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8877 "Parser/parser.cc"
    break;

  case 280:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8886 "Parser/parser.cc"
    break;

  case 281:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8896 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8902 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 284:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8917 "Parser/parser.cc"
    break;

  case 285:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8927 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8933 "Parser/parser.cc"
    break;

  case 287:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8942 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8952 "Parser/parser.cc"
    break;

  case 289:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8958 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8964 "Parser/parser.cc"
    break;

  case 291:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8970 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8976 "Parser/parser.cc"
    break;

  case 293:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8985 "Parser/parser.cc"
    break;

  case 294:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8995 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9001 "Parser/parser.cc"
    break;

  case 296:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9010 "Parser/parser.cc"
    break;

  case 297:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9020 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9026 "Parser/parser.cc"
    break;

  case 299:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9035 "Parser/parser.cc"
    break;

  case 300:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9045 "Parser/parser.cc"
    break;

  case 301:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9051 "Parser/parser.cc"
    break;

  case 302:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9060 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9069 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9075 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9081 "Parser/parser.cc"
    break;

  case 306:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9087 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9093 "Parser/parser.cc"
    break;

  case 308:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9099 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9105 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9111 "Parser/parser.cc"
    break;

  case 312:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 313:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9123 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 317:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 318:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 319:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9159 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9165 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9171 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9177 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9183 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9189 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9201 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9207 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9213 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9219 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9225 "Parser/parser.cc"
    break;

  case 331:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9231 "Parser/parser.cc"
    break;

  case 334:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9237 "Parser/parser.cc"
    break;

  case 335:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9243 "Parser/parser.cc"
    break;

  case 336:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9249 "Parser/parser.cc"
    break;

  case 337:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9255 "Parser/parser.cc"
    break;

  case 339:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9261 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9267 "Parser/parser.cc"
    break;

  case 342:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9273 "Parser/parser.cc"
    break;

  case 343:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9279 "Parser/parser.cc"
    break;

  case 344:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9285 "Parser/parser.cc"
    break;

  case 345:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9291 "Parser/parser.cc"
    break;

  case 346:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9297 "Parser/parser.cc"
    break;

  case 347:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9303 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9309 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9315 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9321 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9327 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9333 "Parser/parser.cc"
    break;

  case 353:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9339 "Parser/parser.cc"
    break;

  case 354:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9345 "Parser/parser.cc"
    break;

  case 355:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9351 "Parser/parser.cc"
    break;

  case 356:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9357 "Parser/parser.cc"
    break;

  case 357:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9363 "Parser/parser.cc"
    break;

  case 358:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9369 "Parser/parser.cc"
    break;

  case 359:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9375 "Parser/parser.cc"
    break;

  case 360:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9381 "Parser/parser.cc"
    break;

  case 361:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9387 "Parser/parser.cc"
    break;

  case 362:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9393 "Parser/parser.cc"
    break;

  case 363:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9399 "Parser/parser.cc"
    break;

  case 365:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9405 "Parser/parser.cc"
    break;

  case 366:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 367:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9417 "Parser/parser.cc"
    break;

  case 372:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9423 "Parser/parser.cc"
    break;

  case 373:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9429 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9435 "Parser/parser.cc"
    break;

  case 375:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9441 "Parser/parser.cc"
    break;

  case 376:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 377:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9453 "Parser/parser.cc"
    break;

  case 378:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9459 "Parser/parser.cc"
    break;

  case 379:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9465 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9471 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9477 "Parser/parser.cc"
    break;

  case 384:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9483 "Parser/parser.cc"
    break;

  case 385:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9489 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9495 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9501 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9510 "Parser/parser.cc"
    break;

  case 389:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9519 "Parser/parser.cc"
    break;

  case 390:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9525 "Parser/parser.cc"
    break;

  case 393:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9531 "Parser/parser.cc"
    break;

  case 394:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9537 "Parser/parser.cc"
    break;

  case 396:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9543 "Parser/parser.cc"
    break;

  case 397:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9549 "Parser/parser.cc"
    break;

  case 404:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9560 "Parser/parser.cc"
    break;

  case 407:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9566 "Parser/parser.cc"
    break;

  case 408:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9572 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9578 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 416:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 417:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 418:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9608 "Parser/parser.cc"
    break;

  case 419:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 421:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 422:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 423:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 424:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9643 "Parser/parser.cc"
    break;

  case 425:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 426:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 428:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 429:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9676 "Parser/parser.cc"
    break;

  case 430:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9685 "Parser/parser.cc"
    break;

  case 431:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9694 "Parser/parser.cc"
    break;

  case 432:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// if type_specifier is an anon aggregate => name 
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9704 "Parser/parser.cc"
    break;

  case 433:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9713 "Parser/parser.cc"
    break;

  case 434:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9722 "Parser/parser.cc"
    break;

  case 435:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9731 "Parser/parser.cc"
    break;

  case 436:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9740 "Parser/parser.cc"
    break;

  case 437:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9748 "Parser/parser.cc"
    break;

  case 438:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9756 "Parser/parser.cc"
    break;

  case 439:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9762 "Parser/parser.cc"
    break;

  case 443:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9768 "Parser/parser.cc"
    break;

  case 444:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9774 "Parser/parser.cc"
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
#line 9786 "Parser/parser.cc"
    break;

  case 456:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9797 "Parser/parser.cc"
    break;

  case 461:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9803 "Parser/parser.cc"
    break;

  case 464:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9809 "Parser/parser.cc"
    break;

  case 467:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9815 "Parser/parser.cc"
    break;

  case 468:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9821 "Parser/parser.cc"
    break;

  case 469:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9827 "Parser/parser.cc"
    break;

  case 470:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9833 "Parser/parser.cc"
    break;

  case 471:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 472:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9845 "Parser/parser.cc"
    break;

  case 474:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9851 "Parser/parser.cc"
    break;

  case 475:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9857 "Parser/parser.cc"
    break;

  case 477:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 478:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9869 "Parser/parser.cc"
    break;

  case 479:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9875 "Parser/parser.cc"
    break;

  case 480:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9881 "Parser/parser.cc"
    break;

  case 481:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9887 "Parser/parser.cc"
    break;

  case 482:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9893 "Parser/parser.cc"
    break;

  case 483:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9899 "Parser/parser.cc"
    break;

  case 484:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9905 "Parser/parser.cc"
    break;

  case 485:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9911 "Parser/parser.cc"
    break;

  case 486:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9917 "Parser/parser.cc"
    break;

  case 487:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9923 "Parser/parser.cc"
    break;

  case 488:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9929 "Parser/parser.cc"
    break;

  case 489:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9935 "Parser/parser.cc"
    break;

  case 490:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9941 "Parser/parser.cc"
    break;

  case 491:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9947 "Parser/parser.cc"
    break;

  case 492:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 493:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9959 "Parser/parser.cc"
    break;

  case 494:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9965 "Parser/parser.cc"
    break;

  case 495:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9971 "Parser/parser.cc"
    break;

  case 496:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9977 "Parser/parser.cc"
    break;

  case 497:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9983 "Parser/parser.cc"
    break;

  case 498:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9989 "Parser/parser.cc"
    break;

  case 499:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9995 "Parser/parser.cc"
    break;

  case 500:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10001 "Parser/parser.cc"
    break;

  case 501:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10007 "Parser/parser.cc"
    break;

  case 502:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10013 "Parser/parser.cc"
    break;

  case 503:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10019 "Parser/parser.cc"
    break;

  case 504:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10025 "Parser/parser.cc"
    break;

  case 505:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10031 "Parser/parser.cc"
    break;

  case 506:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10037 "Parser/parser.cc"
    break;

  case 507:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10043 "Parser/parser.cc"
    break;

  case 508:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10049 "Parser/parser.cc"
    break;

  case 509:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10055 "Parser/parser.cc"
    break;

  case 510:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10061 "Parser/parser.cc"
    break;

  case 511:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10067 "Parser/parser.cc"
    break;

  case 512:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10073 "Parser/parser.cc"
    break;

  case 513:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10079 "Parser/parser.cc"
    break;

  case 515:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10085 "Parser/parser.cc"
    break;

  case 517:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10091 "Parser/parser.cc"
    break;

  case 518:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10097 "Parser/parser.cc"
    break;

  case 519:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10103 "Parser/parser.cc"
    break;

  case 521:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10109 "Parser/parser.cc"
    break;

  case 522:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10115 "Parser/parser.cc"
    break;

  case 523:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10121 "Parser/parser.cc"
    break;

  case 524:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10127 "Parser/parser.cc"
    break;

  case 526:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10133 "Parser/parser.cc"
    break;

  case 528:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10139 "Parser/parser.cc"
    break;

  case 529:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10145 "Parser/parser.cc"
    break;

  case 530:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10151 "Parser/parser.cc"
    break;

  case 531:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10157 "Parser/parser.cc"
    break;

  case 532:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10163 "Parser/parser.cc"
    break;

  case 533:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10169 "Parser/parser.cc"
    break;

  case 534:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10175 "Parser/parser.cc"
    break;

  case 535:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10181 "Parser/parser.cc"
    break;

  case 536:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10187 "Parser/parser.cc"
    break;

  case 537:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10198 "Parser/parser.cc"
    break;

  case 538:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 539:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 540:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10216 "Parser/parser.cc"
    break;

  case 541:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10227 "Parser/parser.cc"
    break;

  case 542:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10233 "Parser/parser.cc"
    break;

  case 543:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 544:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10248 "Parser/parser.cc"
    break;

  case 546:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10254 "Parser/parser.cc"
    break;

  case 547:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10260 "Parser/parser.cc"
    break;

  case 548:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 550:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 551:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10278 "Parser/parser.cc"
    break;

  case 553:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10284 "Parser/parser.cc"
    break;

  case 554:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 555:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 557:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 558:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 559:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 560:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 561:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 563:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10332 "Parser/parser.cc"
    break;

  case 564:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 565:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10344 "Parser/parser.cc"
    break;

  case 566:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10350 "Parser/parser.cc"
    break;

  case 567:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10356 "Parser/parser.cc"
    break;

  case 568:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10367 "Parser/parser.cc"
    break;

  case 572:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10373 "Parser/parser.cc"
    break;

  case 573:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 574:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10388 "Parser/parser.cc"
    break;

  case 575:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10405 "Parser/parser.cc"
    break;

  case 576:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10414 "Parser/parser.cc"
    break;

  case 577:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10424 "Parser/parser.cc"
    break;

  case 578:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10433 "Parser/parser.cc"
    break;

  case 579:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10443 "Parser/parser.cc"
    break;

  case 581:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10449 "Parser/parser.cc"
    break;

  case 582:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10455 "Parser/parser.cc"
    break;

  case 583:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10465 "Parser/parser.cc"
    break;

  case 584:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10480 "Parser/parser.cc"
    break;

  case 587:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10486 "Parser/parser.cc"
    break;

  case 588:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10492 "Parser/parser.cc"
    break;

  case 589:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10498 "Parser/parser.cc"
    break;

  case 590:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10504 "Parser/parser.cc"
    break;

  case 591:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10510 "Parser/parser.cc"
    break;

  case 592:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10516 "Parser/parser.cc"
    break;

  case 593:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10522 "Parser/parser.cc"
    break;

  case 594:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10528 "Parser/parser.cc"
    break;

  case 595:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10534 "Parser/parser.cc"
    break;

  case 596:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10540 "Parser/parser.cc"
    break;

  case 597:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10546 "Parser/parser.cc"
    break;

  case 598:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10552 "Parser/parser.cc"
    break;

  case 599:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10558 "Parser/parser.cc"
    break;

  case 600:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10571 "Parser/parser.cc"
    break;

  case 601:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10577 "Parser/parser.cc"
    break;

  case 602:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10583 "Parser/parser.cc"
    break;

  case 603:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10596 "Parser/parser.cc"
    break;

  case 604:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10602 "Parser/parser.cc"
    break;

  case 607:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10608 "Parser/parser.cc"
    break;

  case 608:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10614 "Parser/parser.cc"
    break;

  case 611:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10620 "Parser/parser.cc"
    break;

  case 613:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10626 "Parser/parser.cc"
    break;

  case 614:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10632 "Parser/parser.cc"
    break;

  case 615:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10638 "Parser/parser.cc"
    break;

  case 616:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10644 "Parser/parser.cc"
    break;

  case 617:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10650 "Parser/parser.cc"
    break;

  case 619:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 621:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 622:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 624:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10674 "Parser/parser.cc"
    break;

  case 625:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10680 "Parser/parser.cc"
    break;

  case 627:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10686 "Parser/parser.cc"
    break;

  case 628:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 629:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 630:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 631:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 632:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10721 "Parser/parser.cc"
    break;

  case 633:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10729 "Parser/parser.cc"
    break;

  case 634:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10738 "Parser/parser.cc"
    break;

  case 635:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10746 "Parser/parser.cc"
    break;

  case 636:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10754 "Parser/parser.cc"
    break;

  case 637:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10762 "Parser/parser.cc"
    break;

  case 638:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10770 "Parser/parser.cc"
    break;

  case 640:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10776 "Parser/parser.cc"
    break;

  case 641:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10782 "Parser/parser.cc"
    break;

  case 642:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 643:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 644:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 645:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10806 "Parser/parser.cc"
    break;

  case 646:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 647:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 649:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10824 "Parser/parser.cc"
    break;

  case 650:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10830 "Parser/parser.cc"
    break;

  case 651:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 652:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10842 "Parser/parser.cc"
    break;

  case 653:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10848 "Parser/parser.cc"
    break;

  case 654:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10854 "Parser/parser.cc"
    break;

  case 657:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 658:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10866 "Parser/parser.cc"
    break;

  case 659:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10872 "Parser/parser.cc"
    break;

  case 661:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 662:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 663:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 665:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 666:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10902 "Parser/parser.cc"
    break;

  case 667:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10908 "Parser/parser.cc"
    break;

  case 669:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10914 "Parser/parser.cc"
    break;

  case 672:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 673:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 675:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 676:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 677:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 682:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 684:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10956 "Parser/parser.cc"
    break;

  case 685:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10962 "Parser/parser.cc"
    break;

  case 686:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10968 "Parser/parser.cc"
    break;

  case 687:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10974 "Parser/parser.cc"
    break;

  case 688:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 689:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 695:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 698:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10998 "Parser/parser.cc"
    break;

  case 699:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11004 "Parser/parser.cc"
    break;

  case 700:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11010 "Parser/parser.cc"
    break;

  case 701:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11016 "Parser/parser.cc"
    break;

  case 702:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 703:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11028 "Parser/parser.cc"
    break;

  case 704:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11034 "Parser/parser.cc"
    break;

  case 706:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 707:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 708:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11052 "Parser/parser.cc"
    break;

  case 710:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 712:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11064 "Parser/parser.cc"
    break;

  case 713:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 714:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11076 "Parser/parser.cc"
    break;

  case 715:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11082 "Parser/parser.cc"
    break;

  case 716:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 717:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11094 "Parser/parser.cc"
    break;

  case 719:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 720:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11106 "Parser/parser.cc"
    break;

  case 721:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11112 "Parser/parser.cc"
    break;

  case 722:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11123 "Parser/parser.cc"
    break;

  case 723:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11129 "Parser/parser.cc"
    break;

  case 724:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11135 "Parser/parser.cc"
    break;

  case 725:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11141 "Parser/parser.cc"
    break;

  case 726:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11150 "Parser/parser.cc"
    break;

  case 727:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11156 "Parser/parser.cc"
    break;

  case 728:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11162 "Parser/parser.cc"
    break;

  case 729:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11168 "Parser/parser.cc"
    break;

  case 730:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11174 "Parser/parser.cc"
    break;

  case 731:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11180 "Parser/parser.cc"
    break;

  case 732:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11186 "Parser/parser.cc"
    break;

  case 733:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11192 "Parser/parser.cc"
    break;

  case 734:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11198 "Parser/parser.cc"
    break;

  case 735:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11204 "Parser/parser.cc"
    break;

  case 736:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11210 "Parser/parser.cc"
    break;

  case 739:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 740:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 741:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11228 "Parser/parser.cc"
    break;

  case 742:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 744:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11240 "Parser/parser.cc"
    break;

  case 745:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11246 "Parser/parser.cc"
    break;

  case 746:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11252 "Parser/parser.cc"
    break;

  case 747:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 748:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 749:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 750:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11276 "Parser/parser.cc"
    break;

  case 751:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11285 "Parser/parser.cc"
    break;

  case 752:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11294 "Parser/parser.cc"
    break;

  case 753:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax, "" );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11303 "Parser/parser.cc"
    break;

  case 754:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11309 "Parser/parser.cc"
    break;

  case 755:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax, "" );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11318 "Parser/parser.cc"
    break;

  case 756:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 758:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 763:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 764:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 765:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 767:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11354 "Parser/parser.cc"
    break;

  case 768:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11360 "Parser/parser.cc"
    break;

  case 769:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11366 "Parser/parser.cc"
    break;

  case 770:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11372 "Parser/parser.cc"
    break;

  case 772:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11378 "Parser/parser.cc"
    break;

  case 773:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11384 "Parser/parser.cc"
    break;

  case 774:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 776:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11396 "Parser/parser.cc"
    break;

  case 777:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11402 "Parser/parser.cc"
    break;

  case 778:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11408 "Parser/parser.cc"
    break;

  case 779:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11414 "Parser/parser.cc"
    break;

  case 780:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11420 "Parser/parser.cc"
    break;

  case 781:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11426 "Parser/parser.cc"
    break;

  case 783:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11435 "Parser/parser.cc"
    break;

  case 784:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 11441 "Parser/parser.cc"
    break;

  case 785:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11450 "Parser/parser.cc"
    break;

  case 786:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11460 "Parser/parser.cc"
    break;

  case 787:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11469 "Parser/parser.cc"
    break;

  case 788:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11479 "Parser/parser.cc"
    break;

  case 789:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11488 "Parser/parser.cc"
    break;

  case 790:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11498 "Parser/parser.cc"
    break;

  case 791:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11507 "Parser/parser.cc"
    break;

  case 792:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11517 "Parser/parser.cc"
    break;

  case 793:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11526 "Parser/parser.cc"
    break;

  case 794:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11536 "Parser/parser.cc"
    break;

  case 796:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11542 "Parser/parser.cc"
    break;

  case 797:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11548 "Parser/parser.cc"
    break;

  case 798:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11554 "Parser/parser.cc"
    break;

  case 799:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11566 "Parser/parser.cc"
    break;

  case 800:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11577 "Parser/parser.cc"
    break;

  case 801:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11586 "Parser/parser.cc"
    break;

  case 802:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11595 "Parser/parser.cc"
    break;

  case 803:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 804:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11607 "Parser/parser.cc"
    break;

  case 805:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 806:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11622 "Parser/parser.cc"
    break;

  case 807:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 808:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11634 "Parser/parser.cc"
    break;

  case 809:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11640 "Parser/parser.cc"
    break;

  case 813:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 814:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11652 "Parser/parser.cc"
    break;

  case 815:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11662 "Parser/parser.cc"
    break;

  case 816:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11668 "Parser/parser.cc"
    break;

  case 819:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11674 "Parser/parser.cc"
    break;

  case 820:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11680 "Parser/parser.cc"
    break;

  case 822:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11686 "Parser/parser.cc"
    break;

  case 823:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11692 "Parser/parser.cc"
    break;

  case 824:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11698 "Parser/parser.cc"
    break;

  case 825:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11704 "Parser/parser.cc"
    break;

  case 830:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11710 "Parser/parser.cc"
    break;

  case 831:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11716 "Parser/parser.cc"
    break;

  case 832:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11722 "Parser/parser.cc"
    break;

  case 833:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11728 "Parser/parser.cc"
    break;

  case 834:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11734 "Parser/parser.cc"
    break;

  case 836:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11740 "Parser/parser.cc"
    break;

  case 837:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11746 "Parser/parser.cc"
    break;

  case 838:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11752 "Parser/parser.cc"
    break;

  case 839:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11758 "Parser/parser.cc"
    break;

  case 840:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 841:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11770 "Parser/parser.cc"
    break;

  case 842:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 843:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 844:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11788 "Parser/parser.cc"
    break;

  case 845:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11794 "Parser/parser.cc"
    break;

  case 846:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11800 "Parser/parser.cc"
    break;

  case 847:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11806 "Parser/parser.cc"
    break;

  case 848:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11812 "Parser/parser.cc"
    break;

  case 849:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11818 "Parser/parser.cc"
    break;

  case 850:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 851:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11830 "Parser/parser.cc"
    break;

  case 852:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11836 "Parser/parser.cc"
    break;

  case 853:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11842 "Parser/parser.cc"
    break;

  case 855:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 856:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 857:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11860 "Parser/parser.cc"
    break;

  case 858:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11866 "Parser/parser.cc"
    break;

  case 859:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11872 "Parser/parser.cc"
    break;

  case 860:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 861:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 862:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 863:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 864:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11902 "Parser/parser.cc"
    break;

  case 865:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11908 "Parser/parser.cc"
    break;

  case 866:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11914 "Parser/parser.cc"
    break;

  case 867:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11920 "Parser/parser.cc"
    break;

  case 868:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11926 "Parser/parser.cc"
    break;

  case 869:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11932 "Parser/parser.cc"
    break;

  case 870:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11938 "Parser/parser.cc"
    break;

  case 874:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11944 "Parser/parser.cc"
    break;

  case 875:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11950 "Parser/parser.cc"
    break;

  case 876:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11956 "Parser/parser.cc"
    break;

  case 877:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11962 "Parser/parser.cc"
    break;

  case 878:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11968 "Parser/parser.cc"
    break;

  case 879:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11974 "Parser/parser.cc"
    break;

  case 880:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11980 "Parser/parser.cc"
    break;

  case 881:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11986 "Parser/parser.cc"
    break;

  case 882:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11992 "Parser/parser.cc"
    break;

  case 883:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11998 "Parser/parser.cc"
    break;

  case 884:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12004 "Parser/parser.cc"
    break;

  case 885:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12010 "Parser/parser.cc"
    break;

  case 886:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12016 "Parser/parser.cc"
    break;

  case 887:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12022 "Parser/parser.cc"
    break;

  case 888:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12028 "Parser/parser.cc"
    break;

  case 889:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12037 "Parser/parser.cc"
    break;

  case 890:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12043 "Parser/parser.cc"
    break;

  case 891:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12049 "Parser/parser.cc"
    break;

  case 893:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12055 "Parser/parser.cc"
    break;

  case 894:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12061 "Parser/parser.cc"
    break;

  case 895:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12067 "Parser/parser.cc"
    break;

  case 896:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12073 "Parser/parser.cc"
    break;

  case 897:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12079 "Parser/parser.cc"
    break;

  case 898:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 899:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 900:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 901:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12103 "Parser/parser.cc"
    break;

  case 902:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 903:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12115 "Parser/parser.cc"
    break;

  case 904:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12121 "Parser/parser.cc"
    break;

  case 905:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 906:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 907:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12139 "Parser/parser.cc"
    break;

  case 908:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12145 "Parser/parser.cc"
    break;

  case 909:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12151 "Parser/parser.cc"
    break;

  case 910:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 911:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 912:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 914:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 915:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12181 "Parser/parser.cc"
    break;

  case 916:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 917:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 918:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12199 "Parser/parser.cc"
    break;

  case 919:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12205 "Parser/parser.cc"
    break;

  case 920:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 921:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 922:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12223 "Parser/parser.cc"
    break;

  case 923:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12229 "Parser/parser.cc"
    break;

  case 924:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12235 "Parser/parser.cc"
    break;

  case 925:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12241 "Parser/parser.cc"
    break;

  case 926:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12247 "Parser/parser.cc"
    break;

  case 927:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12253 "Parser/parser.cc"
    break;

  case 929:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12259 "Parser/parser.cc"
    break;

  case 930:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12265 "Parser/parser.cc"
    break;

  case 931:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12271 "Parser/parser.cc"
    break;

  case 932:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12277 "Parser/parser.cc"
    break;

  case 933:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12283 "Parser/parser.cc"
    break;

  case 934:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12289 "Parser/parser.cc"
    break;

  case 935:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12295 "Parser/parser.cc"
    break;

  case 936:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12301 "Parser/parser.cc"
    break;

  case 937:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 938:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12313 "Parser/parser.cc"
    break;

  case 939:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12319 "Parser/parser.cc"
    break;

  case 941:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 942:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 943:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 944:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12343 "Parser/parser.cc"
    break;

  case 945:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12349 "Parser/parser.cc"
    break;

  case 946:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12355 "Parser/parser.cc"
    break;

  case 947:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12361 "Parser/parser.cc"
    break;

  case 949:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12367 "Parser/parser.cc"
    break;

  case 950:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 951:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12379 "Parser/parser.cc"
    break;

  case 952:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12385 "Parser/parser.cc"
    break;

  case 953:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 954:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12397 "Parser/parser.cc"
    break;

  case 955:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12403 "Parser/parser.cc"
    break;

  case 956:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 957:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 958:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12421 "Parser/parser.cc"
    break;

  case 960:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12427 "Parser/parser.cc"
    break;

  case 961:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12433 "Parser/parser.cc"
    break;

  case 963:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12439 "Parser/parser.cc"
    break;

  case 964:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12445 "Parser/parser.cc"
    break;

  case 966:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12451 "Parser/parser.cc"
    break;

  case 967:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12457 "Parser/parser.cc"
    break;

  case 968:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 12463 "Parser/parser.cc"
    break;

  case 969:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12469 "Parser/parser.cc"
    break;

  case 970:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 12475 "Parser/parser.cc"
    break;

  case 971:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12481 "Parser/parser.cc"
    break;

  case 972:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12487 "Parser/parser.cc"
    break;

  case 975:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12493 "Parser/parser.cc"
    break;

  case 976:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12499 "Parser/parser.cc"
    break;

  case 977:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 978:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 979:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 980:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12523 "Parser/parser.cc"
    break;

  case 981:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 982:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12535 "Parser/parser.cc"
    break;

  case 984:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12541 "Parser/parser.cc"
    break;

  case 985:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12547 "Parser/parser.cc"
    break;

  case 986:
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12553 "Parser/parser.cc"
    break;

  case 987:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12559 "Parser/parser.cc"
    break;

  case 988:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12565 "Parser/parser.cc"
    break;

  case 989:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12571 "Parser/parser.cc"
    break;

  case 991:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12577 "Parser/parser.cc"
    break;

  case 993:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12583 "Parser/parser.cc"
    break;

  case 994:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12589 "Parser/parser.cc"
    break;

  case 995:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 12595 "Parser/parser.cc"
    break;

  case 996:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12601 "Parser/parser.cc"
    break;

  case 997:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12607 "Parser/parser.cc"
    break;

  case 998:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12613 "Parser/parser.cc"
    break;

  case 1000:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12619 "Parser/parser.cc"
    break;

  case 1001:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12625 "Parser/parser.cc"
    break;

  case 1002:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12631 "Parser/parser.cc"
    break;

  case 1003:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12637 "Parser/parser.cc"
    break;

  case 1004:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12643 "Parser/parser.cc"
    break;

  case 1005:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12649 "Parser/parser.cc"
    break;

  case 1006:
#line 3857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12655 "Parser/parser.cc"
    break;

  case 1008:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12661 "Parser/parser.cc"
    break;

  case 1009:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12667 "Parser/parser.cc"
    break;

  case 1010:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12673 "Parser/parser.cc"
    break;

  case 1011:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 1012:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12685 "Parser/parser.cc"
    break;

  case 1015:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 1018:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 1019:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 1020:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 1021:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 1022:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 1023:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 1024:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 1025:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 1026:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 1027:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 1028:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 1029:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 1030:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 1031:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 1032:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 1033:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 1034:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 1035:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12799 "Parser/parser.cc"
    break;

  case 1036:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12805 "Parser/parser.cc"
    break;

  case 1037:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12811 "Parser/parser.cc"
    break;

  case 1039:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 1043:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 1044:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 1045:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 1046:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 1047:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 1048:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 1049:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 1050:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 1051:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 1052:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 1053:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 1054:
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 1055:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 1056:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12901 "Parser/parser.cc"
    break;

  case 1057:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12907 "Parser/parser.cc"
    break;

  case 1058:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12913 "Parser/parser.cc"
    break;

  case 1059:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12919 "Parser/parser.cc"
    break;

  case 1062:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12925 "Parser/parser.cc"
    break;

  case 1063:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12931 "Parser/parser.cc"
    break;


#line 12935 "Parser/parser.cc"

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
#line 4057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
