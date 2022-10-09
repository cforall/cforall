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

#line 679 "Parser/parser.cc"

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
#define YYLAST   22184

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  176
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1040
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2099

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
       0,   579,   579,   583,   590,   591,   592,   593,   594,   598,
     599,   600,   601,   602,   603,   604,   608,   609,   613,   614,
     619,   623,   624,   635,   637,   639,   643,   644,   646,   648,
     650,   652,   662,   664,   666,   668,   670,   672,   677,   678,
     688,   693,   698,   699,   704,   710,   712,   714,   720,   722,
     724,   726,   728,   730,   732,   734,   736,   738,   740,   742,
     744,   746,   748,   750,   752,   762,   763,   767,   768,   773,
     776,   780,   781,   785,   786,   788,   790,   792,   794,   796,
     801,   803,   805,   813,   814,   822,   825,   826,   828,   833,
     849,   851,   853,   855,   857,   859,   861,   863,   865,   873,
     874,   876,   880,   881,   882,   883,   887,   888,   890,   892,
     894,   896,   898,   900,   902,   909,   910,   911,   912,   916,
     917,   921,   922,   927,   928,   930,   932,   937,   938,   940,
     945,   946,   948,   953,   954,   956,   958,   960,   965,   966,
     968,   973,   974,   979,   980,   985,   986,   991,   992,   997,
     998,  1003,  1004,  1007,  1012,  1017,  1018,  1026,  1032,  1033,
    1037,  1038,  1042,  1043,  1047,  1048,  1049,  1050,  1051,  1052,
    1053,  1054,  1055,  1056,  1057,  1067,  1069,  1074,  1075,  1077,
    1079,  1084,  1085,  1091,  1092,  1098,  1099,  1100,  1101,  1102,
    1103,  1104,  1105,  1106,  1107,  1108,  1110,  1111,  1117,  1119,
    1129,  1131,  1139,  1140,  1145,  1147,  1149,  1151,  1153,  1157,
    1158,  1160,  1165,  1167,  1174,  1176,  1178,  1188,  1190,  1192,
    1197,  1202,  1205,  1210,  1212,  1214,  1216,  1224,  1225,  1227,
    1231,  1233,  1237,  1239,  1240,  1242,  1244,  1249,  1250,  1254,
    1259,  1260,  1264,  1266,  1271,  1273,  1278,  1280,  1282,  1284,
    1289,  1291,  1293,  1295,  1300,  1302,  1307,  1308,  1330,  1332,
    1334,  1337,  1339,  1342,  1344,  1347,  1349,  1354,  1359,  1361,
    1366,  1371,  1373,  1375,  1377,  1379,  1382,  1384,  1387,  1389,
    1394,  1400,  1403,  1405,  1410,  1416,  1418,  1423,  1429,  1432,
    1434,  1437,  1439,  1444,  1451,  1453,  1458,  1464,  1466,  1471,
    1477,  1480,  1485,  1493,  1495,  1497,  1502,  1504,  1509,  1510,
    1512,  1517,  1519,  1524,  1526,  1528,  1530,  1533,  1537,  1540,
    1544,  1546,  1548,  1550,  1552,  1554,  1556,  1558,  1560,  1562,
    1564,  1569,  1570,  1574,  1580,  1585,  1590,  1591,  1595,  1599,
    1604,  1605,  1611,  1615,  1617,  1619,  1621,  1624,  1626,  1631,
    1633,  1638,  1640,  1642,  1647,  1649,  1655,  1656,  1660,  1661,
    1662,  1663,  1667,  1672,  1673,  1675,  1677,  1679,  1683,  1687,
    1688,  1692,  1694,  1696,  1698,  1700,  1706,  1707,  1713,  1714,
    1718,  1719,  1724,  1726,  1732,  1733,  1735,  1740,  1745,  1756,
    1757,  1761,  1762,  1768,  1769,  1773,  1775,  1779,  1781,  1785,
    1786,  1790,  1791,  1795,  1802,  1803,  1807,  1809,  1824,  1825,
    1826,  1827,  1829,  1833,  1835,  1839,  1846,  1848,  1850,  1855,
    1856,  1858,  1860,  1862,  1894,  1897,  1902,  1904,  1910,  1915,
    1920,  1931,  1936,  1941,  1946,  1951,  1960,  1964,  1971,  1973,
    1974,  1975,  1981,  1983,  1988,  1989,  1990,  1999,  2000,  2001,
    2005,  2006,  2013,  2022,  2023,  2024,  2029,  2030,  2039,  2040,
    2045,  2046,  2050,  2052,  2054,  2056,  2058,  2062,  2067,  2068,
    2070,  2080,  2081,  2086,  2088,  2090,  2092,  2094,  2096,  2099,
    2101,  2103,  2108,  2110,  2112,  2114,  2116,  2118,  2120,  2122,
    2124,  2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,  2142,
    2144,  2146,  2148,  2150,  2152,  2154,  2156,  2158,  2160,  2162,
    2167,  2168,  2172,  2179,  2180,  2186,  2187,  2189,  2191,  2193,
    2198,  2200,  2205,  2206,  2208,  2210,  2215,  2217,  2219,  2221,
    2223,  2225,  2230,  2237,  2239,  2241,  2246,  2254,  2253,  2257,
    2265,  2266,  2268,  2270,  2275,  2276,  2278,  2283,  2284,  2286,
    2288,  2293,  2294,  2296,  2301,  2303,  2305,  2307,  2308,  2310,
    2315,  2317,  2319,  2324,  2331,  2335,  2336,  2341,  2340,  2345,
    2344,  2363,  2362,  2374,  2373,  2384,  2389,  2390,  2395,  2401,
    2415,  2416,  2420,  2422,  2424,  2430,  2432,  2434,  2436,  2438,
    2440,  2442,  2444,  2450,  2451,  2456,  2465,  2467,  2476,  2478,
    2479,  2480,  2482,  2484,  2485,  2490,  2491,  2492,  2497,  2499,
    2502,  2509,  2510,  2511,  2517,  2522,  2524,  2530,  2531,  2537,
    2538,  2542,  2547,  2550,  2549,  2553,  2556,  2563,  2568,  2567,
    2576,  2581,  2585,  2589,  2593,  2595,  2600,  2602,  2604,  2606,
    2612,  2613,  2614,  2621,  2622,  2624,  2625,  2626,  2628,  2630,
    2637,  2638,  2640,  2642,  2647,  2648,  2654,  2655,  2657,  2658,
    2663,  2664,  2665,  2667,  2675,  2676,  2678,  2681,  2683,  2687,
    2688,  2689,  2691,  2693,  2698,  2700,  2705,  2707,  2716,  2718,
    2723,  2724,  2725,  2729,  2730,  2731,  2736,  2737,  2742,  2743,
    2744,  2745,  2749,  2750,  2755,  2756,  2757,  2758,  2759,  2773,
    2774,  2779,  2780,  2786,  2788,  2791,  2793,  2795,  2818,  2819,
    2825,  2826,  2832,  2831,  2841,  2840,  2844,  2850,  2856,  2857,
    2859,  2863,  2868,  2870,  2872,  2874,  2880,  2881,  2885,  2886,
    2891,  2893,  2900,  2902,  2903,  2905,  2910,  2912,  2914,  2919,
    2921,  2926,  2931,  2939,  2941,  2946,  2947,  2952,  2953,  2957,
    2958,  2959,  2964,  2966,  2972,  2974,  2979,  2981,  2987,  2988,
    2992,  2996,  3000,  3002,  3003,  3005,  3007,  3009,  3011,  3013,
    3015,  3016,  3021,  3024,  3023,  3035,  3034,  3047,  3046,  3058,
    3057,  3069,  3068,  3082,  3088,  3090,  3096,  3097,  3108,  3115,
    3120,  3126,  3129,  3132,  3136,  3142,  3145,  3148,  3153,  3154,
    3155,  3159,  3165,  3166,  3176,  3177,  3181,  3182,  3187,  3192,
    3193,  3199,  3200,  3202,  3207,  3208,  3209,  3210,  3211,  3213,
    3248,  3250,  3255,  3257,  3258,  3260,  3265,  3267,  3269,  3271,
    3276,  3278,  3280,  3282,  3284,  3286,  3288,  3293,  3295,  3297,
    3299,  3308,  3310,  3311,  3316,  3318,  3320,  3322,  3324,  3329,
    3331,  3333,  3335,  3340,  3342,  3344,  3346,  3348,  3350,  3362,
    3363,  3364,  3368,  3370,  3372,  3374,  3376,  3381,  3383,  3385,
    3387,  3392,  3394,  3396,  3398,  3400,  3402,  3417,  3422,  3427,
    3429,  3430,  3432,  3437,  3439,  3441,  3443,  3448,  3450,  3452,
    3454,  3456,  3458,  3460,  3465,  3467,  3469,  3471,  3473,  3483,
    3485,  3487,  3488,  3490,  3495,  3497,  3499,  3504,  3506,  3508,
    3510,  3515,  3517,  3519,  3533,  3535,  3537,  3538,  3540,  3545,
    3547,  3552,  3554,  3556,  3561,  3563,  3568,  3570,  3587,  3588,
    3590,  3595,  3597,  3599,  3601,  3603,  3608,  3609,  3611,  3613,
    3618,  3620,  3622,  3628,  3630,  3632,  3635,  3639,  3641,  3643,
    3645,  3679,  3680,  3682,  3684,  3689,  3691,  3693,  3695,  3697,
    3702,  3703,  3705,  3707,  3712,  3714,  3716,  3722,  3723,  3725,
    3734,  3737,  3739,  3742,  3744,  3746,  3760,  3761,  3763,  3768,
    3770,  3772,  3774,  3776,  3781,  3782,  3784,  3786,  3791,  3793,
    3801,  3802,  3803,  3808,  3809,  3814,  3816,  3818,  3820,  3822,
    3824,  3831,  3833,  3835,  3837,  3839,  3842,  3844,  3846,  3848,
    3850,  3855,  3857,  3859,  3864,  3890,  3891,  3893,  3897,  3898,
    3902,  3904,  3906,  3908,  3910,  3912,  3919,  3921,  3923,  3925,
    3927,  3929,  3934,  3936,  3938,  3945,  3947,  3965,  3967,  3972,
    3973
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

#define YYPACT_NINF (-1693)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-921)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     168, 12314,   229,   246, 17103,   150, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,   161,   789,
     225, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,    61,   375,
   -1693, -1693, -1693, -1693, -1693, -1693,  4610,  4610,   248, 12314,
     254,   284, 21967, -1693,   297, -1693, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693,  2720, -1693,   383,   321, -1693, -1693,
   -1693, -1693, -1693, 16953, -1693, -1693,   326,   373,   322,   327,
   -1693,  4610,   373,   373,   373,   370,  4063,   557,   781, 12474,
   -1693, -1693, -1693, 16803,   976, -1693, -1693, -1693,  2741,   591,
   12526,   853,   725,  2741,   823,   420, -1693, -1693, -1693, -1693,
     581, -1693, -1693, -1693, -1693,   472, -1693, -1693, -1693, -1693,
   -1693,   512,   555,   581, -1693,   581,   597, -1693, -1693, -1693,
   17957,  4610, -1693, -1693,  4610, -1693, 12314, -1693,   595, 18009,
   -1693, -1693,  4241, 18960, -1693,  1124,  1124,   623,  2566, -1693,
   -1693, -1693, -1693,   476, 14412,  3365,   581, -1693, -1693, -1693,
   -1693, -1693, -1693,   648, -1693,   662,   669,   698, -1693,   737,
   21372, -1693, -1693, -1693, -1693, -1693, -1693, -1693, 15735,  2487,
    2720,   217,   723,   736,   744,   746,   765,   778, -1693, -1693,
   18159, 11337,   799, -1693, 17544, -1693, -1693, -1693, -1693,   802,
   -1693, -1693,   721, -1693,  9616,   910, 19644, -1693,   843,  4610,
     555,   857,   783,   812,   846, -1693, -1693, -1693,  2924,  2552,
     874,   845,   295, -1693, -1693,   581,   581,    62,    81,   349,
      62, -1693,   581,   581, -1693,  3793, -1693, -1693,   886,   893,
    1124, 19263, -1693, 16953, -1693, -1693,  2741, -1693,  2515,   420,
     815,   958,    81,  4610,   322, -1693, 13935, -1693,  1124,  1124,
     898,   958,    81,  4610, -1693, 15257, -1693, -1693,  1124, -1693,
    1124, -1693,   647,  4388,  4610, -1693,  1493,   902, -1693, -1693,
   -1693, 16618,   555,   143, -1693, -1693, 19010, -1693,   845,   101,
   -1693, 21372, 18960,  3290,  3793, -1693,   381, -1693, -1693, -1693,
   18009,  4610, -1693,   929, -1693, -1693, -1693, -1693,  4610,  2078,
     380,   405, -1693,  4610,   662, -1693,   703,   581,   581,   934,
   18211,   797, 14889, 19315,  2741,  2741, -1693,  2741,  1124,  2741,
    1124, -1693, -1693,   581, -1693,   932, -1693, 18361, -1693, -1693,
   -1693, 18413,   802, -1693,   936,    15,   991,   947,   420,   950,
   -1693,  2566,   942,   662,  2566,  1481, -1693,   974,  1029, 21444,
     960,   997, 21372, 21516,  1021, 22019, -1693, -1693, -1693, -1693,
   -1693, -1693, 21588, 21588, 15581,  1017,  4448, -1693, -1693, -1693,
   -1693,   110, -1693,   452, -1693,  1459, -1693, 21372, 21372, -1693,
     983,   590,   727,   791,   496,   833,  1006,  1027,  1034,  1068,
      17, -1693,   722, -1693,  1070, -1693,   835,  4609, 16043, -1693,
   -1693,   750,  1070, -1693, -1693,   770, -1693, -1693,  2487,  1057,
    1074,  1077,  1079,  1086,  1091, -1693, -1693,   404,  1104, -1693,
     779,  1104, -1693, -1693, 17957, -1693,   926,  1106, 16197, -1693,
   -1693,  4374,  2624,  1145, 14889,  1163,   554,   749, -1693, -1693,
   -1693, -1693, -1693,  4610,  4586, -1693, -1693, -1693, -1693, -1693,
   -1693, 16514,  3770,  1017,  9616,  1170,  1184, -1693, -1693,  1169,
   19644,   670, -1693, -1693, -1693, 19716,  1193, -1693, -1693, -1693,
   -1693, -1693,  2924,   637,  1197,  1201,  1203,   732,  1206,  1222,
    1224,  2552, -1693, -1693,   581,  1214,   322,  1223, -1693, -1693,
    1226, -1693, -1693,   555,   958, -1693, -1693, -1693,   555, -1693,
   -1693,  3793, -1693, 16043, 16043, -1693,  1124,  4241, 19409, 15048,
   -1693, -1693, -1693, -1693, -1693,   555,   958,   101, -1693, -1693,
    2741,  1236,   958,    81, -1693,   555,   958, -1693, 22113, -1693,
    1124,  1124, -1693, -1693,  1238,   148,  1240,   420,  1242, -1693,
   17262, -1693,   792, -1693,  1320, 19160, -1693,  4241, 16705, 19263,
   -1693, 16618, 21660, -1693, -1693, -1693, -1693, -1693,  3290,   879,
    3793, -1693, 15048,   845, 12314, -1693,  1249, -1693,  1256, -1693,
   -1693, -1693, -1693, -1693,  2566, -1693, -1693,  1334,  4571,  3084,
   18413, 11337, -1693, 18563, -1693,  1124,  1124, -1693, -1693,   802,
   -1693,   862,  1258,  1419, 21372,  1667,  1226,  1266, -1693,   581,
     581, -1693,  1104, -1693, 18211, -1693, -1693, 17703,  1124,  1124,
   -1693,  4571,   581, -1693, 18817, -1693, -1693, 18361, -1693,   476,
    1296,  1282,  1312,   991,   801, 18009,   818, -1693, -1693, -1693,
   -1693, -1693, -1693,   852, -1693,  1323,  1302, -1693, 15889, -1693,
   18615, 18615, -1693, 15889, -1693, 21372, -1693, -1693, -1693, -1693,
   -1693, -1693, 15889, -1693, -1693, 17755, 18615, 18615,   835,  1239,
    1498,   654,  1650, -1693,   861,  1330,   817,  1338, -1693, 19716,
   21372, 19788,  1322, 21372,  1493, 21372,  1493, -1693,  3222, -1693,
   -1693, 19860,  2700, 21372, 19860,  1493, -1693, -1693, 21372, 21372,
   21372, 21372, 21372, 21372, 21372, 21372, 21372, 21372, 21372, 21372,
   21372, 21372, 21372, 21372, 21372, 21372, 21372, 19932,  1321,   737,
    4128, 11337, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693,  1340, 21372, -1693, -1693,   750,  2154, -1693,
   -1693,   581,   581, -1693, -1693, 16043, -1693,   436,  1104, -1693,
     961,  1104, -1693, -1693, -1693,  1226, -1693, -1693,  1226, 21732,
   -1693, -1693, 11337,  1367,  1368,  3198,  1507,  2767,   437,  1266,
   -1693,   581,   581,  1266,   445, -1693,   581,   581, 21372,  4610,
     973,  1065,  1266,   252, 14253, 14253,  4610, -1693, -1693, 21372,
    1169, -1693,  9616,  1378, -1693,   955, -1693, -1693, -1693, -1693,
   -1693,   885, -1693, 14253,  1493,  4241,  1493,   992,  1379,  1382,
    1385,   994,  1386,  1387,  1388,   451,  1104, -1693, -1693,   466,
    1104, -1693, -1693, -1693,  4241,   737, -1693,  1104, 21732, -1693,
     555, 17262, -1693, -1693,  1000,  1390,  1009,  1392, -1693,  1396,
   -1693,   555, -1693, -1693,   555,   958,  1396, -1693,   555,  1397,
    1400,  1402, -1693, -1693, 17703, -1693,  1394, -1693, -1693, -1693,
    1493,  4610, 10491,  1488,  1391,  9596, -1693,  1106, -1693, 14253,
    1013, -1693, -1693,  1396, -1693, 18009, 16043,  1389, -1693,  1389,
   -1693, -1693, -1693,   991,   581,   581, -1693, 18361, -1693, 11500,
   16351, -1693, 17262,  1409,  1413,  1415, -1693,  7323,   581, -1693,
    1667, -1693, -1693, -1693, -1693,  1226, -1693, -1693, -1693,  1124,
   -1693,  3547, -1693, -1693,   420,  1012,  1425, 20004, -1693,   991,
    1296, -1693, -1693,  1417,  1424,  1481, 19860, -1693,  1427,   321,
    1428,  1444,  1445,  1442,  1456, 21372,  1458,  1463,  1464, 11337,
   21372, -1693, -1693,  1680, -1693, -1693, -1693, 21372, -1693,  1470,
    1471, 19572,  1094, -1693, 19860,  1469, -1693,  1472, -1693, -1693,
    3055, -1693, -1693,  1010, -1693, -1693, -1693, -1693,  3055, -1693,
   -1693,  1162,   368, -1693, -1693,   983,   983,   983,   590,   590,
     727,   727,   791,   791,   791,   791,   496,   496,   833,  1006,
    1027,  1034,  1068, 21372,  1179, -1693,  1473,  3055, -1693, -1693,
    9616, -1693, 17262,  1476,  1477,  1478,  2154, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693,  1226, -1693, -1693,  1226, 17262,
   17262, -1693, -1693,  3198,   938,  1480,  1483,  1485,  1486,  2385,
    2767, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693,  1487, -1693,  1266, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693,  1494,  1495, -1693,   322,
    3055,  1196,    26, -1693, -1693,  1462, -1693, 19644, -1693, 21372,
     581, 20076, 14253, -1693, -1693, -1693,  1466,   491,  1104, -1693,
     494,  1104, -1693, -1693, -1693, -1693,  1226, -1693, -1693, -1693,
    1226,   845,  1499,  1226, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693,  1496, -1693, -1693,  1396, -1693,   555, -1693, -1693, -1693,
   -1693, -1693, 13107,  1500,  1497, -1693,   397, -1693,   147,    58,
   11174,  1506, 15418,  1509,  1510,  1191,  2353,  2239, 20148,  1511,
   -1693, -1693,  1519,  1521, -1693, -1693,   555, 21372, 21372,  1636,
    1515,   535, -1693,  1600,  1518,  1501, -1693, -1693, -1693, 10318,
   -1693, -1693, -1693, -1693, -1693,  1316, -1693, -1693, -1693,  1587,
   -1693, -1693, -1693,  1493, -1693, -1693, 12954, 16953,  1522, -1693,
    4610, -1693,  1512,  1530,  1544, -1693,  1212, -1693, -1693, -1693,
   -1693,  4241, -1693, -1693,  1526,  1529,  1018, 18009,   662,   662,
    1296,  1548,  1549, -1693, -1693,  1017,  1106, 16197, -1693,  1070,
   -1693, 11663, -1693,   504,  1104, -1693,  1124,  9301, -1693, -1693,
     991,   581,   581,   476,  1282, -1693,  9616, -1693,  1296,  1560,
    1562, -1693, -1693,  1026,   579, 11337,  1493, -1693,   579, 17807,
     579, -1693, 21372, 21372, 21372, -1693, -1693, -1693, -1693, 21372,
   21372,  1554,  9616, -1693, -1693,  1557,   619, -1693, -1693, -1693,
    3608, -1693, -1693,  1217, -1693,    34, -1693, 19860,  1228, -1693,
   19716, -1693, -1693, 21372,  1540,  1231,  1247,  1169, -1693,   509,
    1104, -1693, -1693, 17262, 17262, -1693, -1693,  1566,   533,  1104,
   -1693,   546,  2825,   581,   581, -1693, -1693, 17262, 17262, -1693,
    1567, -1693, 15048, 15048,  1572,  1569,  1570,  1576, -1693,  1573,
   21372, 21372,  1254,  1577, -1693, -1693, -1693, -1693, -1693, -1693,
   -1693,  1579, 21372, -1693, -1693, -1693,  1226, -1693, -1693, -1693,
    1226, 17262, 17262,   322,   581,  1275,  1585,  1590, -1693, -1693,
    1591, 13260, 13413, 13566, 18009, 18615, 18615,  1592, -1693,  1568,
    1571,  2163, 13776, -1693,   403,  4610, -1693, -1693,  4610, -1693,
    7516,    25,   120, -1693, -1693, -1693, -1693, 21372,  1595,  1662,
   11010, 10664, -1693,  1575, -1693,  1578, 21372,  1580,  9616,  1581,
   21372, 19716, 21372,   887, -1693,  1583,    39, -1693,   198,  1597,
   -1693, -1693,  1599, -1693,  1588, -1693,  1593,  1601, 15418,   518,
   14094,   581,   482, -1693, -1693, -1693,  1602, -1693,  1611, -1693,
    1616, -1693,  1610, -1693,  1614, -1693, -1693, -1693, -1693,  1618,
     991,   991, 11826,  1619,  1620,  1622, -1693,  1621, -1693, -1693,
   -1693,  1226, 21372, 21372,  1106,  1623, -1693,  1296, -1693,  1624,
      27, -1693,  1169,  1634, -1693, -1693, 18009, -1693,  1639,  1629,
    1036, -1693,  1631, -1693, -1693, -1693, -1693, -1693,  9616,  1169,
   19716, -1693,  1673,  3055, -1693,  1673,  1673, -1693,  3055,  3849,
    4302, -1693, -1693,  1280, -1693, -1693, -1693,  1644,  1643, -1693,
   -1693, -1693,  1226, -1693, -1693,  1646,  1649,   581, -1693, -1693,
   -1693,  1226, -1693, -1693, -1693,  1651, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693, -1693,  1654,
   -1693, -1693, -1693, -1693,  1656,  1653,   581, -1693, 17262, 17262,
   -1693, -1693, -1693, -1693, 21372, -1693, -1693,  1657, -1693,  1592,
    1592,  1592,   864,  1641,   493, -1693,  3521,   519, 16043, -1693,
   -1693, -1693,  3874, 21372,  3903,   524, -1693, -1693,    45,  1661,
    1661,  4610, -1693, -1693, 17412, -1693, 21372,  1669,  1671, -1693,
   -1693, -1693, -1693,  1044,  1682, 15418,  1518,  1678, 21372,   326,
    1677,   370, 13726, 18009, -1693, -1693, -1693,   666, 15418, 21372,
    1058,   513, -1693, 21372, 19420, -1693, -1693,   528, -1693,  1169,
   -1693,  1055,  1056,  1080, -1693, -1693, -1693, -1693,   555,   887,
    1684, -1693, -1693, 21372, -1693,  1685,   737, 11174, -1693, -1693,
   -1693, -1693, 21372,  1730, -1693, 10145, -1693,   581, 15048, -1693,
   -1693, 18009, -1693, -1693, -1693,  1296,  1296, -1693, -1693, -1693,
    1687, -1693, 17262, -1693, -1693,  1690, -1693,  1691,  1704,  1681,
     991, -1693,  1706, -1693, -1693, -1693, 21372, -1693, 17807, 21372,
    1169,  1708,  1288, -1693,  1292, -1693,  3055, -1693,  3055, -1693,
   -1693, -1693, -1693, 17262,  1688,  1709, -1693, -1693, 17262, 17262,
    1711,  1712,  1295, 14571, 14730, -1693,  1703, -1693, -1693, -1693,
   -1693,  1713,  1715,  1299, -1693, -1693, -1693, -1693,   864,  2264,
     549, -1693, -1693, -1693, -1693,   581,   581, -1693, -1693, -1693,
     560, -1693,  1108,  3874,   582, -1693,  3903,   581, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693,   569, 15418,   177, 20220,
    1786, 15418,  1518, 15207, -1693, -1693, -1693, -1693, 21372, -1693,
   20292,  1795,  1696, 19496, 20364, 15418, 10837,  1518,   566,  1071,
    1697, 21372, -1693,  1724,   263, 15418, -1693, -1693,  1725, -1693,
   -1693,  1699,   737,   625,  1726,  1733,  1304,  1793, -1693, -1693,
   -1693, -1693,  4610,  4241,  1737,  1738, -1693, -1693,  1734,  1735,
   -1693, -1693, -1693,   991,  1296, -1693,  1739, -1693, -1693, -1693,
    1744, -1693, -1693, -1693,  1308,  1310, -1693, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693, -1693,  1743, -1693, -1693,  1749,
    1750, -1693, -1693, -1693,  1752,  1755,  1757,  2264, -1693,   581,
   -1693, -1693, -1693, -1693, -1693,  1742,  3521, -1693, -1693,  7068,
     137, 11992, -1693, 15300, -1693,    18,  1127, 15418,  1839,   570,
    1748,   298, 15418, 21372,  1762,   566,  1071,  1745, 21804,  1751,
     470,  1844, -1693, 20436, 20508, 21372,  1518,  1753, 12154, -1693,
   -1693, -1693, 18765, -1693,  1773,  1756,   276, 15418, -1693, 21372,
   19860,   412, -1693, -1693, -1693, -1693, -1693,  1781, -1693, -1693,
    1296,  1785, -1693, -1693, -1693, -1693,  1784,  1787,  1789, 15048,
    1788, -1693, -1693,   548,  1104, -1693, -1693,   864, -1693, -1693,
     123, -1693,   119, -1693, -1693, -1693,  1797, 12634, -1693, -1693,
   15418, -1693,    68, -1693, 15418, 21372,  1796, 20580, -1693, -1693,
   20652, 20724, 21372,  1762,  1518, 20796, 20868, 15418,  1782,   471,
    1790,   522, -1693, -1693,  1801, 12634, 18765, -1693,  3995, 18563,
    1493,  1794, -1693,  1850,  1804,   626,  1803, -1693,  1883, -1693,
    1153, 15418,  1816, 15418, 15418, -1693,  1820, -1693, -1693, -1693,
   -1693, -1693, -1693, -1693, -1693,  1226, -1693, 21372, -1693, 21372,
   -1693, -1693,  1399, 12794, -1693, -1693, 15418, -1693, -1693,  1518,
   -1693, -1693,  1518,  1808,   565,  1811,   594, -1693, -1693,  1518,
   -1693,  1518, -1693,  1819, 20940, 21012, 21084, -1693,  1399, -1693,
    1805,  2843,  3128, -1693, -1693, -1693,   276,  1823, 21372,  1810,
     276,   276, 15418, -1693, -1693, 21372,  1871,  1876, -1693, 17262,
   -1693, -1693, 15300, -1693,  1399, -1693, -1693,  1835, 21156, 21228,
   21300, -1693, -1693,  1518, -1693,  1518, -1693,  1518, -1693,  1805,
   21372,  1837,  3128,  1836,   737,  1840, -1693,   645, -1693, -1693,
    1159,  1793,    97, -1693, -1693,  9838,  1845, 15300, -1693, -1693,
    1518, -1693,  1518, -1693,  1518,  1846,  1851, -1693,   555,   737,
    1852, -1693,  1829,   737, -1693, -1693, 15418,  1931,  1855, -1693,
   -1693, -1693, 10015, -1693,   555, -1693, -1693,  1328, 21372, -1693,
    1167, -1693, 15418, -1693, -1693,   737,  1493,  1856,  1841, -1693,
   -1693, -1693,  1168, -1693, -1693,  1842,  1493, -1693, -1693
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   456,     0,     2,   456,   473,   474,   475,   476,   477,
     478,   479,   480,   481,   462,   464,   463,   465,     0,     0,
       0,   482,   484,   505,   485,   506,   488,   489,   503,   504,
     483,   501,   502,   486,   487,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   507,   508,   804,   510,
     583,   584,   587,   589,   585,   591,     0,     0,     0,   456,
       0,     0,    16,   554,   560,     9,    10,    11,    12,    13,
      14,    15,   762,   101,     0,    19,     0,     2,    99,   100,
      17,    18,   820,   456,   763,   405,     0,   408,   688,   410,
     419,     0,   409,   439,   440,     0,     0,     0,     0,   537,
     458,   460,   466,   456,   468,   471,   522,   509,   444,   515,
     520,   445,   532,   446,   547,   551,   557,   536,   563,   575,
     804,   580,   581,   564,   633,   411,   412,     3,   770,   783,
     461,     0,     0,   804,   842,   804,     2,   859,   860,   861,
     456,     0,  1018,  1019,     0,     1,   456,    16,     0,   456,
     428,   429,     0,   537,   450,   451,   452,   773,     0,   586,
     588,   590,   592,     0,   456,     0,   805,   806,   582,   511,
     681,   682,   680,   741,   736,   726,     0,     0,   771,     0,
       0,   473,   764,   768,   769,   765,   766,   767,   456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   555,   558,
     456,   456,     0,  1020,   537,   849,   867,  1024,  1017,  1015,
    1022,   404,     0,   163,   694,   162,     0,   413,     0,     0,
       0,     0,     0,     0,     0,   403,   919,   920,     0,     0,
     438,   802,   804,   798,   823,   804,   804,   800,     2,   804,
     799,   880,   804,   804,   877,     0,   530,   531,     0,     0,
     456,   456,     2,   456,   420,   459,   469,   523,     0,   552,
       0,   786,     2,     0,   688,   421,   537,   516,   533,   548,
       0,   786,     2,     0,   472,   517,   524,   525,   534,   539,
     549,   553,     0,   567,     0,   756,     2,     2,   784,   841,
     843,   456,     0,     2,     2,  1028,   537,  1031,   802,   802,
       3,     0,   537,     0,     0,   431,   804,   800,   799,     2,
     456,     0,   760,     0,   722,   724,   723,   725,     0,     0,
     718,     0,   708,     0,   717,   728,     0,   804,   804,     2,
     456,  1039,   457,   456,   468,   447,   515,   448,   540,   449,
     547,   544,   565,   804,   566,     0,   669,   456,   670,   993,
     994,   456,   671,   673,   554,   560,     0,   634,   635,     0,
     807,     0,   739,   727,     0,   811,    21,     0,    20,     0,
       0,     0,     0,     0,     0,    23,    25,     4,     8,     5,
       6,     7,     0,     0,   456,     2,     0,   102,   103,   104,
     105,    86,    24,    87,    42,    85,   106,     0,     0,   121,
     123,   127,   130,   133,   138,   141,   143,   145,   147,   149,
     151,   154,     0,    26,     0,   561,     2,   106,   456,   155,
     733,   684,   551,   686,   732,     0,   683,   687,     0,     0,
       0,     0,     0,     0,     0,   821,   847,   804,   857,   865,
     869,   875,     2,  1026,   456,  1029,     2,    99,   456,     3,
     668,     0,  1039,     0,   457,   515,   540,   547,     3,     3,
     650,   654,   664,   670,   671,     2,   850,   868,  1016,     2,
       2,    23,     0,     2,   694,    24,     0,   692,   695,  1037,
       0,     0,   701,   690,   689,     0,     0,   788,     2,     2,
       2,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   826,   883,   804,     0,   688,     2,   822,   830,
     946,   824,   825,     0,   786,     2,   879,   887,     0,   881,
     882,     0,   434,   456,   456,   521,   457,     0,   537,   456,
    1021,  1025,  1023,   538,   760,     0,   786,   802,   414,   422,
     470,     0,   786,     2,   760,     0,   786,   737,   518,   519,
     535,   550,   556,   559,   554,   560,   578,   579,     0,   738,
     456,   678,     0,   200,   397,   456,     3,     0,   537,   456,
     785,   456,     0,   416,     2,   417,   757,   436,     0,     0,
       0,     2,   456,   802,   456,   760,     0,     2,     0,   721,
     720,   719,   714,   467,     0,   712,   729,   513,     0,     0,
     456,   456,   995,   457,   453,   454,   455,   999,   990,   991,
     997,     2,     2,   100,     0,   955,   969,  1039,   951,   804,
     804,   960,   967,   676,   456,   545,   672,   457,   541,   542,
     546,     0,   804,  1005,   457,  1010,  1002,   456,  1007,     0,
    1037,   640,     0,     0,     0,   456,     0,   819,   818,   814,
     816,   817,   815,     0,   809,   812,     0,    22,   456,    93,
     456,   456,    88,   456,    95,     0,    32,    36,    37,    33,
      34,    35,   456,    91,    92,   456,   456,   456,     2,   102,
     103,     0,     0,   181,     0,     0,   581,     0,  1015,     0,
       0,     0,     0,     0,     0,     0,     0,    54,     0,    60,
      61,    65,     0,     0,    65,     0,    89,    90,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   456,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   174,   162,     0,   160,   161,     2,   931,   685,
     928,   804,   804,   936,   562,   456,   848,   804,   858,   866,
     870,   876,     2,   851,   853,   855,     2,   871,   873,     0,
    1027,  1030,   456,     0,     0,     2,   100,   955,   804,  1039,
     901,   804,   804,  1039,   804,   916,   804,   804,     3,   672,
       0,     0,  1039,  1039,   456,   456,     0,     2,   703,     0,
    1037,   700,  1038,     0,   696,     0,     2,   699,   702,   178,
     177,     0,     2,   456,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   804,   835,   839,   878,   804,
     892,   897,   827,   884,     0,     0,   442,   943,     0,   789,
       0,   456,   790,   435,     0,     0,     0,     0,   433,     2,
     791,     0,   418,   760,     0,   786,     2,   792,     0,     0,
       0,     0,   593,   657,   457,     3,     3,   661,   660,   862,
       0,     0,   456,   398,     0,   537,     3,    99,     3,   456,
       0,     3,   761,     2,   716,   456,   456,   710,   709,   710,
     514,   512,   634,     0,   804,   804,  1001,   456,  1006,   457,
     456,   992,   456,     0,     0,     0,   970,     0,   804,  1040,
     956,   957,   677,   953,   954,   968,   996,  1000,   998,   543,
     578,     0,  1004,  1009,   637,  1038,     0,     0,   636,     0,
    1037,   742,   740,     0,     0,   811,    65,   772,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   456,
       0,   120,   119,     0,   116,   115,    27,     0,    28,     0,
       0,     0,     0,     3,    65,     0,    50,     0,    51,    58,
       0,    57,    69,     0,    66,    67,    70,    53,     0,    52,
      56,     0,     0,    49,   122,   124,   125,   126,   128,   129,
     131,   132,   136,   137,   134,   135,   139,   140,   142,   144,
     146,   148,   150,     0,     0,   407,     0,     0,    29,     3,
     694,   156,   456,     0,     0,     0,   932,   933,   929,   930,
     735,   734,     2,   852,   854,   856,     2,   872,   874,   456,
     456,   948,   947,     2,     0,     0,     0,     0,     0,   804,
     956,   904,   921,     2,   899,   907,   674,   902,   903,   675,
       2,   914,   924,   917,   918,     0,     3,  1039,   426,     2,
    1032,     2,   665,   666,   644,     3,     3,     3,     3,   688,
       0,   154,     0,     3,     3,     0,   697,     0,   691,     0,
     804,     0,   456,     3,   430,   432,     0,   804,   836,   840,
     804,   893,   898,     2,   828,   831,   833,     2,   885,   888,
     890,   802,     0,   944,     3,   794,     3,   527,   526,   529,
     528,     2,   761,   795,     2,   793,     0,   761,   796,   593,
     593,   593,   456,     0,     0,   679,     0,   401,     0,     0,
     456,     0,     2,     0,     0,     0,     0,     0,   183,     0,
     331,   332,     0,     0,   370,   369,     0,   158,   158,   376,
     554,   560,   197,     0,   184,     0,   208,   185,   186,   456,
     202,   187,   188,   189,   190,     0,   191,   192,   337,     0,
     193,   194,   195,     0,   196,   204,   537,   456,     0,   206,
       0,   395,     0,     0,     0,     3,     0,   774,   761,   749,
     750,     0,     3,   745,     3,     3,     0,   456,   726,   726,
    1037,     0,     0,  1003,  1008,     2,    99,   456,     3,   552,
       3,   457,     3,   804,   963,   966,   456,     3,   952,   958,
       0,   804,   804,     0,   640,   622,   694,   641,  1037,     0,
       2,   808,   810,     0,    94,   456,     0,    98,    96,   456,
       0,   110,     0,     0,     0,   114,   118,   117,   182,     0,
       0,     0,   694,   107,   175,     0,     0,    45,    46,    83,
       0,    83,    83,     0,    71,    73,    48,     0,     0,    44,
       0,    47,   153,     0,     0,     0,     0,  1037,     3,   804,
     939,   942,   934,   456,   456,     3,     3,     0,   804,   910,
     913,   804,     0,   804,   804,   905,   922,   456,   456,  1033,
       0,   667,   456,   456,     0,     0,     0,     0,   415,     3,
       0,     0,     0,     0,   693,   698,     3,   787,   180,   179,
       3,     0,     0,     2,   829,   832,   834,     2,   886,   889,
     891,   456,   456,   688,   804,     0,     0,     0,   761,   797,
       0,   456,   456,   456,   456,   456,   456,   576,   604,     3,
       3,   605,   537,   594,     0,     0,   844,     2,     0,   399,
      65,     0,     0,   322,   323,   205,   207,     0,     0,     0,
     456,   456,   318,     0,   316,     0,     0,     0,   694,     0,
       0,     0,     0,     0,   159,     0,     0,   377,     0,     0,
       3,   212,     0,   203,     0,   313,     0,     0,     2,     0,
     537,   804,     0,   396,   950,   949,     0,     2,     0,   752,
       2,   747,     0,   748,     0,   730,   711,   715,   713,     0,
       0,     0,   456,     0,     0,     0,     3,     0,     2,   959,
     961,   962,     0,     0,    99,     0,     3,  1037,   628,     0,
     640,   638,  1037,     0,   625,   743,   456,   813,     0,     0,
       0,    38,     0,   111,   113,   112,   109,   108,   694,  1037,
       0,    64,    80,     0,    74,    81,    82,    59,     0,     0,
       0,    68,    55,     0,   152,   406,    30,     0,     0,     2,
     935,   937,   938,     3,     3,     0,     0,   804,     2,   906,
     908,   909,     2,   923,   925,     0,   900,   915,     3,     3,
    1034,     3,   652,   651,   655,  1036,     2,     2,  1035,     0,
       3,   801,   704,   705,     0,     0,   804,   437,   456,   456,
       3,     3,   443,   803,     0,   894,   778,     0,   780,   576,
     576,   576,   611,   581,     0,   617,   605,     0,   456,   568,
     603,   599,     0,     0,     0,     0,   606,   608,   804,   619,
     619,     0,   600,   615,   456,   402,     0,     0,    66,   326,
     327,   324,   325,     0,     0,     2,   223,     0,     0,   225,
     410,   224,   537,   456,   304,   303,   305,     0,     2,   183,
     263,     0,   256,     0,   183,   319,   317,     0,   311,  1037,
     320,     0,     0,     0,   358,   359,   360,   361,     0,   351,
       0,   352,   328,     0,   329,     0,     0,   456,   214,   201,
     315,   314,     0,   349,   368,     0,   400,   804,   456,   776,
     731,   456,     2,     2,   627,  1037,  1037,  1011,  1012,  1013,
       0,   964,   456,     3,     3,     0,   972,     0,     0,     0,
       0,   639,     0,   624,     3,    97,     0,    31,   456,     0,
    1037,     0,     0,    84,     0,    72,     0,    78,     0,    76,
      43,   157,   940,   456,     0,     0,   845,   863,   456,   456,
       0,     0,     0,   456,   456,   707,     0,   423,   425,     3,
       3,     0,     0,     0,   782,   572,   574,   570,     0,   979,
       0,   612,   984,   614,   976,   804,   804,   598,   618,   602,
       0,   601,     0,     0,     0,   621,     0,   804,   595,   609,
     620,   610,   616,   659,   663,   662,     0,     2,     0,     0,
     244,     2,   226,   537,   309,   307,   310,   306,     0,   308,
       0,   252,     0,   183,     0,     2,   456,   264,     0,   289,
       0,     0,   312,     0,     0,     2,   335,   362,     0,   353,
       2,     0,     0,     0,     0,   340,     0,   336,   199,   198,
     424,   746,     0,     0,     0,     0,  1014,     3,     0,     0,
     971,   973,   626,     0,  1037,   642,     2,    41,    39,    40,
       0,    62,   176,    75,     0,     0,     3,   846,   864,     3,
       3,   911,   926,   427,     2,   649,     3,   648,   706,     0,
       0,   837,   895,   945,     0,     0,     0,   980,   981,   804,
     597,   977,   978,   596,   577,     0,     0,   213,   334,     0,
       0,     0,   237,     2,   215,     0,     0,     2,   246,   261,
     272,   266,     2,   183,   301,     0,   276,     0,     0,   267,
     265,   254,   257,     0,     0,   183,   290,     0,     0,   218,
     333,     2,   456,   330,     0,     0,   378,     2,   338,     0,
      65,     0,   350,   751,   753,   630,   632,     0,   974,   975,
    1037,     0,   744,    63,    79,    77,     0,     0,     0,   456,
       0,   838,   896,   804,   987,   989,   982,     0,   607,   232,
     227,   230,     0,   229,   236,   235,     0,   456,   239,   238,
       2,   248,     0,   245,     2,     0,     0,     0,   253,   258,
       0,     0,   183,   302,   277,     0,     0,     2,     0,   292,
     293,   291,   260,   321,     0,   456,   456,     3,   363,   457,
     367,     0,   371,     0,     0,     0,   379,   380,   221,   341,
       0,     2,     0,     2,     2,   965,     0,   631,   941,   912,
     927,   653,     2,   983,   985,   986,   613,     0,   234,     0,
     233,   217,   240,   456,   391,   249,     2,   250,   247,   262,
     275,   273,   269,   281,   279,   280,   278,   259,   274,   270,
     271,   268,   255,     0,     0,     0,     0,   220,   240,     3,
     356,     0,   979,   364,   365,   366,   378,     0,     0,     0,
     378,     0,     2,   339,   346,     0,   343,   345,   629,   456,
     228,   231,     2,     3,   241,   392,   251,     0,     0,     0,
       0,   300,   298,   295,   299,   296,   297,   294,     3,   356,
       0,     0,   980,     0,     0,     0,   372,     0,   381,   222,
       0,   336,     0,     3,   209,     0,     0,     2,   288,   286,
     283,   287,   284,   285,   282,     0,     0,   357,     0,   384,
       0,   382,     0,   384,   342,   344,     2,     0,     0,   211,
     210,   216,     0,   219,     0,   354,   385,     0,     0,   373,
       0,   347,     2,   988,   355,     0,     0,     0,     0,   348,
     386,   387,     0,   383,   374,     0,     0,   375,   388
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1693,  6052,  4950, -1693,    -1,   149,  1635,  -153, -1693,  1626,
   -1693,   366, -1693,  -691,   655,   751,  -936, -1083, -1693,   227,
    6331,  1809, -1693,  1111, -1693,  1339,   157,   780,   786,   571,
     782,  1297,  1298,  1300,  1305,  1301, -1693,  -178,  -157,  8440,
     875, -1693,  1608, -1693, -1693,  -674,  6163, -1118,  4207, -1693,
    1113, -1693,   870,   -15, -1693, -1693, -1693,   426,    76, -1693,
   -1647, -1574,   292,    53, -1693, -1693, -1693,   303, -1463, -1693,
   -1280, -1693, -1693, -1693, -1693,     1, -1692,   184, -1693, -1693,
       7, -1693, -1693, -1693,    21,   448,   453,   125, -1693, -1693,
   -1693, -1693,  -670, -1693,    56,    -3, -1693,   133, -1693,  -162,
   -1693, -1693, -1693,   883,  -653,  -900, -1336, -1693,    12, -1323,
      71,  7061,  -803,  -792, -1693,  -279, -1693,    69,  -159,  2543,
    -218,  -240,  3498,   811,  -577, -1693,   183,   319,   941,  2139,
   -1693,  2015, -1693,   162,  3406, -1693, -1693, -1693,    63, -1693,
   -1693,   313,   201,  4314,  2919,   -24,  1812,  -275, -1693, -1693,
   -1693, -1693, -1693,  -623,  4323,  5054, -1693,  -370,   226, -1693,
     531,   253, -1693,   181,   728, -1693,   523,   -44, -1693, -1693,
   -1693,  5234,  -613, -1141,  -654,  -261,  -546,   850, -1693, -1218,
    -146,  -122,   847,   903,  8434,  -267,  -478,  -247,  -194,  -447,
    1276, -1693,  1596,   -38,  1194,  1490, -1693, -1693, -1693, -1693,
     320,  -138,  -172,  -861, -1693,   220, -1693, -1693,   636,   465,
   -1693, -1693, -1693,  2088,  -770,  -491,  -808,   -40, -1693, -1693,
   -1693, -1693, -1693, -1693,   257,  -809,  -144, -1672,  -202,  7781,
     -68,  6516, -1693,  1154, -1693,  2325,  -196,  -204,  -179,  -177,
       8,   -67,   -53,   -46,   806,    -5,     6,    51,  -167,   -57,
    -154,  -147,  -141,  -668,  -709,  -697,  -644,  -650,  -121,  -636,
   -1693, -1693,  -660,  1344,  1347,  1349,  2304,  7161,  -583,  -551,
    -549,  -543,  -743, -1693, -1511, -1650, -1648, -1638,  -593,    99,
    -258, -1693, -1693,   -72,     0,   -86, -1693,  7824,   185,  -384,
    -422
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1153,   222,   391,   392,    81,    82,   393,   368,   394,
    1450,  1451,   395,   973,   974,   975,  1263,  1264,  1265,  1462,
     417,   397,   398,   399,   681,   682,   400,   401,   402,   403,
     404,   405,   406,   407,   408,   409,   410,   419,  1072,   683,
    1385,   744,   216,   746,   413,   811,  1154,  1155,  1156,  1157,
    1158,  1159,  1160,  2045,  1161,  1162,  1390,  1567,  1891,  1892,
    1822,  1823,  1824,  2013,  2014,  1163,  1581,  1582,  1583,  1729,
    1730,  1164,  1165,  1166,  1167,  1168,  1169,  1398,  1756,  1944,
    1862,  1170,  1171,  1599,  2031,  1600,  1601,  1927,  1172,  1173,
    1174,  1388,  1935,  1936,  1937,  2077,  2092,  1962,  1963,   292,
     293,   872,   873,  1126,    84,    85,    86,    87,    88,    89,
     450,    91,    92,    93,    94,    95,   230,   567,   452,   421,
     453,    98,   302,   100,   101,   102,   333,   334,   105,   106,
     168,   107,   891,   335,   154,   110,   250,   111,   155,   258,
     337,   338,   339,   156,   414,   116,   117,   341,   118,   558,
     861,   859,   860,  1539,   342,   343,   121,   122,  1122,  1353,
    1545,  1546,  1690,  1691,  1354,  1534,  1709,  1547,   123,   642,
    1639,   344,   640,   928,  1065,   458,   459,   865,   866,   460,
     461,   867,   346,   562,  1178,   423,   424,   217,   478,   479,
     480,   481,   482,   321,  1198,   322,   889,   887,   592,   323,
     362,   324,   325,   425,   125,   174,   175,   126,  1192,  1193,
    1194,  1195,     2,  1111,  1112,   584,  1187,   127,   312,   313,
     260,   270,   541,   128,   220,   129,   231,  1074,   852,   508,
     166,   130,   653,   654,   655,   131,   233,   234,   235,   236,
     307,   133,   134,   135,   136,   137,   138,   139,   239,   308,
     241,   242,   243,   779,   780,   781,   782,   783,   244,   785,
     786,   787,   749,   750,   751,   752,   509,   140,   617,   618,
     619,   620,   621,   622,  1693,  1694,  1695,  1696,   607,   463,
     349,   350,   351,   426,   208,   142,   143,   144,   353,   803,
     623
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   141,   411,    80,   141,   331,   190,   192,   305,   132,
     525,   207,   566,   982,   686,   962,   150,   538,   345,   178,
    1379,   193,   484,   412,   494,  1196,   367,   800,   194,   506,
     930,   917,   911,   502,  1569,  1045,   881,   363,  1804,   240,
    1805,  1052,  1268,   849,   359,   845,   847,  1570,  1570,   495,
    1806,   496,   199,   856,   297,    80,    80,   625,    80,   141,
     903,   497,   904,  1179,   112,  1861,  1035,   132,   905,   195,
      96,  1275,    90,    80,   498,   151,   514,   207,  1036,   462,
     196,   499,    80,  1441,  1502,  1503,  1117,   500,  1017,   635,
      80,   205,  1900,   638,   883,    80,   573,   575,    80,   494,
     536,   522,    80,   141,   237,   955,   300,   261,   502,  1041,
     546,   271,   602,  1188,   445,   264,    58,   687,  1361,  1362,
    1958,  1741,   112,   429,   495,   197,   496,  1042,    96,   633,
      90,  1037,    58,   636,  1309,  1603,   497,   430,  1894,  1038,
      80,  1068,  1966,    80,   431,    80,   141,  1893,   726,   498,
     218,    80,   596,   148,   132,  1469,   499,    80,  1310,  1083,
     492,   192,   500,   108,    80,   188,   268,   932,  -754,  -393,
     254,  2066,   503,  -920,   265,   193,  1899,  1464,  1808,   625,
     282,   531,   194,   287,   103,   432,  1143,  1470,    80,    80,
     727,   596,  1311,  1901,   911,   912,   433,   205,   505,   507,
    1559,   927,   113,    80,   159,   172,   172,   160,   161,   112,
     162,   164,   466,   475,  1604,    96,   287,    90,    80,  1175,
    -786,   108,  1348,   195,   903,  1185,   904,    80,    80,   145,
     602,  -394,   905,  1363,   196,   578,   192,   205,   531,  -393,
     172,   434,   103,  1967,    80,  1233,  -755,   503,  1067,  1067,
     193,   574,    80,  1819,  1820,  1957,   926,   194,   553,   836,
     113,   205,    80,   348,   693,    80,   157,  1067,   210,   694,
    1838,  1569,    80,  1256,   542,  1959,  1960,  1021,   287,   197,
    1200,  1605,    80,    80,  1570,    80,   804,  1893,   818,  1311,
     172,  1045,   876,   172,  1895,  1561,  1886,  1360,   188,  1641,
    1734,  -394,    80,    80,  1337,   832,  -573,   172,   108,  1340,
      80,   158,   205,   819,   357,   820,  1228,    80,    80,  1349,
     104,  1366,    80,   644,  1035,   821,   646,  1219,   625,   103,
    1350,   784,   980,  1067,   210,  1821,  1036,   539,   822,  1819,
    1820,  1804,   898,  1805,  1338,   823,   207,   113,  1606,  1861,
    1179,   824,   625,  1806,   112,    80,  1282,  1046,   771,   625,
      80,  1049,  1114,    80,   652,  1899,    58,   286,   172,   435,
    1062,  1063,  1295,   427,   818,   163,  1247,   843,   104,   923,
    1408,   855,   896,   848,   832,  1655,  1657,  1659,  2012,  1037,
    1296,   938,   366,   940,   941,    20,   942,  1287,   177,   819,
    1899,   820,   574,   881,   179,   944,   916,   462,   946,   947,
     948,   821,   172,  1570,  2012,   218,  1075,  1994,   256,   922,
      58,  1848,   172,   331,   822,   429,   614,    80,  1320,   608,
    1933,   823,   556,   172,   180,   561,   466,   824,   532,   430,
    2047,  1348,  1348,  1348,   833,   286,   431,   188,   199,   507,
      80,    80,    58,   108,   957,  1502,  1503,  1219,  1843,  1844,
     172,  1732,    80,    80,  1907,   104,  1740,   172,   172,   213,
    1430,    80,   172,   475,   569,    58,   200,  1365,   895,   462,
     214,  1808,   903,   219,   904,  -786,  1941,   432,   198,    64,
     905,    80,   113,   547,   513,   532,   215,   518,   433,   515,
      80,   211,  1397,   507,   559,   172,  1175,    58,    58,   466,
     172,   429,   589,   172,   456,   610,    58,  1271,   535,  1942,
      80,  1886,    58,   833,  1267,   430,    80,  1654,   545,    -3,
    1527,   581,   431,  1066,  1066,   507,  1067,    58,  1349,  1349,
    1349,   590,   591,   434,   882,   225,  1229,   608,  1494,  1350,
    1350,  1350,  1066,  1358,   762,  1910,  1911,   593,   507,  1551,
     245,   594,    58,  1277,    80,    58,    80,    14,    15,    16,
      17,    18,  1359,   282,  1570,    58,  1006,    80,  1552,    80,
      58,    63,    64,    80,   141,   540,  1022,  1043,   466,   625,
     507,   612,   132,    80,  -450,  1050,  1473,    80,    80,   612,
     172,  1093,  1570,   210,    58,   507,   695,  1437,  1076,   462,
     411,   696,   172,   172,   917,  1837,  1097,    58,  1066,    58,
     507,  1071,   716,   717,   625,    58,   957,   957,   284,    76,
      80,  1056,   268,   610,   112,  1301,  1916,  1984,  1358,  1204,
    1570,  1323,    80,   605,  1327,   507,   628,   112,   507,  1698,
     462,   256,    58,    96,  1428,    90,   784,  1616,   612,  1479,
     605,  1951,   286,   507,   605,  1735,   718,   719,  1699,  1557,
    1736,  1085,   462,   462,   870,  1551,   287,  1702,   957,  1203,
    1707,   348,  1102,  1488,   957,   188,  -454,   507,  1986,   881,
    1101,   462,  -682,    80,  1701,    80,  1492,    80,  1952,  1708,
     612,    80,   507,  1742,    80,  1809,   553,  1592,   427,   427,
    1724,  1725,  1726,   287,   148,  1909,  1707,  1568,  1584,  1416,
    1784,   957,  1785,   608,  1810,   957,   957,  1922,  -451,    80,
    1251,  2018,  1727,   108,   435,  1813,   507,  1252,    14,    15,
      16,    17,    18,   172,  1817,  1905,   108,   892,   894,   411,
     957,   709,   552,    64,   569,  -393,  1501,   462,   710,   711,
    2020,   605,    14,    15,    16,    17,    18,   103,  1461,   301,
    1227,   840,   113,   256,    80,  1267,    80,  1855,  1999,  1442,
     920,  -775,  1856,  2000,   900,   113,  1652,   286,    80,   435,
     331,   507,   172,   851,  1977,    80,    58,  2062,   361,   854,
    1222,   475,  2063,   858,    80,  1459,   950,  1625,  1626,   918,
    1724,  1725,  1726,    80,    80,    80,  1419,   951,   952,   364,
      58,  1066,  1308,   805,   806,  1272,  -452,   807,   246,   247,
     427,   248,  1727,    80,   319,   249,    14,    15,    16,    17,
      18,  1728,   456,   966,  1443,   968,   159,   971,   365,   160,
     161,   979,   162,   366,   983,   597,   282,   181,     6,     7,
       8,     9,    10,    11,    12,    13,   985,   986,   987,    80,
      80,   475,   141,   185,   728,   436,   881,   470,   729,  1008,
      73,  -455,   515,  1315,   828,   141,   507,   256,   437,   206,
     712,   713,    80,  1477,    58,   456,   438,  1189,   439,  1333,
     747,  1071,   238,   104,   507,   262,  1685,  1686,  1687,   272,
     255,    78,    79,   605,   456,   714,   715,   440,  1294,   784,
      80,   276,   754,   279,    80,   281,   755,    73,    80,   766,
     441,  1589,   483,   507,   652,   112,   625,   605,   172,   489,
     427,    96,   505,    90,   869,   172,   540,   611,   870,   465,
     605,   612,   469,   931,  1568,   900,  1190,   594,    78,   613,
     462,   720,   721,  1084,   255,  1086,   279,   281,   490,    80,
     933,   614,  1452,   534,   594,  1533,  1432,    80,   959,   960,
     181,     6,     7,     8,     9,    10,    11,    12,    13,   916,
     200,   689,    73,   485,    73,   206,  1594,  1595,  1596,  1597,
    1598,   639,   491,   186,   934,  -441,    80,   488,   935,   475,
     467,  1650,   611,   956,  1688,   255,   612,   957,   507,  1125,
     172,   172,  1223,    78,    79,    78,    79,  1774,  -441,   581,
     504,   435,    80,   507,   108,   206,   523,  1080,    80,    80,
     273,  1081,   172,   524,   456,   274,   218,  1409,   348,   275,
    1355,   563,   278,  1638,   280,  1177,   544,   147,  1642,   206,
     363,   363,    65,    66,    67,    68,    69,    70,    71,    80,
    1221,   427,   543,   113,   172,  1651,   331,   255,   172,   279,
     281,   600,   689,  1584,   632,   456,  1522,   585,  1043,   600,
     435,  1571,   612,   147,  -919,  1474,   170,   171,    65,    66,
      67,    68,    69,    70,    71,  -623,    75,   255,   643,   797,
     660,  1026,  1116,   255,   147,   507,   645,   170,   171,    65,
      66,    67,    68,    69,    70,    71,   656,  1796,  1058,  1059,
     141,   475,   411,   411,    80,    80,    80,    14,    15,    16,
      17,    18,   286,   255,   515,   657,   507,   661,   507,   630,
     708,   281,  1107,  1510,  1511,  1549,   957,  1504,   475,   141,
    1870,  1109,  1266,   581,    80,   957,  1267,   507,   722,  1940,
    1415,   665,    80,   689,   755,    80,    80,   141,  1447,    80,
     462,   462,  1267,   256,   261,   271,   669,   723,  1647,   264,
      80,   104,  1648,   112,   540,    58,  1718,   274,   725,    96,
     957,    90,  1724,  1725,  1726,  1743,   724,  1744,  1745,   756,
     605,   957,  1081,   628,   957,  1724,  1725,  1726,   256,    80,
    1060,  1061,   112,   730,  1727,   475,   757,   957,    96,   758,
      90,   759,  1746,  1733,   467,    80,   957,  1727,   760,   255,
     268,  1764,  1765,   761,  1964,   288,  -184,   254,   265,  1254,
    1081,   475,  -120,  -120,  -120,  -120,  -120,  -120,   442,    80,
    1814,    -3,   456,   331,   755,   255,  1780,   630,   281,  1355,
    1355,  1355,  1964,  1535,  1355,   274,   275,   788,   629,  1902,
     280,   172,   669,   957,   172,   172,   172,   992,   993,   994,
     995,    80,   108,   147,  1550,  -453,   170,   171,    65,    66,
      67,    68,    69,    70,    71,  2003,   608,   467,   172,  1267,
    2015,  2064,   255,  1177,   172,   957,   670,  1269,  1270,  2088,
    2095,   108,   561,  2085,  2096,   802,  2033,   -17,  1571,   172,
    2037,   113,   255,   487,   348,   957,  1273,   255,   494,   255,
    1549,   801,  1177,   812,   141,  1341,  1342,  1343,   502,   825,
      80,  -155,  -155,   826,    80,   827,   150,    80,   829,   255,
     113,   255,   255,   495,   835,   496,  1372,  1060,  1407,   172,
     141,   141,  1467,  1468,   830,   497,   831,   475,   837,   255,
     294,  1452,   427,  1472,  1468,  1448,  1476,  1468,   498,   871,
    1871,   255,   918,  1394,   853,   499,  -571,   475,  -569,    80,
     862,   500,  1032,  1460,   884,   570,   886,   868,   542,  1512,
    1460,   890,   670,   906,   255,   151,   630,   281,   147,    80,
      80,   170,   171,    65,    66,    67,    68,    69,    70,    71,
    1032,  1524,  1714,   112,   112,  1660,  1081,   908,   255,   630,
     614,    90,    90,  1782,  1081,   255,   141,  1783,  1468,   104,
    1793,  1794,   925,  1753,  1803,   957,   927,   475,  1189,   331,
    1859,  1860,    80,  1874,  1468,  1875,  1468,    80,    80,    80,
     929,   539,  1504,   936,  1703,  1819,  1820,   937,   104,  1550,
     964,   274,   958,   662,  2085,  2086,  1946,   503,  1465,  1466,
     961,  1395,   988,   989,   647,   256,  1005,   685,  1010,   818,
     990,   991,   996,   997,   172,  1710,  1710,   172,   706,   707,
     832,  -119,  -119,  -119,  -119,  -119,  -119,  1190,  1417,  1418,
     605,   348,  1031,  1032,   819,  1039,   820,  1078,  1504,   706,
     540,  1087,   108,   108,  1088,    80,   821,  1089,  1090,  1091,
    1092,    80,  1108,    80,  1110,  -758,  -658,   172,   456,   822,
      80,   462,   462,  1573,  1573,  1119,   823,  1180,  1120,   706,
    1121,  1213,   824,  1197,   475,  1214,  1181,  1215,   648,   172,
     172,   113,   113,   141,  1225,  1230,  1231,   475,   697,  1234,
     698,   699,   700,   649,  1236,   264,   650,   651,    65,    66,
      67,    68,    69,    70,    71,   147,  1237,  1238,  1239,  1854,
      65,    66,    67,    68,    69,    70,    71,   141,  1240,   701,
    1242,  1314,   702,   703,   475,  1243,  1244,   704,   705,  1864,
    1549,   141,  1249,  1250,  1257,  1274,   839,  1258,  1279,  1280,
    1281,   842,  1288,  1189,  1928,  1289,   268,  1290,  1291,    80,
    1322,   411,  1299,   254,   265,  -759,  -646,  -645,   850,   833,
    1387,  1334,  1356,  1357,   625,    80,  1367,    80,   857,  1370,
    1371,  1380,  1890,    14,    15,    16,    17,    18,   954,  1381,
     112,  1382,  -681,  1389,   957,   255,  1391,  1397,    90,  1401,
      14,    15,    16,    17,    18,  1404,   255,  1403,   868,   104,
     104,  1106,  1190,    14,    15,    16,    17,    18,  1246,  1405,
     172,  1411,    80,  1934,  1413,    80,  1420,  1421,  1928,  1444,
     255,  1445,  1458,  1460,   172,  1475,   475,   348,  1487,   540,
     475,   255,  1500,   427,  1505,  1506,  1507,   172,  1508,  1468,
     255,  1516,  1513,  1504,   475,   456,   141,  1525,    58,  1526,
    1528,  1564,  1538,  1540,   475,  1360,  1541,  1607,  1609,   868,
    1585,  1612,  1212,  1586,  1617,  1588,  1590,   462,  1602,  1550,
    1619,    80,    80,  1610,   172,  1620,  1622,  1624,  1611,   108,
    1623,   685,    80,  1631,  1627,  1628,   685,  1629,  1636,   411,
    1930,   411,  1640,  1643,  1993,   685,  1646,   494,  1649,   172,
    1573,  1645,  1653,  1661,   539,  1662,   502,    73,  1666,   112,
    2010,  1667,  1890,   435,   685,  1677,  1684,    90,   113,  1675,
      83,  1512,   495,   149,   496,    80,  1697,   611,  1543,   984,
     411,   612,   475,  1717,   497,  1267,   475,   255,    78,   613,
    1721,   475,  1719,   219,  1750,  1752,   832,   498,  1757,  1773,
    1787,  2035,  1766,  1934,   499,  1770,  1771,  1934,  1934,   476,
     500,   255,  2057,  1772,  1930,  1775,   475,  1781,  1798,   868,
    1827,  1788,  1278,  1791,  1792,  1801,   172,  1802,    83,  1832,
     172,  1833,  1845,  1847,  1853,  1851,   868,   868,  1857,  1285,
    1286,  2060,  1143,   189,   172,  1858,  1865,  1866,  1872,  1868,
    1869,   256,    83,  1873,   172,  -647,   507,   141,   108,   475,
     411,  1881,  1882,   475,  1883,   229,  2076,  1884,   253,  1885,
    2076,   172,    83,  1904,  1906,  -554,   475,  1915,  1917,  1573,
    1912,  2087,   172,   578,   192,   141,   104,    80,  1923,    80,
    1931,  1932,  2090,  1945,  1947,   503,  1948,   113,   193,  1949,
     475,  1950,   475,   475,  1794,   194,  1961,  1970,  1983,   149,
    1987,  1996,  1997,  1105,  1998,    83,  1985,  2002,   149,  2001,
     112,   304,   310,   141,  1113,   475,  2005,  1115,    90,  2008,
    2021,  1118,   172,   330,  2017,   833,   172,  2019,  2034,  2041,
    2030,   172,   262,   272,  2042,  2036,  2048,   255,   112,  2058,
      80,    80,  2061,  2059,  2071,  2073,    90,   418,   189,   189,
     205,   475,  2078,  2074,  2079,  2082,   172,  2083,  2093,   149,
     448,   475,   692,   253,  1778,  1558,  2094,  2097,  1471,   998,
     953,   999,   255,  1386,  1000,   745,   112,  1002,   255,  1393,
    1001,    80,  2072,  1754,    90,  2011,  1849,   229,   229,  1842,
     466,  2028,   540,  2067,   475,  1943,   475,  1748,  2065,   172,
    2056,  1989,  1749,   172,   304,   104,  1241,  2038,  1988,   108,
    2080,  1245,    83,  1402,   169,   475,   172,  1700,  1956,  1888,
     533,   475,  1253,  1711,  1537,   253,  1399,   808,  1077,  1995,
    1573,   475,  1644,  1199,   888,    80,  1761,   108,     3,  1232,
     172,  1013,   172,   172,  1014,    80,  1015,     0,   113,     0,
       0,     0,     0,     0,     0,   310,     0,   798,  1573,   476,
       0,   310,   304,   304,     0,   172,     0,     0,     0,   149,
       0,     0,     0,     0,     0,   108,   113,     0,     0,     0,
     868,   868,     0,  1483,  1484,     0,     0,     0,     0,   330,
     615,   624,     0,     0,   868,   868,  1573,  1498,  1499,     0,
       0,   172,     0,     0,     0,     0,   330,     0,     0,     0,
     330,   172,     0,   255,   113,   605,   245,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,   868,   868,
     147,  1520,  1521,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,   418,   172,     0,   172,     0,     0,     0,
       0,   187,     0,     0,     0,     0,   543,     0,     0,     0,
       0,   255,     0,     0,     0,   172,   104,     0,     0,     0,
       0,   172,     0,     0,     0,    58,     0,   418,     0,  1339,
     748,   172,     0,     0,     0,  2091,   587,   189,   257,   605,
       0,   662,  1364,     0,   104,  2098,     0,     0,     0,   277,
       0,     0,     0,   149,     0,     0,     0,   448,     0,  1383,
       0,   777,     0,   624,     0,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,    14,    15,    16,
      17,    18,   104,     0,    73,     0,     0,     0,     0,     0,
       0,     0,   257,    73,     0,     0,     0,     0,     0,     0,
       0,   229,     0,     0,   747,     0,     0,     0,   507,     0,
     229,     0,     0,  1542,    75,    78,    79,     0,   706,     0,
    1543,     0,     0,     0,    78,    79,     0,     0,     0,     0,
     304,     0,   418,   418,     0,    58,   304,     0,   330,     0,
       0,   147,     0,   257,   170,   171,    65,    66,    67,    68,
      69,    70,    71,  1453,  1454,  1455,     0,     0,     0,     0,
    1456,  1457,     0,     0,     0,   868,   868,     0,  1679,  1680,
       0,     0,     0,     0,     0,     0,   304,     0,     0,     0,
       0,     0,     0,   255,     0,     0,     0,   304,     0,   304,
       0,   330,     0,    83,    73,     0,     0,     0,     0,   191,
    1376,  1715,     0,     0,     0,   257,     0,     0,     0,   330,
     448,     0,   624,     0,  1688,     0,     0,     0,   507,     0,
     615,   232,     0,     0,   615,    78,    79,     0,     0,     0,
       0,     0,     0,   330,     0,   257,     0,   476,     0,     0,
     798,   257,     0,   624,     0,     0,   330,     0,     0,     0,
       0,     0,     0,     0,   149,   147,    58,     0,   170,   171,
      65,    66,    67,    68,    69,    70,    71,   418,     0,   149,
     149,   257,   418,     0,  1560,  1562,     0,   306,     0,   868,
       0,   418,  1767,     0,   149,   149,   149,   147,     0,     0,
     226,   227,    65,    66,    67,    68,    69,    70,    71,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
     868,     0,  1614,  1786,   671,   868,   868,   255,  1789,  1790,
       0,     0,     0,     0,     0,     0,     0,     0,  1374,     0,
       0,     0,     0,    19,   255,  1292,    75,     0,     0,     0,
     448,     0,     0,   517,    97,     0,     0,   152,     0,     0,
       0,     0,     0,   493,   232,     0,   748,   748,    58,     0,
       0,     0,     0,     0,   418,    14,    15,    16,    17,    18,
     306,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,   448,     0,     0,   777,     0,   777,     0,     0,   147,
       0,     0,     0,   257,    65,    66,    67,    68,    69,    70,
      71,     0,    97,   330,   330,  1262,     0,     0,     0,     0,
     671,     0,     0,  1262,     0,     0,     0,    73,   255,     0,
       0,     0,   330,    58,   304,     0,   203,     0,   579,   306,
     314,   315,   316,   317,     0,     0,     0,    74,    75,     0,
       0,     0,  1262,   304,     0,   476,     0,     0,    78,    79,
       0,     0,     0,     0,   147,     0,     0,   226,   227,    65,
      66,    67,    68,    69,    70,    71,     0,   257,   147,     0,
       0,   170,   171,    65,    66,    67,    68,    69,    70,    71,
       0,   418,    73,   295,     0,     0,     0,   257,   330,    97,
       0,     0,   203,     0,   149,   418,     0,     0,     0,     0,
       0,     0,   228,    75,     0,  1262,   330,   257,  1207,     0,
       0,  1747,     0,    78,    79,     0,     0,     0,    19,   615,
     318,     0,     0,  1755,     0,   753,   147,     0,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,   319,     0,
     255,   764,   257,   443,   767,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    73,     0,     0,     0,   448,     0,
      52,    53,    54,    55,     0,     0,   257,     0,     0,     0,
       0,     0,     0,   257,   775,    75,     0,   778,   612,     0,
      14,    15,    16,    17,    18,    78,   776,     0,     0,     0,
       0,    58,     0,     0,   527,     0,   530,     0,   614,     0,
       0,   517,   147,     0,     0,     0,     0,    65,    66,    67,
      68,    69,    70,    71,   977,     0,     0,   817,     0,     0,
       0,     0,   147,     0,     0,   748,   232,    65,    66,    67,
      68,    69,    70,    71,    97,     0,     0,     0,    58,     0,
       0,     0,   777,   255,     0,     0,   306,     0,     0,   777,
      73,     0,   306,   530,   978,     0,   868,     0,     0,  2043,
       0,   476,     0,     0,     0,     0,     0,     0,     0,   147,
      74,    75,   226,   227,    65,    66,    67,    68,    69,    70,
      71,    78,    79,     0,     0,     0,     0,   476,     0,     0,
       0,   330,   306,     0,     0,  1262,     0,    73,     0,     0,
       0,     0,     0,   880,     0,   306,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,   775,    75,     0,
     115,   612,     0,   115,     0,     0,     0,   147,    78,   776,
       0,   149,    65,    66,    67,    68,    69,    70,    71,   418,
       0,     0,     0,     0,     0,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,   203,     0,     0,     0,     0,     0,     0,   418,     0,
    1939,     0,     0,    73,     0,  1292,    75,     0,   115,     0,
       0,     0,     0,     0,     0,   253,    83,   770,     0,     0,
       0,     0,     0,  1991,    75,    58,     0,   507,     0,     0,
     304,     0,   115,   257,    78,    79,   149,     0,     0,     0,
       0,     0,     0,   476,   257,     0,   448,     0,   259,     0,
       0,     0,   115,     0,     0,     0,   147,     0,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,   257,     0,
       0,     0,     0,     0,   448,     0,     0,     0,   149,     0,
       0,   753,   753,     0,    73,     0,     0,     0,     0,   115,
       0,  1024,     0,     0,  1027,   115,     0,     0,   115,     0,
       0,     0,   259,     0,   228,    75,     0,     0,     0,     0,
       0,     0,   326,   115,   358,    78,    79,     0,     0,     0,
       0,     0,     0,   476,     0,     0,     0,     0,  1262,     0,
    1034,     0,   778,  1262,  1262,  1262,     0,   422,   152,     0,
       0,   330,   330,     0,    97,     0,     0,     0,     0,   115,
     422,     0,     0,   259,     0,   517,     0,    97,     0,  1095,
       0,     0,     0,  1099,     0,     0,     0,     0,     0,     0,
     306,    14,    15,    16,    17,    18,     0,     0,     0,     0,
     149,   149,   149,   149,   149,   149,     0,   147,     0,   306,
    1544,   310,    65,    66,    67,    68,    69,    70,    71,  1259,
     115,  2075,   115,  1260,     0,  1261,     0,     0,     0,   418,
     418,     0,     0,     0,     0,   259,   147,  2084,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,    58,
       0,     0,   557,     0,     0,     0,    75,     0,     0,   253,
     115,     0,     0,     0,     0,   259,     0,     0,     0,     0,
       0,   259,     0,     0,     0,     0,     0,     0,     0,   115,
     147,   448,     0,   226,   227,    65,    66,    67,    68,    69,
      70,    71,   893,     0,     0,     0,     0,     0,     0,   115,
       0,   259,   115,     0,     0,   149,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,  1991,    75,
       0,     0,   507,     0,     0,     0,     0,     0,     0,    78,
      79,  1262,     0,  1262,     0,     0,     0,     0,     0,     0,
     147,     0,     0,   422,     0,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,   257,     0,     0,     0,     0,
     753,     0,     0,     0,   147,     0,     0,     0,    73,    65,
      66,    67,    68,    69,    70,    71,   969,   422,     0,     0,
       0,  1689,     0,     0,     0,  1544,     0,   418,  1033,    75,
     257,  1544,   612,  1544,     0,     0,     0,     0,  1034,    78,
      79,    58,     0,   115,  1293,   778,     0,   422,     0,     0,
       0,     0,     0,   259,     0,     0,   970,     0,     0,     0,
       0,   310,   149,     0,     0,     0,     0,     0,     0,     0,
       0,  1325,   147,     0,  1329,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,    97,   418,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,   330,  1191,     0,
     149,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     303,    75,   422,   422,     0,     0,     0,   259,   115,     0,
       0,    78,    79,     0,     0,     0,     0,   149,     0,     0,
       0,     0,     0,     0,     0,   109,     0,   147,     0,     0,
     354,   355,    65,    66,    67,    68,    69,    70,    71,   115,
       0,     0,   330,   330,   115,     0,     0,   259,   115,     0,
     115,   257,     0,     0,     0,     0,     0,  1689,  1689,    99,
       0,   115,   153,   115,     0,     0,     0,     0,     0,   267,
       0,     0,  1544,     0,     0,  1544,   306,   358,    76,   115,
     422,     0,   259,   356,     0,     0,     0,     0,     0,     0,
       0,     0,   310,     0,     0,     0,     0,     0,     0,   257,
       0,     0,     0,   115,     0,   418,   259,     0,     0,     0,
     557,     0,   109,   259,     0,     0,   115,    99,   924,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
     336,     0,   304,     0,     0,     0,     0,   422,     0,   115,
     115,   204,   422,  1481,     0,     0,     0,     0,     0,     0,
       0,   422,  1490,     0,   115,   115,   115,     0,     0,     0,
       0,   266,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,     0,     0,     0,  1689,  1495,     0,     0,
       0,     0,     0,   147,     0,  1544,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,   296,     0,
       0,     0,     0,     0,    99,     0,     0,     0,     0,   147,
     422,    73,   226,   227,    65,    66,    67,    68,    69,    70,
      71,   149,   332,     0,     0,  1351,     0,     0,     0,     0,
       0,  1542,    75,    97,   422,     0,  1548,     0,  1543,     0,
       0,   549,    78,    79,     0,     0,     0,   428,   330,     0,
       0,   422,     0,     0,     0,     0,  1689,   109,   296,   454,
       0,     0,    97,     0,     0,  1220,   149,     0,     0,     0,
     147,   257,     0,   115,   115,    65,    66,    67,    68,    69,
      70,    71,  1259,     0,     0,     0,  1260,   501,  1261,     0,
       0,     0,   115,     0,   149,   149,   604,  1992,   310,   267,
       0,     0,     0,   521,     0,     0,     0,     0,   526,   528,
       0,   204,     0,   604,     0,     0,     0,   604,     0,    75,
     115,     0,  1463,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   149,   548,     0,     0,   550,     0,   551,     0,
       0,     0,     0,   259,     0,     0,     0,     0,     0,   568,
       0,   422,     0,     0,   259,     0,     0,     0,   115,     0,
    1992,  1992,   580,     0,   115,   422,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,   115,     0,  1209,   422,
       0,   115,     0,     0,     0,     0,     0,     0,   603,     0,
       0,   627,     0,     0,     0,     0,  1692,     0,     0,     0,
       0,  1992,     0,     0,     0,   634,     0,     0,     0,   634,
       0,     0,     0,     0,   604,     0,     0,     0,     0,     0,
       0,  1548,   257,     0,    58,     0,     0,  1704,   422,  1548,
       0,     0,   147,     0,     0,   198,    64,    65,    66,    67,
      68,    69,    70,    71,  1351,  1351,  1351,   152,  1532,  1536,
       0,     0,     0,     0,     0,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,    97,    97,     0,    14,    15,    16,    17,
      18,    75,     0,    73,   797,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,   455,     0,     0,     0,     0,
       0,     0,   296,   303,    75,    58,   603,     0,   115,   115,
       0,   147,     0,     0,    78,    79,    65,    66,    67,    68,
      69,    70,    71,  1259,     0,     0,   336,  1260,     0,  1261,
       0,     0,     0,     0,    58,   267,   147,   109,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,   455,  1191,
     109,     0,  1692,  1692,     0,     0,     0,     0,     0,     0,
      75,   115,     0,  1656,    73,   147,   604,   455,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,  1542,    75,     0,   454,  1815,     0,
     604,  1548,     0,    73,     0,    78,    79,     0,     0,     0,
       0,   115,     0,   604,     0,     0,     0,     0,     0,   422,
       0,     0,     0,  1542,    75,     0,     0,     0,   864,     0,
       0,     0,     0,   528,    78,    79,     0,   875,   257,   568,
       0,     0,     0,     0,     0,     0,     0,     0,   422,     0,
     332,     0,    99,     0,     0,     0,     0,     0,   306,     0,
       0,     0,     0,     0,     0,   259,   115,   147,   634,   899,
     226,   227,    65,    66,    67,    68,    69,    70,    71,     0,
       0,  1692,     0,   910,     0,     0,   115,     0,     0,     0,
       0,     0,   603,     0,     0,    73,   422,   919,     0,     0,
    1209,     0,     0,     0,     0,   634,     0,   455,     0,     0,
       0,  1548,  1440,     0,     0,  1991,    75,     0,     0,   507,
      97,     0,     0,     0,   422,     0,    78,    79,   115,     0,
       0,     0,     0,     0,  1191,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,   455,     0,
       0,     0,     0,     0,     0,     0,     0,  1954,     0,     0,
       0,  1692,     0,    73,     0,     0,     0,     0,     0,     0,
     336,   336,   115,   115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   228,    75,     0,   115,   115,     0,   336,
       0,   115,   115,     0,    78,    79,     0,     0,     0,   454,
     147,     0,  1692,   552,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,  1016,   336,     0,     0,
     115,   115,     0,   306,     0,     0,     0,     0,     0,     0,
     115,   115,   115,   115,   115,   115,     0,     0,     0,     0,
     899,   259,     0,     0,     0,  1040,     0,     0,   109,    97,
       0,     0,  1007,     0,     0,   336,     0,     0,     0,   422,
     422,     0,   454,   454,     0,  1692,  1692,     0,     0,     0,
       0,     0,     0,   604,     0,     0,   267,     0,   336,     0,
       0,   454,     0,     0,     0,   114,   579,   306,     0,   259,
       0,     0,     0,     0,   119,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,  1692,     0,     0,   864,
       0,   422,     0,   147,     0,     0,   226,   227,    65,    66,
      67,    68,    69,    70,    71,   455,     0,   306,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
    1176,    73,     0,   114,     0,     0,     0,   454,     0,     0,
       0,     0,   119,   153,     0,     0,     0,     0,     0,     0,
       0,   303,    75,     0,     0,   634,     0,     0,  1211,     0,
     864,     0,    78,    79,   147,  1217,   119,     0,     0,    65,
      66,    67,    68,    69,    70,    71,  1259,   269,   336,     0,
    1260,     0,  1261,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,   336,   336,   115,   115,     0,
      97,     0,     0,     0,     0,     0,     0,   332,     0,     0,
       0,     0,     0,    75,     0,     0,  1658,   422,     0,     0,
     114,     0,     0,   119,     0,     0,     0,     0,    97,   119,
       0,     0,   119,   115,     0,     0,   147,     0,   340,   170,
     171,    65,    66,    67,    68,    69,    70,    71,   336,     0,
     147,   259,   115,   554,   555,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,    97,     0,     0,     0,
     864,   119,     0,     0,     0,   457,     0,     0,     0,     0,
       0,     0,     0,   119,   465,     0,   422,   864,   864,     0,
       0,     0,     0,     0,     0,     0,   109,   115,     0,     0,
     115,    76,     0,     0,     0,     0,     0,     0,   374,     0,
     375,   115,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,   109,     0,   115,     0,     0,
       0,     0,     0,     0,   119,     0,   119,     0,     0,     0,
     454,   119,   115,   267,     0,     0,     0,   115,   115,     0,
       0,     0,   115,   115,     0,     0,     0,     0,   691,     0,
       0,    76,   385,     0,     0,   114,     0,     0,     0,     0,
       0,     0,     0,   604,   119,     0,     0,     0,     0,     0,
    1352,     0,     0,     0,     0,     0,     0,     0,  1176,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,   455,   259,     0,   606,     0,     0,   269,     0,     0,
       0,     0,     0,     0,     0,   422,     0,  1176,     0,     0,
       0,   606,     0,     0,     0,   606,     0,     0,     0,     0,
       0,     0,     0,   147,     0,  1400,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,   147,   336,
     336,   170,   171,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,   336,   336,   603,     0,   119,   336,   336,
       0,     0,   147,     0,   526,   170,   171,    65,    66,    67,
      68,    69,    70,    71,    76,     0,     0,     0,     0,     0,
       0,     0,     0,   332,     0,     0,   469,   336,   336,     0,
       0,   119,   732,   733,   734,   735,   736,   737,   738,   739,
     740,   741,   742,     0,     0,     0,   213,     0,     0,     0,
       0,     0,   606,     0,     0,     0,     0,   119,     0,     0,
       0,   115,     0,     0,     0,     0,   109,   109,     0,     0,
       0,   864,   864,   743,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   864,   864,     0,   115,     0,
     454,   454,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,   864,
     864,     0,     0,     0,     0,     0,     0,     0,     0,  1352,
    1352,  1352,   153,   457,   115,   115,   119,   119,   259,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1572,  1572,
       0,     0,     0,     0,   340,     0,     0,     0,     0,     0,
       0,     0,   115,   269,     0,   114,     0,     0,   119,     0,
       0,     0,   119,     0,   119,     0,   457,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,   606,   457,     0,     0,     0,     0,
     332,     0,     0,     0,   336,   336,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   606,     0,
       0,     0,     0,     0,   153,     0,     0,     0,     0,     0,
       0,   606,     0,     0,     0,     0,     0,     0,     0,     0,
     336,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   267,
       0,   119,     0,   119,   119,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   119,   119,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,   864,   864,     0,     0,
       0,     0,     0,     0,   336,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   212,   336,     0,
       0,     0,  1706,   223,   224,   457,     0,     0,     0,     0,
       0,     0,   864,     0,     0,   120,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   336,
       0,  1723,     0,     0,   336,   336,     0,   285,   119,   336,
     336,     0,     0,     0,     0,     0,   457,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1572,     0,     0,   340,   340,
       0,     0,     0,   120,     0,     0,   332,     0,     0,   153,
       0,     0,     0,     0,     0,     0,     0,   340,     0,     0,
     864,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   340,     0,   120,     0,     0,
       0,   864,     0,     0,     0,     0,   864,   864,     0,     0,
       0,   454,   454,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,  1807,     0,     0,
       0,     0,     0,   340,   120,   119,     0,     0,     0,     0,
     120,     0,     0,   120,     0,     0,     0,     0,   119,   119,
       0,   606,     0,     0,   269,     0,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1572,   124,     0,     0,   124,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
     576,     0,     0,     0,   120,     0,     0,     0,   604,     0,
       0,     0,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,   120,     0,   120,     0,     0,
       0,     0,   120,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,   340,     0,     0,     0,
       0,   109,   604,     0,     0,     0,     0,   124,     0,  1369,
       0,     0,     0,   340,   340,   120,     0,     0,     0,     0,
    1929,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,   109,
       0,     0,     0,     0,   124,     0,     0,   454,     0,     0,
     124,     0,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1572,   340,   773,     0,   774,
       0,     0,     0,     0,     0,     0,     0,     0,   790,   791,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,   124,  1572,  1929,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,   120,     0,
       0,     0,     0,     0,   114,   119,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,  1572,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,   114,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,   124,     0,   124,     0,     0,
    2032,   269,   124,     0,     0,     0,     0,     0,   120,     0,
     119,     0,     0,     0,     0,     0,     0,   864,     0,     0,
       0,     0,     0,     0,     0,     0,   874,     0,     0,     0,
     119,   606,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,   457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   340,   340,     0,
       0,     0,     0,     0,     0,  1613,     0,     0,     0,     0,
       0,   340,   340,     0,     0,     0,   340,   340,   124,   120,
       0,     0,     0,   120,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   340,   340,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   119,   119,   119,   119,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,   114,   114,     0,     0,     0,     0,
       0,     0,     0,   119,   119,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,   120,   120,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,   120,
     120,   120,     0,     0,     0,     0,   457,     0,  1055,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,  1720,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1731,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,   124,     0,   124,     0,     0,     0,   120,
       0,     0,     0,     0,     0,  1123,  1124,     0,   124,     0,
       0,     0,  1759,     0,     0,     0,  1182,  1183,  1184,     0,
       0,  1186,   340,   340,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   340,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,   269,     0,     0,
       0,     0,   124,     0,   124,   124,   119,   124,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,   124,
     124,   124,     0,  1255,     0,     0,     0,     0,     0,     0,
       0,   114,     0,     0,  1818,     0,   120,     0,  1828,     0,
     119,     0,   340,     0,     0,     0,     0,     0,     0,   120,
     120,     0,  1841,     0,   119,     0,   340,     0,     0,     0,
       0,     0,  1850,     0,     0,     0,     0,     0,     0,  1276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,   340,     0,     0,
       0,     0,   340,   340,     0,     0,     0,   340,   340,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1300,     0,     0,     0,
       0,     0,     0,     0,     0,  1304,  1305,  1306,  1307,     0,
       0,     0,     0,  1312,  1313,     0,     0,     0,     0,     0,
    1898,     0,     0,  1321,  1903,     0,     0,     0,     0,  1908,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     1,     0,  1335,   146,  1336,     0,     0,   119,
       0,     0,     0,     0,  1938,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,  1965,     0,  1392,
       0,  1968,     0,     0,     0,     0,     0,     0,     0,   124,
     124,     0,     0,     0,  1982,     0,     0,     0,     0,   201,
       0,     0,     0,     0,     0,  1406,     0,     0,     0,     0,
       0,     0,  1410,     0,  1412,  1414,     0,     0,  2004,     0,
    2006,  2007,     0,     0,     0,     0,  1423,     0,  1424,     0,
    1425,     0,  1427,     0,     0,     0,   606,  1435,     0,     0,
       0,     0,     0,  2016,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,   291,     0,
       0,     0,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2039,
       0,   114,     0,   120,     0,     0,     0,     0,     0,  2044,
     119,     0,     0,     0,     0,     0,     0,     0,  1478,     0,
       0,   120,     0,     0,     0,  1485,  1486,     0,     0,   114,
     606,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,   120,  2070,     0,  2044,     0,     0,     0,     0,  1509,
       0,     0,     0,     0,     0,     0,  1514,     0,     0,     0,
    1515,     0,     0,  2081,     0,     0,     0,   114,     0,  2070,
       0,     0,     0,     0,     0,     0,   119,     0,     0,  2089,
     291,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     223,     0,     0,     0,   529,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   291,     0,     0,     0,     0,     0,
       0,     0,     0,   340,   291,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   560,   564,
    1608,     0,     0,     0,     0,   571,   572,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,   582,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1630,     0,     0,     0,
       0,   601,     0,     0,  1635,     0,  1637,     0,     0,     0,
       0,     0,     0,   124,     0,   120,   120,   120,   120,   120,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   120,     0,     0,     0,     0,
       0,   124,     0,  1664,  1665,     0,     0,   690,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1670,  1671,
       0,  1672,     0,     0,     0,     0,     0,     0,     0,     0,
    1676,     0,     0,     0,     0,     0,     0,     0,   731,     0,
    1681,  1682,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   769,     0,     0,     0,   772,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   396,     0,     0,     0,     0,     0,   794,     0,     0,
       0,   795,   796,     0,     0,   799,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     813,   814,   815,   816,     0,     0,     0,   684,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   838,
       0,     0,     0,     0,   167,     0,     0,   841,     0,     0,
       0,     0,     0,     0,     0,   124,   124,   124,   124,   124,
     124,     0,     0,  1768,  1769,     0,     0,     0,     0,     0,
     167,     0,   120,     0,  1776,   291,     0,     0,     0,     0,
       0,     0,     0,     0,   124,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   879,   120,     0,  1799,
    1800,     0,     0,   560,     0,     0,   167,     0,     0,   885,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,   902,   907,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
     124,     0,   360,     0,     0,     0,   844,   846,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     659,     0,   120,   396,   664,     0,   360,     0,     0,     0,
       0,     0,     0,   673,   674,     0,     0,  1867,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   396,   396,
     949,     0,     0,     0,     0,     0,  1876,     0,     0,  1877,
    1878,     0,     0,     0,   167,     0,  1880,     0,   167,   396,
       0,   167,   167,     0,     0,   167,     0,     0,   167,   167,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,   396,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,  1012,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,  1029,     0,     0,     0,  1030,   167,
       0,   684,   167,     0,     0,     0,   684,   902,     0,     0,
       0,     0,     0,     0,     0,   684,     0,     0,     0,     0,
       0,   124,     0,   167,   167,     0,     0,     0,     0,  1070,
       0,     0,     0,     0,   684,   124,     0,     0,  1079,   167,
       0,     0,     0,     0,  1082,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1990,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
    1004,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,  2029,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2046,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,  2055,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1235,     0,  2068,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     167,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   396,
     396,   396,   396,   396,   396,   396,   396,   396,   396,   396,
     396,   396,   396,   396,   396,   396,   396,   396,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1889,
       0,     0,     0,     0,  1283,     0,     0,     0,  1284,     0,
       0,     0,     0,     0,     0,   902,     0,     0,     0,     0,
       0,     0,     0,     0,   360,  1297,     0,     0,     0,     0,
       0,     0,  1298,     0,     0,     0,     0,     0,     0,     0,
       0,  1302,     0,  1303,     0,     0,     0,   369,     0,   396,
       0,   370,     0,   371,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,   167,   167,     0,     0,     0,
     372,     0,     0,     0,   202,  1331,     0,     0,   167,  1332,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,   146,     0,     0,     1,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,     0,
     382,   383,     0,     0,     0,     0,     0,   124,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     202,     0,     0,     0,     0,     0,     0,     0,   384,     0,
       0,    76,   385,     0,     0,   202,     0,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,     0,     0,     0,     0,     0,  1422,   396,     0,
       0,     0,   451,     0,     0,     0,     0,   167,   167,     0,
       0,     0,     0,   167,     0,     0,   396,     0,     0,     0,
       0,   396,  1446,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   396,     0,   167,     0,     0,   167,   167,     0,
     167,     0,   167,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   202,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   347,     0,     0,  1216,     0,
       0,     0,     0,     0,   396,     0,    14,    15,    16,    17,
      18,   167,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   444,   347,     0,     0,     0,     0,     0,     0,     0,
       0,   202,   369,     0,     0,  1518,   370,     0,   371,  1519,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   202,     0,   510,    58,   372,     0,     0,     0,     0,
     510,     0,     0,     0,     0,     0,     0,     0,     0,  1554,
     167,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,   374,   167,   375,     0,   376,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   377,   378,   366,
       0,   379,   380,   381,     0,   382,   383,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,  1618,
       0,   396,  1621,     0,     0,     0,     0,   510,     0,     0,
       0,     0,     0,   384,     0,     0,    76,   385,     0,   202,
    1632,     0,     0,   386,   447,    79,   387,   388,   389,   390,
       0,   347,   616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   202,
       0,     0,   637,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1563,  1663,     0,  1566,  1580,     0,     0,     0,   396,  1587,
    1668,     0,     0,  1591,  1669,  1593,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,  1673,  1674,
       0,     0,     0,     0,     0,   369,     0,     0,     0,   370,
       0,   371,     0,   396,   396,   396,     0,     0,     0,     0,
     396,   396,   510,     0,   202,   202,     0,     0,   372,     0,
     451,     0,     0,     0,     0,     0,   167,     0,   510,   765,
       0,   510,   768,   167,   396,     0,   167,     0,     0,   347,
       0,     0,     0,   616,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,     0,   382,   383,
       0,   396,   396,   202,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,   510,     0,     0,     0,   510,     0,
       0,     0,   451,     0,     0,     0,   384,   972,  1556,    76,
     385,     0,     0,     0,  1762,  1763,   386,    78,    79,   387,
     388,   389,   390,     0,     0,   202,     0,  1683,     0,     0,
     347,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   202,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1716,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,  1722,     0,     0,     0,     0,     0,   167,   167,     0,
     510,     0,     0,   347,     0,     0,  1737,  1739,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   897,   347,     0,     0,     0,     0,     0,     0,     0,
    1566,     0,   616,     0,     0,     0,   616,     0,     0,     0,
       0,     0,     0,   915,     0,   347,     0,     0,     0,     0,
       0,     0,   451,     0,     0,   167,     0,     0,     0,     0,
       0,     0,  1852,     0,   167,     0,     0,   167,     0,   167,
     167,     0,     0,     0,     0,     0,   202,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1621,   165,
       0,     0,     0,   451,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1879,     0,     0,     0,
     167,     0,     0,     0,     0,   451,   451,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1897,   451,     0,     0,     0,     0,     0,
       0,     0,  1826,     0,     0,     0,     0,     0,     0,     0,
       0,  1829,   347,  1831,     0,     0,  1836,  1840,     0,  1580,
    1925,   283,     0,  1926,  1846,     0,     0,   209,   510,   510,
       0,     0,     0,     0,   289,     0,   290,   167,   510,  1025,
       0,   510,  1028,   263,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,   616,     0,   616,   616,
     451,     0,     0,   396,     0,   616,     0,   202,     0,     0,
       0,     0,     0,     0,     0,   347,   347,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   209,   347,     0,     0,   311,   510,     0,
       0,     0,   510,     0,     0,     0,   510,  1096,   352,     0,
     510,  1100,     0,     0,     0,     0,     0,     0,  1103,     0,
       0,  1914,     0,   167,  2009,     0,  1919,  1921,     0,     0,
     202,     0,   209,     0,     0,     0,   511,   512,     0,     0,
     516,     0,     0,   519,   520,   464,     0,     0,   468,     0,
       0,     0,   167,     0,     0,     0,     0,     0,     0,     0,
     347,   510,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   167,     0,     0,     0,  1969,     0,
    1972,   616,     0,  1974,  1976,     0,     0,   209,  1979,  1981,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   598,   599,
     347,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   631,     0,   468,     0,     0,     0,
       0,     0,     0,   167,   209,     0,     0,     0,     0,     0,
       0,     0,     0,   451,     0,     0,     0,  2023,  2025,  2027,
     396,     0,     0,     0,   609,     0,   626,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2040,     0,
       0,     0,     0,     0,     0,     0,     0,   510,     0,     0,
       0,  2050,  2052,  2054,     0,     0,     0,     0,     0,     0,
     396,     0,     0,     0,   616,   616,     0,     0,     0,     0,
       0,   616,     0,     0,     0,     0,     0,     0,   688,     0,
       0,   167,   167,     0,     0,     0,     0,     0,   763,   360,
       0,     0,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   209,   347,     0,     0,     0,     0,   510,  1326,
       0,   510,  1330,     0,     0,     0,     0,     0,   202,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   202,     0,
       0,     0,   609,     0,     0,     0,     0,     0,   789,     0,
       0,     0,     0,     0,     0,   834,     0,     0,   396,     0,
     396,     0,     0,     0,     0,     0,   202,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,     0,   396,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   209,   209,     0,
       0,     0,     0,   464,     0,     0,     0,     0,     0,     0,
       0,   396,     0,   451,   451,     0,     0,     0,   347,     0,
       0,     0,     0,     0,   616,  1431,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   347,     0,     0,   167,
     913,   914,     0,     0,     0,     0,   352,     0,     0,   396,
       0,     0,     0,   921,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,   901,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     510,  1482,     0,     0,     0,     0,     0,     0,   609,   510,
    1491,     0,   616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,   347,     0,     0,     0,     0,   209,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   688,   202,   688,   688,     0,   688,     0,     0,
     173,   176,     0,     0,     0,     0,   688,     0,     0,   688,
     688,   688,     0,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,     0,  1018,  1019,     0,     0,     0,     0,  1023,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   464,     0,     0,     0,  1044,
       0,     0,  1047,  1048,     0,  1051,     0,  1053,  1054,     0,
       0,     0,     0,     0,     0,   298,     0,     0,   299,   209,
       0,     0,     0,   347,     0,     0,     0,     0,     0,     0,
       0,     0,   320,     0,     0,     0,   464,     0,     0,   202,
       0,     0,     0,     0,     0,     0,  1094,     0,     0,     0,
    1098,     0,     0,     0,     0,     0,     0,     0,   464,   464,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
       0,     0,     0,     0,     0,     0,     0,   464,     0,     0,
       0,   449,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   486,   477,     0,   477,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1201,  1202,     0,     0,   202,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1218,
       0,     0,     0,   510,     0,     0,     0,   537,     0,     0,
       0,     0,     0,   464,     0,     0,     0,   173,     0,   510,
     209,     0,     0,     0,     0,     0,     0,     0,   173,     0,
       0,     0,     0,   789,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   451,   451,     0,     0,     0,     0,
       0,   577,     0,     0,     0,   583,     0,     0,     0,     0,
       0,     0,   586,   588,     0,     0,     0,   595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   352,     0,     0,     0,     0,     0,   347,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     641,     0,     0,     0,     0,   320,     0,     0,   320,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1218,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   347,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   510,
     510,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1317,     0,     0,     0,   510,     0,     0,  1324,     0,
       0,  1328,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   792,   793,     0,
       0,     0,     0,     0,     0,     0,   464,     0,     0,     0,
       0,     0,     0,     0,   477,     0,     0,     0,     0,     0,
     477,     0,     0,     0,     0,   810,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     451,     0,     0,     0,     0,     0,   688,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   510,     0,
       0,     0,     0,     0,     0,     0,   510,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1429,     0,     0,     0,     0,     0,
     263,     0,  1438,  1439,     0,     0,     0,     0,     0,     0,
       0,     0,   878,     0,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,     0,     0,     0,     0,   320,     0,
       0,   609,     0,     0,     0,     0,     0,     0,     0,     0,
     347,   449,     0,     0,   510,  1955,     0,     0,   510,     0,
       0,     0,     0,     0,   909,     0,     0,     0,     0,   352,
    1480,     0,     0,   688,     0,     0,     0,     0,     0,  1489,
       0,     0,  1493,     0,  1496,  1497,     0,   641,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   510,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   943,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1523,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   464,   464,     0,   810,
     963,     0,     0,   965,     0,   967,     0,     0,     0,     0,
       0,   976,     0,   981,   976,     0,     0,     0,     0,     0,
       0,     0,   510,   510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   688,   688,   688,     0,   688,
     688,  1009,     0,     0,     0,     0,   468,     0,     0,     0,
       0,     0,  1615,     0,  1011,     0,     0,     0,     0,     0,
       0,     0,     0,   510,     0,  1020,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   449,
       0,     0,  1009,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1057,   263,     0,     0,     0,     0,     0,
    1069,     0,     0,     0,     0,     0,     0,     0,     0,  1073,
       0,     0,   477,     0,     0,     0,   352,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1493,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1678,     0,     0,
       0,     0,     0,     0,     0,  1127,  1433,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,   420,   641,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1208,
    1210,     0,     0,     0,     0,     0,     0,   449,     0,     0,
     369,     0,     0,     0,   370,     0,   371,     0,     0,  1224,
       0,     0,   209,   641,     0,     0,     0,     0,     0,     0,
       0,     0,    58,   372,     0,     0,   976,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1009,
       0,     0,     0,     0,     0,     0,   263,  1248,  1760,     0,
     373,   374,     0,   375,   976,   376,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   377,   378,   366,     0,   379,
     380,   381,     0,   382,   383,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   352,     0,     0,     0,     0,     0,     0,     0,
     477,   384,     0,     0,    76,   385,     0,     0,     0,     0,
       0,   386,  1434,    79,   387,   388,   389,   390,     0,     0,
       0,     0,   688,     0,     0,     0,  1811,  1812,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1816,     0,
       0,     0,     0,     0,     0,     0,     0,   464,   464,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   477,     0,  1316,
       0,  1319,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   263,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1373,
    1375,  1377,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1384,  1384,     0,
    1887,     0,     0,     0,     0,     0,     0,     0,     0,  1396,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1127,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -457,  -457,     0,  -457,    46,    47,     0,
    -457,  1426,     0,     0,   641,     0,     0,  1436,     0,     0,
       0,     0,     0,     0,  1953,   369,   477,    58,     0,   370,
       0,   371,     0,     0,     0,   449,   688,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,   477,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,   464,     0,     0,     0,   976,     0,     0,
     810,     0,     0,     0,     0,   373,   374,     0,   471,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,     0,   382,   383,
       0,     0,     0,     0,     0,     0,    73,     0,     0,    76,
     688,     0,     0,   468,     0,     0,     0,     0,     0,     0,
       0,     0,  1517,     0,     0,     0,   384,    75,     0,   472,
     473,     0,     0,     0,   474,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,     0,     0,  1553,
       0,     0,  1555,     0,     0,     0,     0,     0,     0,     0,
     976,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   477,     0,
       0,   810,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2069,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   641,   641,  1368,     0,     0,     0,
       0,     0,   963,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1633,  1634,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
       0,   370,     0,   371,     0,     0,     0,     0,   477,     0,
     810,     0,     0,     0,     0,     0,     0,     0,  1129,     0,
     372,    -2,     0,  1131,  -242,  -242,  1132,  1133,  1134,  1135,
    1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  -336,  1144,
    1145,  1146,  1147,  1148,     0,  1149,     0,   373,   374,     0,
     471,     0,   376,  1150,  1151,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,  1152,   379,   380,   381,     0,
     382,   383,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
       0,     0,     0,  1705,     0,  1712,     0,  -242,   384,     0,
       0,    76,   385,     0,     0,     0,   287,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,     0,
       0,     0,     0,  -183,     0,     0,  2069,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1368,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1751,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,   370,     0,
     371,     0,     0,     0,   641,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1129,  1777,   372,    -2,  1779,
    1131,  -243,  -243,  1132,  1133,  1134,  1135,  1136,  1137,  1138,
    1139,  1140,  1141,  1142,  1143,  -336,  1144,  1145,  1146,  1147,
    1148,     0,  1149,     0,   373,   374,     0,   471,     0,   376,
    1150,  1151,    65,    66,    67,    68,    69,    70,    71,   377,
     378,   366,  1152,   379,   380,   381,     0,   382,   383,     0,
       0,     0,     0,     0,     0,    73,  1758,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1368,  -243,   384,     0,     0,    76,   385,
       0,     0,     0,   287,     0,   386,    78,    79,   387,   388,
     389,   390,     0,     0,     0,     0,     0,     0,     0,     0,
    -183,     0,     0,     0,   369,     0,  1863,     0,   370,     0,
     371,     0,     0,     0,     0,     0,     0,   641,     0,     0,
       0,     0,     0,     0,     0,  1129,     0,   372,    -2,     0,
    1131,     0,     0,  1132,  1133,  1134,  1135,  1136,  1137,  1138,
    1139,  1140,  1141,  1142,  1143,  -336,  1144,  1145,  1146,  1147,
    1148,     0,  1149,     0,   373,   374,     0,   471,     0,   376,
    1150,  1151,    65,    66,    67,    68,    69,    70,    71,   377,
     378,   366,  1152,   379,   380,   381,     0,   382,   383,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   384,     0,     0,    76,   385,
     976,     0,     0,   287,     0,   386,    78,    79,   387,   388,
     389,   390,     0,     0,     0,     0,     0,     0,     0,     0,
    -183,     4,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1128,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
      47,   370,     0,   371,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1129,    58,
    1130,    -2,     0,  1131,     0,     0,  1132,  1133,  1134,  1135,
    1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  -336,  1144,
    1145,  1146,  1147,  1148,     0,  1149,     0,   373,   374,    61,
     471,     0,   376,  1150,  1151,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,  1152,   379,   380,   381,     0,
     382,   383,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -3,   384,     0,
       0,    76,   416,     0,     0,     0,   287,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,     0,
       0,     0,     0,  -183,     4,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,  1128,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,    47,   370,     0,   371,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,  1129,    58,  1130,    -2,     0,  1131,     0,     0,  1132,
    1133,  1134,  1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,
    1143,  -336,  1144,  1145,  1146,  1147,  1148,     0,  1149,     0,
     373,   374,    61,   471,     0,   376,  1150,  1151,    65,    66,
      67,    68,    69,    70,    71,   377,   378,   366,  1152,   379,
     380,   381,     0,   382,   383,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   384,     0,     0,    76,   416,     0,     0,     0,   287,
       0,   386,    78,    79,   387,   388,   389,   390,     0,     0,
       0,     0,     0,     0,     0,     0,  -183,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,    47,   370,     0,   371,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   372,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,   374,    61,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,     0,   382,   383,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1574,  1575,
    1576,     0,     0,     0,   384,  1577,  1578,    76,   416,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,     0,     0,     0,     0,  1579,
       4,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,    47,
     370,     0,   371,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,   374,    61,   375,
       0,   376,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   377,   378,   366,     0,   379,   380,   381,     0,   382,
     383,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1574,  1575,  1576,     0,     0,     0,   384,  1577,     0,
      76,   416,     0,     0,     0,     0,     0,   386,    78,    79,
     387,   388,   389,   390,     0,     0,     0,     0,     0,     0,
       0,     0,  1579,     4,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   369,
       0,    46,    47,   370,     0,   371,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
     374,    61,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,     0,   382,   383,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     384,     0,  1565,    76,   416,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,    47,   370,     0,   371,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   372,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,   374,    61,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,     0,   382,   383,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   384,     0,     0,    76,   416,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,    47,
     370,     0,   371,   327,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,   374,     0,   375,
       0,   376,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   377,   378,   366,     0,   379,   380,   381,     0,   382,
     383,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   384,     0,     0,
      76,   446,     0,     0,     0,     0,     0,   386,   447,    79,
     387,   388,   389,   390,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   369,
       0,    46,    47,   370,     0,   371,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,     0,   382,   383,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     384,     0,     0,    76,  1205,     0,     0,     0,     0,     0,
     386,  1206,    79,   387,   388,   389,   390,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,    47,   370,     0,   371,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,   374,     0,   375,     0,   376,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   377,   378,   366,
       0,   379,   380,   381,     0,   382,   383,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   384,     0,     0,    76,   385,     0,     0,
       0,     0,     0,   386,    78,    79,   387,   388,   389,   390,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   369,     0,    46,    47,   370,
       0,   371,   327,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   372,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,     0,   382,   383,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   384,     0,     0,    76,
     446,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,  1896,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,  1924,     0,    -2,    -2,    -2,
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
       0,     0,     0,     0,     0,    -2,    -2,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    61,    62,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,   251,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,   147,    46,    47,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   252,     0,
       0,     0,  -777,     0,     0,    78,    79,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -389,  -389,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -389,     0,     0,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,     0,     0,     0,
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
       0,     0,     0,     0,     0,    78,    79,   251,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,     0,
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
    1344,     0,     0,     0,     0,    78,    79,  1345,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1346,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1347,     0,     0,     0,
      76,   939,     0,  1344,     0,     0,     0,     0,    78,    79,
    1345,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,  1346,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1529,
       0,     0,     0,    76,   939,     0,  1344,     0,     0,     0,
       0,    78,    79,  1345,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1346,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1530,     0,     0,     0,    76,   939,     0,  1344,
       0,     0,     0,     0,    78,    79,  1345,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1346,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1531,     0,     0,     0,    76,
     939,     0,     0,     0,     0,     0,     0,    78,    79,   251,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -457,  -457,     0,  -457,    46,    47,   251,
    -457,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    58,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -457,  -457,     0,  -457,    46,    47,     0,
    -457,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     252,    63,    64,     0,     0,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     309,     0,     0,     0,     0,     0,     0,    78,    79,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,
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
       0,     0,     0,  -781,     0,     0,    78,    79,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,     0,
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
       0,     0,     0,     0,    46,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,  1064,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -643,    76,   329,     0,     0,
       0,     0,     0,     0,    78,    79,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   327,    49,
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
       0,     0,    46,    47,     0,     0,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,  1795,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   329,     0,     0,     0,     0,
       0,     0,    78,    79,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,  1797,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   329,     0,     0,     0,     0,     0,
       0,    78,    79,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,   327,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   309,     0,     0,     0,     0,     0,     0,
      78,    79,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
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
       0,    76,   329,     0,     0,     0,     0,     0,     0,    78,
      79,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -457,  -457,     0,  -457,    46,    47,
       0,  -457,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -456,  -456,     0,  -456,    46,    47,
       0,  -456,    63,    64,     0,     0,     0,     0,  1368,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
       0,     0,     0,   370,     0,   371,     0,     0,     0,     0,
      76,   252,     0,     0,     0,     0,     0,     0,    78,    79,
    1129,     0,   372,     0,     0,  1131,  1819,  1820,  1132,  1133,
    1134,  1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,
    -336,  1144,  1145,  1146,  1147,  1148,     0,  1149,     0,   373,
     374,     0,   471,     0,   376,  1150,  1151,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,  1152,   379,   380,
     381,     0,   382,   383,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,  1368,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     384,     0,     0,    76,   385,     0,     0,     0,   287,     0,
     386,    78,    79,   387,   388,   389,   390,   369,     0,     0,
       0,   370,     0,   371,     0,  -183,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1129,     0,
     372,     0,     0,  1131,     0,     0,  1132,  1133,  1134,  1135,
    1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  -336,  1144,
    1145,  1146,  1147,  1148,     0,  1149,     0,   373,   374,     0,
     471,     0,   376,  1150,  1151,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,  1152,   379,   380,   381,     0,
     382,   383,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   384,     0,
       0,    76,   385,     0,     0,     0,   287,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,     0,
       0,     0,     0,  -183,    14,    15,    16,    17,    18,    19,
     675,    20,   676,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,    47,   370,     0,   371,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   677,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,   374,     0,   375,     0,   376,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   377,   378,   366,     0,   379,
     380,   381,     0,   382,   383,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   384,     0,     0,    76,   678,     0,     0,     0,   287,
       0,   386,    78,    79,   679,   680,   389,   390,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   369,     0,    46,    47,   370,     0,
     371,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   372,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,   374,     0,   375,     0,   376,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   377,
     378,   366,     0,   379,   380,   381,     0,   382,   383,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   384,     0,   415,    76,   416,
       0,     0,     0,     0,     0,   386,    78,    79,   387,   388,
     389,   390,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,    47,   370,     0,   371,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,   374,
       0,   375,     0,   376,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   377,   378,   366,     0,   379,   380,   381,
       0,   382,   383,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   384,
       0,     0,    76,   678,     0,     0,     0,   287,     0,   386,
      78,    79,   387,   388,   389,   390,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,    47,   370,     0,   371,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,   374,     0,   375,     0,   376,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   377,   378,   366,
       0,   379,   380,   381,     0,   382,   383,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   384,     0,     0,    76,   416,     0,     0,
       0,     0,     0,   386,    78,    79,   387,   388,   389,   390,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,    47,
     370,     0,   371,   327,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,   374,     0,   375,
       0,   376,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   377,   378,   366,     0,   379,   380,   381,     0,   382,
     383,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   384,     0,     0,
      76,   446,     0,     0,     0,     0,     0,   386,    78,    79,
     387,   388,   389,   390,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,    47,   370,     0,   371,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,   374,     0,   375,     0,   376,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   377,   378,   366,     0,   379,
     380,   381,     0,   382,   383,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   384,     0,     0,    76,   385,     0,     0,     0,     0,
       0,   386,    78,    79,   387,   388,   389,   390,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   666,     0,     0,   667,
     668,   565,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,   -16,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   251,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    63,    64,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,    76,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,    77,     0,     0,
       0,  -779,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   863,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -656,    76,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1713,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    76,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -457,  -457,     0,  -457,    46,    47,     0,  -457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   309,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,    76,     0,    46,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1449,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   945,    76,   939,
       0,     0,    63,    64,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   939,     0,     0,     0,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   294,     0,     0,    63,    64,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,    77,     0,     0,     0,     0,     0,     0,
      78,    79,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,    63,    64,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   442,     0,     0,    63,    64,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   329,     0,     0,     0,     0,
       0,     0,    78,    79,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,     0,     0,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,    63,    64,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   294,     0,     0,    63,    64,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   442,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   309,     0,     0,
      63,    64,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   939,
       0,     0,     0,     0,     0,     0,    78,    79,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
       0,   327,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
      63,    64,     0,   327,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   939,
       0,     0,    63,    64,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    14,    15,    16,    17,    18,    78,    79,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -457,  -457,     0,
    -457,    46,    47,     0,  -457,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    58,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -457,  -457,     0,
    -457,    46,    47,     0,  -457,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   309,    63,    64,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,    76,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,     0,     0,     0,
      63,    64,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -457,  -457,     0,  -457,
      46,    47,     0,  -457,     0,     0,     0,     0,    76,   369,
       0,     0,     0,   370,     0,   371,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,     0,   382,   383,     0,   369,     0,     0,     0,   370,
      73,   371,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,  1574,  1575,  1576,     0,   372,     0,
     384,  1738,     0,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,  1834,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,     0,   382,   383,
       0,   369,     0,     0,     0,   370,    73,   371,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1574,  1575,  1576,     0,   372,     0,   384,  1835,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,  1251,     0,    76,   385,     0,     0,     0,
    1252,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   384,     0,     0,    76,   385,     0,
       0,     0,   474,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   384,   809,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   384,     0,
       0,    76,   385,     0,     0,     0,   287,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,   972,     0,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,     0,     0,    76,   385,     0,     0,  1003,
       0,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   384,     0,     0,    76,   385,     0,
       0,     0,  1226,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   384,  1318,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   384,     0,
       0,    76,   385,     0,     0,     0,  1378,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,     0,  1825,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,  1830,     0,    76,   385,     0,     0,     0,
       0,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   384,  1839,     0,    76,   385,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   384,  1918,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   384,  1920,
       0,    76,   385,     0,     0,     0,     0,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,  1971,     0,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,  1973,     0,    76,   385,     0,     0,     0,
       0,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   384,  1975,     0,    76,   385,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   384,  1978,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   384,  1980,
       0,    76,   385,     0,     0,     0,     0,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,  2022,     0,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,  2024,     0,    76,   385,     0,     0,     0,
       0,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   384,  2026,     0,    76,   385,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   384,  2049,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   384,  2051,
       0,    76,   385,     0,     0,     0,     0,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,  2053,     0,    76,   385,     0,     0,     0,     0,     0,
     386,    78,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,     0,     0,    76,   385,     0,     0,     0,
       0,     0,   386,    78,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,   369,   382,   383,     0,   370,
       0,   371,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,   658,     0,     0,    76,   385,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,     0,     0,     0,     0,   373,   374,     0,   375,     0,
     376,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     377,   378,   366,     0,   379,   380,   381,   369,   382,   383,
       0,   370,     0,   371,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,     0,     0,   663,     0,     0,    76,
     385,     0,     0,     0,     0,     0,   386,    78,    79,   387,
     388,   389,   390,     0,     0,     0,     0,   373,   374,     0,
     375,     0,   376,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   377,   378,   366,     0,   379,   380,   381,   369,
     382,   383,     0,   370,     0,   371,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,     0,     0,   672,     0,
       0,    76,   385,     0,     0,     0,     0,     0,   386,    78,
      79,   387,   388,   389,   390,     0,     0,     0,     0,   373,
     374,     0,   375,     0,   376,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   377,   378,   366,     0,   379,   380,
     381,   369,   382,   383,     0,   370,     0,   371,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,     0,     0,
     384,     0,     0,    76,   385,     0,     0,     0,     0,     0,
     386,   877,    79,   387,   388,   389,   390,     0,     0,     0,
       0,   373,   374,     0,   375,     0,   376,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   377,   378,   366,     0,
     379,   380,   381,   369,   382,   383,     0,   370,     0,   371,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,   384,     0,     0,    76,   385,     0,     0,     0,
       0,     0,   386,   447,    79,   387,   388,   389,   390,     0,
       0,     0,     0,   373,   374,     0,   375,     0,   376,  1913,
      64,    65,    66,    67,    68,    69,    70,    71,   377,   378,
     366,     0,   379,   380,   381,     0,   382,   383,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   384,     0,     0,    76,   385,     0,
       0,     0,     0,     0,   386,    78,    79,   387,   388,   389,
     390,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,   182,
      46,    47,   183,   184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   666,     0,     0,   667,   668,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -457,  -457,     0,  -457,    46,    47,     0,  -457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,     1,   180,     4,     4,   164,    74,    74,   152,     1,
     250,    83,   291,   704,   384,   689,     4,   264,   164,    59,
    1138,    74,   216,   180,   228,   886,   179,   474,    74,   231,
     643,   624,   615,   229,  1370,   778,   582,   175,  1688,    96,
    1688,   784,   978,   534,   165,   523,   524,  1370,  1371,   228,
    1688,   228,    76,   544,   140,    56,    57,   332,    59,    59,
     611,   228,   611,   872,     1,  1757,   775,    59,   611,    74,
       1,  1007,     1,    74,   228,     4,   238,   149,   775,   201,
      74,   228,    83,  1224,  1302,  1303,   856,   228,   748,   347,
      91,    83,    74,   351,   585,    96,   298,   299,    99,   303,
     262,   245,   103,   103,    96,   682,   146,    99,   304,   777,
     272,   103,   330,   883,   200,   103,    71,   384,    60,    61,
       1,  1584,    59,   190,   303,    74,   303,   777,    59,   347,
      59,   775,    71,   351,  1070,    96,   303,   190,     1,   775,
     141,   795,    74,   144,   190,   146,   146,  1819,   131,   303,
      88,   152,   324,     4,   146,   121,   303,   158,   132,   813,
     228,   228,   303,     1,   165,   150,   103,   645,     0,    88,
      99,    74,   229,   158,   103,   228,  1823,  1260,  1689,   454,
     153,   253,   228,   158,     1,   190,    89,   153,   189,   190,
     173,   363,   166,   175,   777,   617,   190,   189,    97,   154,
     175,   174,     1,   204,    57,    56,    57,    60,    61,   146,
      63,   150,   204,   214,   175,   146,   158,   146,   219,   872,
     158,    59,  1122,   228,   775,   879,   775,   228,   229,     0,
     448,    88,   775,   175,   228,   303,   303,   229,   310,   158,
      91,   190,    59,   175,   245,   936,     0,   304,   794,   795,
     303,   150,   253,    76,    77,   132,   640,   303,   282,   506,
      59,   253,   263,   164,   154,   266,   116,   813,    83,   159,
    1733,  1607,   273,   964,   266,   156,   157,   755,   158,   228,
     893,    83,   283,   284,  1607,   286,   480,  1959,   492,   166,
     141,  1034,   571,   144,   157,   175,  1807,   150,   150,  1440,
    1580,   158,   303,   304,  1112,   501,   158,   158,   146,  1117,
     311,   150,   304,   492,   165,   492,   929,   318,   319,  1122,
       1,  1130,   323,   361,  1033,   492,   364,   910,   603,   146,
    1122,   452,   702,   879,   149,   158,  1033,   266,   492,    76,
      77,  1991,   600,  1991,  1114,   492,   418,   146,   150,  2041,
    1159,   492,   627,  1991,   291,   356,  1016,   779,   444,   634,
     361,   783,   853,   364,   365,  2012,    71,   150,   219,   152,
     792,   793,  1040,   188,   578,   150,   953,   521,    59,   637,
    1188,   543,   600,   527,   580,  1468,  1469,  1470,  1962,  1033,
    1040,   658,   116,   660,   661,    20,   663,  1033,   150,   578,
    2047,   578,   150,   949,   150,   672,   624,   529,   675,   676,
     677,   578,   263,  1736,  1988,    88,   800,  1928,    99,   637,
      71,   158,   273,   582,   578,   492,   174,   428,  1082,   330,
     154,   578,   283,   284,   150,   286,   428,   578,   253,   492,
    2014,  1341,  1342,  1343,   501,   150,   492,   150,   472,   154,
     451,   452,    71,   291,   156,  1673,  1674,  1040,  1738,  1739,
     311,  1579,   463,   464,   166,   146,  1584,   318,   319,   147,
    1213,   472,   323,   474,   291,    71,   155,  1130,   599,   601,
     158,  1992,  1033,   156,  1033,   158,    74,   492,   105,   106,
    1033,   492,   291,   273,   237,   310,   174,   240,   492,   150,
     501,   175,    90,   154,   284,   356,  1159,    71,    71,   501,
     361,   578,   132,   364,   201,   330,    71,   149,   261,   107,
     521,  2032,    71,   580,   156,   578,   527,  1463,   271,   156,
    1338,   150,   578,   794,   795,   154,  1082,    71,  1341,  1342,
    1343,   161,   162,   492,   584,   175,   930,   448,  1291,  1341,
    1342,  1343,   813,   156,   150,  1835,  1836,   152,   154,   156,
       3,   156,    71,  1010,   565,    71,   567,    13,    14,    15,
      16,    17,   175,   153,  1897,    71,   729,   578,   175,   580,
      71,   105,   106,   584,   584,   266,   150,   150,   580,   864,
     154,   154,   584,   594,     3,   150,  1270,   598,   599,   154,
     451,   150,  1925,   418,    71,   154,   154,  1220,   802,   731,
     788,   159,   463,   464,  1207,  1733,   150,    71,   879,    71,
     154,   799,   126,   127,   899,    71,   156,   156,   156,   153,
     631,   788,   569,   448,   571,  1057,   166,   166,   156,   897,
    1963,   150,   643,   330,   150,   154,   333,   584,   154,   156,
     772,   332,    71,   584,   150,   584,   777,   175,   154,   150,
     347,  1879,   150,   154,   351,   152,   170,   171,   175,  1360,
     157,   815,   794,   795,   156,   156,   158,  1538,   156,   897,
     156,   582,   835,   150,   156,   150,   132,   154,   166,  1235,
     834,   813,   157,   694,   175,   696,   150,   698,   150,   175,
     154,   702,   154,   175,   705,   156,   730,  1381,   523,   524,
     144,   145,   146,   158,   565,  1833,   156,  1370,  1371,  1197,
    1656,   156,  1658,   624,   175,   156,   156,  1845,     3,   730,
     151,   166,   166,   571,   152,   175,   154,   158,    13,    14,
      15,    16,    17,   594,   175,   175,   584,   598,   599,   927,
     156,   161,   105,   106,   571,   158,  1302,   879,   168,   169,
     166,   448,    13,    14,    15,    16,    17,   584,   149,   174,
     927,   514,   571,   454,   775,   156,   777,   152,   152,  1226,
     631,   158,   157,   157,   601,   584,  1460,   150,   789,   152,
     949,   154,   643,   536,  1912,   796,    71,   152,   150,   542,
     921,   802,   157,   546,   805,  1252,   152,  1420,  1421,   624,
     144,   145,   146,   814,   815,   816,  1200,   163,   164,   150,
      71,  1082,  1069,   153,   154,  1003,     3,   157,    47,    48,
     645,    50,   166,   834,   172,    54,    13,    14,    15,    16,
      17,   175,   529,   694,  1228,   696,    57,   698,   150,    60,
      61,   702,    63,   116,   705,   152,   153,     4,     5,     6,
       7,     8,     9,    10,    11,    12,   709,   710,   711,   870,
     871,   872,   872,    62,   152,   152,  1422,   156,   156,   730,
     130,   132,   150,  1077,   152,   885,   154,   568,   152,    83,
     163,   164,   893,  1277,    71,   582,   152,   885,   152,  1101,
     150,  1079,    96,   584,   154,    99,  1529,  1530,  1531,   103,
      99,   161,   162,   600,   601,   124,   125,   152,  1039,  1040,
     921,   110,   152,   112,   925,   114,   156,   130,   929,   150,
     152,  1378,    22,   154,   935,   872,  1211,   624,   789,   156,
     755,   872,    97,   872,   152,   796,   627,   150,   156,   150,
     637,   154,   150,   152,  1607,   772,   885,   156,   161,   162,
    1082,   128,   129,   814,   153,   816,   155,   156,   156,   970,
     152,   174,  1239,   158,   156,  1345,  1216,   978,   161,   162,
       4,     5,     6,     7,     8,     9,    10,    11,    12,  1207,
     155,   156,   130,   150,   130,   189,   109,   110,   111,   112,
     113,    10,   156,    62,   152,   152,  1007,   150,   156,  1010,
     204,  1458,   150,   152,   150,   204,   154,   156,   154,   870,
     871,   872,    10,   161,   162,   161,   162,  1640,   175,   150,
     156,   152,  1033,   154,   872,   229,   150,   152,  1039,  1040,
      64,   156,   893,   150,   731,   104,    88,  1191,   949,   108,
    1122,   149,   111,  1437,   113,   872,   158,   102,  1442,   253,
    1198,  1199,   107,   108,   109,   110,   111,   112,   113,  1070,
     921,   886,   266,   872,   925,  1459,  1235,   266,   929,   268,
     269,   155,   156,  1736,   152,   772,  1333,   158,   150,   155,
     152,  1370,   154,   102,   158,  1273,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   158,   151,   296,   158,   154,
     150,   150,   855,   302,   102,   154,   174,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   152,  1673,   155,   156,
    1130,  1132,  1310,  1311,  1135,  1136,  1137,    13,    14,    15,
      16,    17,   150,   332,   150,   116,   154,   150,   154,   338,
     167,   340,   152,  1310,  1311,  1351,   156,  1303,  1159,  1159,
    1773,   152,   152,   150,  1165,   156,   156,   154,   162,  1860,
     152,   150,  1173,   156,   156,  1176,  1177,  1177,   152,  1180,
    1302,  1303,   156,   864,  1176,  1177,   375,   160,   152,  1177,
    1191,   872,   156,  1130,   875,    71,   152,   256,   130,  1130,
     156,  1130,   144,   145,   146,  1589,   172,   152,   152,   152,
     897,   156,   156,   900,   156,   144,   145,   146,   899,  1220,
     155,   156,  1159,   153,   166,  1226,   152,   156,  1159,   152,
    1159,   152,   152,   175,   428,  1236,   156,   166,   152,   428,
    1177,  1625,  1626,   152,  1897,   132,   175,  1176,  1177,   155,
     156,  1252,    13,    14,    15,    16,    17,    18,   154,  1260,
     152,   155,   949,  1422,   156,   454,  1650,   456,   457,  1341,
    1342,  1343,  1925,  1345,  1346,   334,   335,   132,   337,   152,
     339,  1132,   471,   156,  1135,  1136,  1137,   716,   717,   718,
     719,  1292,  1130,   102,  1351,   132,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   152,  1207,   501,  1159,   156,
    1963,   152,   501,  1130,  1165,   156,   375,   155,   156,   152,
     152,  1159,  1173,   156,   156,   156,  1996,   157,  1607,  1180,
    2000,  1130,   521,   220,  1235,   156,   157,   526,  1542,   528,
    1536,   157,  1159,   150,  1344,  1119,  1120,  1121,  1544,   152,
    1351,   155,   156,   152,  1355,   152,  1344,  1358,   152,   548,
    1159,   550,   551,  1542,   150,  1542,   175,   155,   156,  1220,
    1370,  1371,   155,   156,   152,  1542,   152,  1378,   155,   568,
     154,  1648,  1197,   155,   156,  1236,   155,   156,  1542,    69,
    1774,   580,  1207,    77,   158,  1542,   158,  1398,   158,  1400,
     158,  1542,   155,   156,   155,   292,   150,   560,  1400,   155,
     156,    77,   471,   155,   603,  1344,   605,   606,   102,  1420,
    1421,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     155,   156,  1554,  1370,  1371,   155,   156,    18,   627,   628,
     174,  1370,  1371,   155,   156,   634,  1446,   155,   156,  1130,
     155,   156,   156,  1606,   155,   156,   174,  1458,  1446,  1618,
     156,   157,  1463,   155,   156,   155,   156,  1468,  1469,  1470,
     158,  1400,  1618,   150,  1542,    76,    77,   175,  1159,  1536,
     158,   540,   152,   372,   156,   157,  1870,  1544,  1261,  1262,
     152,   175,   712,   713,    13,  1176,   175,   384,   158,  1703,
     714,   715,   720,   721,  1355,  1549,  1550,  1358,   397,   398,
    1706,    13,    14,    15,    16,    17,    18,  1446,  1198,  1199,
    1207,  1422,   155,   155,  1703,    18,  1703,   149,  1674,   418,
    1211,   152,  1370,  1371,   152,  1536,  1703,   152,   152,   152,
     152,  1542,   152,  1544,   152,   149,   152,  1398,  1235,  1703,
    1551,  1673,  1674,  1370,  1371,   158,  1703,    69,   158,   448,
     158,   152,  1703,   174,  1565,   152,   175,   152,    87,  1420,
    1421,  1370,  1371,  1573,   149,   158,   152,  1578,   119,   152,
     121,   122,   123,   102,   156,  1573,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   102,   152,   152,   156,  1752,
     107,   108,   109,   110,   111,   112,   113,  1607,   152,   150,
     152,   149,   153,   154,  1615,   152,   152,   158,   159,  1763,
    1816,  1621,   152,   152,   155,   152,   513,   155,   152,   152,
     152,   518,   152,  1621,  1852,   152,  1573,   152,   152,  1640,
     174,  1819,   155,  1572,  1573,   149,   152,   152,   535,  1706,
      14,   152,   152,   156,  1929,  1656,   150,  1658,   545,   150,
     150,   150,  1819,    13,    14,    15,    16,    17,    18,   150,
    1607,   150,   157,    73,   156,   864,   175,    90,  1607,   157,
      13,    14,    15,    16,    17,   155,   875,   175,   841,  1370,
    1371,   841,  1621,    13,    14,    15,    16,    17,    18,   155,
    1551,   175,  1703,  1856,   175,  1706,   158,   158,  1926,   149,
     899,   149,   158,   156,  1565,   175,  1717,  1618,   152,  1400,
    1721,   910,   155,  1538,   152,   156,   156,  1578,   152,   156,
     919,   152,   155,  1879,  1735,  1422,  1736,   152,    71,   149,
     149,    79,   150,   175,  1745,   150,   175,   150,   149,   902,
     175,   150,   902,   175,   152,   175,   175,  1879,   175,  1816,
     149,  1762,  1763,   175,  1615,   149,   156,   149,   175,  1607,
     156,   658,  1773,   152,   155,   155,   663,   155,   155,  1957,
    1852,  1959,   158,   149,  1928,   672,   157,  1991,   157,  1640,
    1607,   152,   119,   149,  1723,   152,  1992,   130,   152,  1736,
    1957,   152,  1959,   152,   691,   152,   149,  1736,  1607,   155,
       1,   155,  1991,     4,  1991,  1816,   175,   150,   157,   708,
    1998,   154,  1823,   152,  1991,   156,  1827,  1016,   161,   162,
     152,  1832,   150,   156,   150,   150,  2032,  1991,   108,   158,
     152,  1998,   155,  1996,  1991,   155,   155,  2000,  2001,   214,
    1991,  1040,  2030,   149,  1926,   149,  1857,   149,   155,  1012,
      74,   152,  1012,   152,   152,   152,  1717,   152,    59,    74,
    1721,   175,   175,   149,   175,   150,  1029,  1030,   152,  1029,
    1030,  2034,    89,    74,  1735,   152,   149,   149,   149,   155,
     155,  1572,    83,   149,  1745,   152,   154,  1897,  1736,  1900,
    2078,   152,   152,  1904,   152,    96,  2059,   152,    99,   152,
    2063,  1762,   103,    74,   166,   153,  1917,   166,    74,  1736,
     175,  2078,  1773,  1991,  1991,  1925,  1607,  1928,   175,  1930,
     157,   175,  2085,   152,   149,  1992,   152,  1736,  1991,   152,
    1941,   152,  1943,  1944,   156,  1991,   149,   151,   166,   140,
     149,   157,   102,   840,   150,   146,   166,    74,   149,   156,
    1897,   152,   153,  1963,   851,  1966,   150,   854,  1897,   149,
     151,   858,  1823,   164,   166,  2032,  1827,   166,   155,   108,
     175,  1832,  1176,  1177,   108,   175,   151,  1176,  1925,   152,
    1991,  1992,   152,   157,   149,   149,  1925,   188,   189,   190,
    1992,  2002,   150,   152,   175,    74,  1857,   152,   152,   200,
     201,  2012,   386,   204,  1648,  1360,   175,   175,  1267,   722,
     681,   723,  1211,  1148,   724,   417,  1963,   726,  1217,  1159,
     725,  2032,  2047,  1607,  1963,  1959,  1744,   228,   229,  1736,
    2032,  1988,  1723,  2042,  2045,  1861,  2047,  1599,  2041,  1900,
    2029,  1926,  1599,  1904,   245,  1736,   945,  2001,  1925,  1897,
    2063,   950,   253,  1180,    49,  2066,  1917,  1536,  1887,  1816,
     258,  2072,   961,  1550,  1346,   266,  1173,   481,   802,  1930,
    1897,  2082,  1446,   889,   594,  2086,  1621,  1925,     0,   935,
    1941,   747,  1943,  1944,   747,  2096,   747,    -1,  1897,    -1,
      -1,    -1,    -1,    -1,    -1,   296,    -1,   472,  1925,   474,
      -1,   302,   303,   304,    -1,  1966,    -1,    -1,    -1,   310,
      -1,    -1,    -1,    -1,    -1,  1963,  1925,    -1,    -1,    -1,
    1283,  1284,    -1,  1283,  1284,    -1,    -1,    -1,    -1,   330,
     331,   332,    -1,    -1,  1297,  1298,  1963,  1297,  1298,    -1,
      -1,  2002,    -1,    -1,    -1,    -1,   347,    -1,    -1,    -1,
     351,  2012,    -1,  1352,  1963,  1852,     3,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,  1331,  1332,
     102,  1331,  1332,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,   384,  2045,    -1,  2047,    -1,    -1,    -1,
      -1,    62,    -1,    -1,    -1,    -1,  1400,    -1,    -1,    -1,
      -1,  1400,    -1,    -1,    -1,  2066,  1897,    -1,    -1,    -1,
      -1,  2072,    -1,    -1,    -1,    71,    -1,   418,    -1,  1116,
     421,  2082,    -1,    -1,    -1,  2086,   158,   428,    99,  1926,
      -1,  1130,  1129,    -1,  1925,  2096,    -1,    -1,    -1,   110,
      -1,    -1,    -1,   444,    -1,    -1,    -1,   448,    -1,  1146,
      -1,   452,    -1,   454,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    13,    14,    15,
      16,    17,  1963,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   492,    -1,    -1,   150,    -1,    -1,    -1,   154,    -1,
     501,    -1,    -1,   150,   151,   161,   162,    -1,  1207,    -1,
     157,    -1,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,
     521,    -1,   523,   524,    -1,    71,   527,    -1,   529,    -1,
      -1,   102,    -1,   204,   105,   106,   107,   108,   109,   110,
     111,   112,   113,  1242,  1243,  1244,    -1,    -1,    -1,    -1,
    1249,  1250,    -1,    -1,    -1,  1518,  1519,    -1,  1518,  1519,
      -1,    -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,    -1,
      -1,    -1,    -1,  1572,    -1,    -1,    -1,   578,    -1,   580,
      -1,   582,    -1,   584,   130,    -1,    -1,    -1,    -1,    74,
     161,  1554,    -1,    -1,    -1,   266,    -1,    -1,    -1,   600,
     601,    -1,   603,    -1,   150,    -1,    -1,    -1,   154,    -1,
     611,    96,    -1,    -1,   615,   161,   162,    -1,    -1,    -1,
      -1,    -1,    -1,   624,    -1,   296,    -1,   802,    -1,    -1,
     805,   302,    -1,   634,    -1,    -1,   637,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   645,   102,    71,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   658,    -1,   660,
     661,   332,   663,    -1,  1361,  1362,    -1,   152,    -1,  1632,
      -1,   672,  1632,    -1,   675,   676,   677,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
    1663,    -1,  1399,  1663,   375,  1668,  1669,  1706,  1668,  1669,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,
      -1,    -1,    -1,    18,  1723,   150,   151,    -1,    -1,    -1,
     731,    -1,    -1,   239,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,   228,   229,    -1,   747,   748,    71,    -1,
      -1,    -1,    -1,    -1,   755,    13,    14,    15,    16,    17,
     245,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,   772,    -1,    -1,   775,    -1,   777,    -1,    -1,   102,
      -1,    -1,    -1,   454,   107,   108,   109,   110,   111,   112,
     113,    -1,    59,   794,   795,   970,    -1,    -1,    -1,    -1,
     471,    -1,    -1,   978,    -1,    -1,    -1,   130,  1807,    -1,
      -1,    -1,   813,    71,   815,    -1,    83,    -1,   303,   304,
      64,    65,    66,    67,    -1,    -1,    -1,   150,   151,    -1,
      -1,    -1,  1007,   834,    -1,  1010,    -1,    -1,   161,   162,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,   528,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,   872,   130,   140,    -1,    -1,    -1,   548,   879,   146,
      -1,    -1,   149,    -1,   885,   886,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,  1070,   897,   568,   899,    -1,
      -1,  1598,    -1,   161,   162,    -1,    -1,    -1,    18,   910,
     154,    -1,    -1,  1612,    -1,   421,   102,    -1,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   172,    -1,
    1929,   437,   603,   200,   440,     4,     5,     6,     7,     8,
       9,    10,    11,    12,   130,    -1,    -1,    -1,   949,    -1,
      60,    61,    62,    63,    -1,    -1,   627,    -1,    -1,    -1,
      -1,    -1,    -1,   634,   150,   151,    -1,   452,   154,    -1,
      13,    14,    15,    16,    17,   161,   162,    -1,    -1,    -1,
      -1,    71,    -1,    -1,   251,    -1,   253,    -1,   174,    -1,
      -1,   497,   102,    -1,    -1,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,   492,    -1,    -1,
      -1,    -1,   102,    -1,    -1,  1016,   501,   107,   108,   109,
     110,   111,   112,   113,   291,    -1,    -1,    -1,    71,    -1,
      -1,    -1,  1033,  2032,    -1,    -1,   521,    -1,    -1,  1040,
     130,    -1,   527,   310,   154,    -1,  2009,    -1,    -1,  2009,
      -1,  1226,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
     150,   151,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   161,   162,    -1,    -1,    -1,    -1,  1252,    -1,    -1,
      -1,  1082,   567,    -1,    -1,  1260,    -1,   130,    -1,    -1,
      -1,    -1,    -1,   578,    -1,   580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,   150,   151,    -1,
       1,   154,    -1,     4,    -1,    -1,    -1,   102,   161,   162,
      -1,  1122,   107,   108,   109,   110,   111,   112,   113,  1130,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,   418,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,
    1859,    -1,    -1,   130,    -1,   150,   151,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,  1176,  1177,   444,    -1,    -1,
      -1,    -1,    -1,   150,   151,    71,    -1,   154,    -1,    -1,
    1191,    -1,    83,   864,   161,   162,  1197,    -1,    -1,    -1,
      -1,    -1,    -1,  1378,   875,    -1,  1207,    -1,    99,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   102,    -1,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   899,    -1,
      -1,    -1,    -1,    -1,  1235,    -1,    -1,    -1,  1239,    -1,
      -1,   747,   748,    -1,   130,    -1,    -1,    -1,    -1,   140,
      -1,   757,    -1,    -1,   760,   146,    -1,    -1,   149,    -1,
      -1,    -1,   153,    -1,   150,   151,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,   165,   161,   162,    -1,    -1,    -1,
      -1,    -1,    -1,  1458,    -1,    -1,    -1,    -1,  1463,    -1,
     775,    -1,   777,  1468,  1469,  1470,    -1,   188,   565,    -1,
      -1,  1302,  1303,    -1,   571,    -1,    -1,    -1,    -1,   200,
     201,    -1,    -1,   204,    -1,   821,    -1,   584,    -1,   825,
      -1,    -1,    -1,   829,    -1,    -1,    -1,    -1,    -1,    -1,
     815,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
    1341,  1342,  1343,  1344,  1345,  1346,    -1,   102,    -1,   834,
    1351,  1352,   107,   108,   109,   110,   111,   112,   113,   114,
     251,  2058,   253,   118,    -1,   120,    -1,    -1,    -1,  1370,
    1371,    -1,    -1,    -1,    -1,   266,   102,  2074,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    71,
      -1,    -1,   283,    -1,    -1,    -1,   151,    -1,    -1,  1400,
     291,    -1,    -1,    -1,    -1,   296,    -1,    -1,    -1,    -1,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   310,
     102,  1422,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   158,    -1,    -1,    -1,    -1,    -1,    -1,   330,
      -1,   332,   333,    -1,    -1,  1446,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   347,    -1,    -1,    -1,
     351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,  1656,    -1,  1658,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,   384,    -1,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,    -1,  1176,    -1,    -1,    -1,    -1,
    1016,    -1,    -1,    -1,   102,    -1,    -1,    -1,   130,   107,
     108,   109,   110,   111,   112,   113,   114,   418,    -1,    -1,
      -1,  1532,    -1,    -1,    -1,  1536,    -1,  1538,   150,   151,
    1211,  1542,   154,  1544,    -1,    -1,    -1,    -1,  1033,   161,
     162,    71,    -1,   444,  1039,  1040,    -1,   448,    -1,    -1,
      -1,    -1,    -1,   454,    -1,    -1,   154,    -1,    -1,    -1,
      -1,  1572,  1573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1087,   102,    -1,  1090,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   872,  1607,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,  1618,   885,    -1,
    1621,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,   151,   523,   524,    -1,    -1,    -1,   528,   529,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,  1648,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    59,    -1,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   560,
      -1,    -1,  1673,  1674,   565,    -1,    -1,   568,   569,    -1,
     571,  1352,    -1,    -1,    -1,    -1,    -1,  1688,  1689,     1,
      -1,   582,     4,   584,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,  1703,    -1,    -1,  1706,  1191,   598,   153,   600,
     601,    -1,   603,   158,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1723,    -1,    -1,    -1,    -1,    -1,    -1,  1400,
      -1,    -1,    -1,   624,    -1,  1736,   627,    -1,    -1,    -1,
     631,    -1,   146,   634,    -1,    -1,   637,    59,   639,    -1,
      -1,    -1,    -1,    -1,   645,    -1,    -1,    -1,    -1,    -1,
     164,    -1,  1763,    -1,    -1,    -1,    -1,   658,    -1,   660,
     661,    83,   663,  1279,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   672,  1288,    -1,   675,   676,   677,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1807,  1292,    -1,    -1,
      -1,    -1,    -1,   102,    -1,  1816,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,   140,    -1,
      -1,    -1,    -1,    -1,   146,    -1,    -1,    -1,    -1,   102,
     731,   130,   105,   106,   107,   108,   109,   110,   111,   112,
     113,  1852,   164,    -1,    -1,  1122,    -1,    -1,    -1,    -1,
      -1,   150,   151,  1130,   755,    -1,  1351,    -1,   157,    -1,
      -1,   275,   161,   162,    -1,    -1,    -1,   189,  1879,    -1,
      -1,   772,    -1,    -1,    -1,    -1,  1887,   291,   200,   201,
      -1,    -1,  1159,    -1,    -1,   158,  1897,    -1,    -1,    -1,
     102,  1572,    -1,   794,   795,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,    -1,   118,   229,   120,    -1,
      -1,    -1,   813,    -1,  1925,  1926,   330,  1928,  1929,   333,
      -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,   250,   251,
      -1,   253,    -1,   347,    -1,    -1,    -1,   351,    -1,   151,
     841,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1963,   275,    -1,    -1,   278,    -1,   280,    -1,
      -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,    -1,   291,
      -1,   872,    -1,    -1,   875,    -1,    -1,    -1,   879,    -1,
    1991,  1992,   304,    -1,   885,   886,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,   897,    -1,   899,   900,
      -1,   902,    -1,    -1,    -1,    -1,    -1,    -1,   330,    -1,
      -1,   333,    -1,    -1,    -1,    -1,  1532,    -1,    -1,    -1,
      -1,  2032,    -1,    -1,    -1,   347,    -1,    -1,    -1,   351,
      -1,    -1,    -1,    -1,   448,    -1,    -1,    -1,    -1,    -1,
      -1,  1536,  1723,    -1,    71,    -1,    -1,  1542,   949,  1544,
      -1,    -1,   102,    -1,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,  1341,  1342,  1343,  1344,  1345,  1346,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,  1370,  1371,    -1,    13,    14,    15,    16,
      17,   151,    -1,   130,   154,    -1,    -1,    -1,    -1,    -1,
      -1,  1012,    -1,    -1,    -1,   529,    -1,    -1,    -1,    -1,
      -1,    -1,   444,   150,   151,    71,   448,    -1,  1029,  1030,
      -1,   102,    -1,    -1,   161,   162,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,   560,   118,    -1,   120,
      -1,    -1,    -1,    -1,    71,   569,   102,   571,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   582,  1446,
     584,    -1,  1688,  1689,    -1,    -1,    -1,    -1,    -1,    -1,
     151,  1082,    -1,   154,   130,   102,   600,   601,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   529,  1703,    -1,
     624,  1706,    -1,   130,    -1,   161,   162,    -1,    -1,    -1,
      -1,  1122,    -1,   637,    -1,    -1,    -1,    -1,    -1,  1130,
      -1,    -1,    -1,   150,   151,    -1,    -1,    -1,   560,    -1,
      -1,    -1,    -1,   565,   161,   162,    -1,   569,  1929,   571,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,
     582,    -1,   584,    -1,    -1,    -1,    -1,    -1,  1763,    -1,
      -1,    -1,    -1,    -1,    -1,  1176,  1177,   102,   600,   601,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,  1807,    -1,   615,    -1,    -1,  1197,    -1,    -1,    -1,
      -1,    -1,   624,    -1,    -1,   130,  1207,   629,    -1,    -1,
    1211,    -1,    -1,    -1,    -1,   637,    -1,   731,    -1,    -1,
      -1,  1816,  1223,    -1,    -1,   150,   151,    -1,    -1,   154,
    1607,    -1,    -1,    -1,  1235,    -1,   161,   162,  1239,    -1,
      -1,    -1,    -1,    -1,  1621,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,   772,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1883,    -1,    -1,
      -1,  1887,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
     794,   795,  1283,  1284,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,  1297,  1298,    -1,   813,
      -1,  1302,  1303,    -1,   161,   162,    -1,    -1,    -1,   731,
     102,    -1,  1928,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,    -1,    -1,   748,   841,    -1,    -1,
    1331,  1332,    -1,  1928,    -1,    -1,    -1,    -1,    -1,    -1,
    1341,  1342,  1343,  1344,  1345,  1346,    -1,    -1,    -1,    -1,
     772,  1352,    -1,    -1,    -1,   777,    -1,    -1,   872,  1736,
      -1,    -1,   154,    -1,    -1,   879,    -1,    -1,    -1,  1370,
    1371,    -1,   794,   795,    -1,  1991,  1992,    -1,    -1,    -1,
      -1,    -1,    -1,   897,    -1,    -1,   900,    -1,   902,    -1,
      -1,   813,    -1,    -1,    -1,     1,  1991,  1992,    -1,  1400,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2032,    -1,    -1,   841,
      -1,  1422,    -1,   102,    -1,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   949,    -1,  2032,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1446,    -1,    -1,    -1,    -1,
     872,   130,    -1,    59,    -1,    -1,    -1,   879,    -1,    -1,
      -1,    -1,    59,   885,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,    -1,   897,    -1,    -1,   900,    -1,
     902,    -1,   161,   162,   102,   907,    83,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   103,  1012,    -1,
     118,    -1,   120,    -1,    -1,    -1,   103,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1029,  1030,  1518,  1519,    -1,
    1897,    -1,    -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,  1538,    -1,    -1,
     146,    -1,    -1,   140,    -1,    -1,    -1,    -1,  1925,   146,
      -1,    -1,   149,  1554,    -1,    -1,   102,    -1,   164,   105,
     106,   107,   108,   109,   110,   111,   112,   113,  1082,    -1,
     102,  1572,  1573,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,    -1,    -1,  1963,    -1,    -1,    -1,
    1012,   188,    -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   200,   150,    -1,  1607,  1029,  1030,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1130,  1618,    -1,    -1,
    1621,   153,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
     102,  1632,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,    -1,  1159,    -1,  1648,    -1,    -1,
      -1,    -1,    -1,    -1,   251,    -1,   253,    -1,    -1,    -1,
    1082,   258,  1663,  1177,    -1,    -1,    -1,  1668,  1669,    -1,
      -1,    -1,  1673,  1674,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1207,   291,    -1,    -1,    -1,    -1,    -1,
    1122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1130,    -1,
      -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1235,  1723,    -1,   330,    -1,    -1,   333,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1736,    -1,  1159,    -1,    -1,
      -1,   347,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,    -1,  1177,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,   102,  1283,
    1284,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,  1297,  1298,  1207,    -1,   384,  1302,  1303,
      -1,    -1,   102,    -1,  1216,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   153,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1235,    -1,    -1,   150,  1331,  1332,    -1,
      -1,   418,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,    -1,    -1,    -1,   147,    -1,    -1,    -1,
      -1,    -1,   448,    -1,    -1,    -1,    -1,   444,    -1,    -1,
      -1,  1852,    -1,    -1,    -1,    -1,  1370,  1371,    -1,    -1,
      -1,  1283,  1284,   174,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1297,  1298,    -1,  1879,    -1,
    1302,  1303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1897,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1422,  1331,
    1332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1341,
    1342,  1343,  1344,   529,  1925,  1926,   523,   524,  1929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1370,  1371,
      -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1963,   569,    -1,   571,    -1,    -1,   565,    -1,
      -1,    -1,   569,    -1,   571,    -1,   582,    -1,   584,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,    -1,
      -1,    -1,    -1,    -1,   600,   601,    -1,    -1,    -1,    -1,
    1422,    -1,    -1,    -1,  1518,  1519,    -1,    -1,  2009,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   624,    -1,
      -1,    -1,    -1,    -1,  1446,    -1,    -1,    -1,    -1,    -1,
      -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1554,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,
      -1,   658,    -1,   660,   661,    -1,   663,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   672,    -1,    -1,   675,   676,
     677,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1607,    -1,    -1,  1518,  1519,    -1,    -1,
      -1,    -1,    -1,    -1,  1618,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,  1632,    -1,
      -1,    -1,  1544,    93,    94,   731,    -1,    -1,    -1,    -1,
      -1,    -1,  1554,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1663,
      -1,  1573,    -1,    -1,  1668,  1669,    -1,   127,   755,  1673,
    1674,    -1,    -1,    -1,    -1,    -1,   772,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,   794,   795,
      -1,    -1,    -1,    59,    -1,    -1,  1618,    -1,    -1,  1621,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,
    1632,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,  1736,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   841,    -1,   103,    -1,    -1,
      -1,  1663,    -1,    -1,    -1,    -1,  1668,  1669,    -1,    -1,
      -1,  1673,  1674,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   872,  1689,    -1,    -1,
      -1,    -1,    -1,   879,   140,   872,    -1,    -1,    -1,    -1,
     146,    -1,    -1,   149,    -1,    -1,    -1,    -1,   885,   886,
      -1,   897,    -1,    -1,   900,    -1,   902,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1736,     1,    -1,    -1,     4,    -1,
      -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,    -1,    -1,   200,    -1,    -1,    -1,  1852,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1897,    -1,   251,    -1,   253,    -1,    -1,
      -1,    -1,   258,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1012,    -1,    -1,    -1,
      -1,  1925,  1926,    -1,    -1,    -1,    -1,   103,    -1,  1132,
      -1,    -1,    -1,  1029,  1030,   291,    -1,    -1,    -1,    -1,
    1852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,  1963,
      -1,    -1,    -1,    -1,   140,    -1,    -1,  1879,    -1,    -1,
     146,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1897,  1082,   447,    -1,   449,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,   459,
      -1,    -1,    -1,    -1,    -1,  2009,    -1,    -1,    -1,    -1,
      -1,    -1,   188,  1925,  1926,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   200,    -1,    -1,    -1,   384,    -1,
      -1,    -1,    -1,    -1,  1130,  1122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1963,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   418,  1159,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1159,    -1,    -1,   251,    -1,   253,    -1,    -1,
    1992,  1177,   258,    -1,    -1,    -1,    -1,    -1,   444,    -1,
    1177,    -1,    -1,    -1,    -1,    -1,    -1,  2009,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   566,    -1,    -1,    -1,
    1197,  1207,    -1,    -1,    -1,   291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,  1235,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,   524,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1283,  1284,    -1,
      -1,    -1,    -1,    -1,    -1,  1398,    -1,    -1,    -1,    -1,
      -1,  1297,  1298,    -1,    -1,    -1,  1302,  1303,   384,   565,
      -1,    -1,    -1,   569,    -1,   571,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,
      -1,    -1,    -1,    -1,    -1,  1331,  1332,    -1,    -1,    -1,
      -1,    -1,   418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1341,  1342,  1343,  1344,  1345,  1346,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   444,    -1,
      -1,    -1,    -1,    -1,  1370,  1371,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1370,  1371,    -1,    -1,    -1,    -1,   645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   658,    -1,   660,   661,    -1,   663,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   672,    -1,    -1,   675,
     676,   677,    -1,    -1,    -1,    -1,  1422,    -1,   788,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,   524,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1446,
      -1,    -1,  1565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1578,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   565,
      -1,    -1,    -1,   569,    -1,   571,    -1,    -1,    -1,   755,
      -1,    -1,    -1,    -1,    -1,   865,   866,    -1,   584,    -1,
      -1,    -1,  1615,    -1,    -1,    -1,   876,   877,   878,    -1,
      -1,   881,  1518,  1519,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,    -1,    -1,  1554,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,   658,    -1,   660,   661,  1573,   663,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   672,    -1,    -1,   675,
     676,   677,    -1,   963,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1607,    -1,    -1,  1717,    -1,   872,    -1,  1721,    -1,
    1607,    -1,  1618,    -1,    -1,    -1,    -1,    -1,    -1,   885,
     886,    -1,  1735,    -1,  1621,    -1,  1632,    -1,    -1,    -1,
      -1,    -1,  1745,    -1,    -1,    -1,    -1,    -1,    -1,  1009,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1648,    -1,    -1,    -1,    -1,    -1,  1663,    -1,    -1,
      -1,    -1,  1668,  1669,    -1,    -1,    -1,  1673,  1674,   755,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1056,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1065,  1066,  1067,  1068,    -1,
      -1,    -1,    -1,  1073,  1074,    -1,    -1,    -1,    -1,    -1,
    1823,    -1,    -1,  1083,  1827,    -1,    -1,    -1,    -1,  1832,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1736,    -1,     0,    -1,  1104,     3,  1106,    -1,    -1,  1736,
      -1,    -1,    -1,    -1,  1857,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   872,  1900,    -1,  1159,
      -1,  1904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   885,
     886,    -1,    -1,    -1,  1917,    -1,    -1,    -1,    -1,    77,
      -1,    -1,    -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,
      -1,    -1,  1192,    -1,  1194,  1195,    -1,    -1,  1941,    -1,
    1943,  1944,    -1,    -1,    -1,    -1,  1206,    -1,  1208,    -1,
    1210,    -1,  1212,    -1,    -1,    -1,  1852,  1217,    -1,    -1,
      -1,    -1,    -1,  1966,    -1,    -1,  1122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1130,    -1,    -1,    -1,   136,    -1,
      -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2002,
      -1,  1897,    -1,  1159,    -1,    -1,    -1,    -1,    -1,  2012,
    1897,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,    -1,
      -1,  1177,    -1,    -1,    -1,  1285,  1286,    -1,    -1,  1925,
    1926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1925,    -1,
      -1,  1197,  2045,    -1,  2047,    -1,    -1,    -1,    -1,  1309,
      -1,    -1,    -1,    -1,    -1,    -1,  1316,    -1,    -1,    -1,
    1320,    -1,    -1,  2066,    -1,    -1,    -1,  1963,    -1,  2072,
      -1,    -1,    -1,    -1,    -1,    -1,  1963,    -1,    -1,  2082,
     238,    -1,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,
    1350,    -1,    -1,    -1,   252,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   262,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2009,   272,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   286,   287,
    1390,    -1,    -1,    -1,    -1,   293,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1122,    -1,    -1,    -1,
      -1,   309,    -1,    -1,  1130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1426,    -1,    -1,    -1,
      -1,   329,    -1,    -1,  1434,    -1,  1436,    -1,    -1,    -1,
      -1,    -1,    -1,  1159,    -1,  1341,  1342,  1343,  1344,  1345,
    1346,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1370,  1371,    -1,    -1,    -1,    -1,
      -1,  1197,    -1,  1483,  1484,    -1,    -1,   385,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1498,  1499,
      -1,  1501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1510,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,    -1,
    1520,  1521,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   442,    -1,    -1,    -1,   446,    -1,
    1446,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,    -1,    -1,    -1,    -1,    -1,   465,    -1,    -1,
      -1,   469,   470,    -1,    -1,   473,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     488,   489,   490,   491,    -1,    -1,    -1,   384,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   507,
      -1,    -1,    -1,    -1,    48,    -1,    -1,   515,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1341,  1342,  1343,  1344,  1345,
    1346,    -1,    -1,  1633,  1634,    -1,    -1,    -1,    -1,    -1,
      74,    -1,  1538,    -1,  1644,   543,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1370,  1371,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   574,  1573,    -1,  1679,
    1680,    -1,    -1,   581,    -1,    -1,   120,    -1,    -1,   587,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1607,    -1,   611,   612,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1621,    -1,    -1,    -1,    -1,
    1446,    -1,   166,    -1,    -1,    -1,   523,   524,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     369,    -1,  1648,   372,   373,    -1,   190,    -1,    -1,    -1,
      -1,    -1,    -1,   382,   383,    -1,    -1,  1767,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   397,   398,
     678,    -1,    -1,    -1,    -1,    -1,  1786,    -1,    -1,  1789,
    1790,    -1,    -1,    -1,   228,    -1,  1796,    -1,   232,   418,
      -1,   235,   236,    -1,    -1,   239,    -1,    -1,   242,   243,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,    -1,   448,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1736,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   747,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,    -1,    -1,   762,    -1,    -1,    -1,   766,   303,
      -1,   658,   306,    -1,    -1,    -1,   663,   775,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   672,    -1,    -1,    -1,    -1,
      -1,  1607,    -1,   327,   328,    -1,    -1,    -1,    -1,   797,
      -1,    -1,    -1,    -1,   691,  1621,    -1,    -1,   806,   343,
      -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1927,    -1,    -1,
      -1,    -1,  1648,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   849,    -1,    -1,    -1,    -1,    -1,    -1,   856,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,  1989,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1897,    -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2013,    -1,    -1,    -1,    -1,    -1,    -1,
    1736,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2028,  1925,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   939,    -1,  2043,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1963,    -1,    -1,
     504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   708,
     709,   710,   711,   712,   713,   714,   715,   716,   717,   718,
     719,   720,   721,   722,   723,   724,   725,   726,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,  1022,    -1,    -1,    -1,  1026,    -1,
      -1,    -1,    -1,    -1,    -1,  1033,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   578,  1043,    -1,    -1,    -1,    -1,
      -1,    -1,  1050,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1059,    -1,  1061,    -1,    -1,    -1,    49,    -1,   788,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1897,    -1,    -1,    -1,   619,   620,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    83,  1093,    -1,    -1,   632,  1097,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1925,
      -1,    -1,    -1,  1111,    -1,    -1,  1114,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,  1963,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,   164,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,   927,    -1,
      -1,    -1,   201,    -1,    -1,    -1,    -1,   751,   752,    -1,
      -1,    -1,    -1,   757,    -1,    -1,   945,    -1,    -1,    -1,
      -1,   950,  1230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   961,    -1,   778,    -1,    -1,   781,   782,    -1,
     784,    -1,   786,   787,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,  1003,    -1,    13,    14,    15,    16,
      17,   825,    -1,    -1,    -1,   829,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   200,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   310,    49,    -1,    -1,  1323,    53,    -1,    55,  1327,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   330,    -1,   232,    71,    72,    -1,    -1,    -1,    -1,
     239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1357,
     894,   895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,   908,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,  1407,
      -1,  1130,  1410,    -1,    -1,    -1,    -1,   306,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,   418,
    1428,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,   330,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   448,
      -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1367,  1479,    -1,  1370,  1371,    -1,    -1,    -1,  1207,  1376,
    1488,    -1,    -1,  1380,  1492,  1382,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1039,    -1,    -1,  1506,  1507,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    53,
      -1,    55,    -1,  1242,  1243,  1244,    -1,    -1,    -1,    -1,
    1249,  1250,   421,    -1,   523,   524,    -1,    -1,    72,    -1,
     529,    -1,    -1,    -1,    -1,    -1,  1080,    -1,   437,   438,
      -1,   440,   441,  1087,  1273,    -1,  1090,    -1,    -1,   448,
      -1,    -1,    -1,   452,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,  1310,  1311,   582,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   493,    -1,    -1,    -1,   497,    -1,
      -1,    -1,   601,    -1,    -1,    -1,   150,   151,   152,   153,
     154,    -1,    -1,    -1,  1622,  1623,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,   624,    -1,  1524,    -1,    -1,
     529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1556,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1213,
      -1,  1568,    -1,    -1,    -1,    -1,    -1,  1221,  1222,    -1,
     579,    -1,    -1,   582,    -1,    -1,  1583,  1584,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   600,   601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1607,    -1,   611,    -1,    -1,    -1,   615,    -1,    -1,    -1,
      -1,    -1,    -1,   622,    -1,   624,    -1,    -1,    -1,    -1,
      -1,    -1,   731,    -1,    -1,  1279,    -1,    -1,    -1,    -1,
      -1,    -1,  1750,    -1,  1288,    -1,    -1,  1291,    -1,  1293,
    1294,    -1,    -1,    -1,    -1,    -1,   755,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1776,    48,
      -1,    -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1794,    -1,    -1,    -1,
    1334,    -1,    -1,    -1,    -1,   794,   795,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1821,   813,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1719,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1728,   731,  1730,    -1,    -1,  1733,  1734,    -1,  1736,
    1848,   120,    -1,  1851,  1741,    -1,    -1,    83,   747,   748,
      -1,    -1,    -1,    -1,   133,    -1,   135,  1401,   757,   758,
      -1,   760,   761,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   772,    -1,    -1,   775,    -1,   777,   778,
     879,    -1,    -1,  1612,    -1,   784,    -1,   886,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   794,   795,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   813,    -1,    -1,   153,   817,    -1,
      -1,    -1,   821,    -1,    -1,    -1,   825,   826,   164,    -1,
     829,   830,    -1,    -1,    -1,    -1,    -1,    -1,   837,    -1,
      -1,  1838,    -1,  1487,  1952,    -1,  1843,  1844,    -1,    -1,
     949,    -1,   188,    -1,    -1,    -1,   235,   236,    -1,    -1,
     239,    -1,    -1,   242,   243,   201,    -1,    -1,   204,    -1,
      -1,    -1,  1516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     879,   880,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,    -1,
      -1,    -1,    -1,    -1,  1548,    -1,    -1,    -1,  1905,    -1,
    1907,   910,    -1,  1910,  1911,    -1,    -1,   253,  1915,  1916,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     266,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   327,   328,
     949,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   343,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,  1617,   310,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1082,    -1,    -1,    -1,  1984,  1985,  1986,
    1819,    -1,    -1,    -1,   330,    -1,   332,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2005,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1016,    -1,    -1,
      -1,  2018,  2019,  2020,    -1,    -1,    -1,    -1,    -1,    -1,
    1859,    -1,    -1,    -1,  1033,  1034,    -1,    -1,    -1,    -1,
      -1,  1040,    -1,    -1,    -1,    -1,    -1,    -1,   384,    -1,
      -1,  1695,  1696,    -1,    -1,    -1,    -1,    -1,   437,  1703,
      -1,    -1,    -1,  1707,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   418,  1082,    -1,    -1,    -1,    -1,  1087,  1088,
      -1,  1090,  1091,    -1,    -1,    -1,    -1,    -1,  1197,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1207,    -1,
      -1,    -1,   448,    -1,    -1,    -1,    -1,    -1,   454,    -1,
      -1,    -1,    -1,    -1,    -1,   504,    -1,    -1,  1957,    -1,
    1959,    -1,    -1,    -1,    -1,    -1,  1235,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,  1998,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   523,   524,    -1,
      -1,    -1,    -1,   529,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2030,    -1,  1302,  1303,    -1,    -1,    -1,  1207,    -1,
      -1,    -1,    -1,    -1,  1213,  1214,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1235,    -1,    -1,  1883,
     619,   620,    -1,    -1,    -1,    -1,   582,    -1,    -1,  2078,
      -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   601,    -1,   603,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1279,  1280,    -1,    -1,    -1,    -1,    -1,    -1,   624,  1288,
    1289,    -1,  1291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1302,  1303,    -1,    -1,    -1,    -1,   645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   658,  1422,   660,   661,    -1,   663,    -1,    -1,
      56,    57,    -1,    -1,    -1,    -1,   672,    -1,    -1,   675,
     676,   677,    -1,    -1,    -1,    -1,    -1,  1991,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,   751,   752,    -1,    -1,    -1,    -1,   757,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,    -1,   778,
      -1,    -1,   781,   782,    -1,   784,    -1,   786,   787,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,   144,   755,
      -1,    -1,    -1,  1422,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,    -1,    -1,    -1,   772,    -1,    -1,  1538,
      -1,    -1,    -1,    -1,    -1,    -1,   825,    -1,    -1,    -1,
     829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   794,   795,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,
      -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   219,   214,    -1,   216,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   894,   895,    -1,    -1,  1618,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   908,
      -1,    -1,    -1,  1532,    -1,    -1,    -1,   263,    -1,    -1,
      -1,    -1,    -1,   879,    -1,    -1,    -1,   273,    -1,  1548,
     886,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,   899,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1673,  1674,    -1,    -1,    -1,    -1,
      -1,   301,    -1,    -1,    -1,   311,    -1,    -1,    -1,    -1,
      -1,    -1,   318,   319,    -1,    -1,    -1,   323,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,  1618,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     356,    -1,    -1,    -1,    -1,   361,    -1,    -1,   364,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1039,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1673,  1674,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1688,
    1689,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1080,    -1,    -1,    -1,  1704,    -1,    -1,  1087,    -1,
      -1,  1090,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   451,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   463,   464,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1082,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   474,    -1,    -1,    -1,    -1,    -1,
     480,    -1,    -1,    -1,    -1,   485,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1879,    -1,    -1,    -1,    -1,    -1,  1122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1807,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1815,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1213,    -1,    -1,    -1,    -1,    -1,
    1176,    -1,  1221,  1222,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1197,    -1,    -1,    -1,    -1,    -1,    -1,   594,    -1,
      -1,  1207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1879,   601,    -1,    -1,  1883,  1884,    -1,    -1,  1887,    -1,
      -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,    -1,  1235,
    1279,    -1,    -1,  1239,    -1,    -1,    -1,    -1,    -1,  1288,
      -1,    -1,  1291,    -1,  1293,  1294,    -1,   643,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1928,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   665,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1334,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1302,  1303,    -1,   689,
     690,    -1,    -1,   693,    -1,   695,    -1,    -1,    -1,    -1,
      -1,   701,    -1,   703,   704,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1991,  1992,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1341,  1342,  1343,    -1,  1345,
    1346,   731,    -1,    -1,    -1,    -1,  1352,    -1,    -1,    -1,
      -1,    -1,  1401,    -1,   744,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2032,    -1,   755,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   769,
      -1,    -1,   772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   789,  1400,    -1,    -1,    -1,    -1,    -1,
     796,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,
      -1,    -1,   802,    -1,    -1,    -1,  1422,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1516,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   871,     5,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   886,   893,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   899,
     900,    -1,    -1,    -1,    -1,    -1,    -1,   907,    -1,    -1,
      49,    -1,    -1,    -1,    53,    -1,    55,    -1,    -1,   925,
      -1,    -1,  1538,   929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   949,
      -1,    -1,    -1,    -1,    -1,    -1,  1572,   957,  1617,    -1,
      99,   100,    -1,   102,   964,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1618,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1010,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,  1648,    -1,    -1,    -1,  1695,  1696,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1707,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1673,  1674,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1077,    -1,  1079,
      -1,  1081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1723,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1135,
    1136,  1137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1147,  1148,    -1,
    1809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1165,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1180,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,  1211,    -1,    -1,  1220,    -1,    -1,  1217,    -1,    -1,
      -1,    -1,    -1,    -1,  1883,    49,  1226,    71,    -1,    53,
      -1,    55,    -1,    -1,    -1,  1235,  1852,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,  1252,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,   106,  1879,    -1,    -1,    -1,  1267,    -1,    -1,
    1270,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,   153,
    1926,    -1,    -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1322,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,  1355,
      -1,    -1,  1358,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1378,    -1,
      -1,  1381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1420,  1421,    18,    -1,    -1,    -1,
      -1,    -1,  1422,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1432,  1433,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,  1458,    -1,
    1460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      72,    73,    -1,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    -1,    97,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1538,    -1,
      -1,    -1,    -1,  1543,    -1,  1551,    -1,   149,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1603,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    53,    -1,
      55,    -1,    -1,    -1,  1640,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,  1646,    72,    73,  1649,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    -1,    97,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,   130,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,   149,   150,    -1,    -1,   153,   154,
      -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     175,    -1,    -1,    -1,    49,    -1,  1762,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    72,    73,    -1,
      75,    -1,    -1,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    -1,    97,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,
    1860,    -1,    -1,   158,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     175,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    68,    -1,    70,    71,
      72,    73,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    -1,    97,    -1,    99,   100,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    68,
      -1,    70,    71,    72,    73,    -1,    75,    -1,    -1,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    -1,    97,    -1,
      99,   100,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   175,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    68,    -1,    -1,    71,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,   145,
     146,    -1,    -1,    -1,   150,   151,   152,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    68,    -1,    -1,    71,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,   145,   146,    -1,    -1,    -1,   150,   151,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   175,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    68,    -1,
      -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    68,    -1,    -1,    71,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
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
     160,   161,   162,   163,   164,   165,   166,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
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
     164,   165,   166,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    52,    -1,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    -1,    -1,    71,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,     1,    -1,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,   161,   162,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    68,    -1,    -1,    71,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,   101,   102,    -1,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   161,   162,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    71,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,   102,    51,    52,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,   158,    -1,    -1,   161,   162,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   161,   162,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   161,   162,     3,     4,     5,
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
       3,    -1,    -1,    -1,    -1,   161,   162,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,
     153,   154,    -1,     3,    -1,    -1,    -1,    -1,   161,   162,
      10,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,    -1,   153,   154,    -1,     3,    -1,    -1,    -1,
      -1,   161,   162,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    -1,   153,   154,    -1,     3,
      -1,    -1,    -1,    -1,   161,   162,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,     3,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    71,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,   105,   106,    -1,    -1,    -1,    -1,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,   158,    -1,    -1,   161,   162,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,
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
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   130,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    71,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    52,
      -1,    54,   105,   106,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
      70,    -1,    72,    -1,    -1,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    -1,    97,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,
     160,   161,   162,   163,   164,   165,   166,    49,    -1,    -1,
      -1,    53,    -1,    55,    -1,   175,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      72,    -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    -1,    97,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,
      -1,   160,   161,   162,   163,   164,   165,   166,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,   150,    -1,   152,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    13,    14,    15,    16,    17,    18,    -1,    20,
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
      -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,
     161,   162,   163,   164,   165,   166,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,
     106,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,   157,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   105,   106,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,   153,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,   158,    -1,    -1,   161,   162,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   161,   162,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,
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
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,   153,    22,    23,    24,    25,
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
      -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
      13,    14,    15,    16,    17,    18,    71,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,   153,    -1,    51,    52,
     105,   106,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    77,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,
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
     161,   162,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    13,    14,    15,    16,    17,    18,
      71,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,   105,   106,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,    -1,    -1,   105,   106,    -1,    -1,
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
     153,    -1,    -1,    13,    14,    15,    16,    17,   161,   162,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    71,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,   105,   106,    -1,    -1,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,   153,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    13,    14,
      15,    16,    17,    18,    71,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,
     105,   106,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    52,    -1,    54,    -1,    -1,    -1,    -1,   153,    49,
      -1,    -1,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    -1,   122,   123,    -1,    49,    -1,    -1,    -1,    53,
     130,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,    -1,   144,   145,   146,    -1,    72,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    49,    -1,    -1,    -1,    53,   130,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,   145,   146,    -1,    72,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
     158,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,   158,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,   157,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,   158,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    71,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,   102,
      51,    52,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,   105,   106,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71
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
     443,   460,   461,   462,   463,     0,   177,   102,   181,   197,
     284,   286,   295,   298,   310,   314,   319,   116,   150,    57,
      60,    61,    63,   150,   150,   405,   406,   407,   306,   307,
     105,   106,   181,   360,   381,   382,   360,   150,   393,   150,
     150,     4,   102,   105,   106,   299,   304,   305,   150,   197,
     406,   411,   417,   418,   419,   421,   422,   423,   105,   321,
     155,   177,   287,   295,   298,   416,   420,   459,   460,   463,
     464,   175,   178,   147,   158,   174,   218,   363,    88,   156,
     400,   360,   178,   178,   178,   175,   105,   106,   150,   197,
     292,   402,   411,   412,   413,   414,   415,   416,   420,   424,
     425,   426,   427,   428,   434,     3,    47,    48,    50,    54,
     312,     3,   154,   197,   286,   299,   303,   305,   315,   320,
     396,   416,   420,   463,   284,   286,   298,   310,   314,   319,
     397,   416,   420,    64,   304,   304,   299,   305,   304,   299,
     304,   299,   153,   405,   156,   178,   150,   158,   226,   405,
     405,   177,   275,   276,   154,   295,   298,   461,   360,   360,
     393,   174,   298,   150,   197,   402,   411,   416,   425,   154,
     197,   463,   394,   395,    64,    65,    66,    67,   154,   172,
     360,   369,   371,   375,   377,   378,   320,    56,   152,   154,
     197,   294,   298,   302,   303,   309,   310,   316,   317,   318,
     319,   323,   330,   331,   347,   356,   358,   443,   455,   456,
     457,   458,   463,   464,   105,   106,   158,   181,   320,   434,
     407,   150,   376,   377,   150,   150,   116,   183,   184,    49,
      53,    55,    72,    99,   100,   102,   104,   114,   115,   118,
     119,   120,   122,   123,   150,   154,   160,   163,   164,   165,
     166,   179,   180,   183,   185,   188,   196,   197,   198,   199,
     202,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,   214,   220,   320,   152,   154,   196,   197,   213,
     215,   295,   320,   361,   362,   379,   459,   464,   298,   417,
     418,   419,   421,   422,   423,   152,   152,   152,   152,   152,
     152,   152,   154,   295,   443,   461,   154,   161,   197,   215,
     286,   287,   294,   296,   298,   310,   317,   319,   351,   352,
     355,   356,   357,   455,   463,   150,   416,   420,   463,   150,
     156,   102,   153,   154,   158,   180,   182,   215,   364,   365,
     366,   367,   368,    22,   364,   150,   360,   226,   150,   156,
     156,   156,   406,   411,   413,   414,   415,   424,   426,   427,
     428,   298,   412,   425,   156,    97,   404,   154,   405,   442,
     443,   405,   405,   400,   275,   150,   405,   442,   400,   405,
     405,   298,   402,   150,   150,   297,   298,   295,   298,   177,
     295,   459,   464,   322,   158,   400,   275,   360,   363,   286,
     303,   398,   416,   420,   158,   400,   275,   381,   298,   310,
     298,   298,   105,   321,   105,   106,   181,   320,   325,   381,
     177,   181,   359,   149,   177,     3,   291,   293,   298,   302,
     226,   177,   177,   404,   150,   404,   178,   215,   406,   411,
     298,   150,   177,   360,   391,   158,   360,   158,   360,   132,
     161,   162,   374,   152,   156,   360,   378,   152,   405,   405,
     155,   177,   296,   298,   310,   317,   319,   454,   455,   463,
     464,   150,   154,   162,   174,   197,   443,   444,   445,   446,
     447,   448,   449,   466,   197,   323,   463,   298,   317,   304,
     299,   405,   152,   296,   298,   456,   296,   443,   456,    10,
     348,   360,   345,   158,   369,   174,   369,    13,    87,   102,
     105,   106,   180,   408,   409,   410,   152,   116,   150,   196,
     150,   150,   199,   150,   196,   150,   102,   105,   106,   299,
     304,   305,   150,   196,   196,    19,    21,    84,   154,   163,
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
      77,   308,   181,   158,   181,   434,   296,   443,   456,   298,
     302,   463,   177,   445,   446,   447,   155,   177,    18,   215,
     298,   444,   466,   405,   405,   443,   296,   454,   464,   298,
     181,   405,   296,   456,   320,   156,   465,   174,   349,   158,
     348,   152,   362,   152,   152,   156,   150,   175,   361,   154,
     361,   361,   361,   215,   361,   152,   361,   361,   361,   177,
     152,   163,   164,   201,    18,   300,   152,   156,   152,   161,
     162,   152,   221,   215,   158,   215,   181,   215,   181,   114,
     154,   181,   151,   189,   190,   191,   215,   114,   154,   181,
     333,   215,   189,   181,   199,   202,   202,   202,   203,   203,
     204,   204,   205,   205,   205,   205,   206,   206,   207,   208,
     209,   210,   211,   157,   222,   175,   183,   154,   181,   215,
     158,   215,   177,   439,   440,   441,   298,   438,   405,   405,
     215,   362,   150,   405,   442,   443,   150,   442,   443,   177,
     177,   155,   155,   150,   411,   430,   431,   432,   435,    18,
     298,   429,   433,   150,   405,   448,   466,   405,   405,   466,
     150,   405,   448,   405,   405,   178,   214,   360,   155,   156,
     155,   156,   466,   466,   132,   350,   351,   352,   350,   360,
     177,   213,   214,   215,   403,   465,   364,   366,   149,   177,
     152,   156,   177,   350,   181,   402,   181,   152,   152,   152,
     152,   152,   152,   150,   405,   442,   443,   150,   405,   442,
     443,   402,   183,   443,   215,   226,   353,   152,   152,   152,
     152,   389,   390,   226,   391,   226,   400,   390,   226,   158,
     158,   158,   334,   178,   178,   181,   279,   360,    18,    70,
      72,    75,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    91,    92,    93,    94,    95,    97,
     105,   106,   117,   177,   222,   223,   224,   225,   226,   227,
     228,   230,   231,   241,   247,   248,   249,   250,   251,   252,
     257,   258,   264,   265,   266,   280,   298,   302,   360,   401,
      69,   175,   178,   178,   178,   350,   178,   392,   390,   284,
     286,   295,   384,   385,   386,   387,   379,   174,   370,   370,
     348,   405,   405,   296,   456,   154,   161,   197,   215,   320,
     215,   298,   353,   152,   152,   152,     5,   298,   405,   444,
     158,   181,   434,    10,   360,   149,   158,   214,   348,   465,
     158,   152,   409,   189,   152,   177,   156,   152,   152,   156,
     152,   199,   152,   152,   152,   199,    18,   300,   215,   152,
     152,   151,   158,   199,   155,   178,   189,   155,   155,   114,
     118,   120,   182,   192,   193,   194,   152,   156,   192,   155,
     156,   149,   213,   157,   152,   192,   178,   365,   353,   152,
     152,   152,   438,   177,   177,   353,   353,   435,   152,   152,
     152,   152,   150,   411,   434,   429,   433,   177,   177,   155,
     178,   466,   177,   177,   178,   178,   178,   178,   363,   192,
     132,   166,   178,   178,   149,   364,   215,   405,   151,   215,
     350,   178,   174,   150,   405,   442,   443,   150,   405,   442,
     443,   177,   177,   404,   152,   178,   178,   392,   390,   226,
     392,   334,   334,   334,     3,    10,    72,   149,   281,   288,
     289,   295,   298,   335,   340,   459,   152,   156,   156,   175,
     150,    60,    61,   175,   226,   280,   401,   150,    18,   224,
     150,   150,   175,   360,   175,   360,   161,   360,   158,   223,
     150,   150,   150,   226,   215,   216,   216,    14,   267,    73,
     232,   175,   178,   228,    77,   175,   360,    90,   253,   359,
     298,   157,   279,   175,   155,   155,   178,   156,   392,   402,
     178,   175,   178,   175,   178,   152,   362,   376,   376,   465,
     158,   158,   177,   178,   178,   178,   215,   178,   150,   405,
     448,   443,   297,     5,   161,   178,   215,   348,   405,   405,
     320,   349,   365,   465,   149,   149,   177,   152,   181,    77,
     186,   187,   361,   199,   199,   199,   199,   199,   158,   365,
     156,   149,   195,   154,   193,   195,   195,   155,   156,   121,
     153,   191,   155,   221,   213,   175,   155,   465,   178,   150,
     405,   442,   443,   353,   353,   178,   178,   152,   150,   405,
     442,   443,   150,   405,   448,   411,   405,   405,   353,   353,
     155,   352,   355,   355,   356,   152,   156,   156,   152,   178,
     214,   214,   155,   155,   178,   178,   152,   215,   177,   177,
     353,   353,   363,   405,   156,   152,   149,   392,   149,   149,
     149,   149,   295,   333,   341,   459,   295,   340,   150,   329,
     175,   175,   150,   157,   197,   336,   337,   343,   411,   412,
     425,   156,   175,   360,   177,   360,   152,   189,   190,   175,
     226,   175,   226,   222,    79,   152,   222,   233,   280,   282,
     285,   291,   298,   302,   144,   145,   146,   151,   152,   175,
     222,   242,   243,   244,   280,   175,   175,   222,   175,   365,
     175,   222,   221,   222,   109,   110,   111,   112,   113,   259,
     261,   262,   175,    96,   175,    83,   150,   150,   178,   149,
     175,   175,   150,   224,   226,   405,   175,   152,   177,   149,
     149,   177,   156,   156,   149,   348,   348,   155,   155,   155,
     178,   152,   177,   215,   215,   178,   155,   178,   465,   346,
     158,   349,   465,   149,   384,   152,   157,   152,   156,   157,
     365,   465,   221,   119,   192,   193,   154,   193,   154,   193,
     155,   149,   152,   177,   178,   178,   152,   152,   177,   177,
     178,   178,   178,   177,   177,   155,   178,   152,   405,   353,
     353,   178,   178,   222,   149,   329,   329,   329,   150,   197,
     338,   339,   442,   450,   451,   452,   453,   175,   156,   175,
     336,   175,   379,   406,   411,   215,   298,   156,   175,   342,
     343,   342,   360,   132,   357,   358,   222,   152,   152,   150,
     224,   152,   222,   298,   144,   145,   146,   166,   175,   245,
     246,   224,   223,   175,   246,   152,   157,   222,   151,   222,
     223,   244,   175,   465,   152,   152,   152,   226,   261,   262,
     150,   215,   150,   183,   233,   199,   254,   108,     1,   224,
     405,   385,   177,   177,   465,   465,   155,   353,   178,   178,
     155,   155,   149,   158,   348,   149,   178,   215,   187,   215,
     465,   149,   155,   155,   192,   192,   353,   152,   152,   353,
     353,   152,   152,   155,   156,   132,   352,   132,   155,   178,
     178,   152,   152,   155,   451,   452,   453,   298,   450,   156,
     175,   405,   405,   175,   152,   411,   405,   175,   224,    76,
      77,   158,   236,   237,   238,   152,   222,    74,   224,   222,
     151,   222,    74,   175,   105,   151,   222,   223,   244,   151,
     222,   224,   243,   246,   246,   175,   222,   149,   158,   238,
     224,   150,   177,   175,   183,   152,   157,   152,   152,   156,
     157,   252,   256,   360,   402,   149,   149,   178,   155,   155,
     348,   465,   149,   149,   155,   155,   178,   178,   178,   177,
     178,   152,   152,   152,   152,   152,   450,   405,   337,     1,
     214,   234,   235,   403,     1,   157,     1,   177,   224,   236,
      74,   175,   152,   224,    74,   175,   166,   166,   224,   223,
     246,   246,   175,   105,   222,   166,   166,    74,   151,   222,
     151,   222,   223,   175,     1,   177,   177,   263,   296,   298,
     459,   157,   175,   154,   183,   268,   269,   270,   224,   199,
     189,    74,   107,   253,   255,   152,   465,   149,   152,   152,
     152,   355,   150,   405,   442,   443,   339,   132,     1,   156,
     157,   149,   273,   274,   280,   224,    74,   175,   224,   222,
     151,   151,   222,   151,   222,   151,   222,   223,   151,   222,
     151,   222,   224,   166,   166,   166,   166,   149,   273,   263,
     178,   150,   197,   402,   450,   181,   157,   102,   150,   152,
     157,   156,    74,   152,   224,   150,   224,   224,   149,   177,
     214,   234,   237,   239,   240,   280,   224,   166,   166,   166,
     166,   151,   151,   222,   151,   222,   151,   222,   239,   178,
     175,   260,   298,   268,   155,   214,   175,   268,   270,   224,
     222,   108,   108,   353,   224,   229,   178,   237,   151,   151,
     222,   151,   222,   151,   222,   178,   260,   213,   152,   157,
     183,   152,   152,   157,   152,   256,    74,   251,   178,     1,
     224,   149,   229,   149,   152,   226,   183,   271,   150,   175,
     271,   224,    74,   152,   226,   156,   157,   214,   152,   224,
     183,   181,   272,   152,   175,   152,   156,   175,   181
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   176,   177,   178,   179,   179,   179,   179,   179,   180,
     180,   180,   180,   180,   180,   180,   181,   181,   182,   182,
     183,   184,   184,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   185,   185,   185,   185,   186,   186,
     187,   187,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   189,   189,   190,   190,   191,
     191,   192,   192,   193,   193,   193,   193,   193,   193,   193,
     194,   194,   194,   195,   195,   196,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   197,
     197,   197,   198,   198,   198,   198,   199,   199,   199,   199,
     199,   199,   199,   199,   199,   200,   200,   200,   200,   201,
     201,   202,   202,   203,   203,   203,   203,   204,   204,   204,
     205,   205,   205,   206,   206,   206,   206,   206,   207,   207,
     207,   208,   208,   209,   209,   210,   210,   211,   211,   212,
     212,   213,   213,   213,   214,   215,   215,   215,   216,   216,
     217,   217,   218,   218,   219,   219,   219,   219,   219,   219,
     219,   219,   219,   219,   219,   220,   220,   221,   221,   221,
     221,   222,   222,   223,   223,   224,   224,   224,   224,   224,
     224,   224,   224,   224,   224,   224,   224,   224,   225,   225,
     226,   226,   227,   227,   228,   228,   228,   228,   228,   229,
     229,   229,   230,   230,   231,   231,   231,   231,   231,   231,
     231,   232,   232,   233,   233,   233,   233,   234,   234,   234,
     235,   235,   236,   236,   236,   236,   236,   237,   237,   238,
     239,   239,   240,   240,   241,   241,   241,   241,   241,   241,
     241,   241,   241,   241,   241,   241,   242,   242,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   244,   244,   244,   245,   245,   246,   246,
     246,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   248,   248,   249,   250,   251,   252,   252,   253,   253,
     254,   254,   255,   256,   256,   256,   256,   256,   256,   257,
     257,   258,   258,   258,   259,   259,   260,   260,   261,   261,
     261,   261,   262,   263,   263,   263,   263,   263,   264,   265,
     265,   266,   266,   266,   266,   266,   267,   267,   268,   268,
     269,   269,   270,   270,   271,   271,   271,   272,   272,   273,
     273,   274,   274,   275,   275,   276,   276,   277,   277,   278,
     278,   279,   279,   280,   280,   280,   281,   281,   282,   282,
     282,   282,   282,   283,   283,   283,   284,   284,   284,   285,
     285,   285,   285,   285,   286,   286,   287,   287,   288,   288,
     288,   289,   289,   289,   289,   289,   290,   290,   291,   291,
     291,   291,   292,   292,   293,   293,   293,   294,   294,   294,
     295,   295,   295,   296,   296,   296,   297,   297,   298,   298,
     299,   299,   300,   300,   300,   300,   300,   301,   302,   302,
     302,   303,   303,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     306,   306,   307,   308,   308,   309,   309,   309,   309,   309,
     310,   310,   311,   311,   311,   311,   312,   312,   312,   312,
     312,   312,   313,   313,   313,   313,   314,   315,   314,   314,
     316,   316,   316,   316,   317,   317,   317,   318,   318,   318,
     318,   319,   319,   319,   320,   320,   320,   320,   320,   320,
     321,   321,   321,   322,   322,   323,   323,   325,   324,   326,
     324,   327,   324,   328,   324,   324,   329,   329,   330,   330,
     331,   331,   332,   332,   332,   333,   333,   333,   333,   333,
     333,   333,   333,   334,   334,   335,   335,   335,   335,   335,
     335,   335,   335,   335,   335,   336,   336,   336,   337,   337,
     337,   338,   338,   338,   339,   340,   340,   341,   341,   342,
     342,   343,   344,   345,   344,   344,   344,   344,   346,   344,
     344,   344,   344,   344,   347,   347,   348,   348,   348,   348,
     349,   349,   349,   350,   350,   350,   350,   350,   350,   350,
     351,   351,   351,   351,   352,   352,   353,   353,   353,   353,
     354,   354,   354,   354,   355,   355,   355,   355,   355,   356,
     356,   356,   356,   356,   357,   357,   358,   358,   359,   359,
     360,   360,   360,   361,   361,   361,   362,   362,   363,   363,
     363,   363,   364,   364,   365,   365,   365,   365,   365,   366,
     366,   367,   367,   368,   368,   368,   368,   368,   369,   369,
     370,   370,   372,   371,   373,   371,   371,   371,   374,   374,
     374,   374,   375,   375,   375,   375,   376,   376,   377,   377,
     378,   378,   379,   379,   379,   379,   380,   380,   380,   381,
     381,   382,   382,   383,   383,   384,   384,   385,   385,   386,
     386,   386,   387,   387,   388,   388,   389,   389,   390,   390,
     391,   392,   393,   393,   393,   393,   393,   393,   393,   393,
     393,   393,   393,   394,   393,   395,   393,   396,   393,   397,
     393,   398,   393,   399,   399,   399,   400,   400,   401,   401,
     401,   401,   401,   401,   401,   401,   401,   401,   402,   402,
     402,   403,   404,   404,   405,   405,   406,   406,   407,   408,
     408,   409,   409,   409,   410,   410,   410,   410,   410,   410,
     411,   411,   412,   412,   412,   412,   413,   413,   413,   413,
     414,   414,   414,   414,   414,   414,   414,   415,   415,   415,
     415,   416,   416,   416,   417,   417,   417,   417,   417,   418,
     418,   418,   418,   419,   419,   419,   419,   419,   419,   420,
     420,   420,   421,   421,   421,   421,   421,   422,   422,   422,
     422,   423,   423,   423,   423,   423,   423,   424,   424,   425,
     425,   425,   425,   426,   426,   426,   426,   427,   427,   427,
     427,   427,   427,   427,   428,   428,   428,   428,   428,   429,
     429,   429,   429,   429,   430,   430,   430,   431,   431,   431,
     431,   432,   432,   432,   433,   433,   433,   433,   433,   434,
     434,   435,   435,   435,   436,   436,   437,   437,   438,   438,
     438,   439,   439,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   442,   442,   442,   442,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   445,   445,
     446,   446,   446,   446,   447,   447,   447,   448,   448,   448,
     449,   449,   449,   449,   449,   449,   450,   450,   450,   451,
     451,   451,   451,   451,   452,   452,   452,   452,   453,   453,
     454,   454,   454,   455,   455,   456,   456,   456,   456,   456,
     456,   457,   457,   457,   457,   457,   457,   457,   457,   457,
     457,   458,   458,   458,   458,   459,   459,   459,   460,   460,
     461,   461,   461,   461,   461,   461,   462,   462,   462,   462,
     462,   462,   463,   463,   463,   464,   464,   465,   465,   466,
     466
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     2,     2,     2,     2,     2,     2,     1,     3,
       3,     3,     1,     6,     4,     4,     4,     4,     4,     3,
       3,     3,     3,     3,     2,     5,     3,     3,     3,     5,
       2,     2,     7,     8,     5,     0,     1,     1,     3,     1,
       1,     1,     3,     1,     2,     4,     3,     5,     3,     5,
       2,     2,     2,     0,     2,     1,     1,     1,     2,     2,
       2,     2,     2,     2,     4,     2,     4,     6,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     5,     5,
       4,     5,     5,     5,     4,     2,     2,     3,     3,     1,
       1,     1,     3,     1,     3,     3,     3,     1,     3,     3,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       3,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     5,     4,     1,     1,     3,     6,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     7,     1,     1,     3,
       3,     1,     3,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
       2,     6,     1,     2,     1,     2,     1,     2,     1,     1,
       2,     2,     2,     5,     3,     5,    10,     7,     5,    10,
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
       1,     1,     3,     6,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     4,     1,     2,
       3,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     5,     0,     1,     1,     2,     2,     3,     3,
       1,     3,     1,     2,     2,     2,     4,     4,     4,     4,
       1,     1,     1,     2,     2,     3,     1,     0,     3,     2,
       1,     2,     2,     3,     1,     2,     2,     1,     2,     2,
       3,     1,     2,     2,     1,     2,     3,     1,     2,     3,
       1,     3,     4,     1,     1,     1,     1,     0,     7,     0,
       8,     0,     8,     0,     8,     1,     0,     3,     3,     3,
       1,     1,     2,     1,     1,     1,     2,     1,     2,     1,
       2,     1,     2,     0,     2,     3,     4,     4,     3,     2,
       2,     3,     3,     2,     1,     0,     1,     4,     1,     2,
       2,     0,     1,     4,     1,     2,     3,     1,     2,     0,
       1,     2,     6,     0,     8,     7,     9,     8,     0,    12,
      10,    11,    10,     1,     3,     3,     2,     2,     4,     5,
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
       4,     1,     4,     7,    10,     1,     4,     2,     2,     1,
       1,     5,     2,     5,     0,     1,     3,     4,     0,     1,
       0,     0,     1,     1,     2,     2,     2,     2,     2,     2,
       1,     2,     5,     0,     6,     0,     8,     0,     7,     0,
       7,     0,     8,     1,     2,     3,     0,     5,     3,     4,
       4,     4,     4,     5,     5,     5,     5,     6,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     1,
       3,     0,     1,     4,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     1,     2,     2,     2,     3,     4,     5,
       2,     4,     5,     4,     5,     3,     4,     8,     9,     3,
       4,     2,     1,     2,     6,     8,     9,     3,     4,     2,
       3,     4,     5,     4,     5,     4,     5,     3,     4,     1,
       1,     1,     4,     8,     9,     3,     4,     2,     3,     3,
       4,     4,     5,     4,     5,     3,     4,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     6,     8,     9,     3,     4,     2,
       4,     1,     2,     2,     2,     3,     4,     2,     4,     4,
       3,     6,     8,     3,     2,     4,     1,     2,     2,     1,
       1,     2,     3,     4,     2,     4,     6,     8,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       5,     8,     3,     2,     3,     7,     1,     5,     5,     6,
       6,     1,     3,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     5,     8,     3,     1,     2,     1,
       2,     6,     5,     6,     7,     7,     1,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     8,     3,
       1,     1,     2,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     2,     4,     3,     2,     3,     2,     4,     3,
       2,     6,     6,     6,     7,     1,     2,     1,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     4,     2,     3,
       4,     2,     5,     6,     7,     6,     6,     0,     1,     0,
       2
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
#line 7542 "Parser/parser.cc"
    break;

  case 3:
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7548 "Parser/parser.cc"
    break;

  case 4:
#line 590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7554 "Parser/parser.cc"
    break;

  case 5:
#line 591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7560 "Parser/parser.cc"
    break;

  case 6:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7566 "Parser/parser.cc"
    break;

  case 7:
#line 593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 8:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7578 "Parser/parser.cc"
    break;

  case 19:
#line 615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7584 "Parser/parser.cc"
    break;

  case 20:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7590 "Parser/parser.cc"
    break;

  case 21:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7596 "Parser/parser.cc"
    break;

  case 22:
#line 625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7606 "Parser/parser.cc"
    break;

  case 23:
#line 636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 24:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 25:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 27:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7630 "Parser/parser.cc"
    break;

  case 28:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 29:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 30:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7648 "Parser/parser.cc"
    break;

  case 31:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7658 "Parser/parser.cc"
    break;

  case 32:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7664 "Parser/parser.cc"
    break;

  case 33:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7670 "Parser/parser.cc"
    break;

  case 34:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7676 "Parser/parser.cc"
    break;

  case 35:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7682 "Parser/parser.cc"
    break;

  case 36:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7688 "Parser/parser.cc"
    break;

  case 37:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7694 "Parser/parser.cc"
    break;

  case 39:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7705 "Parser/parser.cc"
    break;

  case 40:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7714 "Parser/parser.cc"
    break;

  case 41:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7720 "Parser/parser.cc"
    break;

  case 43:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7726 "Parser/parser.cc"
    break;

  case 44:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7732 "Parser/parser.cc"
    break;

  case 45:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7738 "Parser/parser.cc"
    break;

  case 46:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7744 "Parser/parser.cc"
    break;

  case 47:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7754 "Parser/parser.cc"
    break;

  case 48:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7760 "Parser/parser.cc"
    break;

  case 49:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7766 "Parser/parser.cc"
    break;

  case 50:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7772 "Parser/parser.cc"
    break;

  case 51:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7778 "Parser/parser.cc"
    break;

  case 52:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7784 "Parser/parser.cc"
    break;

  case 53:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7790 "Parser/parser.cc"
    break;

  case 54:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7796 "Parser/parser.cc"
    break;

  case 55:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7802 "Parser/parser.cc"
    break;

  case 56:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7808 "Parser/parser.cc"
    break;

  case 57:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7814 "Parser/parser.cc"
    break;

  case 58:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7820 "Parser/parser.cc"
    break;

  case 59:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7826 "Parser/parser.cc"
    break;

  case 60:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7832 "Parser/parser.cc"
    break;

  case 61:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7838 "Parser/parser.cc"
    break;

  case 62:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7844 "Parser/parser.cc"
    break;

  case 63:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7850 "Parser/parser.cc"
    break;

  case 64:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7860 "Parser/parser.cc"
    break;

  case 65:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7866 "Parser/parser.cc"
    break;

  case 68:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7872 "Parser/parser.cc"
    break;

  case 69:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7878 "Parser/parser.cc"
    break;

  case 72:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7884 "Parser/parser.cc"
    break;

  case 74:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7890 "Parser/parser.cc"
    break;

  case 75:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7896 "Parser/parser.cc"
    break;

  case 76:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 77:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 78:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 79:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 80:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 81:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7932 "Parser/parser.cc"
    break;

  case 82:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7940 "Parser/parser.cc"
    break;

  case 83:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7946 "Parser/parser.cc"
    break;

  case 84:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7955 "Parser/parser.cc"
    break;

  case 87:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7961 "Parser/parser.cc"
    break;

  case 88:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7967 "Parser/parser.cc"
    break;

  case 89:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7987 "Parser/parser.cc"
    break;

  case 90:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 91:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 92:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 93:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8011 "Parser/parser.cc"
    break;

  case 94:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 95:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8023 "Parser/parser.cc"
    break;

  case 96:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8029 "Parser/parser.cc"
    break;

  case 97:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8035 "Parser/parser.cc"
    break;

  case 98:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8044 "Parser/parser.cc"
    break;

  case 99:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8050 "Parser/parser.cc"
    break;

  case 100:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8056 "Parser/parser.cc"
    break;

  case 101:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8062 "Parser/parser.cc"
    break;

  case 102:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8068 "Parser/parser.cc"
    break;

  case 103:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8074 "Parser/parser.cc"
    break;

  case 104:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8080 "Parser/parser.cc"
    break;

  case 105:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8086 "Parser/parser.cc"
    break;

  case 107:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8092 "Parser/parser.cc"
    break;

  case 108:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8098 "Parser/parser.cc"
    break;

  case 109:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8104 "Parser/parser.cc"
    break;

  case 110:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8110 "Parser/parser.cc"
    break;

  case 111:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8116 "Parser/parser.cc"
    break;

  case 112:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8122 "Parser/parser.cc"
    break;

  case 113:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8128 "Parser/parser.cc"
    break;

  case 114:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8134 "Parser/parser.cc"
    break;

  case 122:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8140 "Parser/parser.cc"
    break;

  case 124:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8146 "Parser/parser.cc"
    break;

  case 125:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8152 "Parser/parser.cc"
    break;

  case 126:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8158 "Parser/parser.cc"
    break;

  case 128:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8164 "Parser/parser.cc"
    break;

  case 129:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8170 "Parser/parser.cc"
    break;

  case 131:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8176 "Parser/parser.cc"
    break;

  case 132:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8182 "Parser/parser.cc"
    break;

  case 134:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8188 "Parser/parser.cc"
    break;

  case 135:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8194 "Parser/parser.cc"
    break;

  case 136:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8200 "Parser/parser.cc"
    break;

  case 137:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8206 "Parser/parser.cc"
    break;

  case 139:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8212 "Parser/parser.cc"
    break;

  case 140:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8218 "Parser/parser.cc"
    break;

  case 142:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8224 "Parser/parser.cc"
    break;

  case 144:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8230 "Parser/parser.cc"
    break;

  case 146:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8236 "Parser/parser.cc"
    break;

  case 148:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8242 "Parser/parser.cc"
    break;

  case 150:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8248 "Parser/parser.cc"
    break;

  case 152:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8254 "Parser/parser.cc"
    break;

  case 153:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8260 "Parser/parser.cc"
    break;

  case 156:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8272 "Parser/parser.cc"
    break;

  case 157:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8278 "Parser/parser.cc"
    break;

  case 158:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8284 "Parser/parser.cc"
    break;

  case 162:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8290 "Parser/parser.cc"
    break;

  case 163:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8296 "Parser/parser.cc"
    break;

  case 164:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8302 "Parser/parser.cc"
    break;

  case 165:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8308 "Parser/parser.cc"
    break;

  case 166:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8314 "Parser/parser.cc"
    break;

  case 167:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8320 "Parser/parser.cc"
    break;

  case 168:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8326 "Parser/parser.cc"
    break;

  case 169:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8332 "Parser/parser.cc"
    break;

  case 170:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8338 "Parser/parser.cc"
    break;

  case 171:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8344 "Parser/parser.cc"
    break;

  case 172:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8350 "Parser/parser.cc"
    break;

  case 173:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8356 "Parser/parser.cc"
    break;

  case 174:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8362 "Parser/parser.cc"
    break;

  case 175:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8368 "Parser/parser.cc"
    break;

  case 176:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8374 "Parser/parser.cc"
    break;

  case 178:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8380 "Parser/parser.cc"
    break;

  case 179:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8386 "Parser/parser.cc"
    break;

  case 180:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8392 "Parser/parser.cc"
    break;

  case 182:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8398 "Parser/parser.cc"
    break;

  case 183:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8404 "Parser/parser.cc"
    break;

  case 195:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8410 "Parser/parser.cc"
    break;

  case 197:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8416 "Parser/parser.cc"
    break;

  case 198:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8422 "Parser/parser.cc"
    break;

  case 199:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8433 "Parser/parser.cc"
    break;

  case 200:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8439 "Parser/parser.cc"
    break;

  case 201:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8445 "Parser/parser.cc"
    break;

  case 203:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8451 "Parser/parser.cc"
    break;

  case 204:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8457 "Parser/parser.cc"
    break;

  case 205:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8463 "Parser/parser.cc"
    break;

  case 206:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8469 "Parser/parser.cc"
    break;

  case 207:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8475 "Parser/parser.cc"
    break;

  case 210:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8481 "Parser/parser.cc"
    break;

  case 211:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8487 "Parser/parser.cc"
    break;

  case 212:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8493 "Parser/parser.cc"
    break;

  case 213:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8499 "Parser/parser.cc"
    break;

  case 214:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8505 "Parser/parser.cc"
    break;

  case 215:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8511 "Parser/parser.cc"
    break;

  case 216:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8525 "Parser/parser.cc"
    break;

  case 217:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8531 "Parser/parser.cc"
    break;

  case 218:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 219:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8546 "Parser/parser.cc"
    break;

  case 220:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8552 "Parser/parser.cc"
    break;

  case 221:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 222:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 223:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 224:
#line 1213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8576 "Parser/parser.cc"
    break;

  case 225:
#line 1215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8582 "Parser/parser.cc"
    break;

  case 226:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 227:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8594 "Parser/parser.cc"
    break;

  case 228:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 230:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 231:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 232:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8618 "Parser/parser.cc"
    break;

  case 233:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8624 "Parser/parser.cc"
    break;

  case 234:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8630 "Parser/parser.cc"
    break;

  case 235:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8636 "Parser/parser.cc"
    break;

  case 236:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8642 "Parser/parser.cc"
    break;

  case 238:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8648 "Parser/parser.cc"
    break;

  case 239:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 240:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8660 "Parser/parser.cc"
    break;

  case 242:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 243:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 244:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8678 "Parser/parser.cc"
    break;

  case 245:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8687 "Parser/parser.cc"
    break;

  case 246:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 247:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 248:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 249:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8714 "Parser/parser.cc"
    break;

  case 250:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8720 "Parser/parser.cc"
    break;

  case 251:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8726 "Parser/parser.cc"
    break;

  case 252:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8732 "Parser/parser.cc"
    break;

  case 253:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8741 "Parser/parser.cc"
    break;

  case 254:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8747 "Parser/parser.cc"
    break;

  case 255:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8753 "Parser/parser.cc"
    break;

  case 257:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8772 "Parser/parser.cc"
    break;

  case 258:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 259:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8784 "Parser/parser.cc"
    break;

  case 260:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8790 "Parser/parser.cc"
    break;

  case 261:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8796 "Parser/parser.cc"
    break;

  case 262:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8802 "Parser/parser.cc"
    break;

  case 263:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8808 "Parser/parser.cc"
    break;

  case 264:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8814 "Parser/parser.cc"
    break;

  case 265:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8820 "Parser/parser.cc"
    break;

  case 266:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8829 "Parser/parser.cc"
    break;

  case 267:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8838 "Parser/parser.cc"
    break;

  case 268:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 269:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8853 "Parser/parser.cc"
    break;

  case 270:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8862 "Parser/parser.cc"
    break;

  case 271:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8868 "Parser/parser.cc"
    break;

  case 272:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8874 "Parser/parser.cc"
    break;

  case 273:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8880 "Parser/parser.cc"
    break;

  case 274:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8886 "Parser/parser.cc"
    break;

  case 275:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8892 "Parser/parser.cc"
    break;

  case 276:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8898 "Parser/parser.cc"
    break;

  case 277:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8904 "Parser/parser.cc"
    break;

  case 278:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8910 "Parser/parser.cc"
    break;

  case 279:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8919 "Parser/parser.cc"
    break;

  case 280:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8929 "Parser/parser.cc"
    break;

  case 281:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8935 "Parser/parser.cc"
    break;

  case 282:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8941 "Parser/parser.cc"
    break;

  case 283:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8950 "Parser/parser.cc"
    break;

  case 284:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8960 "Parser/parser.cc"
    break;

  case 285:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8966 "Parser/parser.cc"
    break;

  case 286:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8975 "Parser/parser.cc"
    break;

  case 287:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8985 "Parser/parser.cc"
    break;

  case 288:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8991 "Parser/parser.cc"
    break;

  case 289:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8997 "Parser/parser.cc"
    break;

  case 290:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9003 "Parser/parser.cc"
    break;

  case 291:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9009 "Parser/parser.cc"
    break;

  case 292:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9018 "Parser/parser.cc"
    break;

  case 293:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9028 "Parser/parser.cc"
    break;

  case 294:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9034 "Parser/parser.cc"
    break;

  case 295:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9043 "Parser/parser.cc"
    break;

  case 296:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9053 "Parser/parser.cc"
    break;

  case 297:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9059 "Parser/parser.cc"
    break;

  case 298:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9068 "Parser/parser.cc"
    break;

  case 299:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9078 "Parser/parser.cc"
    break;

  case 300:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9084 "Parser/parser.cc"
    break;

  case 301:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9093 "Parser/parser.cc"
    break;

  case 302:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9102 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9108 "Parser/parser.cc"
    break;

  case 304:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9114 "Parser/parser.cc"
    break;

  case 305:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9120 "Parser/parser.cc"
    break;

  case 306:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9126 "Parser/parser.cc"
    break;

  case 307:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9132 "Parser/parser.cc"
    break;

  case 309:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9138 "Parser/parser.cc"
    break;

  case 310:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9144 "Parser/parser.cc"
    break;

  case 311:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 312:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 313:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 314:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 315:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9174 "Parser/parser.cc"
    break;

  case 316:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 317:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9186 "Parser/parser.cc"
    break;

  case 318:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9192 "Parser/parser.cc"
    break;

  case 319:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9198 "Parser/parser.cc"
    break;

  case 320:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9204 "Parser/parser.cc"
    break;

  case 321:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9210 "Parser/parser.cc"
    break;

  case 322:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9216 "Parser/parser.cc"
    break;

  case 323:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 324:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9228 "Parser/parser.cc"
    break;

  case 325:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9234 "Parser/parser.cc"
    break;

  case 326:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9240 "Parser/parser.cc"
    break;

  case 327:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 328:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9252 "Parser/parser.cc"
    break;

  case 329:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9258 "Parser/parser.cc"
    break;

  case 330:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9264 "Parser/parser.cc"
    break;

  case 333:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9270 "Parser/parser.cc"
    break;

  case 334:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9276 "Parser/parser.cc"
    break;

  case 335:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9282 "Parser/parser.cc"
    break;

  case 336:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9288 "Parser/parser.cc"
    break;

  case 338:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9294 "Parser/parser.cc"
    break;

  case 339:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9300 "Parser/parser.cc"
    break;

  case 341:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9306 "Parser/parser.cc"
    break;

  case 342:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9312 "Parser/parser.cc"
    break;

  case 343:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9318 "Parser/parser.cc"
    break;

  case 344:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9324 "Parser/parser.cc"
    break;

  case 345:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9330 "Parser/parser.cc"
    break;

  case 346:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9336 "Parser/parser.cc"
    break;

  case 347:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9342 "Parser/parser.cc"
    break;

  case 348:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9348 "Parser/parser.cc"
    break;

  case 349:
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9354 "Parser/parser.cc"
    break;

  case 350:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9360 "Parser/parser.cc"
    break;

  case 351:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9366 "Parser/parser.cc"
    break;

  case 352:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9372 "Parser/parser.cc"
    break;

  case 353:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9378 "Parser/parser.cc"
    break;

  case 354:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 355:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9390 "Parser/parser.cc"
    break;

  case 356:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9396 "Parser/parser.cc"
    break;

  case 357:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9402 "Parser/parser.cc"
    break;

  case 358:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9408 "Parser/parser.cc"
    break;

  case 359:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9414 "Parser/parser.cc"
    break;

  case 360:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9420 "Parser/parser.cc"
    break;

  case 361:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9426 "Parser/parser.cc"
    break;

  case 362:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 364:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9438 "Parser/parser.cc"
    break;

  case 365:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9444 "Parser/parser.cc"
    break;

  case 366:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9450 "Parser/parser.cc"
    break;

  case 371:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9456 "Parser/parser.cc"
    break;

  case 372:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9462 "Parser/parser.cc"
    break;

  case 373:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9468 "Parser/parser.cc"
    break;

  case 374:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 375:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 376:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9486 "Parser/parser.cc"
    break;

  case 377:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9492 "Parser/parser.cc"
    break;

  case 378:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9498 "Parser/parser.cc"
    break;

  case 381:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9504 "Parser/parser.cc"
    break;

  case 382:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 383:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 384:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9522 "Parser/parser.cc"
    break;

  case 385:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 386:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9534 "Parser/parser.cc"
    break;

  case 387:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9543 "Parser/parser.cc"
    break;

  case 388:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9552 "Parser/parser.cc"
    break;

  case 389:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9558 "Parser/parser.cc"
    break;

  case 392:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9564 "Parser/parser.cc"
    break;

  case 393:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9570 "Parser/parser.cc"
    break;

  case 395:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9576 "Parser/parser.cc"
    break;

  case 396:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9582 "Parser/parser.cc"
    break;

  case 403:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9593 "Parser/parser.cc"
    break;

  case 406:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9599 "Parser/parser.cc"
    break;

  case 407:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9605 "Parser/parser.cc"
    break;

  case 411:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9611 "Parser/parser.cc"
    break;

  case 413:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9617 "Parser/parser.cc"
    break;

  case 414:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9623 "Parser/parser.cc"
    break;

  case 415:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9629 "Parser/parser.cc"
    break;

  case 416:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9635 "Parser/parser.cc"
    break;

  case 417:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9641 "Parser/parser.cc"
    break;

  case 418:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9647 "Parser/parser.cc"
    break;

  case 420:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9653 "Parser/parser.cc"
    break;

  case 421:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9659 "Parser/parser.cc"
    break;

  case 422:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9665 "Parser/parser.cc"
    break;

  case 423:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9676 "Parser/parser.cc"
    break;

  case 424:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 425:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 426:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9694 "Parser/parser.cc"
    break;

  case 427:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9700 "Parser/parser.cc"
    break;

  case 428:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9709 "Parser/parser.cc"
    break;

  case 429:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9718 "Parser/parser.cc"
    break;

  case 430:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9727 "Parser/parser.cc"
    break;

  case 431:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9736 "Parser/parser.cc"
    break;

  case 432:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9745 "Parser/parser.cc"
    break;

  case 433:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9754 "Parser/parser.cc"
    break;

  case 434:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9763 "Parser/parser.cc"
    break;

  case 435:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9772 "Parser/parser.cc"
    break;

  case 436:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9780 "Parser/parser.cc"
    break;

  case 437:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9788 "Parser/parser.cc"
    break;

  case 438:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9794 "Parser/parser.cc"
    break;

  case 442:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9800 "Parser/parser.cc"
    break;

  case 443:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 451:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9817 "Parser/parser.cc"
    break;

  case 456:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9823 "Parser/parser.cc"
    break;

  case 459:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9829 "Parser/parser.cc"
    break;

  case 462:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9835 "Parser/parser.cc"
    break;

  case 463:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9841 "Parser/parser.cc"
    break;

  case 464:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9847 "Parser/parser.cc"
    break;

  case 465:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9853 "Parser/parser.cc"
    break;

  case 467:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 469:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 470:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9871 "Parser/parser.cc"
    break;

  case 472:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9877 "Parser/parser.cc"
    break;

  case 473:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9883 "Parser/parser.cc"
    break;

  case 474:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9889 "Parser/parser.cc"
    break;

  case 475:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9895 "Parser/parser.cc"
    break;

  case 476:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9901 "Parser/parser.cc"
    break;

  case 477:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9907 "Parser/parser.cc"
    break;

  case 478:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9913 "Parser/parser.cc"
    break;

  case 479:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9919 "Parser/parser.cc"
    break;

  case 480:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9925 "Parser/parser.cc"
    break;

  case 481:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9931 "Parser/parser.cc"
    break;

  case 482:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9937 "Parser/parser.cc"
    break;

  case 483:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9943 "Parser/parser.cc"
    break;

  case 484:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9949 "Parser/parser.cc"
    break;

  case 485:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9955 "Parser/parser.cc"
    break;

  case 486:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9961 "Parser/parser.cc"
    break;

  case 487:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9967 "Parser/parser.cc"
    break;

  case 488:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9973 "Parser/parser.cc"
    break;

  case 489:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9979 "Parser/parser.cc"
    break;

  case 490:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9985 "Parser/parser.cc"
    break;

  case 491:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9991 "Parser/parser.cc"
    break;

  case 492:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9997 "Parser/parser.cc"
    break;

  case 493:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10003 "Parser/parser.cc"
    break;

  case 494:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10009 "Parser/parser.cc"
    break;

  case 495:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10015 "Parser/parser.cc"
    break;

  case 496:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10021 "Parser/parser.cc"
    break;

  case 497:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10027 "Parser/parser.cc"
    break;

  case 498:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10033 "Parser/parser.cc"
    break;

  case 499:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10039 "Parser/parser.cc"
    break;

  case 500:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10045 "Parser/parser.cc"
    break;

  case 501:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10051 "Parser/parser.cc"
    break;

  case 502:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10057 "Parser/parser.cc"
    break;

  case 503:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10063 "Parser/parser.cc"
    break;

  case 504:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10069 "Parser/parser.cc"
    break;

  case 505:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10075 "Parser/parser.cc"
    break;

  case 506:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10081 "Parser/parser.cc"
    break;

  case 507:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10087 "Parser/parser.cc"
    break;

  case 508:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10093 "Parser/parser.cc"
    break;

  case 510:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10099 "Parser/parser.cc"
    break;

  case 512:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10105 "Parser/parser.cc"
    break;

  case 513:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10111 "Parser/parser.cc"
    break;

  case 514:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10117 "Parser/parser.cc"
    break;

  case 516:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 517:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10129 "Parser/parser.cc"
    break;

  case 518:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 519:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10141 "Parser/parser.cc"
    break;

  case 521:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10147 "Parser/parser.cc"
    break;

  case 523:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 524:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 525:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 526:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10171 "Parser/parser.cc"
    break;

  case 527:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 528:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10183 "Parser/parser.cc"
    break;

  case 529:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10189 "Parser/parser.cc"
    break;

  case 530:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10195 "Parser/parser.cc"
    break;

  case 531:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10201 "Parser/parser.cc"
    break;

  case 532:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10212 "Parser/parser.cc"
    break;

  case 533:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10218 "Parser/parser.cc"
    break;

  case 534:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10224 "Parser/parser.cc"
    break;

  case 535:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10230 "Parser/parser.cc"
    break;

  case 536:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10241 "Parser/parser.cc"
    break;

  case 537:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10247 "Parser/parser.cc"
    break;

  case 538:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10253 "Parser/parser.cc"
    break;

  case 539:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10262 "Parser/parser.cc"
    break;

  case 541:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10268 "Parser/parser.cc"
    break;

  case 542:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10274 "Parser/parser.cc"
    break;

  case 543:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10280 "Parser/parser.cc"
    break;

  case 545:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 546:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 548:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 549:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 550:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 552:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10316 "Parser/parser.cc"
    break;

  case 553:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10322 "Parser/parser.cc"
    break;

  case 554:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10328 "Parser/parser.cc"
    break;

  case 555:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10334 "Parser/parser.cc"
    break;

  case 556:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10340 "Parser/parser.cc"
    break;

  case 558:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10346 "Parser/parser.cc"
    break;

  case 559:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10352 "Parser/parser.cc"
    break;

  case 560:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10358 "Parser/parser.cc"
    break;

  case 561:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10364 "Parser/parser.cc"
    break;

  case 562:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10370 "Parser/parser.cc"
    break;

  case 563:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10381 "Parser/parser.cc"
    break;

  case 567:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10387 "Parser/parser.cc"
    break;

  case 568:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 569:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10402 "Parser/parser.cc"
    break;

  case 570:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10419 "Parser/parser.cc"
    break;

  case 571:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10428 "Parser/parser.cc"
    break;

  case 572:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10438 "Parser/parser.cc"
    break;

  case 573:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10447 "Parser/parser.cc"
    break;

  case 574:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10457 "Parser/parser.cc"
    break;

  case 576:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10463 "Parser/parser.cc"
    break;

  case 577:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10469 "Parser/parser.cc"
    break;

  case 578:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10479 "Parser/parser.cc"
    break;

  case 579:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10494 "Parser/parser.cc"
    break;

  case 582:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10500 "Parser/parser.cc"
    break;

  case 583:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10506 "Parser/parser.cc"
    break;

  case 584:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10512 "Parser/parser.cc"
    break;

  case 585:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10518 "Parser/parser.cc"
    break;

  case 586:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10524 "Parser/parser.cc"
    break;

  case 587:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10530 "Parser/parser.cc"
    break;

  case 588:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10536 "Parser/parser.cc"
    break;

  case 589:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10542 "Parser/parser.cc"
    break;

  case 590:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10548 "Parser/parser.cc"
    break;

  case 591:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10554 "Parser/parser.cc"
    break;

  case 592:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10560 "Parser/parser.cc"
    break;

  case 593:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10566 "Parser/parser.cc"
    break;

  case 594:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10572 "Parser/parser.cc"
    break;

  case 595:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10585 "Parser/parser.cc"
    break;

  case 596:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 597:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10604 "Parser/parser.cc"
    break;

  case 598:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10610 "Parser/parser.cc"
    break;

  case 601:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10616 "Parser/parser.cc"
    break;

  case 602:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10622 "Parser/parser.cc"
    break;

  case 605:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10628 "Parser/parser.cc"
    break;

  case 607:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 608:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 609:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10646 "Parser/parser.cc"
    break;

  case 610:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 611:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10658 "Parser/parser.cc"
    break;

  case 613:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 615:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 616:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 618:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 619:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10688 "Parser/parser.cc"
    break;

  case 621:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10694 "Parser/parser.cc"
    break;

  case 622:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 623:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10706 "Parser/parser.cc"
    break;

  case 624:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 625:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 626:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10729 "Parser/parser.cc"
    break;

  case 627:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10737 "Parser/parser.cc"
    break;

  case 628:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10746 "Parser/parser.cc"
    break;

  case 629:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10754 "Parser/parser.cc"
    break;

  case 630:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10762 "Parser/parser.cc"
    break;

  case 631:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10770 "Parser/parser.cc"
    break;

  case 632:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10778 "Parser/parser.cc"
    break;

  case 634:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10784 "Parser/parser.cc"
    break;

  case 635:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 636:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 637:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10802 "Parser/parser.cc"
    break;

  case 638:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 639:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 640:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10820 "Parser/parser.cc"
    break;

  case 641:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 642:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10832 "Parser/parser.cc"
    break;

  case 643:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10838 "Parser/parser.cc"
    break;

  case 644:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10844 "Parser/parser.cc"
    break;

  case 647:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 648:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10856 "Parser/parser.cc"
    break;

  case 649:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10862 "Parser/parser.cc"
    break;

  case 651:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 652:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 653:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 655:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 656:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10892 "Parser/parser.cc"
    break;

  case 657:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10898 "Parser/parser.cc"
    break;

  case 659:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10904 "Parser/parser.cc"
    break;

  case 662:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 663:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 665:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 666:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 667:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 672:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 674:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10946 "Parser/parser.cc"
    break;

  case 675:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10952 "Parser/parser.cc"
    break;

  case 676:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10958 "Parser/parser.cc"
    break;

  case 677:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10964 "Parser/parser.cc"
    break;

  case 678:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10970 "Parser/parser.cc"
    break;

  case 679:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10976 "Parser/parser.cc"
    break;

  case 685:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10982 "Parser/parser.cc"
    break;

  case 688:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10988 "Parser/parser.cc"
    break;

  case 689:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10994 "Parser/parser.cc"
    break;

  case 690:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11000 "Parser/parser.cc"
    break;

  case 691:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11006 "Parser/parser.cc"
    break;

  case 692:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11012 "Parser/parser.cc"
    break;

  case 693:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11018 "Parser/parser.cc"
    break;

  case 694:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11024 "Parser/parser.cc"
    break;

  case 696:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 697:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11036 "Parser/parser.cc"
    break;

  case 698:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11042 "Parser/parser.cc"
    break;

  case 700:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11048 "Parser/parser.cc"
    break;

  case 702:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11054 "Parser/parser.cc"
    break;

  case 703:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11060 "Parser/parser.cc"
    break;

  case 704:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11066 "Parser/parser.cc"
    break;

  case 705:
#line 2792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11072 "Parser/parser.cc"
    break;

  case 706:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11078 "Parser/parser.cc"
    break;

  case 707:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11084 "Parser/parser.cc"
    break;

  case 709:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11090 "Parser/parser.cc"
    break;

  case 710:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11096 "Parser/parser.cc"
    break;

  case 711:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11102 "Parser/parser.cc"
    break;

  case 712:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11113 "Parser/parser.cc"
    break;

  case 713:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 714:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11125 "Parser/parser.cc"
    break;

  case 715:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 716:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11140 "Parser/parser.cc"
    break;

  case 717:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11146 "Parser/parser.cc"
    break;

  case 718:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11152 "Parser/parser.cc"
    break;

  case 719:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11158 "Parser/parser.cc"
    break;

  case 720:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11164 "Parser/parser.cc"
    break;

  case 721:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11170 "Parser/parser.cc"
    break;

  case 722:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11176 "Parser/parser.cc"
    break;

  case 723:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11182 "Parser/parser.cc"
    break;

  case 724:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11188 "Parser/parser.cc"
    break;

  case 725:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11194 "Parser/parser.cc"
    break;

  case 726:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11200 "Parser/parser.cc"
    break;

  case 729:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11206 "Parser/parser.cc"
    break;

  case 730:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 731:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11218 "Parser/parser.cc"
    break;

  case 732:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11224 "Parser/parser.cc"
    break;

  case 734:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11230 "Parser/parser.cc"
    break;

  case 735:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11236 "Parser/parser.cc"
    break;

  case 736:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11242 "Parser/parser.cc"
    break;

  case 737:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 738:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 739:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 740:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 741:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 742:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11284 "Parser/parser.cc"
    break;

  case 743:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11290 "Parser/parser.cc"
    break;

  case 744:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11296 "Parser/parser.cc"
    break;

  case 746:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 751:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 752:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11314 "Parser/parser.cc"
    break;

  case 753:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 755:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11326 "Parser/parser.cc"
    break;

  case 756:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11332 "Parser/parser.cc"
    break;

  case 757:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11338 "Parser/parser.cc"
    break;

  case 758:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11344 "Parser/parser.cc"
    break;

  case 760:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11350 "Parser/parser.cc"
    break;

  case 761:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11356 "Parser/parser.cc"
    break;

  case 762:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 764:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11368 "Parser/parser.cc"
    break;

  case 765:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11374 "Parser/parser.cc"
    break;

  case 766:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11380 "Parser/parser.cc"
    break;

  case 767:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11386 "Parser/parser.cc"
    break;

  case 768:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11392 "Parser/parser.cc"
    break;

  case 769:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11398 "Parser/parser.cc"
    break;

  case 771:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11407 "Parser/parser.cc"
    break;

  case 772:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 773:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11422 "Parser/parser.cc"
    break;

  case 774:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11432 "Parser/parser.cc"
    break;

  case 775:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11441 "Parser/parser.cc"
    break;

  case 776:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11451 "Parser/parser.cc"
    break;

  case 777:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11460 "Parser/parser.cc"
    break;

  case 778:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11470 "Parser/parser.cc"
    break;

  case 779:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11479 "Parser/parser.cc"
    break;

  case 780:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11489 "Parser/parser.cc"
    break;

  case 781:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11498 "Parser/parser.cc"
    break;

  case 782:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11508 "Parser/parser.cc"
    break;

  case 784:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11514 "Parser/parser.cc"
    break;

  case 785:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11520 "Parser/parser.cc"
    break;

  case 786:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11526 "Parser/parser.cc"
    break;

  case 787:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11538 "Parser/parser.cc"
    break;

  case 788:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11549 "Parser/parser.cc"
    break;

  case 789:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11558 "Parser/parser.cc"
    break;

  case 790:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11567 "Parser/parser.cc"
    break;

  case 791:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11573 "Parser/parser.cc"
    break;

  case 792:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11579 "Parser/parser.cc"
    break;

  case 793:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11585 "Parser/parser.cc"
    break;

  case 794:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11594 "Parser/parser.cc"
    break;

  case 795:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 796:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 797:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 801:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 802:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11624 "Parser/parser.cc"
    break;

  case 803:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11634 "Parser/parser.cc"
    break;

  case 804:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11640 "Parser/parser.cc"
    break;

  case 807:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 808:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11652 "Parser/parser.cc"
    break;

  case 810:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11658 "Parser/parser.cc"
    break;

  case 811:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11664 "Parser/parser.cc"
    break;

  case 812:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11670 "Parser/parser.cc"
    break;

  case 813:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11676 "Parser/parser.cc"
    break;

  case 818:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11682 "Parser/parser.cc"
    break;

  case 819:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11688 "Parser/parser.cc"
    break;

  case 820:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 821:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11700 "Parser/parser.cc"
    break;

  case 822:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 824:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 825:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 826:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 827:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 828:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11736 "Parser/parser.cc"
    break;

  case 829:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 830:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 831:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11754 "Parser/parser.cc"
    break;

  case 832:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11760 "Parser/parser.cc"
    break;

  case 833:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11766 "Parser/parser.cc"
    break;

  case 834:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11772 "Parser/parser.cc"
    break;

  case 835:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11778 "Parser/parser.cc"
    break;

  case 836:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11784 "Parser/parser.cc"
    break;

  case 837:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11790 "Parser/parser.cc"
    break;

  case 838:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11796 "Parser/parser.cc"
    break;

  case 839:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11802 "Parser/parser.cc"
    break;

  case 840:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11808 "Parser/parser.cc"
    break;

  case 841:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11814 "Parser/parser.cc"
    break;

  case 843:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11820 "Parser/parser.cc"
    break;

  case 844:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11826 "Parser/parser.cc"
    break;

  case 845:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11832 "Parser/parser.cc"
    break;

  case 846:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11838 "Parser/parser.cc"
    break;

  case 847:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11844 "Parser/parser.cc"
    break;

  case 848:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11850 "Parser/parser.cc"
    break;

  case 849:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11856 "Parser/parser.cc"
    break;

  case 850:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11862 "Parser/parser.cc"
    break;

  case 851:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11868 "Parser/parser.cc"
    break;

  case 852:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11874 "Parser/parser.cc"
    break;

  case 853:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11880 "Parser/parser.cc"
    break;

  case 854:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11886 "Parser/parser.cc"
    break;

  case 855:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11892 "Parser/parser.cc"
    break;

  case 856:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11898 "Parser/parser.cc"
    break;

  case 857:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11904 "Parser/parser.cc"
    break;

  case 858:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11910 "Parser/parser.cc"
    break;

  case 862:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11916 "Parser/parser.cc"
    break;

  case 863:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11922 "Parser/parser.cc"
    break;

  case 864:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11928 "Parser/parser.cc"
    break;

  case 865:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11934 "Parser/parser.cc"
    break;

  case 866:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11940 "Parser/parser.cc"
    break;

  case 867:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11946 "Parser/parser.cc"
    break;

  case 868:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11952 "Parser/parser.cc"
    break;

  case 869:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11958 "Parser/parser.cc"
    break;

  case 870:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11964 "Parser/parser.cc"
    break;

  case 871:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11970 "Parser/parser.cc"
    break;

  case 872:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11976 "Parser/parser.cc"
    break;

  case 873:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11982 "Parser/parser.cc"
    break;

  case 874:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11988 "Parser/parser.cc"
    break;

  case 875:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11994 "Parser/parser.cc"
    break;

  case 876:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12000 "Parser/parser.cc"
    break;

  case 877:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12009 "Parser/parser.cc"
    break;

  case 878:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12015 "Parser/parser.cc"
    break;

  case 879:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12021 "Parser/parser.cc"
    break;

  case 881:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12027 "Parser/parser.cc"
    break;

  case 882:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 883:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12039 "Parser/parser.cc"
    break;

  case 884:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12045 "Parser/parser.cc"
    break;

  case 885:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12051 "Parser/parser.cc"
    break;

  case 886:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12057 "Parser/parser.cc"
    break;

  case 887:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12063 "Parser/parser.cc"
    break;

  case 888:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12069 "Parser/parser.cc"
    break;

  case 889:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12075 "Parser/parser.cc"
    break;

  case 890:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 891:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12087 "Parser/parser.cc"
    break;

  case 892:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12093 "Parser/parser.cc"
    break;

  case 893:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 894:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 895:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12111 "Parser/parser.cc"
    break;

  case 896:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12117 "Parser/parser.cc"
    break;

  case 897:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12123 "Parser/parser.cc"
    break;

  case 898:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12129 "Parser/parser.cc"
    break;

  case 899:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12135 "Parser/parser.cc"
    break;

  case 900:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12141 "Parser/parser.cc"
    break;

  case 902:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 903:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12153 "Parser/parser.cc"
    break;

  case 904:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12159 "Parser/parser.cc"
    break;

  case 905:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12165 "Parser/parser.cc"
    break;

  case 906:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12171 "Parser/parser.cc"
    break;

  case 907:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12177 "Parser/parser.cc"
    break;

  case 908:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12183 "Parser/parser.cc"
    break;

  case 909:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12189 "Parser/parser.cc"
    break;

  case 910:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12195 "Parser/parser.cc"
    break;

  case 911:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12201 "Parser/parser.cc"
    break;

  case 912:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 913:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12213 "Parser/parser.cc"
    break;

  case 914:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12219 "Parser/parser.cc"
    break;

  case 915:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12225 "Parser/parser.cc"
    break;

  case 917:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12231 "Parser/parser.cc"
    break;

  case 918:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12237 "Parser/parser.cc"
    break;

  case 919:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12243 "Parser/parser.cc"
    break;

  case 920:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12249 "Parser/parser.cc"
    break;

  case 921:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12255 "Parser/parser.cc"
    break;

  case 922:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12261 "Parser/parser.cc"
    break;

  case 923:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12267 "Parser/parser.cc"
    break;

  case 924:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12273 "Parser/parser.cc"
    break;

  case 925:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12279 "Parser/parser.cc"
    break;

  case 926:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12285 "Parser/parser.cc"
    break;

  case 927:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12291 "Parser/parser.cc"
    break;

  case 929:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12297 "Parser/parser.cc"
    break;

  case 930:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 931:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12309 "Parser/parser.cc"
    break;

  case 932:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12315 "Parser/parser.cc"
    break;

  case 933:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12321 "Parser/parser.cc"
    break;

  case 934:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12327 "Parser/parser.cc"
    break;

  case 935:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12333 "Parser/parser.cc"
    break;

  case 937:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12339 "Parser/parser.cc"
    break;

  case 938:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12345 "Parser/parser.cc"
    break;

  case 939:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12351 "Parser/parser.cc"
    break;

  case 940:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12357 "Parser/parser.cc"
    break;

  case 941:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12363 "Parser/parser.cc"
    break;

  case 942:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12369 "Parser/parser.cc"
    break;

  case 943:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12375 "Parser/parser.cc"
    break;

  case 944:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12381 "Parser/parser.cc"
    break;

  case 945:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12387 "Parser/parser.cc"
    break;

  case 947:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12393 "Parser/parser.cc"
    break;

  case 948:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12399 "Parser/parser.cc"
    break;

  case 949:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12405 "Parser/parser.cc"
    break;

  case 950:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12411 "Parser/parser.cc"
    break;

  case 952:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12417 "Parser/parser.cc"
    break;

  case 953:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12423 "Parser/parser.cc"
    break;

  case 954:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12429 "Parser/parser.cc"
    break;

  case 955:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12435 "Parser/parser.cc"
    break;

  case 956:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12441 "Parser/parser.cc"
    break;

  case 957:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12447 "Parser/parser.cc"
    break;

  case 958:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12453 "Parser/parser.cc"
    break;

  case 959:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12459 "Parser/parser.cc"
    break;

  case 961:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12465 "Parser/parser.cc"
    break;

  case 962:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12471 "Parser/parser.cc"
    break;

  case 963:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12477 "Parser/parser.cc"
    break;

  case 964:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12483 "Parser/parser.cc"
    break;

  case 965:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12489 "Parser/parser.cc"
    break;

  case 966:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12495 "Parser/parser.cc"
    break;

  case 968:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12501 "Parser/parser.cc"
    break;

  case 970:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12507 "Parser/parser.cc"
    break;

  case 971:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12513 "Parser/parser.cc"
    break;

  case 972:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12519 "Parser/parser.cc"
    break;

  case 973:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12525 "Parser/parser.cc"
    break;

  case 974:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12531 "Parser/parser.cc"
    break;

  case 975:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12537 "Parser/parser.cc"
    break;

  case 977:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12543 "Parser/parser.cc"
    break;

  case 978:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12549 "Parser/parser.cc"
    break;

  case 979:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12555 "Parser/parser.cc"
    break;

  case 980:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12561 "Parser/parser.cc"
    break;

  case 981:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12567 "Parser/parser.cc"
    break;

  case 982:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12573 "Parser/parser.cc"
    break;

  case 983:
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12579 "Parser/parser.cc"
    break;

  case 985:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12585 "Parser/parser.cc"
    break;

  case 986:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12591 "Parser/parser.cc"
    break;

  case 987:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12597 "Parser/parser.cc"
    break;

  case 988:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12603 "Parser/parser.cc"
    break;

  case 989:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12609 "Parser/parser.cc"
    break;

  case 992:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12615 "Parser/parser.cc"
    break;

  case 995:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12621 "Parser/parser.cc"
    break;

  case 996:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12627 "Parser/parser.cc"
    break;

  case 997:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12633 "Parser/parser.cc"
    break;

  case 998:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12639 "Parser/parser.cc"
    break;

  case 999:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12645 "Parser/parser.cc"
    break;

  case 1000:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12651 "Parser/parser.cc"
    break;

  case 1001:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12657 "Parser/parser.cc"
    break;

  case 1002:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12663 "Parser/parser.cc"
    break;

  case 1003:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12669 "Parser/parser.cc"
    break;

  case 1004:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12675 "Parser/parser.cc"
    break;

  case 1005:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12681 "Parser/parser.cc"
    break;

  case 1006:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12687 "Parser/parser.cc"
    break;

  case 1007:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12693 "Parser/parser.cc"
    break;

  case 1008:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12699 "Parser/parser.cc"
    break;

  case 1009:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12705 "Parser/parser.cc"
    break;

  case 1010:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12711 "Parser/parser.cc"
    break;

  case 1011:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12717 "Parser/parser.cc"
    break;

  case 1012:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12723 "Parser/parser.cc"
    break;

  case 1013:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12729 "Parser/parser.cc"
    break;

  case 1014:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12735 "Parser/parser.cc"
    break;

  case 1016:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 1020:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12747 "Parser/parser.cc"
    break;

  case 1021:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 1022:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 1023:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 1024:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 1025:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12777 "Parser/parser.cc"
    break;

  case 1026:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12783 "Parser/parser.cc"
    break;

  case 1027:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12789 "Parser/parser.cc"
    break;

  case 1028:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12795 "Parser/parser.cc"
    break;

  case 1029:
#line 3926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12801 "Parser/parser.cc"
    break;

  case 1030:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12807 "Parser/parser.cc"
    break;

  case 1031:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12813 "Parser/parser.cc"
    break;

  case 1032:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12819 "Parser/parser.cc"
    break;

  case 1033:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12825 "Parser/parser.cc"
    break;

  case 1034:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12831 "Parser/parser.cc"
    break;

  case 1035:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12837 "Parser/parser.cc"
    break;

  case 1036:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12843 "Parser/parser.cc"
    break;

  case 1039:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12849 "Parser/parser.cc"
    break;

  case 1040:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12855 "Parser/parser.cc"
    break;


#line 12859 "Parser/parser.cc"

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
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
