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

#include "DeclarationNode.hpp"                          // for DeclarationNode, ...
#include "ExpressionNode.hpp"                           // for ExpressionNode, ...
#include "InitializerNode.hpp"                          // for InitializerNode, ...
#include "ParserTypes.hpp"
#include "StatementNode.hpp"                            // for build_...
#include "TypedefTable.hpp"
#include "TypeData.hpp"
#include "AST/Type.hpp"                                 // for BasicType, BasicKind
#include "Common/SemanticError.hpp"                     // error_str
#include "Common/Utility.hpp"                           // for maybeMoveBuild, maybeBuild, CodeLo...

// lex uses __null in a boolean context, it's fine.
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wparentheses-equality"
#endif

extern DeclarationNode * parseTree;
extern ast::Linkage::Spec linkage;
extern TypedefTable typedefTable;

stack<ast::Linkage::Spec> linkageStack;

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
	// Distribute type specifier across all declared variables, e.g., static, const, __attribute__.
	assert( declList );

	// Do not distribute attributes for aggregates because the attributes surrounding the aggregate belong it not the
	// variables in the declaration list, e.g.,
	//
	//   struct __attribute__(( aligned(128) )) S { ...
	//   } v1 __attribute__(( aligned(64) )), v2 __attribute__(( aligned(32) )), v3;
	//   struct S v4;
	//
	// v1 => 64, v2 =>32, v3 => 128, v2 => 128
	//
	// Anonymous aggregates are a special case because there is no aggregate to bind the attribute to; hence it floats
	// to the declaration list.
	//
	//   struct __attribute__(( aligned(128) )) /*anonymous */ { ... } v1;
	//
	// v1 => 128

	bool copyattr = ! (typeSpec->type && typeSpec->type->kind == TypeData::Aggregate && ! typeSpec->type->aggregate.anon );

	// addType copies the type information for the aggregate instances from typeSpec into cl's aggInst.aggregate.
	DeclarationNode * cl = (new DeclarationNode)->addType( typeSpec ); // typeSpec IS DELETED!!!

	// Start at second variable in declaration list and clone the type specifiers for each variable.
	for ( DeclarationNode * cur = declList->next ; cur != nullptr; cur = cur->next ) {
		cl->cloneBaseType( cur, copyattr );				// cur is modified
	} // for

	// Add first variable in declaration list with hidden type information in aggInst.aggregate, which is used by
	// extractType to recover the type for the aggregate instances.
	declList->addType( cl, copyattr );					// cl IS DELETED!!!
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *iter = declaration ; iter != nullptr ; iter = iter->next ) {
		iter->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * iter = declaration ; iter != nullptr ; iter = iter->next ) {
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
	if ( nullptr == fieldList ) {
		if ( !( typeSpec->type && typeSpec->type->kind == TypeData::Aggregate ) ) {
			stringstream ss;
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr );
	} // if

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distAttr( typeSpec, fieldList ); // mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( yylloc, *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( yylloc, *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)
#define MISSING_ANON_FIELD "syntax error, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "syntax error, missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "syntax error, missing high value for down-to range so index is uninitialized."

static ForCtrl * makeForCtrl(
		const CodeLocation & location,
		DeclarationNode * init,
		enum OperKinds compop,
		ExpressionNode * comp,
		ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val( location,
		compop,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val( location,
		// choose += or -= for upto/downto
		compop == OperKinds::LThan || compop == OperKinds::LEThan ? OperKinds::PlusAssn : OperKinds::MinusAssn,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "syntax error, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "syntax error, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ast::ConstantExpr * constant = dynamic_cast<ast::ConstantExpr *>(type->expr.get());
	if ( constant && (constant->rep == "0" || constant->rep == "1") ) {
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicKind::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else if ( auto commaExpr = dynamic_cast<ast::CommaExpr *>( index->expr.get() ) ) {
		if ( auto identifier = commaExpr->arg1.as<ast::NameExpr>() ) {
			return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
		} else {
			SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
		} // if
	} else {
		SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
	} // if
} // forCtrl

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, ExpressionNode * range_over_expr ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl =
			DeclarationNode::newName( new std::string(identifier->name) );
		assert( range_over_expr );
		auto node = new StatementNode( indexDecl ); // <- this cause this error
		return new ForCtrl( node, range_over_expr );
	} else if (auto commaExpr = dynamic_cast<ast::CommaExpr *>( index_expr->expr.get() )) {
		if ( auto identifier = commaExpr->arg1.as<ast::NameExpr>() ) {
			assert( range_over_expr );
			DeclarationNode * indexDecl = distAttr(
				DeclarationNode::newTypeof( range_over_expr, true ),
				DeclarationNode::newName( new std::string( identifier->name) ) );
			return new ForCtrl( new StatementNode( indexDecl ), range_over_expr );
		} else {
			SemanticError( yylloc, "syntax error, loop-index name missing. Expression disallowed." ); return nullptr;
		} // if
	} else {
		assert( false );
	} // if
} // enumRangeCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, "syntax error, adjacent identifiers \"%s\" and \"%s\" are not meaningful in an %s.\n"
				   "Possible cause is misspelled type name or missing generic parameter.",
				   identifier1.c_str(), identifier2.c_str(), kind );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, "syntax error, identifier \"%s\" cannot appear before a %s.\n"
				   "Possible cause is misspelled storage/CV qualifier, misspelled typename, or missing generic parameter.",
				   identifier.c_str(), kind );
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

#line 369 "Parser/parser.cc"

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
    COUNTOF = 309,
    OFFSETOF = 310,
    BASETYPEOF = 311,
    TYPEID = 312,
    ENUM = 313,
    STRUCT = 314,
    UNION = 315,
    EXCEPTION = 316,
    GENERATOR = 317,
    COROUTINE = 318,
    MONITOR = 319,
    THREAD = 320,
    OTYPE = 321,
    FTYPE = 322,
    DTYPE = 323,
    TTYPE = 324,
    TRAIT = 325,
    LABEL = 326,
    SUSPEND = 327,
    ATTRIBUTE = 328,
    EXTENSION = 329,
    IF = 330,
    ELSE = 331,
    SWITCH = 332,
    CASE = 333,
    DEFAULT = 334,
    DO = 335,
    WHILE = 336,
    FOR = 337,
    BREAK = 338,
    CONTINUE = 339,
    GOTO = 340,
    RETURN = 341,
    CHOOSE = 342,
    FALLTHRU = 343,
    FALLTHROUGH = 344,
    WITH = 345,
    WHEN = 346,
    WAITFOR = 347,
    WAITUNTIL = 348,
    CORUN = 349,
    COFOR = 350,
    DISABLE = 351,
    ENABLE = 352,
    TRY = 353,
    THROW = 354,
    THROWRESUME = 355,
    AT = 356,
    ASM = 357,
    ALIGNAS = 358,
    ALIGNOF = 359,
    GENERIC = 360,
    STATICASSERT = 361,
    IDENTIFIER = 362,
    TYPEDIMname = 363,
    TYPEDEFname = 364,
    TYPEGENname = 365,
    TIMEOUT = 366,
    WAND = 367,
    WOR = 368,
    CATCH = 369,
    RECOVER = 370,
    CATCHRESUME = 371,
    FIXUP = 372,
    FINALLY = 373,
    INTEGERconstant = 374,
    CHARACTERconstant = 375,
    STRINGliteral = 376,
    DIRECTIVE = 377,
    FLOATING_DECIMALconstant = 378,
    FLOATING_FRACTIONconstant = 379,
    FLOATINGconstant = 380,
    ARROW = 381,
    ICR = 382,
    DECR = 383,
    LS = 384,
    RS = 385,
    LE = 386,
    GE = 387,
    EQ = 388,
    NE = 389,
    ANDAND = 390,
    OROR = 391,
    ATTR = 392,
    ELLIPSIS = 393,
    EXPassign = 394,
    MULTassign = 395,
    DIVassign = 396,
    MODassign = 397,
    PLUSassign = 398,
    MINUSassign = 399,
    LSassign = 400,
    RSassign = 401,
    ANDassign = 402,
    ERassign = 403,
    ORassign = 404,
    ErangeUpEq = 405,
    ErangeDown = 406,
    ErangeDownEq = 407,
    ATassign = 408,
    THEN = 409
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
#define COUNTOF 309
#define OFFSETOF 310
#define BASETYPEOF 311
#define TYPEID 312
#define ENUM 313
#define STRUCT 314
#define UNION 315
#define EXCEPTION 316
#define GENERATOR 317
#define COROUTINE 318
#define MONITOR 319
#define THREAD 320
#define OTYPE 321
#define FTYPE 322
#define DTYPE 323
#define TTYPE 324
#define TRAIT 325
#define LABEL 326
#define SUSPEND 327
#define ATTRIBUTE 328
#define EXTENSION 329
#define IF 330
#define ELSE 331
#define SWITCH 332
#define CASE 333
#define DEFAULT 334
#define DO 335
#define WHILE 336
#define FOR 337
#define BREAK 338
#define CONTINUE 339
#define GOTO 340
#define RETURN 341
#define CHOOSE 342
#define FALLTHRU 343
#define FALLTHROUGH 344
#define WITH 345
#define WHEN 346
#define WAITFOR 347
#define WAITUNTIL 348
#define CORUN 349
#define COFOR 350
#define DISABLE 351
#define ENABLE 352
#define TRY 353
#define THROW 354
#define THROWRESUME 355
#define AT 356
#define ASM 357
#define ALIGNAS 358
#define ALIGNOF 359
#define GENERIC 360
#define STATICASSERT 361
#define IDENTIFIER 362
#define TYPEDIMname 363
#define TYPEDEFname 364
#define TYPEGENname 365
#define TIMEOUT 366
#define WAND 367
#define WOR 368
#define CATCH 369
#define RECOVER 370
#define CATCHRESUME 371
#define FIXUP 372
#define FINALLY 373
#define INTEGERconstant 374
#define CHARACTERconstant 375
#define STRINGliteral 376
#define DIRECTIVE 377
#define FLOATING_DECIMALconstant 378
#define FLOATING_FRACTIONconstant 379
#define FLOATINGconstant 380
#define ARROW 381
#define ICR 382
#define DECR 383
#define LS 384
#define RS 385
#define LE 386
#define GE 387
#define EQ 388
#define NE 389
#define ANDAND 390
#define OROR 391
#define ATTR 392
#define ELLIPSIS 393
#define EXPassign 394
#define MULTassign 395
#define DIVassign 396
#define MODassign 397
#define PLUSassign 398
#define MINUSassign 399
#define LSassign 400
#define RSassign 401
#define ANDassign 402
#define ERassign 403
#define ORassign 404
#define ErangeUpEq 405
#define ErangeDown 406
#define ErangeDownEq 407
#define ATassign 408
#define THEN 409

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	// A raw token can be used.
	Token tok;

	// The general node types hold some generic node or list of nodes.
	DeclarationNode * decl;
	InitializerNode * init;
	ExpressionNode * expr;
	StatementNode * stmt;
	ClauseNode * clause;
	TypeData * type;

	// Special "nodes" containing compound information.
	CondCtl * ifctl;
	ForCtrl * forctl;
	LabelNode * labels;

	// Various flags and single values that become fields later.
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	OperKinds oper;
	bool is_volatile;
	EnumHiding enum_hiding;
	ast::ExceptionKind except_kind;
	// String passes ownership with it.
	std::string * str;

	// Narrower node types are used to avoid constant unwrapping.
	ast::WaitForStmt * wfs;
	ast::WaitUntilStmt::ClauseNode * wucn;
	ast::GenericExpr * genexpr;

#line 762 "Parser/parser.cc"

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
#define YYFINAL  148
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   26263

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  182
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1116
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2200

#define YYUNDEFTOK  2
#define YYMAXUTOK   409


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
       2,     2,     2,   171,     2,     2,     2,   175,   168,     2,
     156,   158,   167,   169,   162,   170,   159,   174,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   163,   181,
     176,   180,   177,   179,   157,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   160,   173,   161,   166,     2,   165,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   164,   178,   155,   172,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   664,   664,   668,   675,   676,   677,   678,   679,   683,
     684,   685,   686,   687,   688,   689,   690,   694,   695,   699,
     700,   705,   706,   707,   711,   715,   716,   727,   729,   731,
     733,   734,   736,   738,   740,   742,   752,   754,   756,   758,
     760,   762,   767,   768,   779,   784,   789,   790,   795,   801,
     803,   805,   811,   813,   817,   819,   821,   841,   844,   846,
     848,   850,   852,   854,   856,   858,   860,   862,   864,   866,
     876,   877,   881,   882,   887,   890,   894,   895,   899,   900,
     902,   904,   906,   908,   910,   915,   917,   919,   927,   928,
     936,   939,   940,   942,   947,   963,   965,   967,   969,   971,
     973,   975,   980,   982,   985,   987,   992,   994,   999,  1000,
    1002,  1006,  1007,  1008,  1009,  1013,  1014,  1016,  1018,  1020,
    1022,  1024,  1026,  1028,  1035,  1036,  1037,  1038,  1042,  1043,
    1047,  1048,  1053,  1054,  1056,  1058,  1063,  1064,  1066,  1071,
    1072,  1074,  1079,  1080,  1082,  1084,  1086,  1091,  1092,  1094,
    1099,  1100,  1105,  1106,  1111,  1112,  1117,  1118,  1123,  1124,
    1129,  1130,  1132,  1137,  1142,  1143,  1151,  1157,  1158,  1162,
    1163,  1167,  1168,  1172,  1173,  1174,  1175,  1176,  1177,  1178,
    1179,  1180,  1181,  1182,  1192,  1194,  1199,  1200,  1202,  1204,
    1209,  1210,  1216,  1217,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  1230,  1231,  1232,  1233,  1234,  1235,  1236,  1238,  1239,
    1245,  1247,  1257,  1259,  1267,  1268,  1273,  1275,  1277,  1279,
    1281,  1285,  1286,  1288,  1294,  1323,  1326,  1328,  1330,  1340,
    1342,  1344,  1349,  1354,  1356,  1358,  1360,  1368,  1369,  1371,
    1375,  1377,  1381,  1383,  1384,  1386,  1388,  1393,  1394,  1398,
    1403,  1404,  1408,  1410,  1415,  1417,  1422,  1424,  1426,  1428,
    1433,  1435,  1437,  1439,  1444,  1446,  1451,  1452,  1474,  1476,
    1481,  1484,  1486,  1489,  1491,  1494,  1496,  1501,  1506,  1508,
    1513,  1518,  1520,  1522,  1524,  1526,  1529,  1531,  1534,  1536,
    1541,  1547,  1550,  1552,  1557,  1563,  1565,  1570,  1576,  1579,
    1581,  1584,  1586,  1591,  1598,  1600,  1605,  1611,  1613,  1618,
    1624,  1627,  1631,  1641,  1643,  1645,  1650,  1652,  1654,  1659,
    1661,  1666,  1667,  1669,  1674,  1676,  1681,  1683,  1685,  1687,
    1690,  1694,  1697,  1701,  1703,  1705,  1707,  1709,  1711,  1713,
    1715,  1717,  1719,  1721,  1726,  1727,  1731,  1737,  1745,  1750,
    1751,  1755,  1756,  1761,  1765,  1766,  1769,  1771,  1776,  1779,
    1781,  1783,  1786,  1788,  1793,  1798,  1799,  1803,  1808,  1810,
    1815,  1817,  1822,  1824,  1826,  1831,  1836,  1841,  1846,  1848,
    1850,  1855,  1857,  1863,  1864,  1868,  1869,  1870,  1871,  1875,
    1880,  1881,  1883,  1885,  1887,  1891,  1895,  1896,  1900,  1902,
    1904,  1906,  1908,  1914,  1915,  1921,  1922,  1926,  1927,  1932,
    1934,  1943,  1944,  1946,  1951,  1956,  1967,  1968,  1972,  1973,
    1979,  1980,  1984,  1986,  1990,  1992,  1996,  1997,  2001,  2002,
    2006,  2007,  2008,  2012,  2014,  2029,  2030,  2031,  2032,  2034,
    2038,  2040,  2044,  2051,  2053,  2055,  2063,  2065,  2070,  2071,
    2073,  2075,  2077,  2087,  2089,  2101,  2104,  2109,  2111,  2117,
    2122,  2127,  2138,  2145,  2150,  2152,  2154,  2160,  2164,  2171,
    2173,  2174,  2175,  2191,  2193,  2196,  2198,  2201,  2206,  2207,
    2211,  2212,  2213,  2214,  2223,  2224,  2225,  2234,  2235,  2236,
    2240,  2241,  2242,  2251,  2252,  2253,  2258,  2259,  2268,  2269,
    2274,  2276,  2280,  2282,  2284,  2286,  2293,  2298,  2303,  2304,
    2306,  2316,  2317,  2322,  2324,  2326,  2328,  2330,  2332,  2335,
    2337,  2339,  2344,  2350,  2352,  2354,  2356,  2358,  2360,  2362,
    2364,  2366,  2368,  2370,  2372,  2374,  2376,  2378,  2380,  2382,
    2384,  2386,  2388,  2390,  2392,  2394,  2396,  2398,  2400,  2402,
    2404,  2409,  2410,  2414,  2420,  2421,  2427,  2428,  2430,  2432,
    2434,  2439,  2441,  2446,  2447,  2449,  2451,  2456,  2458,  2460,
    2462,  2464,  2466,  2471,  2472,  2474,  2476,  2481,  2483,  2482,
    2486,  2494,  2495,  2497,  2499,  2504,  2505,  2507,  2512,  2513,
    2515,  2517,  2522,  2524,  2526,  2531,  2533,  2535,  2537,  2538,
    2540,  2545,  2547,  2549,  2554,  2555,  2559,  2560,  2567,  2566,
    2571,  2570,  2580,  2579,  2590,  2589,  2599,  2604,  2605,  2610,
    2616,  2634,  2635,  2639,  2641,  2643,  2648,  2650,  2652,  2654,
    2659,  2661,  2666,  2668,  2677,  2678,  2683,  2692,  2697,  2699,
    2701,  2710,  2712,  2713,  2714,  2716,  2718,  2719,  2724,  2725,
    2726,  2731,  2733,  2736,  2739,  2746,  2747,  2748,  2754,  2759,
    2761,  2767,  2768,  2774,  2775,  2779,  2787,  2794,  2807,  2806,
    2810,  2813,  2812,  2821,  2825,  2829,  2831,  2837,  2838,  2843,
    2848,  2856,  2858,  2864,  2866,  2871,  2872,  2878,  2879,  2880,
    2889,  2890,  2892,  2893,  2898,  2899,  2900,  2902,  2908,  2909,
    2911,  2912,  2913,  2915,  2917,  2924,  2925,  2927,  2929,  2934,
    2935,  2944,  2946,  2951,  2953,  2958,  2959,  2961,  2964,  2966,
    2970,  2971,  2972,  2974,  2976,  2984,  2986,  2991,  2992,  2993,
    2998,  2999,  3004,  3005,  3006,  3007,  3011,  3012,  3017,  3018,
    3019,  3020,  3021,  3035,  3036,  3041,  3042,  3048,  3050,  3053,
    3055,  3057,  3080,  3081,  3087,  3088,  3094,  3093,  3103,  3102,
    3106,  3112,  3114,  3124,  3125,  3127,  3131,  3136,  3138,  3140,
    3142,  3148,  3149,  3153,  3154,  3159,  3161,  3168,  3170,  3171,
    3173,  3178,  3180,  3182,  3187,  3189,  3194,  3199,  3207,  3212,
    3214,  3219,  3224,  3225,  3230,  3231,  3235,  3236,  3237,  3242,
    3244,  3250,  3252,  3257,  3259,  3265,  3266,  3270,  3274,  3278,
    3280,  3292,  3294,  3296,  3298,  3300,  3302,  3304,  3305,  3310,
    3313,  3312,  3324,  3323,  3336,  3335,  3349,  3348,  3362,  3361,
    3377,  3383,  3385,  3391,  3392,  3403,  3410,  3415,  3421,  3424,
    3427,  3431,  3437,  3440,  3443,  3448,  3449,  3450,  3451,  3455,
    3463,  3464,  3476,  3477,  3481,  3482,  3487,  3489,  3491,  3496,
    3497,  3503,  3504,  3506,  3511,  3512,  3514,  3549,  3551,  3556,
    3558,  3559,  3561,  3566,  3568,  3570,  3572,  3577,  3579,  3581,
    3583,  3585,  3587,  3589,  3594,  3596,  3598,  3600,  3609,  3611,
    3612,  3617,  3619,  3621,  3623,  3625,  3630,  3632,  3634,  3636,
    3641,  3643,  3645,  3647,  3649,  3651,  3663,  3664,  3665,  3669,
    3671,  3673,  3675,  3677,  3682,  3684,  3686,  3688,  3693,  3695,
    3697,  3699,  3701,  3703,  3715,  3720,  3725,  3727,  3728,  3730,
    3735,  3737,  3739,  3741,  3746,  3748,  3750,  3752,  3754,  3756,
    3758,  3763,  3765,  3767,  3769,  3778,  3780,  3781,  3786,  3788,
    3790,  3792,  3794,  3799,  3801,  3803,  3805,  3810,  3812,  3814,
    3816,  3818,  3820,  3830,  3832,  3835,  3836,  3838,  3843,  3845,
    3847,  3852,  3854,  3856,  3858,  3863,  3865,  3867,  3881,  3883,
    3886,  3887,  3889,  3894,  3896,  3901,  3903,  3905,  3910,  3912,
    3917,  3919,  3936,  3937,  3939,  3944,  3946,  3948,  3950,  3952,
    3957,  3958,  3960,  3962,  3967,  3969,  3971,  3977,  3979,  3982,
    3989,  3991,  4000,  4002,  4004,  4005,  4007,  4009,  4013,  4015,
    4020,  4022,  4024,  4026,  4061,  4062,  4066,  4067,  4070,  4072,
    4077,  4079,  4081,  4083,  4085,  4090,  4091,  4093,  4095,  4100,
    4102,  4104,  4110,  4111,  4113,  4122,  4125,  4127,  4130,  4132,
    4134,  4148,  4149,  4151,  4156,  4158,  4160,  4162,  4164,  4169,
    4170,  4172,  4174,  4179,  4181,  4189,  4190,  4191,  4196,  4197,
    4202,  4204,  4206,  4208,  4210,  4212,  4219,  4221,  4223,  4225,
    4227,  4230,  4232,  4234,  4236,  4238,  4243,  4245,  4247,  4252,
    4278,  4279,  4281,  4285,  4286,  4290,  4292,  4294,  4296,  4298,
    4300,  4307,  4309,  4311,  4313,  4315,  4317,  4322,  4324,  4326,
    4331,  4333,  4335,  4353,  4355,  4360,  4361
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
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "COUNTOF", "OFFSETOF", "BASETYPEOF",
  "TYPEID", "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR",
  "COROUTINE", "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE",
  "TRAIT", "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE",
  "SWITCH", "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE",
  "GOTO", "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN",
  "WAITFOR", "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY",
  "THROW", "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
  "FLOATING_DECIMALconstant", "FLOATING_FRACTIONconstant",
  "FLOATINGconstant", "ARROW", "ICR", "DECR", "LS", "RS", "LE", "GE", "EQ",
  "NE", "ANDAND", "OROR", "ATTR", "ELLIPSIS", "EXPassign", "MULTassign",
  "DIVassign", "MODassign", "PLUSassign", "MINUSassign", "LSassign",
  "RSassign", "ANDassign", "ERassign", "ORassign", "ErangeUpEq",
  "ErangeDown", "ErangeDownEq", "ATassign", "THEN", "'}'", "'('", "'@'",
  "')'", "'.'", "'['", "']'", "','", "':'", "'{'", "'`'", "'^'", "'*'",
  "'&'", "'+'", "'-'", "'!'", "'~'", "'\\\\'", "'/'", "'%'", "'<'", "'>'",
  "'|'", "'?'", "'='", "';'", "$accept", "push", "pop", "constant",
  "quasi_keyword", "identifier", "identifier_at",
  "identifier_or_type_name", "string_literal", "string_literal_list",
  "primary_expression", "generic_assoc_list", "generic_association",
  "postfix_expression", "argument_expression_list_opt",
  "argument_expression_list", "argument_expression", "field_name_list",
  "field", "field_name", "fraction_constants_opt", "unary_expression",
  "ptrref_operator", "unary_operator", "cast_expression",
  "qualifier_cast_list", "cast_modifier", "exponential_expression",
  "multiplicative_expression", "additive_expression", "shift_expression",
  "relational_expression", "equality_expression", "AND_expression",
  "exclusive_OR_expression", "inclusive_OR_expression",
  "logical_AND_expression", "logical_OR_expression",
  "conditional_expression", "constant_expression", "assignment_expression",
  "assignment_expression_opt", "assignment_operator",
  "simple_assignment_operator", "compound_assignment_operator", "tuple",
  "tuple_expression_list", "comma_expression", "comma_expression_opt",
  "statement", "labeled_statement", "compound_statement",
  "statement_decl_list", "statement_decl", "statement_list_nodecl",
  "expression_statement", "selection_statement", "conditional_declaration",
  "case_value", "case_value_list", "case_label", "case_label_list",
  "case_clause", "switch_clause_list_opt", "switch_clause_list",
  "iteration_statement", "for_control_expression_list",
  "for_control_expression", "enum_key", "downupdowneq", "updown",
  "updowneq", "jump_statement", "fall_through_name", "with_statement",
  "mutex_statement", "when_clause", "when_clause_opt",
  "cast_expression_list", "timeout", "wor", "waitfor",
  "wor_waitfor_clause", "waitfor_statement", "wand", "waituntil",
  "waituntil_clause", "wand_waituntil_clause", "wor_waituntil_clause",
  "waituntil_statement", "corun_statement", "cofor_statement",
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
  "c_declaration", "declaring_list", "general_function_declarator",
  "declaration_specifier", "invalid_types", "declaration_specifier_nobody",
  "type_specifier", "type_specifier_nobody", "type_qualifier_list_opt",
  "type_qualifier_list", "type_qualifier", "type_qualifier_name", "forall",
  "declaration_qualifier_list", "storage_class_list", "storage_class",
  "basic_type_name", "basic_type_name_type", "vtable_opt", "vtable",
  "default_opt", "basic_declaration_specifier", "basic_type_specifier",
  "direct_type", "indirect_type", "sue_declaration_specifier",
  "sue_type_specifier", "$@1", "sue_declaration_specifier_nobody",
  "sue_type_specifier_nobody", "type_declaration_specifier",
  "type_type_specifier", "type_name", "typegen_name", "elaborated_type",
  "elaborated_type_nobody", "aggregate_type", "$@2", "$@3", "$@4", "$@5",
  "type_parameters_opt", "aggregate_type_nobody", "aggregate_key",
  "aggregate_data", "aggregate_control", "field_declaration_list_opt",
  "field_declaration", "field_declaring_list_opt", "field_declarator",
  "field_abstract_list_opt", "field_abstract", "cfa_field_declaring_list",
  "cfa_field_abstract_list", "bit_subrange_size_opt", "bit_subrange_size",
  "enum_type", "$@6", "$@7", "enumerator_type", "hide_opt",
  "enum_type_nobody", "enumerator_list", "visible_hide_opt",
  "enumerator_value_opt", "parameter_list_ellipsis_opt", "parameter_list",
  "cfa_parameter_list_ellipsis_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_declaration",
  "abstract_parameter_declaration", "cfa_parameter_declaration",
  "cfa_abstract_parameter_declaration", "identifier_list",
  "type_no_function", "type", "initializer_opt", "initializer",
  "initializer_list_opt", "designation", "designator_list", "designator",
  "type_parameter_list", "type_initializer_opt", "type_parameter", "$@8",
  "$@9", "new_type_class", "type_class", "assertion_list_opt",
  "assertion_list", "assertion", "type_list", "type_declaring_list",
  "type_declarator", "type_declarator_name", "trait_specifier",
  "trait_declaration_list", "trait_declaration",
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
     405,   406,   407,   408,   409,   125,    40,    64,    41,    46,
      91,    93,    44,    58,   123,    96,    94,    42,    38,    43,
      45,    33,   126,    92,    47,    37,    60,    62,   124,    63,
      61,    59
};
# endif

#define YYPACT_NINF (-1870)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1115)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      53, 13392,   172,   184, 19881,    -6, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870,   230,   936,
     248, -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870,   362,   410,
   -1870, -1870, -1870, -1870, -1870, -1870,  4531,  4531,   400, 13392,
     416,   487, 23400, -1870,   552, -1870, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870, -1870,   555,  2126, -1870,   384,    79,
   -1870, -1870, -1870, -1870, -1870, 19413, -1870, -1870,   354,   579,
     298,   292, -1870,  4647,   626,   684,   701,   645,  4707,   880,
     756, 13558, -1870, -1870,   852, 19257,  1797, -1870, -1870, -1870,
   -1870,  2645,   890, 23124, 11722,   718,  2645,  1106,   747, -1870,
   -1870, -1870, -1870,   104, -1870, -1870, -1870, -1870,   770, -1870,
   -1870, -1870, -1870, -1870,   804,   815,   104, -1870,   104, 17569,
   -1870, -1870, -1870, 21136,  4531, -1870, -1870,  4531, -1870, 13392,
   -1870,   792, 21190, -1870, -1870,  5694, 22406, -1870, -1870,  1307,
    1307,   830,  3374, -1870, -1870, -1870, -1870,   449, 15854,   104,
    2188,   104, -1870, -1870, -1870, -1870, -1870, -1870,   829, -1870,
     818,   867,  2266, -1870,   911, 25629, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, 17951,  2389,  3819,  2126,   728,   889,   892,
     897,   900,   941,   944, -1870, -1870, 20037, 12139,   914,   905,
   -1870, 20257, -1870, -1870, -1870, -1870,   954, -1870, -1870,   967,
   -1870, 23543,  1072, 23697, -1870,   960,  4531,   815,   976,   981,
   -1870,  2250,  5694,  2250, -1870, -1870, -1870,  4866,  3450,   984,
    1051,   601,  1051, -1870,   104,   104,   -25, 17245,   636,  1051,
   -1870,   104,   104,   -25,   104, -1870,   104, -1870,  4673, -1870,
   -1870,  1008,  1015,  1307,  9603,  1016, 19413, -1870,  4647, -1870,
    2645, -1870,  1753,   747,  1019,  1102, 17245,  4531,  4531,   298,
   -1870, 15029, -1870,  1307,  1307,  1036,  1102, 17245,  4531, -1870,
   16896, -1870, -1870, -1870,  1307, -1870, -1870, -1870, -1870,  1307,
   -1870,   558,  3433,  4531, -1870, 19110,  1048, -1870, -1870, -1870,
   23069,   815, 17407,  1038,  5694, 18972,  9603,  2645, -1870, -1870,
   22607, -1870,  1051,    11, -1870, 25629, 22555,  5472,  4673, -1870,
     641, -1870, -1870, -1870, -1870, -1870, 21190,  1051,  4531, -1870,
    1052,  1049, -1870, -1870, -1870, -1870,  4531,  3261,   439,   386,
   -1870,  4531,   818, -1870,   532,   104, -1870,  1070, 21346,   702,
   16349, 23207,  2645, -1870,  2645,  1307,  2645,  1307, -1870, -1870,
     104, -1870, -1870,  1079, 21400, -1870, -1870, -1870, 21556,   954,
   -1870,  2882,   128,   608, -1870,   302,   747,  1078,  1073, -1870,
    3374,  1075,   818,  3374, -1870, -1870,  2389, -1870,   430, -1870,
    1094, -1870,  1112,  1168, 25706,  1097, 25783,  1136,  1139, 25629,
   25860,  1141, 23452, -1870, -1870, -1870, -1870, -1870, -1870, 25937,
   25937, 17791,  1157,  3530, -1870, -1870, -1870, -1870,   462, -1870,
     622, -1870,  1233, -1870, 25629, 25629, -1870,  1155,   657,   836,
     814,   567,  1095,  1198,  1182,  1232,  1271,   155, -1870,   591,
   -1870,  1226, -1870,  1120,  4653, 18431, -1870, -1870,   774,  1226,
   -1870, -1870,   680, -1870, -1870,   794,  3819,  1263,  1265,  1274,
    1295,  1297,  1305, -1870, -1870,   653,  1283, -1870,   704,  1283,
    1317, -1870,  1325, -1870, 21136, -1870,  1152,  1310, 18591, -1870,
   -1870,  4622,  3545,  1349, 16349,  1362,  1126,  1172,  1333,  1347,
   -1870, -1870, -1870,  4531,  4899, 20614, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, 18920,  3362,  1157, 23543,  1359,  1372, -1870,
   -1870,  1363, 23697,   736, -1870, -1870, -1870, 18431,  1381, -1870,
     911, -1870, -1870, -1870,  1365,  4866,   808,  1384,  1390,  1416,
     906,  1426,  1438,  1447,  1449,  1454,  1465,  3450, -1870, -1870,
   -1870,   104,  1444,  1464, -1870, -1870,  1466,   298, -1870, -1870,
     815,  1102, 20202, -1870, -1870,   298, -1870, -1870,   815, -1870,
   -1870,  4673, -1870, 18431, 18431, -1870,  1307,  5694, 23262,  3285,
   16514, -1870, -1870, -1870, -1870, -1870, -1870,   815,  1102,    11,
    1467, -1870, -1870,  2645,  1468,  1102, 17245, -1870,   815,  1102,
   -1870, 20477, -1870,  1307,  1307, -1870, -1870,  1470,   115,  1471,
     747,  1475, -1870, -1870, -1870, 20560,  1483,  1480, -1870, -1870,
     868, -1870,  1558, -1870,  1462, -1870, -1870, -1870, 21721, 26014,
   -1870, -1870, -1870, -1870, -1870,  5472,   916,  4673, 20202,  1051,
   13392, -1870,  4531,  1484, -1870,  1493, -1870, -1870, -1870, -1870,
   -1870,  3374, -1870, -1870,  1571,  4170, 20770, 12139, -1870, 21775,
   -1870,  1307,  1307, -1870, -1870,   954, -1870, 15359,  1491,  1636,
   25629,   875,  1466,  1479, -1870,   104,   104, -1870,  1283, -1870,
   21346, -1870, -1870, 20560,  1307,  1307, -1870,  4170, -1870, -1870,
   22351, -1870, -1870, 21400, -1870,   104,  1496,   104,  1073,   139,
    1497,   886, 21190,   921,   922, -1870,  2389, 23774,  1474, -1870,
   18111, -1870,  3530, 18271, -1870, 21931, 21190, -1870, 18111, -1870,
   25629, -1870, -1870, -1870, -1870, -1870, -1870, 18271, -1870, -1870,
   20824, 21931, 21931,  1180,  1490,  1537,   494,  1602, -1870,   938,
    1504,  1258,  1505, -1870, 23851, 25629, 23928,  1506, 25629,  2250,
   25629,  2250, -1870,  2297, -1870, -1870, 23774,  2419, 25629, 23774,
    2250, -1870, -1870, 25629, 25629, 25629, 25629, 25629, 25629, 25629,
   25629, 25629, 25629, 25629, 25629, 25629, 25629, 25629, 25629, 25629,
   25629, 25629, 24005,  1487,   911,  4327, 12139, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870, -1870, -1870, -1870, -1870,  1507, 25629,
   -1870, -1870, 15524,   903, -1870, -1870,   104,   104, -1870, -1870,
   18431, -1870, -1870,   663,  1283, -1870,   945,  1283, 20202, -1870,
   -1870,  1466, 20202, -1870,  1466, -1870, 26091, -1870, -1870, -1870,
   19725, 12139,  1511,  1268,  1512, 14864,  1665,  2773,   771,  1479,
   -1870,   104,   104,  1479,   821, -1870,   104,   104, 25629,  4531,
   16514,  1520, 16514,  1523,  1479,   118, 15689, 15689, 16679, 15689,
    4531, -1870, -1870, 25629,  1363, -1870, 23543,  1530, -1870,  3645,
   -1870, -1870, -1870,   966, -1870,  1528, 15689, 25629,   978,  1533,
    1534,  1545,   988,  1546,  1547,  1549,  1553,  1556,  1557,   824,
    1283, -1870, -1870,   881,  1283, -1870, -1870,   896,  1283, -1870,
   -1870, -1870,  5694,  1666,  1283, 22756, -1870, -1870,   815,  1559,
   -1870, -1870, -1870,   993,  1560,   994,  1562, -1870,  1317,  1555,
    1566, -1870,   815, -1870,  1567, -1870,   815,  1102,  1566, -1870,
     815,  1561,  1563,  1564, -1870, -1870, 20422, -1870,  2250,  4531,
   11186,  1652, -1870,  1310, -1870, 15689,  1041, -1870,  1566,  1573,
   -1870, 21985, 18431,  1554, -1870,  1554, -1870, -1870, -1870, -1870,
   21400, -1870, 12308, 18751, -1870,  1577,  1580,  1582,  1583, -1870,
   10174,   104, -1870,   875, -1870, -1870, -1870, -1870,  1466, -1870,
   -1870, -1870,  1307, -1870, -1870, -1870, -1870,   139,  1073,  1565,
     449, -1870, -1870,  1581,  4531,   139, -1870, -1870,  1584,  1589,
   -1870, -1870,  1044, -1870, -1870, -1870, -1870,  1594,  1595,  1592,
    1597,  1596,  1598,  1600,  1601,  1603,  1604,  1611, 25629,  1612,
    1615,  1616, 22141, 12477, 25629, -1870, -1870,  1777, -1870, -1870,
   -1870, 25629, -1870,  1620,  1621, 23620, -1870, -1870,  1275, -1870,
   23774,  1619, -1870,  1622, -1870, -1870,  4510, -1870,  1056, -1870,
    4510, -1870, -1870,  1284,   656, -1870, -1870,  1155,  1155,  1155,
     657,   657,   836,   836,   814,   814,   814,   814,   567,   567,
    1095,  1198,  1182,  1232,  1271, 25629,  1285, -1870,  1623,  4510,
   -1870, -1870, 23543, -1870,  1638,  1639,  1641,  1667,   903, -1870,
   -1870, -1870, -1870, -1870, 20202, -1870, -1870,  1466, 20202, -1870,
    1466,  1669,  1670, 15689, 15689, -1870, -1870, 14864,   932,  1675,
    1678,  1682,  1683,  2720,  2773, -1870, -1870, 20202, -1870, -1870,
   -1870, -1870, -1870, -1870, 20202, -1870, -1870, -1870, -1870,  1660,
   -1870,  1479,  1662, -1870, -1870, -1870, -1870, -1870, -1870, -1870,
   -1870,  1684,  1681,  1685, -1870,  1686, -1870,   298,  4510,  1289,
      44, -1870, -1870,  1689, -1870, 23697, -1870, 25629,   104, 15689,
     104, -1870, -1870,   909,  1283, -1870,   917,  1283, -1870, -1870,
     931,  1283, 20202, -1870, -1870,  1466, 20202, -1870, -1870,  1466,
   20202, -1870, -1870,  1466,  1051, -1870,  1466,    42, -1870,  1226,
    1690, -1870, -1870, -1870, -1870, -1870, -1870,  1696, -1870, -1870,
   -1870, 21985,  1566, -1870,   815, -1870, -1870, -1870, -1870, -1870,
    9741, -1870, -1870, -1870, -1870, -1870,   229,   502,   465, 11970,
    1698,  1699, 17066,  1700,  1704,  2565,  2960,  3019, 24082,  1705,
   -1870, -1870,  1712,  1714, 17066,  1716, -1870, -1870,   815, 25629,
   25629,  1859,  1711,   664, -1870, 17631,  1296,  1713,  1715,  1694,
   -1870, -1870, -1870, 11007, -1870, -1870, -1870, -1870, -1870,  2643,
   -1870, -1870, -1870,  1369,   313, -1870,   417, -1870,   313, -1870,
   -1870, -1870, -1870, -1870,  2250, -1870, -1870, 13724, 19569, -1870,
    4531,  1718,  1719, -1870, -1870, -1870,  4531, -1870, -1870,  5694,
   -1870, -1870,  1702,  1707,  1129, 21190,   818,   818, -1870, -1870,
    1157,  1310, 18591, -1870,  1226, -1870, 12646, -1870,  1013,  1283,
   -1870,  1307, 13222, -1870, -1870,  1073,  1581,  1725,   139,   747,
     429,  1739,  1721,  1581,  1740, -1870, -1870, 23774,   690, -1870,
   20560,   690, 12477,  2250, -1870,   690, -1870, 20980,   690, -1870,
   25629, 25629, 25629, -1870, -1870, -1870, -1870, 25629, 25629,  1734,
   23543, -1870, -1870, 24159,  1737,   725, -1870, -1870, -1870,  3930,
   -1870, -1870,  1306, -1870,   170, -1870,  1308, -1870, 23851, -1870,
   -1870, 25629,  1722,  1315,  1318,  1363, -1870,  1022,  1283, -1870,
   -1870,  1744,  1747, -1870, -1870, -1870, -1870,  1751,  1024,  1283,
   -1870,  1035,  4260,   104,   104, -1870, -1870,  1754,  1757, -1870,
    1750, -1870, 16514,  1758, -1870, 16019, 16184,  1765, 16679,  1766,
   -1870,  1764, 25629, 25629,  1322,  1767, -1870, -1870, -1870, -1870,
   -1870, -1870,  1772, 20202, -1870, -1870,  1466, 20202, -1870, -1870,
    1466, 20202, -1870, -1870,  1466,  1773,  1774,  1776,   298, -1870,
   -1870,  1331, 25629, 22909,  1775,  1783, -1870, -1870, -1870,  1784,
   14215, 14374, 14533, 21985,  9603, 21931, 21931,  1785, -1870,   349,
     357,  2580, 10381, -1870,   369,  4531,  4531, -1870, 23774,   -76,
     159, -1870, -1870, -1870, -1870, 11970, 25629,  1787,  1866, 11800,
   11365, -1870,  1759, -1870,  1768, 25629,  1769, 23543,  1778, 25629,
   18431, 25629, -1870, 11544,  1238, -1870,  1779,   -30, -1870,    18,
    1855,   490,   104, -1870,  1800, -1870,  1782, -1870,  1792,  1809,
    1810, 17066, 17066, -1870, -1870,  1885, -1870, -1870,    92,    92,
      38, 15194,   431, -1870, -1870,  1819,  1825,   439, -1870,  1826,
   -1870,  1820, -1870,  1821, -1870, -1870, -1870, -1870, 12815,  1824,
    1827,  1828, -1870, 20202, -1870, -1870,  1466, 25629, 25629,  1310,
    1829, -1870,  1830,  1836,   139,  1581,   449,  4531, -1870, 24236,
   -1870,  1837, -1870, 21985, -1870,   947,  1838,  1834,  1132, -1870,
    1839, -1870, -1870, -1870, -1870, -1870, 23543,  1363, -1870, -1870,
   23851, -1870,  1876,  4510, -1870,  1876,  1876, -1870,  4510,  4080,
    4229, -1870,  1352, -1870, -1870, -1870,  1846, 20202, -1870, -1870,
    1466, -1870, -1870,  1849,  1850,   104, 20202, -1870, -1870,  1466,
   20202, -1870, -1870,  1851, -1870, -1870, -1870, -1870, -1870, -1870,
   -1870, -1870,  1662, -1870, -1870, -1870,  1843, -1870, -1870, -1870,
   -1870,  1853,  1852,   104,  1854,  1857,  1858, -1870, -1870, -1870,
   -1870, 25629, -1870,    42, -1870,  1226, -1870, -1870,  1869,  1870,
   -1870,  1785,  1785,  1785,  3917,  1080,  1845,   480, -1870,  3917,
     519, 18431, -1870, -1870, -1870,  5601, 25629,  5147,   443, -1870,
   -1870,   102,  1865,  1865,  1865,  4531, -1870, -1870, -1870,  1140,
   -1870, -1870, -1870, -1870,  1715,  1871, 25629,   354,  1874,   645,
   14699, 21985,  1153,  1875, 17066,  1881, -1870, -1870, -1870,   595,
   17066, 25629,  1058,   752, -1870, 25629, 23459, -1870, -1870,   531,
   -1870,  1363, -1870,  1167,  1173,  1175,   765, -1870, -1870, -1870,
   -1870,   815,  1238,  1884, -1870, -1870, 25629, -1870,  1887,   911,
   -1870,  7375, -1870, -1870, -1870, 25629, 25629, -1870, -1870,   493,
      92, -1870,   525, -1870, -1870, -1870,   104, -1870,  1554, -1870,
   21985, -1870, -1870, -1870, -1870, -1870,  1886,  1888, -1870, -1870,
    1890, -1870,  1891,   139, -1870,  1581,  1894,   747,  1721, 23543,
   -1870, -1870, -1870,  1895, -1870, -1870, 25629, -1870, 20980, 25629,
    1363,  1899,  1355, -1870,  1357, -1870,  4510, -1870,  4510, -1870,
   -1870, -1870,  1897,   104,   104,  1900,  1904, -1870,  1902, -1870,
   -1870, -1870, -1870, -1870,  1368, 25629, -1870, -1870, -1870, -1870,
   -1870,   541,  1080,  1663,   556, -1870, -1870, -1870, -1870,   104,
     104, -1870, -1870, -1870,   561, -1870,  1178,  5601,   715, -1870,
    5147, -1870,   104, -1870, -1870, -1870, -1870, -1870, -1870, 17066,
   17066,  1715, 16844,   334, 24313,  1969, 17066, -1870, -1870, -1870,
   -1870, 25629, -1870, 24390,  1993,  1889, 10819, 24467, 17066, 11544,
    1715,   753,  1250,  1893, 25629, -1870,  1921,   381, 17066, -1870,
   17066, -1870,  1915, -1870, -1870,  1896,   911,   850, -1870, -1870,
    1920,  1370,  1185, 17066,  1926, 17066, 17066, 17066, -1870,   818,
   -1870,  4531,  5694, -1870, -1870,  1922,  1923, -1870, -1870,  1581,
    1930, -1870, -1870,  1363,  1931, -1870, -1870, -1870, -1870,  1935,
   -1870, -1870, -1870,  1378,  1401, -1870, -1870, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870,  1934,  1937,  1940,  1663, -1870,   104,
   -1870, -1870, -1870, -1870, -1870,  1933,  3917, -1870,  2024,  6542,
     157, 12987, -1870, 16941, -1870,     7,  1207, 17066,  2026,   577,
    1938,   269, 17066, 25629,  4170,   753,  1250,  1928, -1870, 24551,
    1226,  1939,   511,  2027, -1870, 24628, 24705, 25629,  1715,  1932,
   13155, -1870, -1870, -1870, -1870, 22195, -1870,  1944,  1942,   101,
   -1870, 25629, 23774, -1870, -1870, 25629,   313, -1870, -1870, -1870,
   -1870, -1870, -1870, -1870,  1961, -1870,  1971, -1870, -1870, -1870,
   -1870,  1057,  1283, -1870, -1870,  1080, -1870, 17066, -1870,   261,
   -1870,   284, -1870, -1870, -1870,  1973, 13890, -1870, -1870, 17066,
   -1870,    66, -1870, 17066, 25629,  1967, 24782, -1870, -1870, -1870,
     747, 24859, 24936, 25629,  1715, -1870, 25013, 25090, 17066,  1957,
     557,  1960,   593, -1870, -1870,  1982, 13890, 22195, -1870,  4401,
   21775,  2250,  1975, -1870,  2037,  1989,   851,  1987, -1870, -1870,
    1209,  1216,   361, -1870, -1870, 20202, -1870, -1870,  1466, -1870,
   -1870, 25629, -1870, 25629, -1870, -1870,  1488, 14056, -1870, -1870,
   17066, -1870, -1870,  1715, -1870, -1870,  1715,  1979,   609,  1980,
     613, -1870, -1870,  1715, -1870,  1715, -1870,  1997, 25167, 25244,
   25321, -1870,  1488, -1870,  1977,  4789,  4311, -1870, -1870, -1870,
     101,  1995, 25629,  1978,   101,   101, -1870, -1870, 17066,  2084,
    2005, -1870, -1870, 16941, -1870,  1488, -1870, -1870,  2008, 25398,
   25475, 25552, -1870, -1870,  1715, -1870,  1715, -1870,  1715, -1870,
    1977, 25629,  2009,  4311,  2006,   911,  2010, -1870,   864, -1870,
   -1870, 17066, -1870, -1870, 10533,  2015, 16941, -1870, -1870,  1715,
   -1870,  1715, -1870,  1715,  2018,  2017, -1870,   815,   911,  2021,
   -1870,  1998,   911, -1870, -1870, -1870, -1870, 10735, -1870,   815,
   -1870, -1870,  1408, 25629, -1870,  1225, -1870,   911,  2250,  2022,
    2000, -1870, -1870,  1228, -1870, -1870,  2003,  2250, -1870, -1870
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   496,     0,     2,   496,   513,   514,   515,   516,   517,
     518,   519,   520,   521,   502,   504,   503,   505,     0,     0,
       0,   523,   525,   546,   526,   547,   529,   530,   544,   545,
     524,   542,   543,   527,   528,   531,   532,   533,   534,   535,
     536,   537,   538,   539,   540,   541,   548,   549,   852,   551,
     624,   625,   628,   630,   626,   632,     0,     0,     0,   496,
       0,     0,    17,   595,   601,     9,    10,    11,    12,    13,
      14,    15,    16,   809,   110,     0,     0,    20,     0,     2,
     108,   109,    18,    19,   867,   496,   810,   432,     0,   435,
     732,   437,   448,   850,   436,   470,   471,     0,     0,     0,
       0,   578,   498,   500,   506,   496,   508,   511,   563,   522,
     550,   480,   556,   561,   482,   573,   481,   588,   592,   598,
     577,   604,   616,   852,   621,   622,   605,   674,   438,   439,
       3,   817,   830,   501,     0,     0,   852,   889,   852,   496,
     906,   907,   908,   496,     0,  1093,  1094,     0,     1,   496,
      17,     0,   496,   459,   460,     0,   578,   506,   490,   491,
     492,   820,     0,   627,   629,   631,   633,     0,   496,   852,
     677,   853,   854,   623,   552,    22,    23,    21,   786,   781,
     771,     0,   861,   818,     0,     0,   513,   811,   815,   816,
     812,   813,   814,   496,   861,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596,   599,   496,   496,     2,     0,
    1095,   578,   896,   914,  1099,  1092,  1090,  1097,   431,     0,
     172,   738,   171,     0,   440,     0,     0,     0,     0,     0,
     446,     0,     0,     0,   430,   983,   984,     0,     0,   469,
     850,   852,   850,   870,   852,   852,   479,   496,   852,   850,
     927,   852,   852,   478,   852,   946,   852,   924,     0,   571,
     572,     0,     0,   496,   496,     2,   496,   449,   850,   499,
     509,   564,     0,   593,     0,   833,   496,     0,     0,   732,
     450,   578,   557,   574,   589,     0,   833,   496,     0,   512,
     558,   565,   566,   483,   575,   485,   486,   484,   580,   590,
     594,     0,   608,     0,   803,   496,     2,   831,   888,   890,
     496,     0,   496,     0,     0,   578,   496,   508,     2,  1103,
     578,  1106,   850,   850,     3,     0,   578,     0,     0,   462,
     852,   845,   847,   846,   848,     2,   496,   850,     0,   807,
       0,     0,   767,   769,   768,   770,     0,     0,   763,     0,
     752,     0,   761,   773,     0,   852,   675,     2,   496,  1115,
     497,   496,   487,   556,   488,   581,   489,   588,   585,   606,
     852,   607,   720,     0,   496,   721,  1068,  1069,   496,   722,
     724,   677,   595,   601,   678,   679,   680,     0,   677,   855,
       0,   784,   772,     0,   866,   865,   861,   864,     0,   859,
     862,    25,     0,    24,     0,     0,     0,     0,     0,     0,
       0,     0,    27,    29,     4,     8,     5,     6,     7,     0,
       0,   496,     2,     0,   111,   112,   113,   114,    91,    28,
      92,    46,    90,   115,     0,     0,   130,   132,   136,   139,
     142,   147,   150,   152,   154,   156,   158,   160,   163,     0,
      30,     0,   602,     2,   115,   496,   164,   778,   728,   592,
     730,   777,     0,   727,   731,     0,     0,     0,     0,     0,
       0,     0,     0,   868,   894,   852,   904,   912,   916,   922,
     595,     2,     0,  1101,   496,  1104,     2,   108,   496,     3,
     719,     0,  1115,     0,   497,   556,   581,   588,     3,     3,
     715,   705,   709,   721,   722,   496,     2,     2,   897,   915,
    1091,     2,     2,    27,     0,     2,   738,    28,     0,   736,
     739,  1113,     0,     0,   745,   734,   733,   496,     0,   835,
       0,     2,   461,   463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   873,   930,
     953,   852,   475,     2,   869,   877,  1011,   732,   871,   872,
       0,   833,   496,   926,   934,   732,   928,   929,     0,   945,
     947,     0,   465,   496,   496,   562,   497,     0,   578,     0,
     496,  1096,  1100,  1098,   447,   579,   807,     0,   833,   850,
       0,   441,   451,   510,     0,   833,   496,   807,     0,   833,
     782,   559,   560,   576,   591,   597,   600,   595,   601,   619,
     620,     0,   783,   691,   725,   497,     0,   692,   694,   695,
       0,   212,   424,   832,     0,   422,   479,   478,   578,     0,
     443,     2,   444,   804,   467,     0,     0,     0,   496,   850,
     496,   807,     0,     0,     2,     0,   766,   765,   764,   758,
     507,     0,   756,   774,   554,     0,   496,   496,  1070,   497,
     493,   494,   495,  1074,  1065,  1066,  1072,   496,     2,   109,
       0,  1030,  1044,  1115,  1026,   852,   852,  1035,  1042,   713,
     496,   586,   723,   497,   582,   583,   587,     0,   676,  1080,
     497,  1085,  1077,   496,  1082,   852,     0,   852,   677,   677,
       0,     0,   496,     0,     0,   857,   861,    70,     0,    26,
     496,    98,     0,   496,   107,   496,   496,    93,   496,   100,
       0,    36,    40,    41,    37,    38,    39,   496,    96,    97,
     496,   496,   496,     2,   111,   112,     0,     0,   190,     0,
       0,   622,     0,  1090,     0,     0,     0,     0,     0,     0,
       0,     0,    59,     0,    65,    66,    70,     0,     0,    70,
       0,    94,    95,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   496,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   171,     0,
     169,   170,   496,   995,   729,   992,   852,   852,  1000,   603,
     496,   858,   895,   852,   905,   913,   917,   923,   496,   898,
     900,   902,   496,   918,   920,     2,     0,     2,  1102,  1105,
     496,   496,     0,     2,     0,   496,   109,  1030,   852,  1115,
     965,   852,   852,  1115,   852,   980,   852,   852,     3,   723,
     496,     0,   496,     0,  1115,  1115,   496,   496,   496,   496,
       0,     2,   747,     0,  1113,   744,  1114,     0,   740,     0,
       2,   743,   746,     0,     2,     0,   496,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   852,
     882,   886,   925,   852,   939,   943,   951,   852,   961,   874,
     931,   954,     0,     0,  1007,     0,   473,   836,     0,     0,
     474,   837,   466,     0,     0,     0,     0,   464,     0,     2,
       2,   838,     0,   445,     2,   807,     0,   833,     2,   839,
       0,     0,     0,     0,   634,   891,   496,   909,     0,     0,
     496,   425,   423,   108,     3,   496,     0,   808,     2,     0,
     760,   496,   496,   754,   753,   754,   555,   553,   679,  1076,
     496,  1081,   497,   496,  1067,     0,     0,     0,     0,  1045,
       0,   852,  1116,  1031,  1032,   714,  1028,  1029,  1043,  1071,
    1075,  1073,   584,   619,  1079,  1084,   671,   677,   677,     0,
       0,   686,   685,  1113,     0,   677,   787,   785,     0,     0,
     860,    74,     0,    71,    72,    75,   819,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   496,   496,     0,   129,   128,     0,   125,   124,
      31,     0,    32,     0,     0,     0,   187,   186,     0,     3,
      70,     0,    55,     0,    56,    63,     0,    62,     0,    58,
       0,    57,    61,     0,     0,    54,   131,   133,   134,   135,
     137,   138,   140,   141,   145,   146,   143,   144,   148,   149,
     151,   153,   155,   157,   159,     0,     0,   434,     0,     0,
      33,     3,   738,   165,     0,     0,     0,     0,   996,   997,
     993,   994,   780,   779,   496,   899,   901,   903,   496,   919,
     921,     0,     0,   496,   496,  1021,  1020,   496,     0,     0,
       0,     0,     0,   852,  1031,   968,   985,   496,   963,   971,
     711,   966,   967,   712,   496,   978,   988,   981,   982,     0,
       3,  1115,     3,   707,   457,   706,   710,  1107,   716,   717,
     699,     0,   700,   701,     3,     3,     3,   732,     0,   163,
       0,     3,     3,     0,   741,     0,   735,     0,   852,   496,
     852,     3,   468,   852,   883,   887,   852,   940,   944,   952,
     852,   962,   496,   875,   878,   880,   496,   932,   935,   937,
     496,   955,   957,   959,   850,   476,  1008,     3,  1012,  1013,
       3,   841,   948,   568,   567,   570,   569,     2,   808,   842,
     789,   496,     2,   840,     0,   808,   843,   634,   634,   634,
     496,   693,   696,   697,   726,   428,     0,     0,     0,   496,
       0,     0,   349,     0,     0,     0,     0,     0,   192,     0,
     344,   345,     0,     0,   349,     0,   397,   396,     0,   167,
     167,   403,   595,   601,   209,   496,     2,     0,   193,     0,
     220,   194,   195,   496,   214,   196,   197,   198,   199,     0,
     200,   201,   350,     0,   364,   202,   370,   372,   375,   203,
     204,   205,   206,   207,     0,   208,   216,   578,   496,   218,
       0,     0,     0,     3,   821,   808,     0,   796,   797,     0,
       3,   792,     3,     3,     0,   496,   771,   771,  1078,  1083,
       2,   108,   496,     3,   593,     3,   497,  1039,   852,  1038,
    1041,   496,     3,  1027,  1033,   677,  1113,     0,   677,   682,
     677,     0,   687,  1113,     2,   856,   863,     0,    99,   102,
     496,   106,   496,     0,   105,   101,   103,   496,     0,   119,
       0,     0,     0,   123,   127,   126,   191,     0,     0,     0,
     738,   116,   184,     0,     0,     0,    49,    50,    88,     0,
      88,    88,     0,    76,    78,    52,     0,    48,     0,    51,
     162,     0,     0,     0,     0,  1113,  1004,   852,  1003,  1006,
     998,     0,     0,   892,   910,     3,     3,     0,   852,   974,
     977,   852,     0,   852,   852,   969,   986,     0,     0,  1108,
       0,   718,   496,     0,  1110,   496,   496,     0,   496,     0,
     442,     3,     0,     0,     0,     0,   737,   742,     3,   834,
       3,   851,     0,   496,   876,   879,   881,   496,   933,   936,
     938,   496,   956,   958,   960,     0,     0,     0,   732,  1019,
    1018,     0,     0,     0,     0,     0,     3,   808,   844,     0,
     496,   496,   496,   496,   496,   496,   496,   617,   647,     0,
       0,   648,   578,   635,     0,     0,     0,   426,    70,     0,
       0,   335,   336,   217,   219,   496,     0,     0,     0,   496,
     496,   331,     0,   329,     0,     0,     0,   738,     0,     0,
     496,     0,   376,   496,     0,   168,     0,     0,   404,     0,
       0,     0,   852,   224,     0,   215,     0,   326,     0,     0,
       0,   349,   349,   355,   354,   349,   366,   365,   349,   349,
       0,   578,     0,  1023,  1022,     0,     0,   763,   799,     2,
     794,     0,   795,     0,   775,   755,   759,   757,   496,     0,
       0,     0,     3,   496,  1034,  1036,  1037,     0,     0,   108,
       0,     3,     0,     0,   677,  1113,     0,     0,   666,     0,
     681,     0,   788,   496,    73,  1024,     0,     0,     0,    42,
       0,   120,   122,   121,   118,   117,   738,  1113,   189,   188,
       0,    69,    85,     0,    79,    86,    87,    64,     0,     0,
       0,    60,     0,   161,   433,    34,     0,   496,   999,  1001,
    1002,   893,   911,     0,     0,   852,   496,   970,   972,   973,
     496,   987,   989,     0,   964,   979,   975,   990,  1109,   708,
     458,   703,   702,   704,  1112,  1111,     0,     3,   849,   748,
     749,     0,     0,   852,     0,     0,     0,   884,   941,   949,
     477,     0,  1014,     0,  1015,  1016,  1010,   825,     2,     0,
     827,   617,   617,   617,   648,   655,   622,     0,   661,   648,
       0,   496,   609,   646,   642,     0,     0,     0,     0,   649,
     651,   852,   663,   663,   663,     0,   643,   659,   429,     0,
     339,   340,   337,   338,   233,     0,     0,   235,   437,   234,
     578,   496,     0,     0,   349,     0,   317,   316,   318,     0,
     349,   192,   273,     0,   266,     0,   192,   332,   330,     0,
     324,  1113,   333,     0,     0,     0,     0,   385,   386,   387,
     388,     0,   378,     0,   379,   341,     0,   342,     0,     0,
     369,     0,   213,   328,   327,     0,     0,   358,   368,     0,
     349,   371,     0,   373,   395,   427,   852,   823,   754,   776,
     496,     2,     2,  1086,  1087,  1088,     0,     0,     3,     3,
       0,  1047,     0,   677,   667,  1113,     0,   684,   687,   738,
     688,   670,     3,     0,  1025,   104,     0,    35,   496,     0,
    1113,     0,     0,    89,     0,    77,     0,    83,     0,    81,
      47,   166,     0,   852,   852,     0,     0,   751,     0,   452,
     456,   885,   942,   950,     0,     0,   791,   829,   613,   615,
     611,     0,     0,  1054,     0,   656,  1059,   658,  1051,   852,
     852,   641,   662,   645,     0,   644,     0,     0,     0,   665,
       0,   637,   852,   636,   652,   664,   653,   654,   660,   349,
     349,   236,   578,     0,     0,   254,   349,   322,   320,   323,
     319,     0,   321,     0,   262,     0,   192,     0,   349,   496,
     274,     0,   299,     0,     0,   325,     0,     0,   349,   348,
     349,   389,     0,   380,     2,     0,     0,     0,   211,   210,
     351,     0,     0,   349,     0,   349,   349,   349,   455,   771,
     793,     0,     0,  1089,  1040,     0,     0,  1046,  1048,  1113,
       0,   669,   683,  1113,     2,    53,    45,    43,    44,     0,
      67,   185,    80,     0,     0,  1005,   454,   453,   976,   991,
     750,  1009,  1017,   639,     0,     0,     0,  1055,  1056,   852,
     640,  1052,  1053,   638,   618,     0,     0,   347,   225,     0,
       0,     0,   247,   349,   227,     0,     0,   349,   256,   271,
     282,   276,   349,   192,     0,     0,   286,     0,   311,     0,
     313,   277,   275,   264,   267,     0,     0,   192,   300,     0,
       0,   230,   346,   377,     2,   496,   343,     0,     0,   405,
     356,     0,    70,   367,   360,     0,   361,   359,   374,   762,
     798,   800,  1049,  1050,     0,   673,     0,   790,    68,    84,
      82,   852,  1062,  1064,  1057,     0,   650,   349,   242,   237,
     240,     0,   239,   246,   245,     0,   496,   249,   248,   349,
     258,     0,   255,   349,     0,     0,     0,   263,   268,   314,
     315,     0,     0,   192,   287,   312,     0,     0,   349,     0,
     302,   303,   301,   270,   334,     0,   496,   496,     3,   390,
     497,   394,     0,   398,     0,     0,     0,   406,   407,   352,
       0,     0,     0,   672,   689,   496,  1058,  1060,  1061,   657,
     226,     0,   244,     0,   243,   229,   250,   496,   418,   259,
     349,   260,   257,   272,   285,   283,   279,   291,   289,   290,
     288,   269,   284,   280,   281,   278,   265,     0,     0,     0,
       0,   232,   250,     3,   383,     0,  1054,   391,   392,   393,
     405,     0,     0,     0,   405,     0,   357,   353,   349,     0,
       0,   238,   241,   349,     3,   251,   419,   261,     0,     0,
       0,     0,   310,   308,   305,   309,   306,   307,   304,     3,
     383,     0,     0,  1055,     0,     0,     0,   399,     0,   408,
     362,   349,  1063,   221,     0,     0,   349,   298,   296,   293,
     297,   294,   295,   292,     0,     0,   384,     0,   411,     0,
     409,     0,   411,   363,   223,   222,   228,     0,   231,     0,
     381,   412,     0,     0,   400,     0,   382,     0,     0,     0,
       0,   413,   414,     0,   410,   401,     0,     0,   402,   415
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1870,  6495,  4232, -1870,    -1,   619,  2094,  9217,   -54, -1870,
    -350, -1870,   407, -1870,  -702, -1870,   859,  -797, -1121, -1870,
     216,  3335,  1761, -1870,  1212, -1870,  1455,   148,   812,   823,
     646,   822,  1413,  1414,  1419,  1423,  1412, -1870,   -68,  -151,
    9223,   965, -1870,  1746, -1870, -1870, -1272,  8351, -1017,  2120,
   -1870,    89, -1870,   953,    54, -1870, -1870,   732,   121, -1870,
   -1673, -1516,   345,   112, -1870, -1870,   720,   356,   258, -1598,
   -1870, -1419, -1870, -1870, -1870, -1870,   156, -1201, -1870, -1870,
   -1246,   481, -1870, -1870, -1870, -1870, -1870,    84, -1218, -1870,
   -1870, -1870, -1870, -1870,    81,   500,   501,   177, -1870, -1870,
   -1870, -1870,  -707, -1870,   110,    57, -1870,   180, -1870,  -167,
   -1870, -1870, -1870,   948,  -842,  -964,   -70, -1870,     6,    13,
      85,    22,  -910,  -871, -1870,   -91, -1870, -1870,    15, -1870,
    -162,  3980,  -198,  -254,  3053,  2488,  -642,    19,   120,   668,
    1420,  2705, -1870, -1870,  2182, -1870,   111,  5087, -1870,  2118,
   -1870,    48, -1870, -1870,  1965,   158,  5681,  4392,   -57,  1962,
    -331, -1870, -1870, -1870, -1870, -1870,  -257,  7816,  7863, -1870,
    -402,   206, -1870,  -625,   312, -1870,   245,   795, -1870,   -63,
    -232, -1870, -1870, -1870, -1870,  -155,  8715,  -935,   934,   484,
    1807, -1870,  -310,  -165,  -156,  1415,  2791,  -791,  -137,   983,
    -206,  -355,  -271,  -210,  -509,  1392, -1870,  1736,   220,  -938,
    1609, -1870, -1870,   735, -1870, -1264,  -179,  -270,  -525, -1870,
     280, -1870, -1870, -1161,   514, -1870, -1870, -1870,  2269,  -861,
    -507,  -777,   -12, -1870, -1870, -1870, -1870, -1870, -1870,   717,
    -847,  -196, -1869,   232,  7154,   -71,  6060,  -128,  1569, -1870,
    2947,   -86,  -213,  -199,  -191,    40,   -72,   -46,   -39,   379,
      56,   136,   147,  -187,    28,  -121,  -119,  -118,    35,   -82,
     -64,   -38,  -761,  -733,  -706,  -704,  -743,  -127,  -688, -1870,
   -1870,  -718,  1478,  1508,  1509,  1916, -1870,   627,  6764, -1870,
    -594,  -632,  -628,  -622,  -775, -1870, -1549, -1790, -1748, -1711,
    -655,  -157,  -268, -1870, -1870,   -17,   271,  -115, -1870,  7550,
     447,  -553,  -446
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,   853,   428,   429,   177,    84,  1247,   430,   403,
     431,  1568,  1569,   432,  1002,  1003,  1004,  1362,  1363,  1364,
    1582,   454,   434,   435,   436,   736,   737,   437,   438,   439,
     440,   441,   442,   443,   444,   445,   446,   447,   456,  1150,
     738,  1496,   799,   223,   801,   450,  1038,  1248,  1249,  1250,
    1251,  1252,  1253,  1254,  2154,  1255,  1256,  1685,  2010,  2011,
    1942,  1943,  1944,  2124,  2125,  1257,  1703,  1704,  1958,  1705,
    1852,  1853,  1258,  1259,  1260,  1261,  1262,  1263,  1881,  1885,
    1519,  1511,  1264,  1265,  1518,  1512,  1266,  1267,  1268,  1269,
    1270,  1271,  1272,  1722,  2142,  1723,  1724,  2048,  1273,  1274,
    1275,  1499,  2056,  2057,  2058,  2182,  2193,  2076,  2077,   311,
     312,   940,   941,  1216,    86,    87,    88,    89,    90,  1688,
     490,   209,    94,    95,    96,    97,   239,   240,   314,   293,
     492,   458,   493,   100,   326,   102,   103,   157,   361,   317,
     107,   108,   109,   173,   110,   957,   362,   158,   113,   263,
     114,   159,   272,   364,   365,   366,   160,   451,   119,   120,
     368,   121,   611,   933,   931,   932,  1662,   122,   123,   124,
     125,  1210,  1463,  1668,  1669,  1814,  1815,  1464,  1657,  1834,
    1670,   126,   698,  1315,   169,   992,   127,   993,   994,  1560,
     965,   617,  1141,  1142,  1143,   618,   372,   501,   502,   620,
     460,   461,   224,   520,   521,   522,   523,   524,   349,  1296,
     350,   955,   953,   649,   351,   391,   352,   353,   462,   128,
     179,   180,   129,  1290,  1291,  1292,  1293,     2,  1197,  1198,
     640,  1284,   130,   339,   340,   274,   285,   594,   131,   227,
     132,   329,  1152,   584,   554,   171,   133,   398,   399,   400,
     134,   331,   243,   244,   245,   332,   136,   137,   138,   139,
     140,   141,   142,   248,   333,   250,   251,   252,   334,   254,
     255,   256,   839,   840,   841,   842,   843,   257,   845,   846,
     847,   804,   805,   806,   807,   555,  1190,  1442,   143,  1773,
     673,   674,   675,   676,   677,   678,  1817,  1818,  1819,  1820,
     663,   503,   376,   377,   378,   463,   215,   145,   146,   147,
     380,   867,   679
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      82,   392,   873,    82,   198,   196,   359,   864,   591,   575,
     153,   375,   242,   526,    91,   387,    98,  1297,  1515,   741,
     104,   205,  1924,    93,   537,   980,    93,  1501,   321,   681,
     199,   373,  1536,  1537,   449,   966,   533,   200,   538,   967,
    1446,   135,   498,   388,  1500,   968,   539,   183,   313,   115,
     540,   499,  1316,  -801,  1048,    82,    82,  1054,    82,  1133,
    1323,  1135,   572,  1119,  1925,   225,   465,  1205,   214,  1126,
    2012,  1726,    91,   747,    98,    82,  1115,   974,   104,   920,
     561,    93,   653,  2019,    82,  1089,    92,  1285,   306,   154,
     928,   485,    82,  1279,  1116,  1029,  1592,    82,  1276,   135,
      82,  1926,  1109,  1728,    82,  1680,   691,   115,  1864,   588,
     694,   279,   111,   228,   537,   161,   541,   448,   542,   543,
     599,   105,   653,   268,   467,   212,   249,    93,   538,  1110,
     402,  1111,   201,   253,   948,   214,   539,   324,   246,  -833,
     540,   275,  2080,    82,    92,   286,    82,  1112,    82,   990,
     468,  1727,   548,   283,    82,   544,   313,   469,  2013,   116,
     658,    82,    91,   681,    98,   198,   535,   631,   104,    82,
     111,    93,   148,   545,  1729,    58,   689,    58,   337,   105,
     692,    82,  1412,  1233,  -802,   313,   267,   115,  2020,   135,
     280,   199,  1439,    82,    82,    82,   313,   115,   200,   546,
     938,   664,   306,   966,  2012,    82,   541,   967,   542,   543,
      82,  1488,   202,   968,  1440,   742,  1413,   116,   914,   916,
     517,   624,   401,   203,   307,    82,   696,   975,   242,   491,
      82,    82,    82,   700,    92,   212,    82,    82,  1584,    75,
     206,    75,   548,   974,   606,   544,  1458,  2081,  1740,   582,
     111,   508,   470,  1366,   697,   198,   635,    82,  1959,   316,
     111,  2054,   553,   545,  1928,    82,   549,    82,   704,   105,
    2018,   193,   144,   550,   631,   144,    82,    82,   212,  -614,
      82,   199,  1373,  1857,   681,  2072,   906,    82,   200,   546,
     658,   781,  -983,   201,   910,   115,  1589,   116,   670,  -983,
    1459,    82,    82,   268,    82,   991,   212,   116,  1782,    82,
     384,  1153,   868,    82,  1739,  1924,   529,  1500,  1742,   582,
    2014,   595,   879,   306,   115,   230,    82,    82,   681,  1590,
     144,   664,   471,  1119,   782,   115,   880,    82,  1355,  1460,
    1682,  1447,   249,   472,   881,    82,    82,   997,   882,   627,
      82,  1411,   681,  1395,   626,  1052,   549,  1925,   111,   681,
     115,  1013,  1009,   550,   283,   844,   592,   316,   212,   829,
    1380,  1396,  1474,   202,  1109,   912,   144,  1473,  2004,  1314,
      82,   917,   225,  1555,   203,  1345,   162,   111,   961,    82,
     485,  1466,    82,  1120,  1926,    82,   316,  1123,   111,  2071,
     623,  1110,  1772,  1111,   167,   116,  1279,   316,  1138,  1139,
    1467,  1276,  1939,  1940,   883,   498,   884,   885,  1202,  1387,
     144,  1445,   879,   111,   499,   985,  1513,  1294,  1449,   927,
      20,  1031,   316,  1413,   116,    58,   880,  2118,   214,  1556,
    1321,  2026,  1965,  1966,   881,   116,  2073,  2074,   882,  1514,
    2018,   220,  1233,   886,   226,  1093,  -833,   205,   959,  1939,
    1940,   899,   221,   467,   213,    82,  -668,  1785,  1787,  1789,
     116,   887,   552,  -668,   557,   966,   875,   247,   222,   967,
     276,   565,   979,  2018,   287,   968,  1458,  1458,  1458,   468,
      82,    82,   498,   204,    64,   984,   469,   888,  1941,    75,
    2108,   499,    82,    82,  1007,   313,   508,  1010,  1526,  1012,
     740,   231,  1014,    82,   883,   517,   884,   885,   168,   232,
    1314,  1017,  1501,   664,  1019,  1020,  1021,  1469,  1470,  1516,
    1663,  1675,   217,  1545,    82,   218,  2031,  2032,  1664,  1500,
    1459,  1459,  1459,   989,   650,  1970,    82,  1144,   651,  1146,
    1676,   899,  1517,   886,   630,   632,   182,  1928,    63,    64,
    2123,   163,   961,   467,   164,   165,  1161,   166,   600,  1883,
      82,   887,   184,  1375,   213,   900,    82,   646,    82,  1460,
    1460,  1460,   901,   612, -1114,  1509,  2123,   508,   705,   468,
     509,   470,   706,  1466,  2004,   991,   469,   888,  1831,   217,
     384,  1887,   491,  1513,  1884,  1832,   647,   648,    78,  2156,
     701,  1619,  1745,   703,  1133,  1135,  1612,   213,  1510,  1765,
      83,   498,   748,   151,  1833,  1989,  1514,   749,   947,   306,
     499,   681,   959,  1686,    82,  1283,    82,  1686,  1706,    82,
     464,    82,  1822,   185,   115,   213,  1471,   980,  1730,   907,
      82,  1706,  1024,    91,    82,    98,  1154,   911,  1468,   104,
     596,  1823,    93,  1025,  1026,   900,   498,   605,    64,   106,
    1649,   471,   901,  1031,    58,   499,   921,   508,    83,   491,
     135,  1675,   472,  2037,  1855,  1401,    82,   929,   115,  1863,
     654,   301,  1299,  1031,  1132,    83,   359,  1130,   771,   772,
    1825,   375,  1145,  1832,    83,    82,  1184,   111,   193,    58,
     844,   194,  1865,   583,    58,  1136,   316,    83,  1929,  1031,
      83,  -491,  1923,  1832,    83,    92,    58,   106,   606,  2098,
    1078,    14,    15,    16,    17,    18,    58,  1930,    75,  1031,
    2062,    -3,  1933,   773,   774,  1847,  1848,  1849,    82,   783,
      82,   111,    82,   784,   116,  1031,    82,   305,  2024,    82,
     105,   553,  1298,  1553,   193,  2100,  1679,  1850,    83,   270,
    1561,  1031,  -984,    75,    83,  1031,  1851,   963,    75,  -984,
     448,  2129,   750,   583,    82,  2131,  1784,   751,   231,   385,
      75,    58,   562,  1385,  1386,  1149,   553,   638,   116,   740,
      75,   553,   740,   259,   260,   666,   261,   740,   491,   818,
    1889,  1369,   262,   553,    83,    83,   740,   106,  1327,  1094,
     193,   923,  1596,   553,   764,   482,   234,   -23,  1899,    82,
      83,   765,   766,  1317,    82,   740,    82,    74,   809,  1957,
      83,  1577,   810,   741,    58,   509,   232,  1349,    82,  1420,
     532,    83,   534,   491,  1350,    75,    83,    83,   667,    82,
     822,   359,   668,   233,   553,   517,   375,  1145,    82,    80,
     669,   923,   491,   473,   491,   553,  1410,    83,   491,   491,
    1581,   491,   670,   258,   305,    83,   473,  1327,    14,    15,
      16,    17,    18,  -490,    58,   869,   870,    58,   491,   871,
      83,    82,   217,  1847,  1848,  1849,   301,   485,    75,    74,
    1858,   144,  1057,  1058,  1059,  1859,    14,    15,    16,    17,
      18,   609,   278,  1870,   614,  1850,   509,  1117,  1859,   151,
     802,   668,   303,    83,   553,   666,  2028,    82,    82,   517,
    1535,    80,    81,   769,   770,  1417,    83,    83,    58,   593,
    2043,   963,   811,    91,    58,    98,   706,  1287,    75,   104,
     305,    75,    93,   560,   305,  1714,   473,   491,   553,    58,
     568,  1774,   325,    93,   464,   681,    58,  1124,  1711,   306,
    1172,   668,    58,   270,   553,   390,  1394,   844,   115,  1913,
      58,  1914,   587,    82,  -822,   163,   347,  1191,   164,   165,
     695,   166,  1766,   598,    58,   767,   768,  1370,  1978,  2113,
      74,  1199,    75,  1979,  2114,  1203,  2091,  1706,    75,  1206,
     464,   464,  2171,   393,  1781,    92,   937,  2172,   270,  1811,
     938,   667,   401,    75,  1824,   668,  1288,  1176,    74,   742,
      75,   553,    80,   669,   996,    82,    75,   474,   651,    82,
     475,   111,  1180,  1656,    75,   476,   553,  1547,   477,   802,
    1278,   506,   562,   553,   892,  1423,   553,  1780,    75,   553,
      80,    81,   638,  1427,   473,   505,   553,   553,    82,   998,
     999,   517,    74,   651,   706,    83,    58,  1431,  1117,  1149,
     473,   553,   668,  1528,   525,    58,  1030,    58,   116,   478,
    1031,  1098,   479,   667,   979,   553,    82,   668,    58,  -492,
     511,    83,    82,    82,    80,   669,   527,   392,   392,    14,
      15,    16,    17,    18,  1158,   491,   491,   981,   810,   512,
      58,  1570,   530,    83,   305,    83,  1826,   531,   553,    14,
      15,    16,    17,    18,   562,   664,   551,    82,   553,   464,
      75,  1193,  1195,   228,    83,  1031,  1031,  1008,  1866,    75,
    1552,    75,   270,   464,   573,  1015,    83,  1640,  1565,  1543,
     359,   574,    75,   668,  2078,   375,  1145,   579,  1597,    58,
    1606,   491,   553,   586,   553,    14,    15,    16,    17,    18,
      83,  1610,   225,  1465,    75,   668,    83,   638,   482,    58,
     597,   553,  1326,   621,  2078,   642,  1327,  1287,  1847,  1848,
    1849,   144,  1900,  2065,  1365,    74,   641,   553,  1327,   625,
    1031,   517,   144,    93,    82,    82,    82,  1909,   775,   776,
    1850,   656,    91,   517,    98,  2126,  1812,   688,   104,  1856,
     553,    93,   699,    75,   384,    58,   359,    80,    81,  1622,
     707,   375,   517,   712,    83,   702,    83,   464,    82,    83,
    1903,  1627,  1628,    75,  -494,  1136,    91,   115,    98,  1136,
     708,  1136,   104,    82,   958,    93,    82,    82,   908,    82,
    2060,   505,   744,   270,   279,    82,  1288,  1534,    82,   709,
    1777,   810,   715,  1448,  1778,   716,   593,   720,  1839,   268,
      93,   115,  1327,  1593,    92,   922,   983,  1472,   106,    75,
    -495,  1843,   926,   830,   744,  1031,   930,   275,   286,   744,
      14,    15,    16,    17,    18,  1867,   283,  1494,   763,  1031,
     111,  1868,    82,  1869,   740,   810,  1934,  1031,    92,  1278,
     810,  1022,   744,  1983,   448,   448,  1994,  1031,   778,   517,
    1996,   593,  1717,  1718,  1719,  1720,  1721,   752,    82,   753,
     754,   755,   267,   280,   111,  2021,   777,  2116,  1042,  1031,
    1044,  1327,  1047,  1278,  2117,  1672,   359,   116,  1031,  1055,
      58,   375,  1145,  2190,  1689,   785,  2196,  2187,  1689,   756,
    2197,    82,   757,   758,  1808,  1809,  1810,   759,   760,   464,
    1847,  1848,  1849,  2144,  1080,  1687,   780,  2148,  1770,  1687,
     779,   116,  1031,  1450,  1451,  1452,  1438,  1064,  1065,  1066,
    1067,   812,  1850,   813,   491,  1033,  1034,   491,   491,   656,
     744,  -193,   814,  1465,  1465,  1465,  1352,  1353,  1658,  1465,
    1835,  1835,  1835,   481,    75,  1367,  1368,  1031,  1371,   482,
    -164,  -164,   537,   815,    83,   816,    83,   206,   744,   153,
      82,  1509,  1510,   817,    82,    82,   538,  1587,  1588,  1591,
    1588,    -3,   144,   825,   539,    93,  1595,  1588,   540,  1106,
    1580,   827,   191,  1629,  1580,    83,   517,   848,    83,  1673,
     144,   448,  1106,  1641,   104,   850,  1674,    93,   104,   104,
    -493,    93,    93,  -129,  -129,  -129,  -129,  -129,  -129,   852,
     517,   517,   104,  1790,  1353,    93,  1911,  1353,  1912,  1588,
      82,    83,   -18,   115,   144,   866,   289,   115,   115,  1921,
    1031,   290,  1981,  1982,   294,   865,   299,   874,   154,  1999,
    1588,   115,   889,   268,   541,   877,   542,   543,   890,   144,
    -128,  -128,  -128,  -128,  -128,  -128,    82,  1214,  1681,  1683,
      92,   595,  2000,  1588,    92,    92,  1939,  1940,  1672,  1287,
    2187,  2188,  1570,  1672,   891,   517,  1585,  1586,    92,  1060,
    1061,   548,    82,   544,   893,    93,   111,    82,    82,    82,
     111,   111,  1062,  1063,  1827,  1691,   894,  1068,  1069,  1691,
    1691,   545,  1741,  1743,   111,   895,   592,   896,   106,  1744,
    1836,  1837,   897,  1691,   879,    14,    15,    16,    17,    18,
    1028,   717,   500,   898,   903,   904,   318,   546,   880,   939,
     270,   924,   925,   116,  -612,  -610,   881,   116,   116,   934,
     882,   935,   936,   942,  1204,   950,   761,   762,  1288,   952,
     956,   116,   969,    82,   971,  1006,   276,   287,    82,   670,
     987,   995,  1032,  1035,    82,    83,    82,   761,  1077,    83,
    1040,  1082,  1105,  1106,    82,  1877,    14,    15,    16,    17,
      18,  1134,  1673,  1113,  1137,  1156,  1160,  1673,  1185,  1674,
     289,  1163,  1164,   517,  1674,   549,  1991,   279,    83,   517,
     761,    83,   550,  1165,  1166,  1167,   883,  1168,   884,   885,
     392,  1169,   268,    93,  1170,  1171,   830,  1192,  1194,   681,
    1196,  -805,  1200,  1280,   144,  1207,    83,  1208,  1209,  1318,
     517,  1286,    83,    83,  1295,  1307,    58,   289,  1308,   283,
    1309,  1310,   464,  1320,   899,   886,   144,  1325,  1324,   981,
     144,   144,  1328,  1329,  1330,  1331,  1287,  1022,  1334,  1335,
    1333,  1336,    85,   887,   144,   152,  1337,    83,   517,  1338,
    1340,    19,    93,  1341,  1342,   267,   280,  2049,  1347,  1348,
    1356,  1372,   290,  1357,   685,    82,   299,    82,  2009,   888,
      14,    15,    16,    17,    18,  1344,  1376,  1377,    74,  1378,
      75,   186,     6,     7,     8,     9,    10,    11,    12,    13,
    1871,    48,    49,    50,    51,    52,    53,    54,    55,  1812,
      85,  1399,  1977,   553,  1402,  1379,    82,  1383,  1384,    82,
      80,    81,   725,  1388,   144,  1288,  1389,   195,   517,   517,
    1390,  1391,  1404,  1405,  1416,   517,    85,  1406,  1408,  2049,
    1672,  -806,  1443,  2107,  1475,  1476,  1479,   517,   900,   238,
    1480,  1489,   266,   288,   268,   901,    85,   517,  1490,   517,
    1491,   448,  1493,  1498,   -22,  1503,  1502,  1031,   104,  1523,
    1524,    93,   517,  1530,   517,   517,   517,   106,  1532,  1554,
      82,    82,   537,   614,  1558,  1562,    83,    83,  1576,  1580,
     596,  1559,  1601,  1594,   152,  1602,   538,   115,    83,  1605,
      85,  1618,  1616,   152,   539,  1617,   328,   336,   540,  1620,
    2121,   106,  2009,  1624,  1625,  2055,  1588,   592,  1630,   358,
    1633,  1637,  1638,   725,  1639,    82,  1646,   464,  1647,  1650,
    1707,  1661,   517,  1468,    92,   270,   517,  1693,  1510,  1708,
    1710,   517,  1566,    82,   455,  1732,   195,   195,  2051,  1712,
    1725,  2146,   144,  1733,  1673,  1735,  1736,   152,   488,    83,
     111,  1674,   266,  1734,   593,  1056,  1233,  1746,    83,  1691,
    1747,  1749,  1751,  1752,   541,  1753,   542,   543,  1754,  1755,
    1761,  1764,  1771,   328,  1763,   500,  1775,  1776,   238,   238,
    1783,  1791,  1779,   448,  1797,   448,   517,  1793,  1794,   473,
    1799,    83,  1801,   289,  1629,  1802,  1803,   116,   517,   328,
     548,   144,   517,   544,  1806,  1807,  1821,    85,  1666,  1840,
    2051,  1844,  2189,   198,   635,   104,   226,   517,    93,  1846,
    1874,   545,   266,  1876,   448,  1947,  1894,  1893,    82,  1901,
      82,  1897,  1898,  1905,  1910,  1915,  2055,   899,  1918,   199,
    2055,  2055,  1919,  1920,   115,   104,   200,   546,    93,  1952,
    1953,  1974,   500,  2166,  1967,   328,  1969,  1976,  1980,   517,
      83,   336,  1985,  1992,  1993,  1995,  1997,   336,   328,   328,
    1998,  2169,  2001,   553,   115,  2002,   104,   152,  2003,    93,
    2007,    92,  2023,  2038,    82,    82,    83,  2052,   464,  2033,
    2025,  2036,   616,  2044,  2181,   448,  2063,   517,  2181,   358,
     671,   680,   517,  2053,  2084,   115,  2064,   111,  2075,  2097,
     144,    92,  2099,  2191,   549,   358,  1691,  2101,  2110,   358,
      83,   550,    82,   106,  2111,  2112,   212,   106,   106,  2115,
     517,  2128,  2130,   517,  2132,   517,  2145,   111,  2141,  2147,
    2151,   106,    92,  2152,   564,  2157,  1691,  2167,  2170,  2168,
    2176,   900,   496,  2178,   116,  2179,   517,  2183,   901,  2184,
    2194,  2195,   455,   508,  2198,  1907,  1564,    82,   111,   593,
    1070,  1027,  1071,  1074,  2122,    83,    82,  1691,  1072,    58,
     800,   500,    83,  1073,   116,  1497,  1505,    83,    83,    83,
    2177,  1695,  1971,  1716,  2139,  1964,   455,  2035,  2119,   803,
    1886,  2165,  1872,  1873,  2103,  2149,  2102,   195,  1522,  2185,
    1339,   174,   296,   150,   585,   116,  1343,    65,    66,    67,
      68,    69,    70,    71,    72,   152,   500,  1351,  2006,   488,
    2069,  1660,  1902,   837,  1557,   680,  2180,  1520,  1155,   872,
     954,    74,  1748,    75,  1890,   500,   152,   500,  2186,     3,
    1805,   500,   500,    83,   500,  1000,     0,     0,    83,   394,
    1085,     0,    76,    77,    83,     0,    83,   144,   455,     0,
       0,   500,     0,    80,    81,   150,   238,   382,   383,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   238,     0,
    1086,  1087,     0,     0,     0,   518,     0,   144,     0,     0,
       0,     0,     0,   661,     0,     0,   684,     0,     0,     0,
       0,     0,   328,     0,   455,   455,     0,     0,   328,   661,
       0,   358,     0,   661,     0,     0,     0,    78,   144,     0,
       0,  1212,     0,     0,     0,   395,     0,   150,   270,   384,
     500,    65,    66,    67,    68,    69,    70,    71,    72,   909,
       0,     0,     0,   150,   808,   175,   176,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,    83,     0,
       0,   820,     0,     0,   823,     0,   328,     0,   328,     0,
       0,    85,   394,     0,   150,    83,     0,    83,    65,    66,
      67,    68,    69,    70,    71,    72,  1045,   358,   488,     0,
     680,     0,   396,     0,     0,     0,     0,     0,   671,     0,
       0,   717,   671,     0,     0,     0,     0,    19,     0,     0,
       0,   358,     0,     0,     0,   616,    83,     0,     0,    83,
       0,   680,     0,   661,   358,     0,   564,  1046,     0,     0,
       0,     0,     0,   152,     0,     0,     0,     0,     0,     0,
       0,   455,     0,     0,   455,     0,   152,   152,   395,   455,
       0,    52,    53,    54,    55,     0,     0,     0,   455,     0,
       0,   152,   152,   152,     0,     0,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     593,    83,     0,     0,   761,     0,     0,     0,   500,   500,
       0,     0,     0,     0,     0,     0,   150,   106,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,  1049,     0,
       0,     0,     0,     0,     0,   496,     0,   488,     0,     0,
     190,     0,  1571,  1572,  1573,    83,     0,     0,     0,  1574,
    1575,     0,     0,   803,   803,     0,     0,     0,     0,     0,
       0,   455,     0,  2029,   500,     0,     0,     0,     0,  1050,
       0,     0,     0,   258,     0,     0,     0,     0,     0,   269,
       0,   358,   488,     0,     0,     0,   837,     0,   837,     0,
       0,   291,     0,   298,     0,   300,     0,     0,   862,  1084,
     518,   358,     0,   358,     0,     0,     0,   358,   358,   358,
     358,   661,   496,     0,     0,  1101,     0,     0,     0,  1102,
       0,     0,     0,     0,     0,     0,     0,   358,     0,     0,
       0,     0,     0,     0,   269,   661,     0,   298,   300,   186,
       6,     7,     8,     9,    10,    11,    12,    13,   661,     0,
       0,     0,     0,   328,     0,     0,     0,     0,    83,     0,
    2109,     0,   150,     0,   175,   176,    65,    66,    67,    68,
      69,    70,    71,    72,   106,     0,     0,   150,     0,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,   269,
       0,   455,     0,     0,     0,     0,   358,     0,     0,     0,
       0,     0,   152,   455,   106,    74,     0,     0,   808,   808,
       0,   358,  1506,  1302,    83,    83,     0,     0,     0,  1096,
       0,     0,  1099,     0,   671,     0,  1665,    77,     0,     0,
       0,     0,     0,  1666,     0,   106,  1481,    80,    81,     0,
     150,   496,   175,   176,    65,    66,    67,    68,    69,    70,
      71,    72,    83,     0,     0,     0,     0,   192,     0,   269,
       0,   298,   300,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,   488,     0,    14,    15,    16,    17,
      18,     0,     0,    58,     0,   661,   496,     0,   564,     0,
       0,     0,     0,   269,     0,  1174,   271,  2192,   269,  1178,
       0,     0,     0,  1182,   269,     0,  2199,   500,   292,   295,
     500,   500,     0,   496,  1507,     0,     0,   150,     0,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,    58,     0,   269,   803,
       0,     0,     0,   686,     0,   300,     0,    75,     0,     0,
       0,   271,     0,     0,   358,   358,     0,     0,   837,     0,
       0,     0,     0,     0,     0,   837,  1392,    77,     0,     0,
     150,     0,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
     724,  1381,     0,     0,     0,  1382,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,   271,     0,     0,     0,
     358,     0,     0,     0,  1397,   661,     0,     0,   684,   835,
      77,  1398,     0,   668,     0,     0,     0,     0,     0,     0,
      80,   836,     0,     0,     0,     0,     0,  1880,     0,     0,
       0,     0,     0,     0,   269,     0,     0,     0,     0,     0,
     518,     0,   152,   862,     0,     0,     0,     0,     0,     0,
       0,   152,     0,     0,     0,     0,     0,     0,     0,  1435,
     455,     0,   269,  1436,   686,   300,   271,  1437,   496,   150,
       0,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,   724,     0,     0,   808,     0,   455,     0,     0,     0,
       0,     0,     0,     0,   455,     0,     0,     0,     0,     0,
     271,     0,     0,   197,     0,   271,     0,     0,     0,     0,
       0,   271,     0,     0,     0,   269,     0,     0,   266,    85,
       0,     0,     0,     0,     0,   241,     0,     0,     0,     0,
     328,     0,     0,   384,   101,     0,   152,   156,     0,   269,
       0,     0,     0,   488,   269,   271,   269,   150,     0,   175,
     176,    65,    66,    67,    68,    69,    70,    71,    72,  1425,
       0,     0,  1429,     0,     0,     0,  1433,     0,     0,   269,
       0,   269,   269,   488,     0,     0,   619,     0,   152,     0,
       0,     0,   330,   269,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,   269,   726,     0,     0,
       0,     0,     0,     0,     0,   269,   150,     0,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,   211,     0,
    1361,  1483,     0,     0,  1361,     0,     0,   269,     0,   686,
     300,     0,     0,     0,     0,     0,     0,     0,   281,     0,
       0,     0,     0,   358,     0,     0,   358,   358,     0,   358,
       0,   269,   686,  1361,     0,     0,   518,     0,   269,   330,
       0,     0,     0,     0,   536,   241,  1485,     0,     0,     0,
       0,     0,   315,  2059,     0,     0,   320,     0,     0,   271,
       0,     0,   101,     0,     0,   330,     0,     0,     0,     0,
       0,   152,   152,   152,   152,     0,   152,   152,   726,     0,
       0,   360,  1667,   336,     0,     0,     0,     0,     0,     0,
    1634,     0,     0,     0,  1635,     0,   455,     0,  1636,     0,
     455,   455,  1361,     0,     0,     0,     0,     0,   466,     0,
       0,   455,     0,     0,   455,     0,     0,     0,     0,   320,
     494,   330,     0,     0,     0,     0,     0,   661,     0,     0,
       0,     0,     0,     0,   636,   330,     0,     0,     0,     0,
       0,     0,   266,   271,     0,     0,     0,     0,     0,     0,
       0,   547,     0,  1599,     0,     0,     0,   496,     0,   488,
     315,     0,     0,     0,  1608,     0,   271,     0,     0,     0,
       0,   571,     0,     0,     0,     0,   576,   578,     0,   211,
     271,     0,     0,     0,   152,     0,   671,     0,     0,   315,
       0,     0,     0,   271,     0,     0,     0,     0,     0,     0,
     315,     0,  1478,   601,     0,     0,     0,   603,     0,     0,
    1757,     0,   604,   619,  1492,     0,     0,     0,   615,     0,
       0,     0,     0,   578,   271,   315,     0,     0,   150,   628,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   637,     0,     0,     0,     0,     0,     0,   271,     0,
       0,     0,   150,     0,   918,   271,    65,    66,    67,    68,
      69,    70,    71,    72,  1792,     0,     0,     0,     0,     0,
       0,   659,     0,  1795,   683,  1667,  1813,  1796,     0,     0,
    1667,     0,   455,     0,     0,   644,  1667,   690,  1667,   619,
       0,   690,   341,     0,     0,     0,     0,     0,     0,   838,
     342,   343,   344,   345,   518,     0,     0,     0,     0,     0,
     269,   336,   152,  1361,     0,     0,     0,     0,   619,     0,
       0,   269,     0,    14,    15,    16,    17,    18,     0,   150,
     269,   204,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   150,   878,   175,   176,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   241,     0,     0,     0,     0,     0,
       0,     0,     0,   496,     0,     0,     0,     0,     0,     0,
       0,   152,     0,     0,     0,     0,     0,     0,   330,    77,
     433,     0,   861,    58,   330,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   346,     0,     0,   320,     0,   152,
     150,   659,   607,   608,    65,    66,    67,    68,    69,    70,
      71,    72,   347,     0,     0,     0,     0,   150,   320,   235,
     236,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,  1816,     0,  1813,  1813,     0,   269,     0,     0,     0,
       0,   518,   946,     0,   330,    74,     0,    75,  1667,     0,
       0,  1667,    78,   619,     0,     0,     0,     0,     0,     0,
       0,     0,   269,   336,     0,     0,   237,    77,     0,   619,
    1188,     0,     0,   619,     0,   615,     0,    80,    81,     0,
     455,     0,     0,     0,     0,     0,   619,     0,     0,     0,
       0,  1737,  1738,   494,     0,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   315,
       0,     0,   150,   328,   235,   236,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,   271,     0,     0,
     518,     0,     0,     0,     0,     0,     0,  1361,     0,     0,
      74,     0,  1361,  1361,  1361,     0,   746,     0,  1813,    78,
     422,   615,     0,   101,     0,     0,     0,  1667,     0,     0,
       0,   835,    77,     0,     0,   668,     0,     0,     0,   690,
     962,     0,    80,   836,     0,     0,     0,     0,     0,     0,
     615,     0,     0,     0,   973,   670,     0,  1213,  1816,  1816,
       0,     0,     0,   659,     0,     0,   152,     0,   982,   711,
       0,   714,     0,     0,   433,   719,   690,     0,     0,     0,
       0,     0,   150,     0,   728,   729,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   269,  1813,     0,     0,   433,
     433,     0,     0,     0,     0,     0,     0,   152,     0,     0,
       0,     0,  1108,     0,   838,     0,     0,     0,     0,     0,
     433,     0,     0,     0,   269,     0,     0,     0,     0,     0,
     269,     0,    77,     0,     0,   861,     0,   152,   152,     0,
    2106,   336,     0,     0,  1845,     0,     0,     0,     0,     0,
    1854,     0,     0,   433,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,   152,   494,
       0,     0,     0,  1816,     0,     0,     0,     0,     0,   330,
       0,  1879,     0,     0,     0,   615,  1088,     0,     0,     0,
       0,     0,     0,   518,     0,     0,  2106,  2106,     0,     0,
       0,   615,  2120,     0,     0,   615,     0,     0,     0,     0,
    1361,     0,  1361,   690,   962,   619,     0,     0,   615,   619,
    1114,     0,    58,     0,     0,     0,     0,     0,   619,     0,
       0,     0,     0,   494,  2106,   494,     0,     0,   619,   494,
     494,   360,   494,     0,     0,   619,     0,  2067,     0,     0,
       0,  1816,     0,     0,     0,     0,   150,     0,     0,   494,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     661,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     269,     0,     0,     0,    74,     0,    75,     0,     0,  1937,
    1938,     0,     0,   619,     0,  1816,  1948,   619,     0,     0,
       0,   619,     0,     0,     0,    76,    77,     0,  1963,     0,
       0,    99,   271,     0,   155,     0,    80,    81,  1972,   615,
    1973,     0,     0,  1277,     0,     0,     0,     0,   494,     0,
       0,     0,     0,  1984,   156,  1986,  1987,  1988,     0,   269,
       0,   271,   661,   690,     0,     0,  1306,     0,     0,     0,
       0,  1816,  1816,  1312,   150,     0,   235,   236,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   150,     0,    99,
       0,    65,    66,    67,    68,    69,    70,    71,    72,  1358,
       0,     0,    74,  1359,  1108,  1360,     0,     0,     0,  1816,
    1393,   838,     0,  2017,     0,   210,     0,  2022,     0,     0,
       0,     0,  2027,  1665,    77,   320,   360,     0,     0,     0,
    1666,     0,     0,     0,    80,    81,     0,    77,     0,     0,
    1583,     0,     0,     0,     0,     0,     0,     0,   433,   433,
     433,   433,   433,   433,   433,   433,   433,   433,   433,   433,
     433,   433,   433,   433,   433,   433,   433,     0,     0,    99,
       0,     0,     0,   319,     0,     0,     0,  2070,     0,    99,
       0,     0,   210,     0,     0,     0,     0,     0,     0,  2079,
       0,     0,     0,  2082,     0,     0,     0,   615,  1644,     0,
       0,   615,     0,     0,     0,     0,   494,   494,  2096,     0,
     615,     0,     0,     0,     0,     0,     0,   271,     0,     0,
     615,     0,     0,     0,     0,     0,     0,   615,   269,     0,
       0,     0,     0,   433,     0,     0,   483,   150,     0,     0,
       0,    65,    66,    67,    68,    69,    70,    71,    72,  1358,
    2127,     0,     0,  1359,     0,  1360,     0,     0,     0,     0,
       0,     0,   494,     0,   619,     0,     0,     0,   619,     0,
       0,     0,   619,     0,     0,   615,   271,    99,     0,   615,
       0,     0,     0,   615,     0,     0,   330,    77,  2150,     0,
    1786,     0,     0,  2153,   577,     0,   581,     0,     0,     0,
       0,     0,     0,     0,   156,     0,    99,     0,     0,     0,
       0,     0,     0,  1462,     0,     0,     0,    99,     0,     0,
       0,  2173,  1277,     0,  2175,     0,  2153,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     155,     0,    99,     0,     0,     0,     0,  2175,     0,     0,
       0,     0,     0,     0,     0,     0,  1277,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   581,     0,   269,     0,
       0,   219,     0,     0,    14,    15,    16,    17,    18,    78,
     269,  1521,     0,     0,   619,     0,   150,     0,     0,  1613,
      65,    66,    67,    68,    69,    70,    71,    72,  1358,     0,
       0,     0,  1359,   433,  1360,   659,     0,     0,     0,   433,
       0,     0,   304,     0,   576,     0,     0,   150,     0,     0,
     433,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,   615,    58,   360,    77,     0,   619,  1788,
       0,     0,     0,   118,     0,   271,   118,   619,     0,     0,
       0,   619,     0,     0,     0,     0,     0,     0,  1671,     0,
     433,     0,     0,     0,     0,   269,  1392,    77,   150,     0,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   150,   210,   605,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    74,     0,    75,     0,
       0,   118,     0,     0,     0,   494,     0,     0,   494,   494,
       0,   360,     0,     0,   828,     0,     0,  2105,    77,     0,
       0,   553,     0,     0,     0,     0,   615,   118,    80,    81,
     615,     0,     0,     0,   615,   483,     0,  1079,     0,     0,
       0,     0,     0,   273,     0,     0,     0,   118,     0,     0,
       0,     0,     0,  1462,  1462,  1462,   156,   578,   150,     0,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,  1690,     0,
       0,   118,  1690,  1690,     0,   118,    74,     0,   269,     0,
       0,   118,     0,     0,   118,     0,  1690,   271,   273,     0,
       0,     0,     0,     0,   433,     0,   633,  2105,    77,   354,
     118,   553,   386,     0,     0,     0,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
       0,     0,     0,     0,     0,   459,     0,     0,     0,     0,
       0,   360,     0,     0,     0,     0,   615,     0,   118,   459,
       0,  1671,     0,   273,     0,     0,  1671,     0,     0,     0,
       0,     0,  1828,     0,  1671,     0,   156,   150,     0,     0,
      99,    65,    66,    67,    68,    69,    70,    71,    72,  1358,
       0,   269,     0,  1359,     0,  1360,     0,   433,   150,   118,
     175,   176,    65,    66,    67,    68,    69,    70,    71,    72,
     615,     0,     0,     0,     0,     0,   118,     0,   118,   615,
       0,     0,     0,   615,     0,     0,     0,    77,   118,     0,
       0,     0,     0,   273,     0,   433,   433,   433,     0,   118,
       0,     0,   433,   433,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,   610,     0,     0,   118,     0,     0,
       0,     0,   118,     0,   118,     0,   433,   273,   118,     0,
       0,     0,   273,     0,     0,     0,     0,     0,   273,   832,
    1830,   834,     0,     0,     0,     0,     0,     0,   118,   150,
     851,   175,   176,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,  1842,     0,    58,   433,   433,   228,
     118,     0,   273,   118,   150,   271,   175,   176,    65,    66,
      67,    68,    69,    70,    71,    72,   118,     0,     0,     0,
     118,     0,     0,     0,  1935,     0,     0,  1671,   506,     0,
     150,     0,   235,   236,    65,    66,    67,    68,    69,    70,
      71,    72,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   156,     0,     0,   220,     0,    74,     0,
      75,     0,     0,   459,   150,     0,   235,   236,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,   327,
      77,     0,     0,   798,     0,     0,     0,     0,     0,   330,
      80,    81,    74,     0,     0,     0,     0,   459,     0,     0,
       0,     0,     0,     0,     0,     0,   619,     0,     0,     0,
       0,     0,    58,   237,    77,     0,  1927,     0,     0,     0,
       0,     0,     0,     0,    80,    81,   118,     0,     0,     0,
     459,     0,     0,  1671,     0,     0,   273,     0,     0,     0,
       0,     0,     0,     0,   433,     0,   150,   118,   235,   236,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,  1690,     0,     0,     0,     0,     0,     0,   459,
      99,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,  1289,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,  2105,    77,     0,     0,   553,
       0,     0,     0,     0,   118,     0,    80,    81,     0,     0,
       0,     0,     0,     0,     0,   459,   459,     0,     0,     0,
     273,     0,   118,   150,     0,   235,   236,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,   330,     0,     0,     0,
       0,    74,   483,    75,     0,     0,   150,   273,   175,   176,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
     273,     0,   237,    77,     0,     0,     0,     0,  2050,     0,
     118,     0,   118,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,   118,   459,
       0,   273,   636,   330,     0,   511,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1690,
     433,     0,   118,     0,     0,   273,     0,     0,     0,   610,
    1129,     0,   273,     0,     0,   118,     0,     0,   112,     0,
     330,     0,     0,     0,   118,     0,     0,     0,     0,  1690,
    2050,     0,   459,     0,     0,   459,     0,   118,   118,     0,
     459,     0,     0,     0,     0,     0,     0,     0,   615,   459,
       0,     0,   118,   118,   118,     0,     0,     0,     0,     0,
    1690,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2143,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1281,  1282,     0,   459,     0,
       0,  1289,     0,     0,     0,     0,     0,     0,     0,     0,
    1461,     0,   282,     0,   118,     0,     0,     0,     0,    99,
       0,     0,   459,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   118,     0,     0,     0,     0,     0,
      58,     0,   118,   459,     0,     0,   112,   118,     0,     0,
       0,     0,     0,    99,     0,     0,   112,     0,     0,     0,
       0,     0,   118,     0,   118,     0,     0,     0,   118,   118,
     118,   118,     0,     0,   150,   363,   235,   236,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   118,     0,
       0,  1354,     0,     0,   433,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   495,     0,     0,  1189,     0,     0,
       0,     0,     0,  1665,    77,     0,     0,     0,     0,     0,
       0,     0,     0,  1374,    80,    81,   433,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,   459,     0,   112,     0,     0,   118,     0,     0,
       0,     0,     0,   118,   459,     0,     0,     0,     0,     0,
       0,     0,   118,     0,  1304,   459,     0,     0,     0,     0,
       0,     0,  1400,   112,  1403,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,  1407,   602,  1409,     0,
       0,     0,  1319,  1414,  1415,     0,     0,     0,     0,     0,
       0,     0,   363,  1422,     0,     0,     0,     0,     0,   112,
       0,     0,     0,   282,     0,     0,   433,     0,   433,     0,
       0,     0,     0,     0,   118,   459,     0,     0,     0,  1441,
       0,     0,  1444,     0,     0,     0,     0,     0,     0,     0,
    1461,  1461,  1461,   155,  1654,  1655,  1659,     0,     0,     0,
       0,     0,     0,     0,     0,   660,     0,   433,   282,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    99,
      99,   660,     0,     0,     0,   660,     0,     0,     0,     0,
       0,     0,     0,    99,     0,     0,   433,     0,     0,     0,
       0,     0,     0,     0,     0,  1504,   118,     0,     0,     0,
     118,     0,     0,     0,     0,   118,   118,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,  1525,   118,     0,   433,     0,
       0,     0,  1529,     0,  1531,  1533,     0,     0,     0,     0,
       0,     0,     0,  1539,     0,  1540,     0,  1541,     0,     0,
       0,     0,     0,  1289,  1550,    58,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   118,     0,
       0,     0,   118,     0,     0,   660,     0,     0,     0,   150,
       0,   235,   236,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,    74,     0,    75,
       0,   459,     0,     0,     0,     0,     0,  1603,  1604,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   327,    77,
       0,     0,     0,     0,     0,     0,     0,   459,     0,    80,
      81,     0,     0,  1626,     0,   459,     0,     0,     0,   363,
    1631,     0,  1632,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   495,     0,   273,
     118,     0,     0,     0,    58,     0,     0,     0,  1648,     0,
       0,     0,   117,   112,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   459,     0,     0,     0,  1304,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
     235,   236,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,   118,     0,   459,   363,     0,   112,     0,   118,
    1289,     0,     0,     0,     0,     0,    74,     0,    75,     0,
     117,     0,     0,   660,   495,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,  1665,    77,     0,
       0,     0,     0,     0,     0,     0,     0,   660,    80,    81,
       0,     0,     0,     0,  1756,     0,     0,     0,     0,     0,
     660,  1760,     0,  1762,     0,     0,   284,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,   118,   118,     0,
     118,   150,     0,   235,   236,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,   118,     0,     0,     0,   118,
     117,     0,     0,   118,     0,     0,     0,     0,     0,    74,
     117,     0,     0,     0,     0,  1645,     0,     0,     0,    99,
       0,     0,   118,   118,   118,   118,   118,   118,   118,   367,
     327,    77,     0,     0,   273,     0,     0,     0,     0,  1798,
       0,    80,    81,     0,     0,     0,     0,   459,     0,     0,
       0,   459,   459,   495,     0,     0,     0,     0,     0,     0,
       0,     0,   459,     0,     0,   459,     0,     0,   497,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,   363,
       0,     0,     0,   273,     0,     0,     0,   660,   495,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   117,     0,
     459,     0,     0,     0,     0,   118,     0,   363,     0,   363,
       0,     0,     0,   363,   363,   495,   363,     0,  1767,     0,
       0,     0,     0,     0,     0,   118,     0,   117,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,   118,
    1895,  1896,     0,   117,     0,     0,    99,   284,   118,     0,
       0,     0,   118,     0,  1904,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,    99,   112,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,   662,
       0,     0,   284,     0,     0,     0,     0,   660,     0,     0,
     282,     0,     0,   459,     0,   662,     0,    99,     0,   662,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   273,   118,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
     495,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   662,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,   172,     0,   363,     0,     0,     0,     0,
     363,   363,     0,     0,   363,     0,   172,     0,   172,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   172,
       0,   389,     0,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,   363,     0,  1960,     0,
       0,   459,     0,     0,     0,     0,   389,     0,     0,   363,
       0,   497,     0,   363,     0,     0,     0,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,     0,     0,
    2104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   172,     0,     0,
       0,   172,     0,     0,   172,   172,   112,     0,   172,     0,
       0,   172,   172,     0,   172,     0,   172,     0,     0,   367,
       0,   117,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2140,     0,   662,   497,     0,
     112,     0,     0,     0,     0,     0,  2030,     0,   367,     0,
       0,  1960,     0,     0,     0,     0,  2155,     0,     0,     0,
       0,   662,     0,     0,     0,   282,     0,   118,     0,     0,
       0,  2164,     0,     0,   662,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   172,     0,   660,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,   172,     0,   363,     0,   495,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,     0,     0,     0,     0,     0,   118,   118,
       0,     0,   273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   497,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,     0,     0,     0,   363,
       0,     0,   363,   363,     0,   363,     0,     0,   149,   367,
       0,     0,     0,   367,     0,     0,     0,     0,     0,     0,
     363,   662,   497,     0,   363,     0,   367,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   367,     0,   367,     0,   172,     0,   367,   367,   497,
     367,     0,     0,  2008,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,     0,   112,     0,     0,     0,   112,   112,     0,     0,
       0,     0,     0,     0,   207,     0,     0,     0,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,     0,     0,   405,   389,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,     0,     0,     0,     0,   409,   367,     0,     0,
       0,   117,     0,     0,     0,   495,   367,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   662,     0,     0,   284,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,   389,     0,   363,   421,     0,
       0,    78,   422,   207,   497,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   172,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   172,     0,   172,     0,     0,
     580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   367,     0,     0,   282,   367,
       0,     0,     0,     0,   367,   367,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   367,     0,
       0,   622,     0,     0,     0,   367,     0,     0,     0,     0,
       0,     0,     0,   629,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     367,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   657,   367,     0,     0,     0,   367,     0,     0,
       0,   367,     0,     0,     0,     0,   172,   172,     0,     0,
       0,     0,     0,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,     0,
     117,   172,   172,     0,   172,     0,   172,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   745,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,   786,   172,
       0,     0,     0,   172,     0,     0,     0,   172,     0,   284,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     484,   374,     0,     0,     0,     0,   826,     0,     0,     0,
       0,   831,     0,   662,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   857,   858,     0,     0,   556,   859,   860,     0,     0,
     863,   367,   556,   497,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   876,     0,     0,     0,
       0,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   905,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   660,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   367,     0,     0,   367,   367,     0,   367,
       0,     0,     0,     0,   556,     0,     0,     0,     0,     0,
       0,     0,     0,   112,   367,     0,     0,     0,   367,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   672,     0,     0,   945,     0,     0,     0,
       0,     0,     0,   112,   660,     0,     0,     0,     0,   951,
       0,     0,   693,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,   117,     0,     0,     0,
     117,   117,     0,   970,   112,     0,     0,     0,     0,     0,
       0,     0,     0,   172,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   170,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,   497,
     172,     0,   556,   172,   367,     0,   172,     0,  1023,     0,
     172,     0,     0,     0,     0,     0,     0,     0,     0,   556,
     821,     0,   556,   824,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,   672,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   484,
       0,     0,     0,     0,     0,     0,     0,   302,   367,     0,
       0,     0,     0,     0,     0,     0,     0,   367,     0,     0,
     308,   367,   309,     0,     0,     0,     0,     0,     0,     0,
     556,     0,     0,     0,   556,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1103,     0,  1104,   381,     0,     0,     0,     0,   831,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1148,     0,     0,     0,
       0,     0,     0,     0,     0,  1157,     0,     0,   172,  1159,
       0,     0,   284,     0,     0,     0,  1878,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1477,     0,     0,     0,     0,   558,   559,
     556,     0,   563,     0,     0,   566,   567,     0,   569,     0,
     570,     0,     0,     0,   657,     0,     0,     0,     0,  1201,
     960,   374,     0,     0,   404,     0,     0,   405,     0,   406,
     407,   672,   408,     0,     0,   672,     0,   172,     0,     0,
       0,     0,   978,     0,   374,     0,     0,  1218,   172,   409,
    1220,   172,  1221,   172,   172,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,  1230,  1231,  1232,  1233,  -349,  -349,  1234,
    1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,   410,
     411,     0,   513,   413,  1242,  1243,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,  1244,   416,   417,
     418,     0,   419,   420,     0,     0,  1332,     0,     0,   655,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   687,     0,     0,     0,     0,     0,
       0,  1245,     0,     0,    78,   422,     0,     0,     0,   306,
     117,   423,    80,    81,   424,   425,   426,   427,     0,     0,
     374,     0,     0,     0,     0,     0,  -192,     0,     0,     0,
       0,     0,   172,     0,     0,     0,   556,   556,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   556,  1097,     0,
     556,  1100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   960,   374,     0,     0,     0,   672,
       0,   672,   672,     0,     0,     0,     0,     0,   672,     0,
       0,     0,     0,     0,   374,     0,   374,     0,     0,     0,
     374,   374,   374,   374,     0,     0,     0,     0,     0,   819,
       0,     0,     0,     0,     0,   216,     0,     0,     0,     0,
     374,     0,   556,     0,     0,     0,   556,     0,     0,     0,
       0,   277,     0,   556,  1175,     0,   662,   556,  1179,     0,
       0,   556,  1183,     0,     0,   172,     0,     0,  1186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   149,   172,     0,     0,     0,   117,     0,     0,
       0,     0,   216,     0,     0,   902,   338,     0,     0,   374,
     556,     0,     0,     0,     0,     0,     0,     0,   379,     0,
       0,     0,     0,     0,     0,   172,     0,   117,   662,     0,
       0,   172,     0,     0,     0,     0,     0,   672,     0,     0,
       0,   786,     0,   216,     0,     0,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   504,   117,     0,
       0,   510,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   484,   374,     0,     0,
       0,     0,     0,     0,     0,  1538,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,     0,     0,  1563,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   976,
     977,   277,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   986,
       0,   988,   556,   172,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   374,     0,
       0,   672,   672,     0,     0,     0,   510,     0,   672,   172,
     172,     0,     0,     0,     0,     0,   216,   389,     0,     0,
       0,     0,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   665,     0,
     682,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,     0,   556,  1426,     0,
     556,  1430,     0,     0,   556,  1434,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1090,  1091,     0,     0,     0,     0,     0,  1095,     0,     0,
       0,   743,     0,     0,     0,     0,     0,   297,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,     0,   172,
       0,     0,  1118,     0,     0,  1121,  1122,     0,  1125,     0,
    1127,  1128,     0,     0,     0,   216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,  1750,     0,     0,     0,     0,     0,
       0,   370,     0,     0,     0,     0,     0,     0,   665,     0,
       0,     0,     0,  1173,   849,     0,     0,  1177,     0,     0,
       0,  1181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   172,     0,     0,     0,     0,   374,     0,     0,     0,
     370,     0,   672,  1546,     0,     0,     0,   216,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   369,     0,   216,   216,  1313,     0,     0,     0,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   556,  1600,  1750,     0,     0,     0,     0,     0,     0,
       0,     0,   556,  1609,     0,   672,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   172,   374,     0,   370,   374,
     374,     0,   374,     0,   369,     0,   369,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,   369,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   504,     0,   964,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,   370,   370,     0,     0,     0,     0,     0,
     665,     0,     0,     0,     0,     0,     0,   370,     0,     0,
       0,   370,     0,     0,     0,     0,  1891,  1892,     0,     0,
       0,     0,   216,     0,     0,     0,     0,     0,     0,     0,
     216,     0,     0,   743,     0,   743,   216,  1313,   216,     0,
       0,     0,     0,     0,     0,     0,     0,   743,     0,     0,
     743,   743,   743,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,   369,     0,     0,     0,     0,     0,
     369,     0,  1419,     0,  1421,     0,     0,  1424,     0,     0,
    1428,     0,     0,     0,  1432,     0,     0,     0,     0,   672,
       0,     0,     0,     0,     0,     0,   504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,     0,     0,     0,     0,   370,     0,     0,
     216,     0,     0,     0,     0,     0,     0,     0,     0,  1975,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   504,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,     0,  1750,
     504,     0,   504,     0,     0,     0,   504,   504,   379,   504,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   556,
       0,     0,     0,     0,     0,   370,   504,     0,     0,     0,
       0,   369,     0,     0,     0,   556,  2016,     0,     0,     0,
       0,     0,     0,   370,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,     0,     0,
       0,     0,  1544,     0,     0,  2046,     0,     0,     0,  2047,
       0,     0,   369,   369,     0,   369,     0,     0,   370,     0,
       0,     0,     0,   369,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   504,   369,     0,     0,   369,
       0,   370,   216,     0,     0,     0,   369,     0,     0,   369,
       0,     0,   849,     0,     0,     0,     0,     0,     0,   370,
     370,     0,   370,     0,     0,     0,     0,     0,     0,     0,
     370,  1598,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1607,   370,     0,  1611,   370,  1614,  1615,     0,
       0,     0,     0,   370,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   379,     0,     0,   556,   556,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   556,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,     0,   369,     0,
       0,     0,     0,     0,     0,     0,   369,   369,     0,   370,
       0,   369,     0,   504,   504,     0,  1731,     0,     0,     0,
       0,     0,     0,     0,     0,   370,   369,     0,   369,     0,
       0,     0,   369,   369,   369,   369,     0,     0,     0,     0,
       0,   370,     0,     0,     0,   370,     0,     0,     0,     0,
       0,   556,   369,   370,   370,     0,     0,     0,   370,   556,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   504,
       0,     0,     0,   370,     0,   370,     0,     0,     0,   370,
     370,   370,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,  1611,
     743,   369,     0,     0,     0,   556,  2068,     0,     0,   556,
       0,     0,   739,     0,     0,     0,   369,     0,   369,   369,
       0,     0,     0,     0,     0,     0,     0,  1800,     0,     0,
       0,     0,     0,     0,     0,   743,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,   370,     0,
       0,     0,     0,   556,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   370,     0,   370,   370,   277,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
       0,     0,     0,     0,     0,   216,     0,     0,     0,     0,
       0,     0,   665,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   556,
     556,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,   371,     0,     0,   370,   743,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1888,     0,     0,     0,     0,     0,     0,   556,     0,     0,
     369,     0,     0,     0,   369,     0,     0,     0,     0,   369,
     369,     0,   371,   369,   913,   915,     0,     0,     0,     0,
       0,     0,     0,   369,     0,     0,     0,     0,     0,     0,
     369,     0,     0,     0,     0,     0,     0,  1916,  1917,     0,
       0,     0,   504,     0,     0,   504,   504,   370,   379,     0,
       0,   370,     0,     0,     0,     0,   370,   370,     0,     0,
     370,     0,     0,  1931,  1932,   369,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,  1936,   370,   369,     0,
       0,     0,   369,     0,     0,     0,   369,     0,     0,     0,
     743,   743,   743,     0,     0,   743,   743,     0,     0,     0,
       0,     0,   510,     0,     0,     0,     0,     0,     0,     0,
     371,     0,   370,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,   370,
     216,     0,     0,   370,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   739,     0,     0,   739,     0,     0,     0,     0,   739,
       0,   277,     0,   371,     0,   371,   371,     0,   739,     0,
       0,     0,     0,  2005,     0,     0,     0,     0,   379,   371,
       0,     0,     0,   371,     0,     0,     0,   739,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,     0,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1076,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   369,     0,   369,     0,
       0,     0,     0,     0,     0,  2066,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   370,     0,   370,     0,     0,     0,     0,
       0,     0,     0,   371,     0,     0,     0,     0,     0,   371,
       0,   216,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   369,   369,     0,   369,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   369,
     277,     0,     0,   369,     0,     0,     0,   369,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,     0,     0,   370,   370,
       0,   370,     0,   178,   181,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
     370,     0,     0,     0,   370,   371,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     229,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   743,     0,
     371,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   371,   369,     0,     0,     0,     0,   369,
       0,   322,     0,     0,   323,     0,     0,     0,     0,     0,
       0,   371,   371,     0,   371,     0,     0,     0,     0,   348,
       0,     0,   371,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   277,     0,     0,   371,     0,     0,   371,   397,
       0,   370,     0,     0,     0,   371,   370,     0,   371,     0,
       0,   397,     0,   369,     0,     0,   457,     0,     0,     0,
       0,     0,   369,     0,     0,     0,   369,     0,     0,     0,
     489,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   528,   519,     0,   519,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     370,     0,     0,     0,     0,     0,     0,     0,     0,   370,
       0,     0,     0,   370,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   229,     0,     0,     0,     0,
       0,     0,     0,     0,   589,   590,     0,     0,     0,     0,
       0,   371,     0,     0,     0,   178,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
     178,     0,     0,     0,     0,   743,     0,     0,     0,     0,
       0,     0,     0,   371,     0,     0,     0,   371,     0,     0,
       0,     0,     0,     0,     0,   371,   371,     0,   634,     0,
     371,     0,     0,     0,     0,   639,     0,     0,     0,     0,
       0,     0,     0,   643,   645,   371,     0,   371,   652,     0,
       0,   371,   371,   371,   371,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   371,     0,     0,     0,     0,   739,   743,     0,     0,
     510,     0,     0,     0,     0,     0,     0,   348,     0,     0,
     348,     0,     0,   397,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,   371,     0,     0,    46,     0,    47,     0,     0,     0,
     371,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,   371,    58,   371,   371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   229,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
     854,   855,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,   519,
      75,     0,     0,     0,  1453,   519,  1454,     0,     0,     0,
     457,  1455,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,    78,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,   369,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,   371,
       0,     0,     0,   371,    58,  1456,     0,     0,   371,   371,
       0,     0,   371,     0,     0,     0,  1684,  1692,     0,     0,
    1684,  1702,   371,     0,     0,     0,  1709,     0,   370,   371,
    1713,     0,  1715,     0,  1702,     0,     0,    61,     0,     0,
      63,    64,   944,     0,     0,     0,     0,     0,     0,   949,
       0,     0,     0,   369,     0,     0,   369,     0,   348,     0,
       0,     0,     0,     0,   371,     0,    74,     0,    75,     0,
     489,   369,     0,     0,     0,     0,     0,   371,     0,     0,
       0,   371,     0,   972,     0,   371,  1457,     0,     0,     0,
      78,  1011,     0,     0,     0,     0,     0,     0,    80,    81,
     370,     0,     0,   370,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   397,     0,     0,     0,     0,   370,     0,
    1005,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1016,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1037,  1039,     0,
       0,  1041,     0,  1043,  1051,     0,     0,     0,     0,  1005,
       0,  1053,  1005,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1804,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1081,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
       0,   371,  1083,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1092,     0,     0,     0,  1841,     0,     0,
       0,     0,     0,     0,     0,   371,     0,   371,     0,   489,
       0,     0,     0,     0,  1081,     0,  1860,  1862,     0,     0,
       0,     0,     0,     0,     0,     0,  1131,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1147,     0,     0,
       0,     0,     0,     0,     0,     0,  1151,  1882,     0,   519,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1162,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   371,     0,     0,
     371,   371,     0,   371,     0,     0,     0,     0,  1187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,   371,     0,     0,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1215,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,     0,     0,     0,  1311,
       0,     0,     0,     0,     0,  1303,  1305,    14,    15,    16,
      17,    18,     0,   489,     0,  1946,     0,     0,     0,     0,
       0,     0,  1949,     0,  1951,     0,     0,  1956,  1962,     0,
    1702,  1322,     0,     0,     0,  1968,     0,     0,     0,     0,
       0,     0,     0,   404,     0,     0,   405,     0,   406,   407,
       0,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1081,    58,   409,     0,
       0,     0,     0,   371,  1346,     0,     0,     0,   371,     0,
       0,     0,     0,  1005,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,   519,     0,     0,     0,    74,
    2034,    75,   371,     0,     0,     0,  2040,  2042,     0,     0,
       0,   371,     0,     0,     0,   371,     0,     0,     0,     0,
     421,     0,     0,    78,   422,     0,  2061,     0,     0,     0,
     423,   487,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2083,     0,  2086,   519,     0,
    1418,     0,  2088,  2090,   264,     0,     0,  2093,  2095,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -497,  -497,
       0,  -497,    46,     0,    47,     0,     0,  -497,     0,     0,
       0,     0,  1482,  1484,  1486,     0,     0,     0,     0,  2134,
    2136,  2138,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,  1495,  1495,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1508,     0,     0,     0,
    2159,  2161,  2163,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,  1215,     0,     0,
       0,     0,     0,  1527,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1542,
       0,     0,     0,     0,  2174,  1551,     0,     0,     0,     0,
      78,   507,     0,     0,     0,     0,     0,     0,    80,    81,
    1005,  1477,     0,     0,     0,   489,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   519,     0,     0,  1579,     0,     0,     0,
       0,     0,   404,     0,     0,   405,     0,   406,   407,     0,
     408,  1037,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1218,     0,   409,  1220,     0,
    1221,  -252,  -252,  1222,  1223,  1224,  1225,  1226,  1227,  1228,
    1229,  1230,  1231,  1232,  1233,  -349,  -349,  1234,  1235,  1236,
    1237,  1238,  1239,  1240,     0,  1241,     0,   410,   411,     0,
     513,   413,  1242,  1243,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,  1244,   416,   417,   418,     0,
     419,   420,     0,     0,     0,  1642,  1643,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1677,  1678,     0,     0,     0,     0,  -252,  1245,
     371,  1005,    78,   422,     0,     0,     0,   306,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
     519,     0,     0,   457,  -192,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1477,     0,     0,     0,     0,     0,     0,
       0,  1039,   371,     0,     0,   371,     0,     0,     0,     0,
    1758,  1759,     0,     0,  1768,     0,     0,     0,     0,     0,
     371,     0,     0,     0,   404,     0,     0,   405,     0,   406,
     407,     0,   408,     0,     0,     0,     0,     0,     0,   519,
       0,     0,     0,  1037,     0,     0,     0,  1218,     0,   409,
    1220,     0,  1221,  -253,  -253,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1229,  1230,  1231,  1232,  1233,  -349,  -349,  1234,
    1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,   410,
     411,     0,   513,   413,  1242,  1243,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,  1244,   416,   417,
     418,     0,   419,   420,     0,     0,     0,     0,   404,     0,
      74,   405,     0,   406,   407,     0,   408,  1954,     0,     0,
       0,     0,     0,     0,   457,     0,     0,     0,     0,  1829,
    -253,  1245,  1838,   409,    78,   422,     0,     0,     0,   306,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,  -192,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,  1875,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1696,
    1697,  1698,     0,     0,     0,   421,  1955,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,   519,     0,     0,     0,     0,     0,     0,  1906,
       0,     0,  1908,     0,     0,     0,     0,     0,     0,     0,
       4,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1217,     0,    20,  1922,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1218,
      58,  1219,  1220,     0,  1221,     0,     0,  1222,  1223,  1224,
    1225,  1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -349,
    -349,  1234,  1235,  1236,  1237,  1238,  1239,  1240,  1990,  1241,
       0,   410,   411,    61,   513,   413,  1242,  1243,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,  1244,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -3,  1245,     0,     0,    78,  1246,     0,     0,
       0,   306,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,  -192,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1217,  1005,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,   407,     0,   408,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1218,    58,
    1219,  1220,     0,  1221,     0,     0,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,  -349,  -349,
    1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,
     410,   411,    61,   513,   413,  1242,  1243,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,  1244,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1245,     0,     0,    78,  1246,     0,     0,     0,
     306,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,  -192,     4,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
     407,     0,   408,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,    61,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,     0,   419,   420,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1696,  1697,  1698,     0,     0,
       0,   421,  1699,  1700,    78,  1246,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,  1701,     4,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   404,     0,    46,   405,    47,   406,   407,
       0,   408,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
      61,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
       0,   419,   420,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1696,  1697,  1698,     0,     0,     0,
     421,  1699,     0,    78,  1246,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,  1701,   186,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   259,
     260,     0,   261,    46,     0,    47,     0,     0,   262,     0,
       0,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   404,
       0,    46,   405,    47,   406,   407,     0,   408,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   409,     0,     0,     0,     0,     0,
    -472,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -472,   410,   411,    61,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   421,     0,  1694,    78,
    1246,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   404,
       0,    46,   405,    47,   406,   407,     0,   408,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,    61,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,     0,   419,   420,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   421,     0,     0,    78,
    1246,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,   407,     0,   408,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   486,
       0,     0,     0,     0,     0,   423,   487,    81,   424,   425,
     426,   427,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   404,     0,    46,
     405,    47,   406,   407,     0,   408,   355,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,     0,   419,   420,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,     0,    78,  1300,     0,
       0,     0,     0,     0,   423,  1301,    81,   424,   425,   426,
     427,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   404,     0,    46,   405,
      47,   406,   407,     0,   408,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   409,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,     0,     0,    78,   833,     0,     0,
       0,     0,     0,   423,   487,    81,   424,   425,   426,   427,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,   407,     0,   408,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   421,     0,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   404,     0,    46,   405,    47,   406,
     407,     0,   408,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,     0,   419,   420,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   421,     0,     0,    78,   833,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,  2015,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,  2045,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,  1548,    -2,     0,
       0,     0,     0,    -2,    -2,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,   404,     0,     0,   405,     0,   406,   407,     0,   408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,    -2,     0,     0,    58,   409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,     0,   419,
     420,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   421,     0,
       0,    78,   422,     0,     0,     0,     0,     0,   423,  1549,
      81,   424,   425,   426,   427,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,    59,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    61,    62,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,    77,
       0,    78,    79,     0,     0,     0,     0,     0,     0,    80,
      81,   264,   186,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -497,  -497,     0,  -497,    46,
       0,    47,     0,     0,  -497,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   150,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,    78,   265,     0,
       0,     0,  -824,     0,     0,    80,    81,   264,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -497,  -497,     0,  -497,    46,     0,    47,     0,     0,
    -497,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   265,     0,     0,     0,     0,     0,
       0,    80,    81,     4,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -416,  -416,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -416,     0,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,    80,    81,     4,
     186,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -417,  -417,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -417,     0,     0,     0,    78,    79,     0,  1453,     0,
    1454,     0,     0,    80,    81,  1455,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1456,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1651,     0,     0,     0,    78,  1011,     0,  1453,     0,  1454,
       0,     0,    80,    81,  1455,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1652,
       0,     0,     0,    78,  1011,     0,  1453,     0,  1454,     0,
       0,    80,    81,  1455,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1456,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1653,     0,
       0,     0,    78,  1011,     0,     0,     0,     0,     0,     0,
      80,    81,   264,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -497,  -497,     0,  -497,
      46,     0,    47,     0,     0,  -497,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   265,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   613,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1107,    77,  -690,    78,   668,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -497,  -497,     0,  -497,
      46,     0,    47,     0,     0,  -497,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,   265,
       0,     0,     0,  -828,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -497,  -497,     0,  -497,    46,     0,    47,     0,     0,
    -497,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,    77,     0,    78,   265,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,   613,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   667,     0,  -690,    78,   668,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,   613,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     802,     0,  -690,    78,   553,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,  1140,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -698,    78,   919,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   356,    78,   357,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,  1621,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   919,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,  1623,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   919,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   507,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,     0,   355,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    78,   919,     0,     0,     0,     0,     0,
       0,    80,    81,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,   355,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,   357,
       0,     0,     0,     0,     0,     0,    80,    81,   186,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -497,  -497,     0,  -497,    46,     0,    47,     0,     0,
    -497,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    58,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -496,  -496,     0,  -496,    46,     0,    47,
       0,     0,  -496,    63,    64,     0,     0,     0,     0,  1477,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
     404,     0,     0,   405,     0,   406,   407,     0,   408,     0,
       0,     0,     0,    78,   265,     0,     0,     0,     0,     0,
       0,    80,    81,  1218,     0,   409,  1220,     0,  1221,  1939,
    1940,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,  1230,
    1231,  1232,  1233,    75,     0,  1234,  1235,  1236,  1237,  1238,
    1239,  1240,     0,  1241,     0,   410,   411,     0,   513,   413,
    1242,  1243,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,  1244,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,  1477,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1245,     0,     0,
      78,   422,     0,     0,     0,   306,     0,   423,    80,    81,
     424,   425,   426,   427,     0,   404,     0,     0,   405,     0,
     406,   407,  -192,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1218,     0,
     409,  1220,     0,  1221,     0,     0,  1222,  1223,  1224,  1225,
    1226,  1227,  1228,  1229,  1230,  1231,  1232,  1233,     0,     0,
    1234,  1235,  1236,  1237,  1238,  1239,  1240,     0,  1241,     0,
     410,   411,     0,   513,   413,  1242,  1243,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,  1244,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1245,     0,     0,    78,   422,     0,     0,     0,
     306,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,  -192,   310,   186,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -420,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,  -420,
     310,   186,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -421,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,  -421,   310,   186,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,    14,    15,    16,    17,    18,    19,
     730,    20,   731,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    63,    64,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   732,     0,     0,
       0,     0,  1233,     0,  -349,     0,     0,     0,    78,     0,
       0,     0,     0,  -420,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1245,     0,     0,
      78,   733,     0,     0,     0,   306,     0,   423,    80,    81,
     734,   735,   426,   427,    14,    15,    16,    17,    18,    19,
     730,    20,   731,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   732,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   733,     0,     0,     0,   306,     0,   423,    80,    81,
     734,   735,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,   452,
      78,   453,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   453,     0,     0,     0,   306,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   733,     0,     0,     0,   306,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   453,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   833,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     404,     0,    46,   405,    47,   406,   407,     0,   408,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,   264,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,    58,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -497,
    -497,     0,  -497,    46,     0,    47,     0,   721,  -497,   722,
     723,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,   -17,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   150,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   613,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -690,    78,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,    79,     0,     0,
       0,  -826,     0,     0,    80,    81,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     150,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,    78,   208,     0,     0,     0,     0,     0,     0,
      80,    81,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,    78,    79,
       0,     0,     0,     0,     0,     0,    80,    81,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   150,     0,   480,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   856,     0,     0,    78,   481,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,    80,    81,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,   480,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,   481,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
      14,    15,    16,    17,    18,    58,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -497,  -497,     0,  -497,    46,     0,
      47,    63,    64,  -497,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     613,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -690,    78,     0,     0,   150,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    77,     0,    78,   507,     0,     0,
       0,     0,     0,     0,    80,    81,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
     355,    49,    50,    51,    52,    53,    54,    55,     0,     0,
      14,    15,    16,    17,    18,    58,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -497,  -497,     0,  -497,    46,     0,
      47,    63,    64,  -497,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,    75,
    1211,     0,     0,     0,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    75,     0,     0,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     856,     0,     0,    78,   481,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   856,     0,     0,    78,
     481,     0,     0,    63,    64,     0,     0,    80,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1018,    78,  1011,     0,     0,     0,     0,     0,
       0,    80,    81,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,  1567,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
    1011,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   318,     0,     0,    63,
      64,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     208,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,    63,    64,     0,   355,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   357,     0,     0,    63,
      64,     0,     0,    80,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    78,
     318,     0,     0,     0,     0,     0,     0,    80,    81,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,     0,   355,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   481,     0,     0,     0,
       0,     0,     0,    80,    81,   186,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -497,  -497,
       0,  -497,    46,     0,    47,     0,     0,  -497,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   507,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,  1011,     0,     0,    63,    64,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
      63,    64,     0,   355,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,   481,     0,     0,    63,    64,     0,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,  1011,     0,     0,     0,     0,
       0,     0,    80,    81,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,   355,
      49,    50,    51,    52,    53,    54,    55,     0,     0,    14,
      15,    16,    17,    18,    58,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -497,  -497,     0,  -497,    46,     0,    47,
      63,    64,  -497,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    74,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,     0,     0,     0,     0,    63,    64,     0,    80,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,   335,     0,    14,    15,
      16,    17,    18,    80,    81,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -497,  -497,     0,  -497,    46,     0,    47,     0,
       0,  -497,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -497,  -497,     0,  -497,    46,     0,
      47,     0,     0,  -497,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    78,   507,    63,    64,     0,     0,
       0,     0,    80,    81,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,     0,     0,     0,    80,    81,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   404,     0,    46,   405,    47,
     406,   407,     0,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,     0,   419,   420,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   421,     0,     0,    78,   422,     0,     0,     0,
       0,     0,   423,   487,    81,   424,   425,   426,   427,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   404,     0,
      46,   405,    47,   406,   407,     0,   408,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,    14,    15,    16,
      17,    18,    58,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,   150,    47,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    78,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,    75,     0,     0,     0,   355,    49,    50,    51,    52,
      53,    54,    55,     0,     0,    14,    15,    16,    17,    18,
      58,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -497,
    -497,     0,  -497,    46,     0,    47,    63,    64,  -497,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,   186,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,    78,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,   186,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,    58,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,   187,   404,   188,
     189,   405,     0,   406,   407,     0,   408,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   721,
       0,   722,   723,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,     0,   419,   420,     0,    75,
       0,     0,   404,     0,    74,   405,     0,   406,   407,     0,
     408,     0,     0,     0,     0,     0,     0,     0,     0,  1696,
    1697,  1698,     0,     0,     0,   421,  1861,   409,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     513,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,   421,
      77,     0,   514,   515,     0,     0,     0,   516,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,   421,  1349,     0,    78,
     422,     0,     0,     0,  1350,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   421,     0,     0,    78,   422,     0,     0,
       0,   516,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
     421,  1001,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,   421,  1036,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,   421,     0,     0,    78,   422,     0,
       0,     0,   306,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,   421,     0,     0,    78,   422,     0,     0,  1075,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,   421,     0,
       0,    78,   422,     0,     0,     0,  1487,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,   421,  1578,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,   421,     0,     0,    78,   422,     0,     0,     0,
    1769,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,   421,
       0,  1945,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,   421,  1950,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,     0,   419,   420,     0,     0,     0,     0,
     404,     0,    74,   405,     0,   406,   407,     0,   408,  1954,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,  1961,   409,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,   421,     0,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,   421,  2039,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,   421,  2041,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,   421,  2085,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,   421,  2087,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,   421,  2089,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,   421,
    2092,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,   421,  2094,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   421,  2133,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
     421,  2135,     0,    78,   422,     0,     0,     0,     0,     0,
     423,    80,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,   404,   419,   420,
     405,     0,   406,   407,     0,   408,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,     0,     0,     0,   421,  2137,     0,
      78,   422,     0,     0,     0,     0,     0,   423,    80,    81,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   411,     0,   412,   413,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   414,   415,   401,
       0,   416,   417,   418,   404,   419,   420,   405,     0,   406,
     407,     0,   408,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,   421,  2158,     0,    78,   422,     0,
       0,     0,     0,     0,   423,    80,    81,   424,   425,   426,
     427,     0,     0,     0,     0,     0,     0,     0,     0,   410,
     411,     0,   412,   413,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   414,   415,   401,     0,   416,   417,
     418,   404,   419,   420,   405,     0,   406,   407,     0,   408,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,   421,  2160,     0,    78,   422,     0,     0,     0,     0,
       0,   423,    80,    81,   424,   425,   426,   427,     0,     0,
       0,     0,     0,     0,     0,     0,   410,   411,     0,   412,
     413,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,   414,   415,   401,     0,   416,   417,   418,   404,   419,
     420,   405,     0,   406,   407,     0,   408,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,   421,  2162,
       0,    78,   422,     0,     0,     0,     0,     0,   423,    80,
      81,   424,   425,   426,   427,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   411,     0,   412,   413,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   414,   415,
     401,     0,   416,   417,   418,   404,   419,   420,   405,     0,
     406,   407,     0,   408,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,   421,     0,     0,    78,   422,
       0,     0,     0,     0,     0,   423,    80,    81,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,     0,     0,
     410,   411,     0,   412,   413,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   414,   415,   401,     0,   416,
     417,   418,   404,   419,   420,   405,     0,   406,   407,     0,
     408,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,   710,     0,     0,    78,   422,     0,     0,     0,
       0,     0,   423,    80,    81,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   411,     0,
     412,   413,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   414,   415,   401,     0,   416,   417,   418,   404,
     419,   420,   405,     0,   406,   407,     0,   408,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,   713,
       0,     0,    78,   422,     0,     0,     0,     0,     0,   423,
      80,    81,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   411,     0,   412,   413,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   414,
     415,   401,     0,   416,   417,   418,   404,   419,   420,   405,
       0,   406,   407,     0,   408,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,     0,   718,     0,     0,    78,
     422,     0,     0,     0,     0,     0,   423,    80,    81,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,   410,   411,     0,   412,   413,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   414,   415,   401,     0,
     416,   417,   418,   404,   419,   420,   405,     0,   406,   407,
       0,   408,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   727,     0,     0,    78,   422,     0,     0,
       0,     0,     0,   423,    80,    81,   424,   425,   426,   427,
       0,     0,     0,     0,     0,     0,     0,     0,   410,   411,
       0,   412,   413,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   414,   415,   401,     0,   416,   417,   418,
     404,   419,   420,   405,     0,   406,   407,     0,   408,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
     421,     0,     0,    78,   422,     0,     0,     0,     0,     0,
     423,   943,    81,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,   410,   411,     0,   412,   413,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     414,   415,   401,     0,   416,   417,   418,     0,   419,   420,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,     0,     0,
      78,   422,     0,     0,     0,     0,     0,   423,   487,    81,
     424,   425,   426,   427
};

static const yytype_int16 yycheck[] =
{
       1,   180,   527,     4,    76,    76,   168,   516,   279,   263,
       4,   168,    98,   223,     1,   170,     1,   955,  1264,   421,
       1,    78,  1812,     1,   237,   680,     4,  1245,   143,   360,
      76,   168,  1296,  1297,   185,   667,   232,    76,   237,   667,
    1201,     1,   207,   170,  1245,   667,   237,    59,   139,     1,
     237,   207,   987,     0,   756,    56,    57,   759,    59,   850,
     995,   852,   258,   838,  1812,    90,   194,   928,    85,   844,
    1939,   101,    59,   423,    59,    76,   837,   671,    59,   586,
     247,    59,   352,    76,    85,   803,     1,   948,   164,     4,
     597,   206,    93,   940,   837,   737,  1368,    98,   940,    59,
     101,  1812,   835,    85,   105,   181,   374,    59,  1706,   276,
     378,   105,     1,   102,   327,   121,   237,   185,   237,   237,
     287,     1,   392,   101,   196,    85,    98,   105,   327,   835,
     184,   835,    76,    98,   641,   152,   327,   149,    98,   164,
     327,   101,    76,   144,    59,   105,   147,   835,   149,    10,
     196,   181,   238,   105,   155,   237,   247,   196,     1,     1,
     358,   162,   149,   494,   149,   237,   237,   156,   149,   170,
      59,   149,     0,   237,   156,    73,   374,    73,   156,    59,
     378,   182,   138,    91,     0,   276,   101,   139,   181,   149,
     105,   237,   150,   194,   195,   196,   287,   149,   237,   237,
     162,   358,   164,   835,  2073,   206,   327,   835,   327,   327,
     211,  1228,    76,   835,   172,   421,   172,    59,   573,   574,
     221,   312,   121,    76,   135,   226,   381,   673,   314,   207,
     231,   232,   233,   388,   149,   195,   237,   238,  1359,   137,
     161,   137,   328,   837,   301,   327,  1210,   181,   156,   266,
     139,   211,   196,  1050,   381,   327,   327,   258,  1856,   139,
     149,   160,   160,   327,  1813,   266,   238,   268,   396,   149,
    1943,   156,     1,   238,   156,     4,   277,   278,   238,   164,
     281,   327,  1079,  1702,   615,     1,   557,   288,   327,   327,
     488,   136,   164,   237,   565,   247,   126,   139,   180,   171,
    1210,   302,   303,   281,   305,   166,   266,   149,  1580,   310,
     171,   864,   522,   314,  1515,  2105,   227,  1518,  1519,   336,
     163,   281,   535,   164,   276,    93,   327,   328,   659,   159,
      59,   488,   196,  1108,   179,   287,   535,   338,  1040,  1210,
     181,  1202,   314,   196,   535,   346,   347,   702,   535,   314,
     351,  1148,   683,  1114,   314,   757,   328,  2105,   247,   690,
     312,   716,   712,   328,   316,   492,   281,   247,   328,   484,
    1088,  1114,  1219,   237,  1107,   571,   105,  1219,  1927,   973,
     381,   577,    90,  1318,   237,  1027,   156,   276,   656,   390,
     505,   162,   393,   839,  2105,   396,   276,   843,   287,   138,
     311,  1107,  1563,  1107,   156,   247,  1253,   287,   854,   855,
     181,  1253,    78,    79,   535,   580,   535,   535,   925,  1107,
     149,  1198,   635,   312,   580,   693,   113,   952,  1205,   596,
      20,   162,   312,   172,   276,    73,   635,    76,   455,    10,
     993,   172,  1861,  1862,   635,   287,   162,   163,   635,   136,
    2123,   153,    91,   535,   162,   810,   164,   514,   656,    78,
      79,   547,   164,   535,    85,   466,   164,  1588,  1589,  1590,
     312,   535,   240,   171,   242,  1107,   530,    98,   180,  1107,
     101,   249,   680,  2156,   105,  1107,  1450,  1451,  1452,   535,
     491,   492,   657,   109,   110,   693,   535,   535,   164,   137,
    2049,   657,   503,   504,   710,   596,   466,   713,  1285,   715,
     421,   162,   718,   514,   635,   516,   635,   635,   156,   162,
    1114,   727,  1740,   680,   730,   731,   732,    62,    63,   112,
     181,   162,    85,  1308,   535,   181,  1955,  1956,   181,  1740,
    1450,  1451,  1452,   698,   158,   164,   547,   857,   162,   859,
     181,   637,   135,   635,   322,   323,   156,  2106,   109,   110,
    2076,    59,   830,   635,    62,    63,   876,    65,   288,    76,
     571,   635,   156,  1082,   195,   547,   577,   138,   579,  1450,
    1451,  1452,   547,   303,   155,    92,  2102,   547,   158,   635,
     211,   535,   162,   162,  2143,   166,   635,   635,   155,   152,
     171,    76,   580,   113,   111,   162,   167,   168,   159,  2125,
     390,  1402,   181,   393,  1405,  1406,  1391,   238,    93,  1554,
       1,   786,   160,     4,   181,  1889,   136,   165,   640,   164,
     786,   962,   830,  1475,   635,   945,   637,  1479,  1480,   640,
     193,   642,   162,   156,   596,   266,   181,  1302,   158,   560,
     651,  1493,   158,   640,   655,   640,   866,   568,   156,   640,
     281,   181,   640,   169,   170,   637,   831,   109,   110,     1,
    1447,   535,   637,   162,    73,   831,   587,   637,    59,   657,
     640,   162,   535,   172,  1701,  1131,   687,   598,   640,  1706,
     158,   159,   960,   162,   850,    76,   858,   848,   131,   132,
     181,   858,   858,   162,    85,   706,   902,   596,   156,    73,
     837,   156,   181,   266,    73,   852,   596,    98,   162,   162,
     101,     3,   181,   162,   105,   640,    73,    59,   785,   172,
     784,    13,    14,    15,    16,    17,    73,   181,   137,   162,
    1986,   162,   181,   176,   177,   150,   151,   152,   749,   158,
     751,   640,   753,   162,   596,   162,   757,   156,   181,   760,
     640,   160,   960,  1316,   156,   172,  1468,   172,   149,   101,
    1323,   162,   164,   137,   155,   162,   181,   657,   137,   171,
     848,   172,   160,   336,   785,   172,  1583,   165,   162,   170,
     137,    73,   156,  1103,  1104,   863,   160,   156,   640,   710,
     137,   160,   713,    47,    48,   358,    50,   718,   786,   156,
    1748,   155,    56,   160,   195,   196,   727,   149,   162,   156,
     156,   589,  1375,   160,   167,   206,   181,   163,  1763,   830,
     211,   174,   175,   988,   835,   746,   837,   135,   158,  1856,
     221,  1350,   162,  1245,    73,   466,   162,   157,   849,  1159,
     231,   232,   233,   831,   164,   137,   237,   238,   156,   860,
     156,  1023,   160,   162,   160,   866,  1023,  1023,   869,   167,
     168,   639,   850,   158,   852,   160,  1147,   258,   856,   857,
     155,   859,   180,     3,   156,   266,   158,   162,    13,    14,
      15,    16,    17,     3,    73,   159,   160,    73,   876,   163,
     281,   902,   455,   150,   151,   152,   159,  1022,   137,   135,
     158,   640,   764,   765,   766,   163,    13,    14,    15,    16,
      17,   302,    70,   158,   305,   172,   547,   156,   163,   310,
     156,   160,   162,   314,   160,   488,  1953,   938,   939,   940,
    1295,   167,   168,   129,   130,  1155,   327,   328,    73,   281,
    1967,   831,   158,   940,    73,   940,   162,   951,   137,   940,
     156,   137,   940,   246,   156,  1490,   158,   945,   160,    73,
     253,  1565,   180,   951,   527,  1306,    73,   156,  1487,   164,
     156,   160,    73,   315,   160,   156,  1113,  1114,   940,  1786,
      73,  1788,   275,   994,   164,    59,   178,   908,    62,    63,
     381,    65,  1555,   286,    73,   169,   170,  1075,   158,   158,
     135,   922,   137,   163,   163,   926,  2033,  1859,   137,   930,
     573,   574,   158,   156,  1577,   940,   158,   163,   360,  1654,
     162,   156,   121,   137,  1659,   160,   951,   156,   135,  1245,
     137,   160,   167,   168,   158,  1046,   137,   158,   162,  1050,
     158,   940,   156,  1455,   137,   158,   160,  1311,   158,   156,
     940,   156,   156,   160,   158,   156,   160,  1576,   137,   160,
     167,   168,   156,   156,   158,   161,   160,   160,  1079,   158,
     158,  1082,   135,   162,   162,   466,    73,   156,   156,  1157,
     158,   160,   160,  1289,    22,    73,   158,    73,   940,   158,
     162,   156,   158,   156,  1302,   160,  1107,   160,    73,     3,
     156,   492,  1113,  1114,   167,   168,   156,  1296,  1297,    13,
      14,    15,    16,    17,   158,  1103,  1104,   680,   162,   162,
      73,  1337,   156,   514,   156,   516,  1661,   156,   160,    13,
      14,    15,    16,    17,   156,  1302,   162,  1148,   160,   702,
     137,   158,   158,   102,   535,   162,   162,   710,  1711,   137,
    1315,   137,   494,   716,   156,   718,   547,  1438,  1330,   156,
    1332,   156,   137,   160,  2016,  1332,  1332,   161,   156,    73,
     156,  1159,   160,   164,   160,    13,    14,    15,    16,    17,
     571,   156,    90,  1210,   137,   160,   577,   156,   579,    73,
     164,   160,   158,   155,  2046,   156,   162,  1201,   150,   151,
     152,   940,  1765,   156,   158,   135,   164,   160,   162,   181,
     162,  1222,   951,  1201,  1225,  1226,  1227,  1780,   133,   134,
     172,   161,  1219,  1234,  1219,  2077,   156,   158,  1219,   181,
     160,  1219,   164,   137,   171,    73,  1408,   167,   168,  1405,
     156,  1408,  1253,   156,   635,   180,   637,   810,  1259,   640,
    1769,  1412,  1413,   137,   138,  1402,  1253,  1219,  1253,  1406,
     158,  1408,  1253,  1274,   655,  1253,  1277,  1278,   561,  1280,
    1982,   161,   162,   615,  1278,  1286,  1201,   158,  1289,   121,
     158,   162,   156,  1204,   162,   156,   628,   156,   158,  1277,
    1278,  1253,   162,  1371,  1219,   588,   687,  1218,   640,   137,
     138,   158,   595,   161,   162,   162,   599,  1277,  1278,   162,
      13,    14,    15,    16,    17,   158,  1278,  1238,   173,   162,
    1219,   158,  1333,   158,  1245,   162,   158,   162,  1253,  1219,
     162,   161,   162,   158,  1412,  1413,  1899,   162,   166,  1350,
    1903,   683,   114,   115,   116,   117,   118,   124,  1359,   126,
     127,   128,  1277,  1278,  1253,   158,   168,   158,   749,   162,
     751,   162,   753,  1253,   158,  1461,  1538,  1219,   162,   760,
      73,  1538,  1538,   158,  1475,   159,   158,   162,  1479,   156,
     162,  1392,   159,   160,  1651,  1652,  1653,   164,   165,   952,
     150,   151,   152,  2110,   785,  1475,   135,  2114,  1559,  1479,
     178,  1253,   162,  1207,  1208,  1209,  1184,   771,   772,   773,
     774,   158,   172,   158,  1402,   167,   168,  1405,  1406,   161,
     162,   181,   158,  1450,  1451,  1452,   161,   162,  1455,  1456,
    1672,  1673,  1674,   160,   137,   161,   162,   162,   163,   830,
     161,   162,  1665,   158,   835,   158,   837,   161,   162,  1453,
    1461,    92,    93,   158,  1465,  1466,  1665,   161,   162,   161,
     162,   161,  1201,   156,  1665,  1453,   161,   162,  1665,   161,
     162,   156,    62,   161,   162,   866,  1487,   138,   869,  1461,
    1219,  1559,   161,   162,  1475,   162,  1461,  1475,  1479,  1480,
     138,  1479,  1480,    13,    14,    15,    16,    17,    18,   162,
    1511,  1512,  1493,   161,   162,  1493,   161,   162,   161,   162,
    1521,   902,   163,  1475,  1253,   162,   106,  1479,  1480,   161,
     162,   111,   162,   163,   114,   163,   116,   156,  1453,   161,
     162,  1493,   158,  1521,  1665,   180,  1665,  1665,   158,  1278,
      13,    14,    15,    16,    17,    18,  1557,   938,  1469,  1470,
    1475,  1521,   161,   162,  1479,  1480,    78,    79,  1654,  1563,
     162,   163,  1778,  1659,   158,  1576,  1360,  1361,  1493,   767,
     768,  1667,  1583,  1665,   158,  1563,  1475,  1588,  1589,  1590,
    1479,  1480,   769,   770,  1665,  1475,   158,   775,   776,  1479,
    1480,  1665,  1518,  1519,  1493,   158,  1521,   158,   940,  1520,
    1673,  1674,   158,  1493,  1827,    13,    14,    15,    16,    17,
      18,   409,   207,   158,   180,   161,   160,  1665,  1827,    71,
     962,   164,   164,  1475,   164,   164,  1827,  1479,  1480,   164,
    1827,   158,   162,   181,   927,   161,   434,   435,  1563,   156,
      79,  1493,   161,  1654,    18,   181,  1277,  1278,  1659,   180,
     164,   164,   158,   158,  1665,  1046,  1667,   455,   181,  1050,
     164,   164,   161,   161,  1675,  1729,    13,    14,    15,    16,
      17,   161,  1654,    18,   161,   155,   158,  1659,    22,  1654,
     270,   158,   158,  1694,  1659,  1667,  1892,  1691,  1079,  1700,
     488,  1082,  1667,   158,   158,   158,  1827,   158,  1827,  1827,
    1889,   158,  1690,  1691,   158,   158,   161,   158,   158,  2050,
     158,   155,   155,    71,  1453,   164,  1107,   164,   164,   164,
    1731,   158,  1113,  1114,   180,   158,    73,   317,   158,  1691,
     158,   158,  1295,   162,  1830,  1827,  1475,   158,   164,  1302,
    1479,  1480,   158,   158,   162,   158,  1750,   161,   158,   158,
     162,   158,     1,  1827,  1493,     4,   162,  1148,  1769,   158,
     158,    18,  1750,   158,   158,  1690,  1691,  1975,   158,   158,
     161,   158,   362,   161,   364,  1786,   366,  1788,  1939,  1827,
      13,    14,    15,    16,    17,    18,   158,   158,   135,   158,
     137,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    1721,    58,    59,    60,    61,    62,    63,    64,    65,   156,
      59,   161,  1876,   160,   162,   158,  1827,   158,   158,  1830,
     167,   168,   412,   158,  1563,  1750,   158,    76,  1839,  1840,
     158,   158,   158,   162,   155,  1846,    85,   162,   162,  2047,
    1936,   155,   162,  2049,   156,   156,   156,  1858,  1830,    98,
     156,   156,   101,    66,  1842,  1830,   105,  1868,   156,  1870,
     156,  1939,   156,    14,   163,   181,   163,   162,  1859,   161,
     161,  1859,  1883,   181,  1885,  1886,  1887,  1219,   181,   164,
    1891,  1892,  2105,  1274,   155,   155,  1277,  1278,   164,   162,
    1521,   180,   158,   181,   143,   158,  2105,  1859,  1289,   158,
     149,   161,   158,   152,  2105,   158,   155,   156,  2105,   161,
    2071,  1253,  2073,   158,   158,  1979,   162,  1842,   161,   168,
     158,   158,   158,   513,   158,  1936,   161,  1490,   155,   155,
     181,   156,  1943,   156,  1859,  1277,  1947,    81,    93,   181,
     181,  1952,  1333,  1954,   193,   155,   195,   196,  1975,   181,
     181,  2112,  1691,   181,  1936,   156,   156,   206,   207,  1350,
    1859,  1936,   211,   181,  1306,   763,    91,   158,  1359,  1859,
     155,   155,   162,   162,  2105,   161,  2105,  2105,   161,   161,
     161,   155,   155,   232,   164,   580,   158,   163,   237,   238,
     124,   155,   163,  2071,   161,  2073,  2007,   158,   158,   158,
     158,  1392,   158,   593,   161,   158,   158,  1859,  2019,   258,
    2106,  1750,  2023,  2105,   155,   155,   181,   266,   163,   158,
    2047,   156,  2183,  2105,  2105,  2016,   162,  2038,  2016,   158,
     156,  2105,   281,   156,  2112,    76,   158,   161,  2049,   155,
    2051,   161,   161,   158,   155,   158,  2110,  2143,   158,  2105,
    2114,  2115,   158,   161,  2016,  2046,  2105,  2105,  2046,    76,
     181,   156,   657,  2141,   181,   314,   155,   181,   158,  2080,
    1461,   320,   156,   161,   161,   155,   155,   326,   327,   328,
     155,  2145,   158,   160,  2046,   158,  2077,   336,   158,  2077,
      76,  2016,    76,    76,  2105,  2106,  1487,   163,  1661,   181,
     172,   172,   305,   181,  2168,  2183,   155,  2118,  2172,   358,
     359,   360,  2123,   181,   157,  2077,   155,  2016,   155,   172,
    1859,  2046,   172,  2187,  2106,   374,  2016,   155,   163,   378,
    1521,  2106,  2143,  1475,   107,   156,  2106,  1479,  1480,   162,
    2151,   172,   172,  2154,   157,  2156,   161,  2046,   181,   181,
      76,  1493,  2077,   158,   248,   157,  2046,   158,   158,   163,
     155,  2143,   207,   155,  2016,   158,  2177,   156,  2143,   181,
     158,   181,   421,  2143,   181,  1778,  1327,  2188,  2077,  1521,
     777,   736,   778,   781,  2073,  1576,  2197,  2077,   779,    73,
     454,   786,  1583,   780,  2046,  1240,  1253,  1588,  1589,  1590,
    2156,  1479,  1867,  1493,  2102,  1859,   455,  1959,  2062,   458,
    1739,  2140,  1722,  1722,  2047,  2115,  2046,   466,  1280,  2172,
    1018,    49,   114,   107,   272,  2077,  1024,   111,   112,   113,
     114,   115,   116,   117,   118,   484,   831,  1035,  1936,   488,
    2005,  1456,  1768,   492,  1320,   494,  2167,  1274,   866,   523,
     651,   135,  1527,   137,  1750,   850,   505,   852,  2179,     0,
    1643,   856,   857,  1654,   859,   706,    -1,    -1,  1659,    13,
     802,    -1,   156,   157,  1665,    -1,  1667,  2016,   527,    -1,
      -1,   876,    -1,   167,   168,   107,   535,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   547,    -1,
     802,   802,    -1,    -1,    -1,   221,    -1,  2046,    -1,    -1,
      -1,    -1,    -1,   358,    -1,    -1,   361,    -1,    -1,    -1,
      -1,    -1,   571,    -1,   573,   574,    -1,    -1,   577,   374,
      -1,   580,    -1,   378,    -1,    -1,    -1,   159,  2077,    -1,
      -1,   936,    -1,    -1,    -1,    89,    -1,   107,  1690,   171,
     945,   111,   112,   113,   114,   115,   116,   117,   118,   562,
      -1,    -1,    -1,   107,   458,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,  1769,    -1,
      -1,   475,    -1,    -1,   478,    -1,   635,    -1,   637,    -1,
      -1,   640,    13,    -1,   107,  1786,    -1,  1788,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   656,   657,    -1,
     659,    -1,   156,    -1,    -1,    -1,    -1,    -1,   667,    -1,
      -1,  1219,   671,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,   680,    -1,    -1,    -1,   638,  1827,    -1,    -1,  1830,
      -1,   690,    -1,   488,   693,    -1,   540,   160,    -1,    -1,
      -1,    -1,    -1,   702,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   710,    -1,    -1,   713,    -1,   715,   716,    89,   718,
      -1,    62,    63,    64,    65,    -1,    -1,    -1,   727,    -1,
      -1,   730,   731,   732,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
    1842,  1892,    -1,    -1,  1302,    -1,    -1,    -1,  1103,  1104,
      -1,    -1,    -1,    -1,    -1,    -1,   107,  1859,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   580,    -1,   786,    -1,    -1,
      62,    -1,  1340,  1341,  1342,  1936,    -1,    -1,    -1,  1347,
    1348,    -1,    -1,   802,   803,    -1,    -1,    -1,    -1,    -1,
      -1,   810,    -1,  1954,  1159,    -1,    -1,    -1,    -1,   160,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,   101,
      -1,   830,   831,    -1,    -1,    -1,   835,    -1,   837,    -1,
      -1,   113,    -1,   115,    -1,   117,    -1,    -1,   514,   802,
     516,   850,    -1,   852,    -1,    -1,    -1,   856,   857,   858,
     859,   656,   657,    -1,    -1,   818,    -1,    -1,    -1,   822,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   680,    -1,   159,   160,     4,
       5,     6,     7,     8,     9,    10,    11,    12,   693,    -1,
      -1,    -1,    -1,   902,    -1,    -1,    -1,    -1,  2049,    -1,
    2051,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,  2016,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   211,
      -1,   940,    -1,    -1,    -1,    -1,   945,    -1,    -1,    -1,
      -1,    -1,   951,   952,  2046,   135,    -1,    -1,   802,   803,
      -1,   960,    79,   962,  2105,  2106,    -1,    -1,    -1,   813,
      -1,    -1,   816,    -1,   973,    -1,   156,   157,    -1,    -1,
      -1,    -1,    -1,   163,    -1,  2077,   181,   167,   168,    -1,
     107,   786,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,  2143,    -1,    -1,    -1,    -1,    62,    -1,   281,
      -1,   283,   284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1022,  1023,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    73,    -1,   830,   831,    -1,   882,    -1,
      -1,    -1,    -1,   315,    -1,   889,   101,  2188,   320,   893,
      -1,    -1,    -1,   897,   326,    -1,  2197,  1402,   113,   114,
    1405,  1406,    -1,   858,   181,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,   360,  1088,
      -1,    -1,    -1,   365,    -1,   367,    -1,   137,    -1,    -1,
      -1,   156,    -1,    -1,  1103,  1104,    -1,    -1,  1107,    -1,
      -1,    -1,    -1,    -1,    -1,  1114,   156,   157,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     412,  1094,    -1,    -1,    -1,  1098,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,   211,    -1,    -1,    -1,
    1159,    -1,    -1,    -1,  1117,   960,    -1,    -1,   963,   156,
     157,  1124,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,  1735,    -1,    -1,
      -1,    -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,
     866,    -1,  1201,   869,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1172,
    1219,    -1,   494,  1176,   496,   497,   281,  1180,  1023,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   513,    -1,    -1,  1088,    -1,  1245,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1253,    -1,    -1,    -1,    -1,    -1,
     315,    -1,    -1,    76,    -1,   320,    -1,    -1,    -1,    -1,
      -1,   326,    -1,    -1,    -1,   547,    -1,    -1,  1277,  1278,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
    1289,    -1,    -1,   171,     1,    -1,  1295,     4,    -1,   571,
      -1,    -1,    -1,  1302,   576,   360,   578,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,  1163,
      -1,    -1,  1166,    -1,    -1,    -1,  1170,    -1,    -1,   601,
      -1,   603,   604,  1332,    -1,    -1,   305,    -1,  1337,    -1,
      -1,    -1,   155,   615,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,   628,   412,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   637,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    85,    -1,
    1046,   181,    -1,    -1,  1050,    -1,    -1,   659,    -1,   661,
     662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,
      -1,   683,   684,  1079,    -1,    -1,  1082,    -1,   690,   232,
      -1,    -1,    -1,    -1,   237,   238,   167,    -1,    -1,    -1,
      -1,    -1,   139,  1981,    -1,    -1,   143,    -1,    -1,   494,
      -1,    -1,   149,    -1,    -1,   258,    -1,    -1,    -1,    -1,
      -1,  1450,  1451,  1452,  1453,    -1,  1455,  1456,   513,    -1,
      -1,   168,  1461,  1462,    -1,    -1,    -1,    -1,    -1,    -1,
    1423,    -1,    -1,    -1,  1427,    -1,  1475,    -1,  1431,    -1,
    1479,  1480,  1148,    -1,    -1,    -1,    -1,    -1,   195,    -1,
      -1,  1490,    -1,    -1,  1493,    -1,    -1,    -1,    -1,   206,
     207,   314,    -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,
      -1,    -1,    -1,    -1,   327,   328,    -1,    -1,    -1,    -1,
      -1,    -1,  1521,   578,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   238,    -1,  1377,    -1,    -1,    -1,  1332,    -1,  1538,
     247,    -1,    -1,    -1,  1388,    -1,   601,    -1,    -1,    -1,
      -1,   258,    -1,    -1,    -1,    -1,   263,   264,    -1,   266,
     615,    -1,    -1,    -1,  1563,    -1,  1565,    -1,    -1,   276,
      -1,    -1,    -1,   628,    -1,    -1,    -1,    -1,    -1,    -1,
     287,    -1,  1222,   290,    -1,    -1,    -1,   294,    -1,    -1,
    1543,    -1,   299,   562,  1234,    -1,    -1,    -1,   305,    -1,
      -1,    -1,    -1,   310,   659,   312,    -1,    -1,   107,   316,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   328,    -1,    -1,    -1,    -1,    -1,    -1,   683,    -1,
      -1,    -1,   107,    -1,   109,   690,   111,   112,   113,   114,
     115,   116,   117,   118,  1597,    -1,    -1,    -1,    -1,    -1,
      -1,   358,    -1,  1606,   361,  1654,  1655,  1610,    -1,    -1,
    1659,    -1,  1661,    -1,    -1,   164,  1665,   374,  1667,   638,
      -1,   378,    58,    -1,    -1,    -1,    -1,    -1,    -1,   492,
      66,    67,    68,    69,  1350,    -1,    -1,    -1,    -1,    -1,
     962,  1690,  1691,  1359,    -1,    -1,    -1,    -1,   667,    -1,
      -1,   973,    -1,    13,    14,    15,    16,    17,    -1,   107,
     982,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   107,   535,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   547,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1750,    -1,    -1,    -1,    -1,    -1,    -1,   571,   157,
     185,    -1,   160,    73,   577,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,    -1,   484,    -1,  1778,
     107,   488,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   178,    -1,    -1,    -1,    -1,   107,   505,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,  1655,    -1,  1812,  1813,    -1,  1088,    -1,    -1,    -1,
      -1,  1487,   635,    -1,   637,   135,    -1,   137,  1827,    -1,
      -1,  1830,   159,   802,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1114,  1842,    -1,    -1,   156,   157,    -1,   818,
     905,    -1,    -1,   822,    -1,   562,    -1,   167,   168,    -1,
    1859,    -1,    -1,    -1,    -1,    -1,   835,    -1,    -1,    -1,
      -1,  1511,  1512,   580,    -1,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   596,
      -1,    -1,   107,  1892,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,   962,    -1,    -1,
    1576,    -1,    -1,    -1,    -1,    -1,    -1,  1583,    -1,    -1,
     135,    -1,  1588,  1589,  1590,    -1,   156,    -1,  1927,   159,
     160,   638,    -1,   640,    -1,    -1,    -1,  1936,    -1,    -1,
      -1,   156,   157,    -1,    -1,   160,    -1,    -1,    -1,   656,
     657,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
     667,    -1,    -1,    -1,   671,   180,    -1,   936,  1812,  1813,
      -1,    -1,    -1,   680,    -1,    -1,  1975,    -1,   685,   404,
      -1,   406,    -1,    -1,   409,   410,   693,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   419,   420,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,  1277,  2005,    -1,    -1,   434,
     435,    -1,    -1,    -1,    -1,    -1,    -1,  2016,    -1,    -1,
      -1,    -1,   835,    -1,   837,    -1,    -1,    -1,    -1,    -1,
     455,    -1,    -1,    -1,  1306,    -1,    -1,    -1,    -1,    -1,
    1312,    -1,   157,    -1,    -1,   160,    -1,  2046,  2047,    -1,
    2049,  2050,    -1,    -1,  1694,    -1,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,  2077,   786,
      -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,    -1,   902,
      -1,  1731,    -1,    -1,    -1,   802,   803,    -1,    -1,    -1,
      -1,    -1,    -1,  1769,    -1,    -1,  2105,  2106,    -1,    -1,
      -1,   818,  2065,    -1,    -1,   822,    -1,    -1,    -1,    -1,
    1786,    -1,  1788,   830,   831,  1094,    -1,    -1,   835,  1098,
     837,    -1,    73,    -1,    -1,    -1,    -1,    -1,  1107,    -1,
      -1,    -1,    -1,   850,  2143,   852,    -1,    -1,  1117,   856,
     857,   858,   859,    -1,    -1,  1124,    -1,  2001,    -1,    -1,
      -1,  2005,    -1,    -1,    -1,    -1,   107,    -1,    -1,   876,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
    1975,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1462,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,  1839,
    1840,    -1,    -1,  1172,    -1,  2049,  1846,  1176,    -1,    -1,
      -1,  1180,    -1,    -1,    -1,   156,   157,    -1,  1858,    -1,
      -1,     1,  1277,    -1,     4,    -1,   167,   168,  1868,   936,
    1870,    -1,    -1,   940,    -1,    -1,    -1,    -1,   945,    -1,
      -1,    -1,    -1,  1883,   951,  1885,  1886,  1887,    -1,  1521,
      -1,  1306,  2047,   960,    -1,    -1,   963,    -1,    -1,    -1,
      -1,  2105,  2106,   970,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   107,    -1,    59,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,    -1,   135,   123,  1107,   125,    -1,    -1,    -1,  2143,
    1113,  1114,    -1,  1943,    -1,    85,    -1,  1947,    -1,    -1,
      -1,    -1,  1952,   156,   157,  1022,  1023,    -1,    -1,    -1,
     163,    -1,    -1,    -1,   167,   168,    -1,   157,    -1,    -1,
     160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,    -1,    -1,   139,
      -1,    -1,    -1,   143,    -1,    -1,    -1,  2007,    -1,   149,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,  2019,
      -1,    -1,    -1,  2023,    -1,    -1,    -1,  1094,  1443,    -1,
      -1,  1098,    -1,    -1,    -1,    -1,  1103,  1104,  2038,    -1,
    1107,    -1,    -1,    -1,    -1,    -1,    -1,  1462,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,    -1,  1124,  1690,    -1,
      -1,    -1,    -1,   848,    -1,    -1,   206,   107,    -1,    -1,
      -1,   111,   112,   113,   114,   115,   116,   117,   118,   119,
    2080,    -1,    -1,   123,    -1,   125,    -1,    -1,    -1,    -1,
      -1,    -1,  1159,    -1,  1423,    -1,    -1,    -1,  1427,    -1,
      -1,    -1,  1431,    -1,    -1,  1172,  1521,   247,    -1,  1176,
      -1,    -1,    -1,  1180,    -1,    -1,  1289,   157,  2118,    -1,
     160,    -1,    -1,  2123,   264,    -1,   266,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1201,    -1,   276,    -1,    -1,    -1,
      -1,    -1,    -1,  1210,    -1,    -1,    -1,   287,    -1,    -1,
      -1,  2151,  1219,    -1,  2154,    -1,  2156,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     310,    -1,   312,    -1,    -1,    -1,    -1,  2177,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1253,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   336,    -1,  1830,    -1,
      -1,    89,    -1,    -1,    13,    14,    15,    16,    17,   159,
    1842,  1278,    -1,    -1,  1543,    -1,   107,    -1,    -1,  1392,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
      -1,    -1,   123,  1018,   125,  1302,    -1,    -1,    -1,  1024,
      -1,    -1,   130,    -1,  1311,    -1,    -1,   107,    -1,    -1,
    1035,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,  1330,    73,  1332,   157,    -1,  1597,   160,
      -1,    -1,    -1,     1,    -1,  1690,     4,  1606,    -1,    -1,
      -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,  1461,    -1,
    1075,    -1,    -1,    -1,    -1,  1927,   156,   157,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,   107,   455,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   135,    -1,   137,    -1,
      -1,    59,    -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,
      -1,  1408,    -1,    -1,   484,    -1,    -1,   156,   157,    -1,
      -1,   160,    -1,    -1,    -1,    -1,  1423,    85,   167,   168,
    1427,    -1,    -1,    -1,  1431,   505,    -1,   160,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,  1450,  1451,  1452,  1453,  1454,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1475,    -1,
      -1,   139,  1479,  1480,    -1,   143,   135,    -1,  2050,    -1,
      -1,   149,    -1,    -1,   152,    -1,  1493,  1842,   156,    -1,
      -1,    -1,    -1,    -1,  1219,    -1,   324,   156,   157,   167,
     168,   160,   170,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,   596,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,
      -1,  1538,    -1,    -1,    -1,    -1,  1543,    -1,   206,   207,
      -1,  1654,    -1,   211,    -1,    -1,  1659,    -1,    -1,    -1,
      -1,    -1,  1665,    -1,  1667,    -1,  1563,   107,    -1,    -1,
     640,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,  2143,    -1,   123,    -1,   125,    -1,  1302,   107,   247,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    1597,    -1,    -1,    -1,    -1,    -1,   264,    -1,   266,  1606,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,   157,   276,    -1,
      -1,    -1,    -1,   281,    -1,  1340,  1341,  1342,    -1,   287,
      -1,    -1,  1347,  1348,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,   302,    -1,    -1,   305,    -1,    -1,
      -1,    -1,   310,    -1,   312,    -1,  1371,   315,   316,    -1,
      -1,    -1,   320,    -1,    -1,    -1,    -1,    -1,   326,   487,
    1667,   489,    -1,    -1,    -1,    -1,    -1,    -1,   336,   107,
     498,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,  1691,    -1,    73,  1412,  1413,   102,
     358,    -1,   360,   361,   107,  2050,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   374,    -1,    -1,    -1,
     378,    -1,    -1,    -1,  1827,    -1,    -1,  1830,   156,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,  1750,    -1,    -1,   153,    -1,   135,    -1,
     137,    -1,    -1,   421,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,   156,
     157,    -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,  1892,
     167,   168,   135,    -1,    -1,    -1,    -1,   455,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2065,    -1,    -1,    -1,
      -1,    -1,    73,   156,   157,    -1,  1813,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,   484,    -1,    -1,    -1,
     488,    -1,    -1,  1936,    -1,    -1,   494,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1559,    -1,   107,   505,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,  1859,    -1,    -1,    -1,    -1,    -1,    -1,   527,
     940,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,   951,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,   156,   157,    -1,    -1,   160,
      -1,    -1,    -1,    -1,   562,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   573,   574,    -1,    -1,    -1,
     578,    -1,   580,   107,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,    -1,    -1,   596,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2049,    -1,    -1,    -1,
      -1,   135,  1022,   137,    -1,    -1,   107,   615,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
     628,    -1,   156,   157,    -1,    -1,    -1,    -1,  1975,    -1,
     638,    -1,   640,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,   656,   657,
      -1,   659,  2105,  2106,    -1,   156,    -1,    -1,    -1,   667,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2016,
    1735,    -1,   680,    -1,    -1,   683,    -1,    -1,    -1,   687,
     848,    -1,   690,    -1,    -1,   693,    -1,    -1,     1,    -1,
    2143,    -1,    -1,    -1,   702,    -1,    -1,    -1,    -1,  2046,
    2047,    -1,   710,    -1,    -1,   713,    -1,   715,   716,    -1,
     718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2065,   727,
      -1,    -1,   730,   731,   732,    -1,    -1,    -1,    -1,    -1,
    2077,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2106,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   943,   944,    -1,   786,    -1,
      -1,  1201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1210,    -1,   105,    -1,   802,    -1,    -1,    -1,    -1,  1219,
      -1,    -1,   810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     818,    -1,    -1,    -1,   822,    -1,    -1,    -1,    -1,    -1,
      73,    -1,   830,   831,    -1,    -1,   139,   835,    -1,    -1,
      -1,    -1,    -1,  1253,    -1,    -1,   149,    -1,    -1,    -1,
      -1,    -1,   850,    -1,   852,    -1,    -1,    -1,   856,   857,
     858,   859,    -1,    -1,   107,   168,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,   876,    -1,
      -1,  1039,    -1,    -1,  1939,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   207,    -1,    -1,   905,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1081,   167,   168,  1981,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,
      -1,    -1,   940,    -1,   247,    -1,    -1,   945,    -1,    -1,
      -1,    -1,    -1,   951,   952,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   960,    -1,   962,   963,    -1,    -1,    -1,    -1,
      -1,    -1,  1130,   276,  1132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   287,    -1,  1144,   290,  1146,    -1,
      -1,    -1,   990,  1151,  1152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   305,  1161,    -1,    -1,    -1,    -1,    -1,   312,
      -1,    -1,    -1,   316,    -1,    -1,  2071,    -1,  2073,    -1,
      -1,    -1,    -1,    -1,  1022,  1023,    -1,    -1,    -1,  1187,
      -1,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1450,  1451,  1452,  1453,  1454,  1455,  1456,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   358,    -1,  2112,   361,    -1,
      -1,    -1,    -1,    -1,    -1,  1475,    -1,    -1,    -1,  1479,
    1480,   374,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1493,    -1,    -1,  2141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1253,  1094,    -1,    -1,    -1,
    1098,    -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1117,
      -1,    -1,    -1,    -1,    -1,  1283,  1124,    -1,  2183,    -1,
      -1,    -1,  1290,    -1,  1292,  1293,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1301,    -1,  1303,    -1,  1305,    -1,    -1,
      -1,    -1,    -1,  1563,  1312,    73,    -1,    -1,    -1,    -1,
      -1,  1159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,  1176,    -1,
      -1,    -1,  1180,    -1,    -1,   488,    -1,    -1,    -1,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,  1201,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1210,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,  1219,    -1,    -1,    -1,    -1,    -1,  1385,  1386,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1245,    -1,   167,
     168,    -1,    -1,  1411,    -1,  1253,    -1,    -1,    -1,   562,
    1418,    -1,  1420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,  1277,
    1278,    -1,    -1,    -1,    73,    -1,    -1,    -1,  1446,    -1,
      -1,    -1,     1,   596,    -1,    -1,    -1,  1295,    -1,    -1,
      -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,  1306,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,  1330,    -1,  1332,   638,    -1,   640,    -1,  1337,
    1750,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      59,    -1,    -1,   656,   657,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   667,    -1,    -1,   156,   157,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,   167,   168,
      -1,    -1,    -1,    -1,  1542,    -1,    -1,    -1,    -1,    -1,
     693,  1549,    -1,  1551,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,    -1,
    1408,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,  1423,    -1,    -1,    -1,  1427,
     139,    -1,    -1,  1431,    -1,    -1,    -1,    -1,    -1,   135,
     149,    -1,    -1,    -1,    -1,  1443,    -1,    -1,    -1,  1859,
      -1,    -1,  1450,  1451,  1452,  1453,  1454,  1455,  1456,   168,
     156,   157,    -1,    -1,  1462,    -1,    -1,    -1,    -1,  1627,
      -1,   167,   168,    -1,    -1,    -1,    -1,  1475,    -1,    -1,
      -1,  1479,  1480,   786,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1490,    -1,    -1,  1493,    -1,    -1,   207,   802,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,   822,
      -1,    -1,    -1,  1521,    -1,    -1,    -1,   830,   831,    -1,
      -1,    -1,   835,    -1,    -1,    -1,    -1,    -1,   247,    -1,
    1538,    -1,    -1,    -1,    -1,  1543,    -1,   850,    -1,   852,
      -1,    -1,    -1,   856,   857,   858,   859,    -1,  1556,    -1,
      -1,    -1,    -1,    -1,    -1,  1563,    -1,   276,    -1,    -1,
      -1,    -1,    -1,   876,    -1,    -1,    -1,    -1,   287,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,  1597,
    1758,  1759,    -1,   312,    -1,    -1,  2016,   316,  1606,    -1,
      -1,    -1,  1610,    -1,  1772,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   936,    -1,    -1,  2046,   940,    -1,    -1,
      -1,    -1,   945,    -1,    -1,    -1,    -1,    -1,    -1,   358,
      -1,    -1,   361,    -1,    -1,    -1,    -1,   960,    -1,    -1,
     963,    -1,    -1,  1661,    -1,   374,    -1,  2077,    -1,   378,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1690,  1691,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
    1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,    -1,  1750,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   488,
    1778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1094,    -1,   123,    -1,  1098,    -1,    -1,    -1,    -1,
    1103,  1104,    -1,    -1,  1107,    -1,   136,    -1,   138,    -1,
      -1,    -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,
      -1,  1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,
      -1,   171,    -1,    -1,  1842,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   562,    -1,    -1,  1159,    -1,  1856,    -1,
      -1,  1859,    -1,    -1,    -1,    -1,   196,    -1,    -1,  1172,
      -1,   580,    -1,  1176,    -1,    -1,    -1,  1180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   596,    -1,    -1,
    2048,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   237,    -1,    -1,
      -1,   241,    -1,    -1,   244,   245,  1219,    -1,   248,    -1,
      -1,   251,   252,    -1,   254,    -1,   256,    -1,    -1,   638,
      -1,   640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2103,    -1,   656,   657,    -1,
    1253,    -1,    -1,    -1,    -1,    -1,  1954,    -1,   667,    -1,
      -1,  1959,    -1,    -1,    -1,    -1,  2124,    -1,    -1,    -1,
      -1,   680,    -1,    -1,    -1,  1278,    -1,  1975,    -1,    -1,
      -1,  2139,    -1,    -1,   693,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,  1302,
     330,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2016,    -1,
      -1,    -1,    -1,    -1,    -1,   355,    -1,  1330,    -1,  1332,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2046,  2047,
      -1,    -1,  2050,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2065,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,    -1,  2077,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   802,    -1,    -1,    -1,    -1,    -1,  1402,
      -1,    -1,  1405,  1406,    -1,  1408,    -1,    -1,     3,   818,
      -1,    -1,    -1,   822,    -1,    -1,    -1,    -1,    -1,    -1,
    1423,   830,   831,    -1,  1427,    -1,   835,    -1,  1431,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   850,    -1,   852,    -1,   475,    -1,   856,   857,   858,
     859,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,
      -1,    -1,  1475,    -1,    -1,    -1,  1479,  1480,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
    1493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,   535,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   551,    -1,    -1,    -1,    -1,    74,   936,    -1,    -1,
      -1,   940,    -1,    -1,    -1,  1538,   945,    -1,    -1,    -1,
    1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   960,    -1,    -1,   963,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,  1597,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1606,    -1,   635,    -1,  1610,   156,    -1,
      -1,   159,   160,   208,  1023,    -1,    -1,    -1,   166,   167,
     168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   675,   676,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   695,    -1,   697,    -1,    -1,
     265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1094,    -1,    -1,  1691,  1098,
      -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,  1107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1117,    -1,
      -1,   306,    -1,    -1,    -1,  1124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     335,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   357,  1172,    -1,    -1,    -1,  1176,    -1,    -1,
      -1,  1180,    -1,    -1,    -1,    -1,   806,   807,    -1,    -1,
      -1,    -1,    -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,
    1219,   841,   842,    -1,   844,    -1,   846,   847,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   422,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,  1253,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1859,    -1,   453,   889,
      -1,    -1,    -1,   893,    -1,    -1,    -1,   897,    -1,  1278,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     206,   207,    -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,
      -1,   486,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   506,   507,    -1,    -1,   241,   511,   512,    -1,    -1,
     515,  1330,   248,  1332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,    -1,
      -1,   971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   553,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1975,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,    -1,  1408,
      -1,    -1,    -1,    -1,   330,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2016,  1423,    -1,    -1,    -1,  1427,    -1,
      -1,    -1,  1431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   358,   359,    -1,    -1,   631,    -1,    -1,    -1,
      -1,    -1,    -1,  2046,  2047,    -1,    -1,    -1,    -1,   644,
      -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2065,    -1,    -1,    -1,  1475,    -1,    -1,    -1,
    1479,  1480,    -1,   668,  2077,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1113,  1493,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1158,  1538,
    1160,    -1,   458,  1163,  1543,    -1,  1166,    -1,   733,    -1,
    1170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   475,
     476,    -1,   478,   479,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488,    -1,    -1,    -1,   492,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,  1597,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,    -1,    -1,
     136,  1610,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     536,    -1,    -1,    -1,   540,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     825,    -1,   827,   169,    -1,    -1,    -1,    -1,   833,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   580,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   870,    -1,    -1,  1308,   874,
      -1,    -1,  1691,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,   244,   245,
     636,    -1,   248,    -1,    -1,   251,   252,    -1,   254,    -1,
     256,    -1,    -1,    -1,   919,    -1,    -1,    -1,    -1,   924,
     656,   657,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,   667,    57,    -1,    -1,   671,    -1,  1377,    -1,    -1,
      -1,    -1,   678,    -1,   680,    -1,    -1,    72,  1388,    74,
      75,  1391,    77,  1393,  1394,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,    -1,   102,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,    -1,   127,   128,    -1,    -1,  1011,    -1,    -1,   355,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   370,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,   164,
    1859,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
     786,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,    -1,
      -1,    -1,  1502,    -1,    -1,    -1,   802,   803,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,   814,    -1,
     816,   817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   830,   831,    -1,    -1,    -1,   835,
      -1,   837,   838,    -1,    -1,    -1,    -1,    -1,   844,    -1,
      -1,    -1,    -1,    -1,   850,    -1,   852,    -1,    -1,    -1,
     856,   857,   858,   859,    -1,    -1,    -1,    -1,    -1,   475,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
     876,    -1,   878,    -1,    -1,    -1,   882,    -1,    -1,    -1,
      -1,   101,    -1,   889,   890,    -1,  1975,   893,   894,    -1,
      -1,   897,   898,    -1,    -1,  1605,    -1,    -1,   904,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1197,  1633,    -1,    -1,    -1,  2016,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   551,   156,    -1,    -1,   945,
     946,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,    -1,    -1,    -1,    -1,  1665,    -1,  2046,  2047,    -1,
      -1,  1671,    -1,    -1,    -1,    -1,    -1,   973,    -1,    -1,
      -1,  1246,    -1,   193,    -1,    -1,  2065,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,  2077,    -1,
      -1,   211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1022,  1023,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1300,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1746,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,  1324,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   675,
     676,   281,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   695,
      -1,   697,  1088,  1793,  1794,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1103,  1104,    -1,
      -1,  1107,  1108,    -1,    -1,    -1,   326,    -1,  1114,  1819,
    1820,    -1,    -1,    -1,    -1,    -1,   336,  1827,    -1,    -1,
      -1,    -1,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   358,    -1,
     360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1159,    -1,    -1,    -1,  1163,  1164,    -1,
    1166,  1167,    -1,    -1,  1170,  1171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     806,   807,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,
      -1,   421,    -1,    -1,    -1,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,    -1,    -1,  1929,
      -1,    -1,   838,    -1,    -1,   841,   842,    -1,   844,    -1,
     846,   847,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   207,  1529,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,
      -1,    -1,    -1,   889,   494,    -1,    -1,   893,    -1,    -1,
      -1,   897,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2001,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,
     207,    -1,  1308,  1309,    -1,    -1,    -1,   527,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1332,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   305,    -1,   573,   574,   971,    -1,    -1,    -1,    -1,
     580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1377,  1378,  1648,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1388,  1389,    -1,  1391,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2105,  1402,    -1,   305,  1405,
    1406,    -1,  1408,    -1,   358,    -1,   360,   361,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     374,    -1,    -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   657,    -1,   659,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   358,    -1,   360,   361,    -1,    -1,    -1,    -1,    -1,
     680,    -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,
      -1,   378,    -1,    -1,    -1,    -1,  1751,  1752,    -1,    -1,
      -1,    -1,   702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     710,    -1,    -1,   713,    -1,   715,   716,  1113,   718,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   727,    -1,    -1,
     730,   731,   732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1538,    -1,   488,    -1,    -1,    -1,    -1,    -1,
     494,    -1,  1158,    -1,  1160,    -1,    -1,  1163,    -1,    -1,
    1166,    -1,    -1,    -1,  1170,    -1,    -1,    -1,    -1,  1565,
      -1,    -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,
     810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1874,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   562,    -1,
      -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,    -1,  1904,
     850,    -1,   852,    -1,    -1,    -1,   856,   857,   858,   859,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,
      -1,    -1,    -1,    -1,    -1,   562,   876,    -1,    -1,    -1,
      -1,   615,    -1,    -1,    -1,  1671,  1941,    -1,    -1,    -1,
      -1,    -1,    -1,   580,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   638,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1308,    -1,    -1,  1970,    -1,    -1,    -1,  1974,
      -1,    -1,   656,   657,    -1,   659,    -1,    -1,   615,    -1,
      -1,    -1,    -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   945,   680,    -1,    -1,   683,
      -1,   638,   952,    -1,    -1,    -1,   690,    -1,    -1,   693,
      -1,    -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,   656,
     657,    -1,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     667,  1377,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1388,   680,    -1,  1391,   683,  1393,  1394,    -1,
      -1,    -1,    -1,   690,    -1,    -1,   693,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1023,    -1,    -1,  1812,  1813,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   802,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,   822,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,   831,    -1,   786,
      -1,   835,    -1,  1103,  1104,    -1,  1502,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   802,   850,    -1,   852,    -1,
      -1,    -1,   856,   857,   858,   859,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,   822,    -1,    -1,    -1,    -1,
      -1,  1927,   876,   830,   831,    -1,    -1,    -1,   835,  1935,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,
      -1,    -1,    -1,   850,    -1,   852,    -1,    -1,    -1,   856,
     857,   858,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,  1605,
    1210,   945,    -1,    -1,    -1,  2001,  2002,    -1,    -1,  2005,
      -1,    -1,   421,    -1,    -1,    -1,   960,    -1,   962,   963,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1633,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1245,    -1,    -1,    -1,   936,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   945,    -1,
      -1,    -1,    -1,  2049,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   960,    -1,   962,   963,  1277,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1023,
      -1,    -1,    -1,    -1,    -1,  1295,    -1,    -1,    -1,    -1,
      -1,    -1,  1302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2105,
    2106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1332,   168,    -1,    -1,  1023,  1337,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1746,    -1,    -1,    -1,    -1,    -1,    -1,  2143,    -1,    -1,
    1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,    -1,  1103,
    1104,    -1,   207,  1107,   573,   574,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,
    1124,    -1,    -1,    -1,    -1,    -1,    -1,  1793,  1794,    -1,
      -1,    -1,  1402,    -1,    -1,  1405,  1406,  1094,  1408,    -1,
      -1,  1098,    -1,    -1,    -1,    -1,  1103,  1104,    -1,    -1,
    1107,    -1,    -1,  1819,  1820,  1159,    -1,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,    -1,  1832,  1124,  1172,    -1,
      -1,    -1,  1176,    -1,    -1,    -1,  1180,    -1,    -1,    -1,
    1450,  1451,  1452,    -1,    -1,  1455,  1456,    -1,    -1,    -1,
      -1,    -1,  1462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     305,    -1,  1159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,  1176,
    1490,    -1,    -1,  1180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   710,    -1,    -1,   713,    -1,    -1,    -1,    -1,   718,
      -1,  1521,    -1,   358,    -1,   360,   361,    -1,   727,    -1,
      -1,    -1,    -1,  1929,    -1,    -1,    -1,    -1,  1538,   374,
      -1,    -1,    -1,   378,    -1,    -1,    -1,   746,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1302,    -1,
      -1,    -1,  1306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,
      -1,    -1,    -1,    -1,    -1,  2001,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,    -1,  1306,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1330,    -1,  1332,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,   494,
      -1,  1661,    -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,
      -1,  1405,  1406,    -1,  1408,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1423,
    1690,    -1,    -1,  1427,    -1,    -1,    -1,  1431,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,  1405,  1406,
      -1,  1408,    -1,    56,    57,    -1,    -1,   562,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,    -1,    -1,
    1427,    -1,    -1,    -1,  1431,   580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1778,    -1,
     615,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   638,  1538,    -1,    -1,    -1,    -1,  1543,
      -1,   144,    -1,    -1,   147,    -1,    -1,    -1,    -1,    -1,
      -1,   656,   657,    -1,   659,    -1,    -1,    -1,    -1,   162,
      -1,    -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1842,    -1,    -1,   680,    -1,    -1,   683,   182,
      -1,  1538,    -1,    -1,    -1,   690,  1543,    -1,   693,    -1,
      -1,   194,    -1,  1597,    -1,    -1,   193,    -1,    -1,    -1,
      -1,    -1,  1606,    -1,    -1,    -1,  1610,    -1,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   226,   221,    -1,   223,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1606,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   277,   278,    -1,    -1,    -1,    -1,
      -1,   786,    -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   802,    -1,    -1,
     303,    -1,    -1,    -1,    -1,  1975,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   818,    -1,    -1,    -1,   822,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   830,   831,    -1,   325,    -1,
     835,    -1,    -1,    -1,    -1,   338,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   346,   347,   850,    -1,   852,   351,    -1,
      -1,   856,   857,   858,   859,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   876,    -1,    -1,    -1,    -1,  1245,  2047,    -1,    -1,
    2050,    -1,    -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,
     393,    -1,    -1,   396,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,   936,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
     945,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,   960,    73,   962,   963,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   491,    -1,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
     503,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1023,   516,
     137,    -1,    -1,    -1,     3,   522,     5,    -1,    -1,    -1,
     527,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   159,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,  1975,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,  1094,
      -1,    -1,    -1,  1098,    73,    74,    -1,    -1,  1103,  1104,
      -1,    -1,  1107,    -1,    -1,    -1,  1475,  1476,    -1,    -1,
    1479,  1480,  1117,    -1,    -1,    -1,  1485,    -1,  1975,  1124,
    1489,    -1,  1491,    -1,  1493,    -1,    -1,   106,    -1,    -1,
     109,   110,   629,    -1,    -1,    -1,    -1,    -1,    -1,   642,
      -1,    -1,    -1,  2047,    -1,    -1,  2050,    -1,   651,    -1,
      -1,    -1,    -1,    -1,  1159,    -1,   135,    -1,   137,    -1,
     657,  2065,    -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,
      -1,  1176,    -1,   670,    -1,  1180,   155,    -1,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
    2047,    -1,    -1,  2050,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   706,    -1,    -1,    -1,    -1,  2065,    -1,
     707,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   744,   745,    -1,
      -1,   748,    -1,   750,   757,    -1,    -1,    -1,    -1,   756,
      -1,   758,   759,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1641,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1302,    -1,    -1,
      -1,  1306,   799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   810,    -1,    -1,    -1,  1686,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1330,    -1,  1332,    -1,   826,
      -1,    -1,    -1,    -1,   831,    -1,  1705,  1706,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   849,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   860,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   863,  1736,    -1,   866,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     877,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,
    1405,  1406,    -1,  1408,    -1,    -1,    -1,    -1,   905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,
      -1,    -1,  1427,    -1,    -1,    -1,  1431,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   939,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   952,    -1,    -1,    -1,     5,
      -1,    -1,    -1,    -1,    -1,   962,   963,    13,    14,    15,
      16,    17,    -1,   970,    -1,  1844,    -1,    -1,    -1,    -1,
      -1,    -1,  1851,    -1,  1853,    -1,    -1,  1856,  1857,    -1,
    1859,   994,    -1,    -1,    -1,  1864,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1023,    73,    74,    -1,
      -1,    -1,    -1,  1538,  1031,    -1,    -1,    -1,  1543,    -1,
      -1,    -1,    -1,  1040,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,  1082,    -1,    -1,    -1,   135,
    1959,   137,  1597,    -1,    -1,    -1,  1965,  1966,    -1,    -1,
      -1,  1606,    -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,
     156,    -1,    -1,   159,   160,    -1,  1985,    -1,    -1,    -1,
     166,   167,   168,   169,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,    -1,  2026,  1155,    -1,
    1157,    -1,  2031,  2032,     3,    -1,    -1,  2036,  2037,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,  1225,  1226,  1227,    -1,    -1,    -1,    -1,  2098,
    2099,  2100,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1239,  1240,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,
    2129,  2130,  2131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,    -1,    -1,    -1,  1280,    -1,    -1,
      -1,    -1,    -1,  1286,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1306,
      -1,    -1,    -1,    -1,     1,  1312,    -1,    -1,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
    1327,    18,    -1,    -1,    -1,  1332,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1350,    -1,    -1,  1353,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,
      57,  1368,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    75,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,    -1,   102,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,    -1,
     127,   128,    -1,    -1,    -1,  1442,  1443,    -1,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1465,  1466,    -1,    -1,    -1,    -1,   155,   156,
    1975,  1468,   159,   160,    -1,    -1,    -1,   164,    -1,   166,
     167,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
    1487,    -1,    -1,  1490,   181,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1538,  2047,    -1,    -1,  2050,    -1,    -1,    -1,    -1,
    1547,  1548,    -1,    -1,  1557,    -1,    -1,    -1,    -1,    -1,
    2065,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,  1576,
      -1,    -1,    -1,  1580,    -1,    -1,    -1,    72,    -1,    74,
      75,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,    -1,   102,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    49,    -1,
     135,    52,    -1,    54,    55,    -1,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,  1661,    -1,    -1,    -1,    -1,  1666,
     155,   156,  1675,    74,   159,   160,    -1,    -1,    -1,   164,
      -1,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,  1726,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
     151,   152,    -1,    -1,    -1,   156,   157,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,  1769,    -1,    -1,    -1,    -1,    -1,    -1,  1776,
      -1,    -1,  1779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,  1805,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    72,
      73,    74,    75,    -1,    77,    -1,    -1,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,  1891,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,   159,   160,    -1,    -1,
      -1,   164,    -1,   166,   167,   168,   169,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   181,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1982,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    -1,    72,    73,
      74,    75,    -1,    77,    -1,    -1,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
     164,    -1,   166,   167,   168,   169,   170,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   181,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      55,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,   151,   152,    -1,    -1,
      -1,   156,   157,   158,   159,   160,    -1,    -1,    -1,    -1,
      -1,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   181,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    -1,    -1,    -1,    70,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,   152,    -1,    -1,    -1,
     156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,   169,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   181,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,
      -1,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
     158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   181,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,   158,   159,
     160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,
     170,   171,   172,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    -1,   127,   128,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,
     170,   171,   172,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,    -1,    -1,   159,   160,    -1,
      -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,   171,
     172,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,   166,   167,   168,   169,   170,   171,   172,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
      -1,    -1,   166,   167,   168,   169,   170,   171,   172,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      55,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,   166,   167,   168,   169,   170,   171,   172,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    -1,    -1,    -1,    70,    -1,     5,    73,    -1,
      -1,    -1,    -1,    78,    79,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    -1,   127,
     128,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,   169,   170,   171,   172,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    -1,
      -1,    -1,    70,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
      -1,   159,   160,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,
      -1,    -1,   164,    -1,    -1,   167,   168,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    -1,    -1,    -1,
      70,    -1,    -1,    73,    -1,    -1,    -1,    -1,    78,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    -1,    -1,    -1,    70,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,    -1,   159,   160,    -1,     3,    -1,
       5,    -1,    -1,   167,   168,    10,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,    -1,    -1,   159,   160,    -1,     3,    -1,     5,
      -1,    -1,   167,   168,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,   159,   160,    -1,     3,    -1,     5,    -1,
      -1,   167,   168,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,   158,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,
      -1,    -1,    -1,   164,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,   158,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,   158,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,   138,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,   138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    73,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    -1,    56,   109,   110,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    55,    -1,    57,    -1,
      -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    72,    -1,    74,    75,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,   137,    -1,    94,    95,    96,    97,    98,
      99,   100,    -1,   102,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
     169,   170,   171,   172,    -1,    49,    -1,    -1,    52,    -1,
      54,    55,   181,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    75,    -1,    77,    -1,    -1,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    -1,    -1,
      94,    95,    96,    97,    98,    99,   100,    -1,   102,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
     164,    -1,   166,   167,   168,   169,   170,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   181,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,    -1,   164,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,
      -1,   164,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,   109,   110,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      -1,    -1,    91,    -1,    93,    -1,    -1,    -1,   159,    -1,
      -1,    -1,    -1,   164,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,   158,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    55,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    73,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,   107,    56,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,   163,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,   159,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   138,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,    -1,
      -1,   164,    -1,    -1,   167,   168,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      13,    14,    15,    16,    17,    73,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     158,   159,    -1,    -1,   107,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      13,    14,    15,    16,    17,    73,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,   109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,   159,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   137,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    13,    14,    15,
      16,    17,    18,    73,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,
     110,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
      -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    13,    14,    15,
      16,    17,    18,    73,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   109,
     110,    -1,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,
     160,    -1,    -1,   109,   110,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   158,   159,   160,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,   160,    -1,    -1,   109,
     110,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    13,    14,    15,    16,    17,    18,    73,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,   109,   110,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,   160,    -1,    -1,   109,
     110,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,   160,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    -1,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,    -1,    -1,    -1,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,   160,    -1,    -1,   109,   110,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    13,    14,
      15,    16,    17,    18,    73,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
     109,   110,    -1,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,   160,    -1,    -1,   109,   110,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    13,
      14,    15,    16,    17,    73,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
     109,   110,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     159,    -1,    -1,    -1,    -1,   109,   110,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   159,   160,    -1,    13,    14,
      15,    16,    17,   167,   168,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    73,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    -1,    56,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,   160,   109,   110,    -1,    -1,
      -1,    -1,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
      -1,    -1,   166,   167,   168,   169,   170,   171,   172,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,    -1,
      -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    13,    14,    15,
      16,    17,    73,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,   107,    53,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   159,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,   137,    -1,    -1,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    13,    14,    15,    16,    17,
      73,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,   109,   110,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,
      -1,   109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,   159,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    73,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,   107,    49,   109,
     110,    52,    -1,    54,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,   137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,   109,   110,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    -1,   127,   128,    -1,   137,
      -1,    -1,    49,    -1,   135,    52,    -1,    54,    55,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
     151,   152,    -1,    -1,    -1,   156,   157,    74,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,
     157,    -1,   159,   160,    -1,    -1,    -1,   164,    -1,   166,
     167,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,
     160,    -1,    -1,    -1,   164,    -1,   166,   167,   168,   169,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,
      -1,   164,    -1,   166,   167,   168,   169,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
     156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,   169,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,   156,   157,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,   156,    -1,    -1,   159,   160,    -1,
      -1,    -1,   164,    -1,   166,   167,   168,   169,   170,   171,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,   156,    -1,    -1,   159,   160,    -1,    -1,   163,    -1,
      -1,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,    -1,
      -1,   159,   160,    -1,    -1,    -1,   164,    -1,   166,   167,
     168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
     164,    -1,   166,   167,   168,   169,   170,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,
      -1,   158,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,
     167,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    -1,   127,   128,    -1,    -1,    -1,    -1,
      49,    -1,   135,    52,    -1,    54,    55,    -1,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    74,   159,   160,    -1,    -1,
      -1,    -1,    -1,   166,   167,   168,   169,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,
      -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,   171,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,   156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,   157,
      -1,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,   156,   157,    -1,   159,   160,    -1,    -1,    -1,
      -1,    -1,   166,   167,   168,   169,   170,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,
     157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,
     167,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,   156,   157,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,   166,   167,   168,   169,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
     156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,   169,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    49,   127,   128,
      52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,   156,   157,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,   125,    49,   127,   128,    52,    -1,    54,
      55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,   156,   157,    -1,   159,   160,    -1,
      -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,   171,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
     125,    49,   127,   128,    52,    -1,    54,    55,    -1,    57,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,   156,   157,    -1,   159,   160,    -1,    -1,    -1,    -1,
      -1,   166,   167,   168,   169,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,   125,    49,   127,
     128,    52,    -1,    54,    55,    -1,    57,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,   157,
      -1,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,
     168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,   125,    49,   127,   128,    52,    -1,
      54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,   160,
      -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,   170,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,   125,    49,   127,   128,    52,    -1,    54,    55,    -1,
      57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,    -1,
      -1,    -1,   166,   167,   168,   169,   170,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,   125,    49,
     127,   128,    52,    -1,    54,    55,    -1,    57,   135,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,   156,
      -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,   166,
     167,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,   125,    49,   127,   128,    52,
      -1,    54,    55,    -1,    57,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,   156,    -1,    -1,   159,
     160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,   169,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,   125,    49,   127,   128,    52,    -1,    54,    55,
      -1,    57,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,   156,    -1,    -1,   159,   160,    -1,    -1,
      -1,    -1,    -1,   166,   167,   168,   169,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,   125,
      49,   127,   128,    52,    -1,    54,    55,    -1,    57,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
     156,    -1,    -1,   159,   160,    -1,    -1,    -1,    -1,    -1,
     166,   167,   168,   169,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,   125,    -1,   127,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     159,   160,    -1,    -1,    -1,    -1,    -1,   166,   167,   168,
     169,   170,   171,   172
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   183,   409,   410,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    70,    73,    74,
     102,   106,   107,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   122,   135,   137,   156,   157,   159,   160,
     167,   168,   186,   187,   188,   204,   296,   297,   298,   299,
     300,   301,   302,   303,   304,   305,   306,   307,   310,   313,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     326,   328,   329,   330,   332,   333,   337,   338,   339,   340,
     341,   343,   349,   350,   351,   352,   363,   368,   401,   404,
     414,   420,   422,   428,   432,   437,   438,   439,   440,   441,
     442,   443,   444,   470,   488,   489,   490,   491,     0,   183,
     107,   187,   204,   300,   302,   313,   316,   319,   329,   333,
     338,   121,   156,    59,    62,    63,    65,   156,   156,   366,
     426,   427,   428,   325,   326,   109,   110,   187,   189,   402,
     403,   189,   156,   414,   156,   156,     4,   107,   109,   110,
     317,   322,   323,   156,   156,   204,   427,   432,   438,   439,
     440,   442,   443,   444,   109,   340,   161,   183,   160,   303,
     313,   316,   437,   441,   487,   488,   491,   492,   181,   184,
     153,   164,   180,   225,   384,    90,   162,   421,   102,   189,
     425,   162,   162,   162,   181,   109,   110,   156,   204,   308,
     309,   432,   433,   434,   435,   436,   437,   441,   445,   446,
     447,   448,   449,   450,   451,   452,   453,   459,     3,    47,
      48,    50,    56,   331,     3,   160,   204,   302,   303,   317,
     321,   323,   334,   339,   417,   437,   441,   491,    70,   300,
     302,   316,   329,   333,   338,   418,   437,   441,    66,   322,
     322,   317,   323,   311,   322,   323,   331,   350,   317,   322,
     317,   159,   426,   162,   184,   156,   164,   233,   426,   426,
       3,   291,   292,   307,   310,   316,   320,   321,   160,   313,
     316,   489,   189,   189,   414,   180,   316,   156,   204,   423,
     432,   433,   437,   446,   450,   160,   204,   303,   491,   415,
     416,    58,    66,    67,    68,    69,   160,   178,   189,   390,
     392,   396,   398,   399,   339,    58,   158,   160,   204,   312,
     316,   320,   328,   329,   335,   336,   337,   338,   342,   349,
     350,   368,   378,   380,   470,   483,   484,   485,   486,   491,
     492,   426,   109,   110,   171,   187,   339,   367,   459,   428,
     156,   397,   398,   156,    13,    89,   156,   189,   429,   430,
     431,   121,   190,   191,    49,    52,    54,    55,    57,    74,
     104,   105,   107,   108,   119,   120,   123,   124,   125,   127,
     128,   156,   160,   166,   169,   170,   171,   172,   185,   186,
     190,   192,   195,   203,   204,   205,   206,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   218,   219,   220,   221,
     227,   339,   158,   160,   203,   204,   220,   222,   313,   339,
     382,   383,   400,   487,   492,   429,   316,   438,   439,   440,
     442,   443,   444,   158,   158,   158,   158,   158,   158,   158,
     109,   160,   187,   313,   470,   489,   160,   167,   204,   222,
     302,   303,   312,   314,   316,   329,   336,   338,   375,   376,
     377,   379,   380,   483,   491,   161,   156,   160,   437,   441,
     491,   156,   162,   107,   159,   160,   164,   186,   188,   222,
     385,   386,   387,   388,   389,    22,   385,   156,   189,   233,
     156,   156,   187,   423,   187,   427,   432,   434,   435,   436,
     445,   447,   448,   449,   451,   452,   453,   316,   433,   446,
     450,   162,   425,   160,   426,   467,   470,   425,   426,   426,
     421,   291,   156,   426,   467,   425,   426,   426,   421,   426,
     426,   316,   423,   156,   156,   315,   316,   313,   316,   161,
     183,   313,   487,   492,   425,   341,   164,   421,   291,   189,
     189,   384,   302,   321,   419,   437,   441,   164,   421,   291,
     402,   316,   329,   316,   316,   109,   340,   109,   110,   187,
     339,   344,   402,   138,   187,   316,   372,   373,   377,   378,
     381,   155,   183,   233,   307,   181,   437,   450,   316,   183,
     425,   156,   425,   184,   222,   427,   432,   316,   156,   189,
     412,   164,   156,   189,   164,   189,   138,   167,   168,   395,
     158,   162,   189,   399,   158,   426,   161,   183,   314,   316,
     329,   336,   338,   482,   483,   491,   492,   156,   160,   168,
     180,   204,   470,   472,   473,   474,   475,   476,   477,   494,
     204,   342,   491,   316,   336,   322,   317,   426,   158,   314,
     316,   484,   314,   470,   484,   187,   367,   459,   364,   164,
     367,   390,   180,   390,   429,   158,   162,   156,   158,   121,
     156,   203,   156,   156,   203,   156,   156,   206,   156,   203,
     156,   107,   109,   110,   317,   322,   323,   156,   203,   203,
      19,    21,    86,   160,   169,   170,   207,   208,   222,   229,
     233,   352,   382,   491,   162,   183,   156,   192,   160,   165,
     160,   165,   124,   126,   127,   128,   156,   159,   160,   164,
     165,   206,   206,   173,   167,   174,   175,   169,   170,   129,
     130,   131,   132,   176,   177,   133,   134,   168,   166,   178,
     135,   136,   179,   158,   162,   159,   183,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   180,   224,
     225,   226,   156,   204,   463,   464,   465,   466,   467,   158,
     162,   158,   158,   158,   158,   158,   158,   158,   156,   426,
     467,   470,   156,   467,   470,   156,   183,   156,   313,   489,
     161,   183,   184,   160,   184,   156,   168,   204,   432,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   138,   491,
     162,   184,   162,   184,   189,   189,   156,   183,   183,   183,
     183,   160,   188,   183,   386,   163,   162,   493,   385,   159,
     160,   163,   389,   400,   156,   190,   183,   180,   432,   434,
     435,   436,   445,   447,   448,   449,   451,   452,   453,   158,
     158,   158,   158,   158,   158,   158,   158,   158,   158,   433,
     446,   450,   426,   180,   161,   183,   384,   233,   421,   372,
     384,   233,   423,   229,   383,   229,   383,   423,   109,   160,
     412,   233,   421,   425,   164,   164,   421,   291,   412,   233,
     421,   346,   347,   345,   164,   158,   162,   158,   162,    71,
     293,   294,   181,   167,   222,   183,   432,   414,   412,   189,
     161,   183,   156,   394,   392,   393,    79,   327,   187,   314,
     470,   484,   316,   320,   491,   372,   473,   474,   475,   161,
     183,    18,   222,   316,   472,   494,   426,   426,   470,   314,
     482,   492,   316,   187,   314,   484,   426,   164,   426,   367,
      10,   166,   367,   369,   370,   164,   158,   383,   158,   158,
     430,   157,   196,   197,   198,   222,   181,   382,   492,   192,
     382,   160,   382,   383,   382,   492,   222,   382,   158,   382,
     382,   382,   161,   183,   158,   169,   170,   208,    18,   318,
     158,   162,   158,   167,   168,   158,   157,   222,   228,   222,
     164,   222,   187,   222,   187,   119,   160,   187,   196,   119,
     160,   189,   352,   222,   196,   187,   206,   209,   209,   209,
     210,   210,   211,   211,   212,   212,   212,   212,   213,   213,
     214,   215,   216,   217,   218,   163,   229,   181,   190,   160,
     187,   222,   164,   222,   372,   464,   465,   466,   316,   463,
     426,   426,   222,   383,   156,   426,   467,   470,   156,   467,
     470,   372,   372,   183,   183,   161,   161,   156,   432,   455,
     456,   457,   460,    18,   316,   454,   458,   156,   426,   476,
     494,   426,   426,   494,   156,   426,   476,   426,   426,   184,
     221,   189,   376,   379,   161,   379,   380,   161,   494,   494,
     138,   374,   375,   376,   374,   376,   374,   189,   183,   220,
     221,   222,   424,   493,   385,   387,   155,   183,   158,   183,
     158,   374,   222,   158,   158,   158,   158,   158,   158,   158,
     158,   158,   156,   426,   467,   470,   156,   426,   467,   470,
     156,   426,   467,   470,   423,    22,   470,   222,   323,   339,
     468,   233,   158,   158,   158,   158,   158,   410,   411,   233,
     155,   183,   412,   233,   421,   411,   233,   164,   164,   164,
     353,   138,   377,   378,   187,   189,   295,    18,    72,    74,
      75,    77,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    94,    95,    96,    97,    98,    99,
     100,   102,   109,   110,   122,   156,   160,   189,   229,   230,
     231,   232,   233,   234,   235,   237,   238,   247,   254,   255,
     256,   257,   258,   259,   264,   265,   268,   269,   270,   271,
     272,   273,   274,   280,   281,   282,   296,   316,   320,   422,
      71,   184,   184,   374,   413,   411,   158,   300,   302,   313,
     405,   406,   407,   408,   400,   180,   391,   391,   314,   484,
     160,   167,   204,   222,   339,   222,   316,   158,   158,   158,
     158,     5,   316,   426,   472,   365,   369,   367,   164,   339,
     162,   493,   189,   369,   164,   158,   158,   162,   158,   158,
     162,   158,   183,   162,   158,   158,   158,   162,   158,   206,
     158,   158,   158,   206,    18,   318,   222,   158,   158,   157,
     164,   206,   161,   162,   184,   196,   161,   161,   119,   123,
     125,   188,   199,   200,   201,   158,   199,   161,   162,   155,
     220,   163,   158,   199,   184,   386,   158,   158,   158,   158,
     463,   372,   372,   158,   158,   374,   374,   460,   158,   158,
     158,   158,   156,   432,   459,   454,   458,   372,   372,   161,
     184,   494,   162,   184,   158,   162,   162,   184,   162,   184,
     384,   199,   138,   172,   184,   184,   155,   385,   222,   426,
     374,   426,   184,   156,   426,   467,   470,   156,   426,   467,
     470,   156,   426,   467,   470,   372,   372,   372,   425,   150,
     172,   184,   469,   162,   184,   413,   405,   411,   233,   413,
     353,   353,   353,     3,     5,    10,    74,   155,   297,   304,
     305,   313,   316,   354,   359,   487,   162,   181,   156,    62,
      63,   181,   233,   296,   422,   156,   156,    18,   231,   156,
     156,   181,   189,   181,   189,   167,   189,   164,   230,   156,
     156,   156,   231,   156,   233,   222,   223,   223,    14,   283,
     259,   270,   163,   181,   184,   235,    79,   181,   189,    92,
      93,   263,   267,   113,   136,   262,   112,   135,   266,   262,
     381,   316,   295,   161,   161,   184,   413,   189,   423,   184,
     181,   184,   181,   184,   158,   383,   397,   397,   183,   184,
     184,   184,   222,   156,   426,   476,   470,   315,     5,   167,
     184,   222,   367,   493,   164,   369,    10,   370,   155,   180,
     371,   493,   155,   183,   198,   312,   187,    79,   193,   194,
     382,   206,   206,   206,   206,   206,   164,   386,   157,   222,
     162,   155,   202,   160,   200,   202,   202,   161,   162,   126,
     159,   161,   228,   220,   181,   161,   493,   156,   426,   467,
     470,   158,   158,   184,   184,   158,   156,   426,   467,   470,
     156,   426,   476,   432,   426,   426,   158,   158,   161,   379,
     161,   138,   376,   138,   158,   158,   184,   221,   221,   161,
     161,   184,   184,   158,   372,   372,   372,   158,   158,   158,
     384,   162,   222,   222,   323,   339,   161,   155,   184,   413,
     155,   155,   155,   155,   313,   313,   352,   360,   487,   313,
     359,   156,   348,   181,   181,   156,   163,   204,   355,   356,
     362,   432,   433,   446,   450,   162,   181,   189,   189,   196,
     181,   233,   181,   233,   229,   239,   296,   298,   301,   307,
     316,   320,   229,    81,   158,   239,   150,   151,   152,   157,
     158,   181,   229,   248,   249,   251,   296,   181,   181,   229,
     181,   386,   181,   229,   400,   229,   248,   114,   115,   116,
     117,   118,   275,   277,   278,   181,   101,   181,    85,   156,
     158,   426,   155,   181,   181,   156,   156,   231,   231,   259,
     156,   269,   259,   269,   233,   181,   158,   155,   395,   155,
     183,   162,   162,   161,   161,   161,   184,   372,   222,   222,
     184,   161,   184,   164,   155,   369,   493,   339,   189,   164,
     221,   155,   405,   471,   472,   158,   163,   158,   162,   163,
     386,   493,   228,   124,   199,   200,   160,   200,   160,   200,
     161,   155,   372,   158,   158,   372,   372,   161,   184,   158,
     426,   158,   158,   158,   229,   469,   155,   155,   348,   348,
     348,   355,   156,   204,   357,   358,   467,   478,   479,   480,
     481,   181,   162,   181,   355,   181,   400,   427,   432,   222,
     316,   155,   162,   181,   361,   362,   361,   361,   189,   158,
     158,   229,   316,   158,   156,   231,   158,   150,   151,   152,
     172,   181,   252,   253,   231,   230,   181,   253,   158,   163,
     229,   157,   229,   230,   251,   181,   493,   158,   158,   158,
     158,   233,   277,   278,   156,   222,   156,   190,     1,   231,
     206,   260,   229,    76,   111,   261,   263,    76,   426,   391,
     406,   183,   183,   161,   158,   184,   184,   161,   161,   369,
     493,   155,   371,   386,   184,   158,   222,   194,   222,   493,
     155,   161,   161,   199,   199,   158,   426,   426,   158,   158,
     161,   161,   222,   181,   479,   480,   481,   316,   478,   162,
     181,   426,   426,   181,   158,   432,   426,   231,   231,    78,
      79,   164,   242,   243,   244,   158,   229,    76,   231,   229,
     157,   229,    76,   181,    58,   157,   229,   230,   250,   251,
     339,   157,   229,   231,   249,   253,   253,   181,   229,   155,
     164,   244,   231,   231,   156,   183,   181,   190,   158,   163,
     158,   162,   163,   158,   231,   156,   231,   231,   231,   397,
     189,   423,   161,   161,   493,   155,   493,   155,   155,   161,
     161,   158,   158,   158,   478,   426,   356,    76,     1,   221,
     240,   241,   424,     1,   163,     1,   183,   231,   242,    76,
     181,   158,   231,    76,   181,   172,   172,   231,   230,   187,
     339,   253,   253,   181,   229,   250,   172,   172,    76,   157,
     229,   157,   229,   230,   181,     1,   183,   183,   279,   314,
     316,   487,   163,   181,   160,   190,   284,   285,   286,   206,
     196,   229,   262,   155,   155,   156,   426,   467,   470,   358,
     231,   138,     1,   162,   163,   155,   289,   290,   296,   231,
      76,   181,   231,   229,   157,   157,   229,   157,   229,   157,
     229,   230,   157,   229,   157,   229,   231,   172,   172,   172,
     172,   155,   289,   279,   184,   156,   204,   423,   478,   187,
     163,   107,   156,   158,   163,   162,   158,   158,    76,   258,
     372,   221,   240,   243,   245,   246,   296,   231,   172,   172,
     172,   172,   157,   157,   229,   157,   229,   157,   229,   245,
     184,   181,   276,   316,   284,   161,   221,   181,   284,   286,
     231,    76,   158,   231,   236,   184,   243,   157,   157,   229,
     157,   229,   157,   229,   184,   276,   220,   158,   163,   190,
     158,   158,   163,   231,     1,   231,   155,   236,   155,   158,
     233,   190,   287,   156,   181,   287,   233,   162,   163,   221,
     158,   190,   187,   288,   158,   181,   158,   162,   181,   187
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   182,   183,   184,   185,   185,   185,   185,   185,   186,
     186,   186,   186,   186,   186,   186,   186,   187,   187,   188,
     188,   189,   189,   189,   190,   191,   191,   192,   192,   192,
     192,   192,   192,   192,   192,   192,   192,   192,   192,   192,
     192,   192,   193,   193,   194,   194,   195,   195,   195,   195,
     195,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     195,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     196,   196,   197,   197,   198,   198,   199,   199,   200,   200,
     200,   200,   200,   200,   200,   201,   201,   201,   202,   202,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   204,   204,
     204,   205,   205,   205,   205,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   207,   207,   207,   207,   208,   208,
     209,   209,   210,   210,   210,   210,   211,   211,   211,   212,
     212,   212,   213,   213,   213,   213,   213,   214,   214,   214,
     215,   215,   216,   216,   217,   217,   218,   218,   219,   219,
     220,   220,   220,   221,   222,   222,   222,   223,   223,   224,
     224,   225,   225,   226,   226,   226,   226,   226,   226,   226,
     226,   226,   226,   226,   227,   227,   228,   228,   228,   228,
     229,   229,   230,   230,   231,   231,   231,   231,   231,   231,
     231,   231,   231,   231,   231,   231,   231,   231,   231,   231,
     232,   232,   233,   233,   234,   234,   235,   235,   235,   235,
     235,   236,   236,   236,   237,   238,   238,   238,   238,   238,
     238,   238,   238,   239,   239,   239,   239,   240,   240,   240,
     241,   241,   242,   242,   242,   242,   242,   243,   243,   244,
     245,   245,   246,   246,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   248,   248,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   250,   250,   250,   251,   251,   251,   252,
     252,   253,   253,   253,   254,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   254,   254,   254,   255,   255,   256,   257,   258,   259,
     259,   260,   260,   261,   262,   262,   263,   263,   264,   264,
     264,   264,   264,   264,   265,   266,   266,   267,   268,   268,
     269,   269,   270,   270,   270,   271,   272,   273,   274,   274,
     274,   275,   275,   276,   276,   277,   277,   277,   277,   278,
     279,   279,   279,   279,   279,   280,   281,   281,   282,   282,
     282,   282,   282,   283,   283,   284,   284,   285,   285,   286,
     286,   287,   287,   287,   288,   288,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   294,   294,   295,   295,
     296,   296,   296,   297,   297,   298,   298,   298,   298,   298,
     299,   299,   299,   300,   300,   300,   300,   300,   301,   301,
     301,   301,   301,   302,   302,   302,   302,   303,   303,   304,
     304,   304,   305,   305,   305,   305,   305,   306,   306,   307,
     307,   307,   307,   308,   308,   308,   308,   308,   309,   309,
     310,   310,   310,   310,   311,   311,   311,   312,   312,   312,
     313,   313,   313,   314,   314,   314,   315,   315,   316,   316,
     317,   317,   318,   318,   318,   318,   318,   319,   320,   320,
     320,   321,   321,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   323,   324,   324,   324,   324,   324,   324,   324,
     324,   324,   324,   324,   324,   324,   324,   324,   324,   324,
     324,   324,   324,   324,   324,   324,   324,   324,   324,   324,
     324,   325,   325,   326,   327,   327,   328,   328,   328,   328,
     328,   329,   329,   330,   330,   330,   330,   331,   331,   331,
     331,   331,   331,   332,   332,   332,   332,   333,   334,   333,
     333,   335,   335,   335,   335,   336,   336,   336,   337,   337,
     337,   337,   338,   338,   338,   339,   339,   339,   339,   339,
     339,   340,   340,   340,   341,   341,   342,   342,   344,   343,
     345,   343,   346,   343,   347,   343,   343,   348,   348,   349,
     349,   350,   350,   351,   351,   351,   352,   352,   352,   352,
     352,   352,   352,   352,   353,   353,   354,   354,   354,   354,
     354,   354,   354,   354,   354,   354,   354,   354,   355,   355,
     355,   356,   356,   356,   356,   357,   357,   357,   358,   359,
     359,   360,   360,   361,   361,   362,   363,   363,   364,   363,
     363,   365,   363,   363,   363,   366,   366,   367,   367,   368,
     368,   369,   369,   369,   369,   370,   370,   371,   371,   371,
     372,   372,   372,   372,   373,   373,   373,   373,   374,   374,
     374,   374,   374,   374,   374,   375,   375,   375,   375,   376,
     376,   377,   377,   378,   378,   379,   379,   379,   379,   379,
     380,   380,   380,   380,   380,   381,   381,   382,   382,   382,
     383,   383,   384,   384,   384,   384,   385,   385,   386,   386,
     386,   386,   386,   387,   387,   388,   388,   389,   389,   389,
     389,   389,   390,   390,   391,   391,   393,   392,   394,   392,
     392,   392,   392,   395,   395,   395,   395,   396,   396,   396,
     396,   397,   397,   398,   398,   399,   399,   400,   400,   400,
     400,   401,   401,   401,   402,   402,   403,   403,   404,   404,
     404,   404,   405,   405,   406,   406,   407,   407,   407,   408,
     408,   409,   409,   410,   410,   411,   411,   412,   413,   414,
     414,   414,   414,   414,   414,   414,   414,   414,   414,   414,
     415,   414,   416,   414,   417,   414,   418,   414,   419,   414,
     420,   420,   420,   421,   421,   422,   422,   422,   422,   422,
     422,   422,   422,   422,   422,   423,   423,   423,   423,   424,
     425,   425,   426,   426,   427,   427,   428,   428,   428,   429,
     429,   430,   430,   430,   431,   431,   431,   432,   432,   433,
     433,   433,   433,   434,   434,   434,   434,   435,   435,   435,
     435,   435,   435,   435,   436,   436,   436,   436,   437,   437,
     437,   438,   438,   438,   438,   438,   439,   439,   439,   439,
     440,   440,   440,   440,   440,   440,   441,   441,   441,   442,
     442,   442,   442,   442,   443,   443,   443,   443,   444,   444,
     444,   444,   444,   444,   445,   445,   446,   446,   446,   446,
     447,   447,   447,   447,   448,   448,   448,   448,   448,   448,
     448,   449,   449,   449,   449,   450,   450,   450,   451,   451,
     451,   451,   451,   452,   452,   452,   452,   453,   453,   453,
     453,   453,   453,   454,   454,   454,   454,   454,   455,   455,
     455,   456,   456,   456,   456,   457,   457,   457,   458,   458,
     458,   458,   458,   459,   459,   460,   460,   460,   461,   461,
     462,   462,   463,   463,   463,   464,   464,   464,   464,   464,
     465,   465,   465,   465,   466,   466,   466,   467,   467,   467,
     467,   467,   468,   468,   468,   468,   468,   468,   469,   469,
     470,   470,   470,   470,   471,   471,   472,   472,   472,   472,
     473,   473,   473,   473,   473,   474,   474,   474,   474,   475,
     475,   475,   476,   476,   476,   477,   477,   477,   477,   477,
     477,   478,   478,   478,   479,   479,   479,   479,   479,   480,
     480,   480,   480,   481,   481,   482,   482,   482,   483,   483,
     484,   484,   484,   484,   484,   484,   485,   485,   485,   485,
     485,   485,   485,   485,   485,   485,   486,   486,   486,   486,
     487,   487,   487,   488,   488,   489,   489,   489,   489,   489,
     489,   490,   490,   490,   490,   490,   490,   491,   491,   491,
     492,   492,   492,   493,   493,   494,   494
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     3,     3,     3,     5,     6,     2,     2,     2,     2,
       2,     2,     1,     3,     3,     3,     1,     6,     4,     4,
       4,     4,     4,     7,     3,     3,     3,     3,     3,     2,
       5,     3,     3,     3,     5,     2,     2,     7,     8,     5,
       0,     1,     1,     3,     1,     1,     1,     3,     1,     2,
       4,     3,     5,     3,     5,     2,     2,     2,     0,     2,
       1,     1,     1,     2,     2,     2,     2,     2,     2,     4,
       2,     4,     4,     4,     6,     4,     4,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     5,     5,     4,
       5,     5,     5,     4,     2,     2,     3,     3,     1,     1,
       1,     3,     1,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     5,     4,     1,     1,     3,     6,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     7,     1,     1,     3,     3,
       1,     3,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     2,     6,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     2,     5,     7,     5,    10,     7,
       5,    10,     7,     1,     1,     1,     2,     1,     3,     1,
       1,     3,     2,     3,     3,     2,     2,     1,     2,     2,
       0,     1,     2,     3,     4,     6,     5,     7,     6,     7,
       7,     8,     4,     6,     5,     7,     1,     3,     4,     5,
       4,     3,     5,     1,     2,     3,     3,     3,     5,     5,
       5,     5,     3,     5,     5,     5,     3,     4,     5,     5,
       5,     5,     7,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       6,     3,     4,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     4,     2,     3,     3,     2,
       3,     2,     3,     3,     6,     2,     2,     3,     3,     3,
       3,     3,     3,     5,     1,     1,     5,     5,     4,     0,
       1,     1,     3,     4,     1,     1,     4,     6,     3,     5,
       5,     5,     8,     9,     1,     1,     1,     4,     3,     3,
       1,     3,     1,     3,     5,     1,     2,     5,     3,     3,
       4,     8,     9,     0,     2,     1,     1,     1,     1,     2,
       1,     2,     2,     2,     1,     3,     1,     1,     6,     8,
      10,    12,    14,     0,     1,     0,     1,     1,     3,     4,
       7,     0,     1,     3,     1,     3,     0,     1,     1,     2,
       0,     1,     2,     3,     0,     1,     3,     4,     1,     3,
       2,     2,     1,     7,     5,     1,     1,     1,     1,     1,
       2,     3,     6,     3,     3,     4,     2,     3,     1,     2,
       2,     3,     8,     9,     9,     8,     8,     5,     7,     2,
       2,     3,     3,     3,     4,     3,     4,     4,     5,     2,
       1,     1,     1,     3,     3,     2,     4,     6,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     4,     1,     2,
       3,     1,     2,     1,     1,     1,     1,     1,     1,     1,
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
       1,     2,     1,     2,     0,     2,     3,     3,     4,     4,
       4,     3,     2,     2,     3,     3,     2,     1,     0,     1,
       4,     1,     2,     2,     2,     0,     1,     4,     1,     2,
       3,     1,     2,     0,     1,     2,     7,     8,     0,     9,
       8,     0,    11,    10,     1,     2,     3,     0,     1,     3,
       3,     3,     2,     5,     4,     1,     1,     0,     2,     5,
       0,     1,     1,     3,     1,     1,     3,     3,     0,     1,
       1,     1,     3,     3,     3,     1,     3,     3,     5,     1,
       3,     3,     3,     2,     3,     1,     3,     3,     4,     1,
       1,     1,     1,     2,     1,     1,     3,     1,     1,     2,
       1,     1,     0,     2,     2,     4,     1,     4,     0,     1,
       2,     3,     4,     2,     2,     1,     2,     2,     5,     5,
       7,     6,     1,     3,     0,     2,     0,     5,     0,     5,
       3,     1,     8,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     6,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,     5,
      10,     8,     1,     4,     2,     2,     1,     1,     5,     2,
       5,     0,     1,     3,     4,     0,     1,     0,     0,     1,
       1,     2,     2,     2,     2,     2,     2,     1,     2,     5,
       0,     6,     0,     8,     0,     7,     0,     7,     0,     8,
       1,     2,     3,     0,     5,     3,     4,     4,     4,     4,
       5,     5,     5,     5,     6,     1,     1,     1,     1,     3,
       0,     5,     0,     1,     1,     2,     6,     4,     4,     1,
       3,     0,     1,     4,     1,     1,     1,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     6,     7,     3,     4,     2,     1,
       2,     4,     6,     7,     3,     4,     2,     3,     4,     5,
       4,     5,     4,     5,     3,     4,     1,     1,     1,     4,
       6,     7,     3,     4,     2,     3,     3,     4,     4,     5,
       4,     5,     3,     4,     1,     3,     2,     1,     2,     2,
       2,     3,     4,     5,     2,     4,     5,     4,     5,     3,
       4,     6,     7,     3,     4,     2,     1,     2,     4,     6,
       7,     3,     4,     2,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     2,     4,     1,     2,     2,     2,     3,
       4,     2,     4,     4,     3,     4,     6,     3,     2,     4,
       1,     2,     2,     1,     1,     2,     3,     4,     2,     4,
       4,     6,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     3,     6,     3,     2,     3,     7,
       5,     1,     1,     1,     3,     3,     3,     5,     1,     1,
       5,     5,     6,     6,     0,     1,     1,     3,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     3,
       6,     3,     1,     2,     1,     2,     6,     5,     6,     7,
       7,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     6,     3,     1,     1,     2,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     2,     4,     3,
       2,     3,     2,     4,     3,     2,     6,     6,     6,     7,
       1,     2,     1,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     4,     2,     3,     4,     2,     5,     6,     7,
       5,     6,     6,     0,     1,     0,     2
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
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 8505 "Parser/parser.cc"
    break;

  case 3:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 8511 "Parser/parser.cc"
    break;

  case 4:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8517 "Parser/parser.cc"
    break;

  case 5:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8523 "Parser/parser.cc"
    break;

  case 6:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8529 "Parser/parser.cc"
    break;

  case 7:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8535 "Parser/parser.cc"
    break;

  case 8:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8541 "Parser/parser.cc"
    break;

  case 20:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8547 "Parser/parser.cc"
    break;

  case 24:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8553 "Parser/parser.cc"
    break;

  case 25:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8559 "Parser/parser.cc"
    break;

  case 26:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8569 "Parser/parser.cc"
    break;

  case 27:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8575 "Parser/parser.cc"
    break;

  case 28:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8581 "Parser/parser.cc"
    break;

  case 29:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8587 "Parser/parser.cc"
    break;

  case 31:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8593 "Parser/parser.cc"
    break;

  case 32:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 33:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 34:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8611 "Parser/parser.cc"
    break;

  case 35:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8621 "Parser/parser.cc"
    break;

  case 36:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 8627 "Parser/parser.cc"
    break;

  case 37:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8633 "Parser/parser.cc"
    break;

  case 38:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8639 "Parser/parser.cc"
    break;

  case 39:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8645 "Parser/parser.cc"
    break;

  case 40:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8651 "Parser/parser.cc"
    break;

  case 41:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8657 "Parser/parser.cc"
    break;

  case 43:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8669 "Parser/parser.cc"
    break;

  case 44:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8678 "Parser/parser.cc"
    break;

  case 45:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8684 "Parser/parser.cc"
    break;

  case 47:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 8690 "Parser/parser.cc"
    break;

  case 48:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 49:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8702 "Parser/parser.cc"
    break;

  case 50:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8708 "Parser/parser.cc"
    break;

  case 51:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8718 "Parser/parser.cc"
    break;

  case 52:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8724 "Parser/parser.cc"
    break;

  case 53:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg" ) ) ),
											   (yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) ) ) ); }
#line 8731 "Parser/parser.cc"
    break;

  case 54:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8737 "Parser/parser.cc"
    break;

  case 55:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8743 "Parser/parser.cc"
    break;

  case 56:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8749 "Parser/parser.cc"
    break;

  case 57:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8755 "Parser/parser.cc"
    break;

  case 58:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8761 "Parser/parser.cc"
    break;

  case 59:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8767 "Parser/parser.cc"
    break;

  case 60:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8773 "Parser/parser.cc"
    break;

  case 61:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8779 "Parser/parser.cc"
    break;

  case 62:
#line 853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8785 "Parser/parser.cc"
    break;

  case 63:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8791 "Parser/parser.cc"
    break;

  case 64:
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8797 "Parser/parser.cc"
    break;

  case 65:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 66:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8809 "Parser/parser.cc"
    break;

  case 67:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8815 "Parser/parser.cc"
    break;

  case 68:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8821 "Parser/parser.cc"
    break;

  case 69:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8831 "Parser/parser.cc"
    break;

  case 70:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8837 "Parser/parser.cc"
    break;

  case 73:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8843 "Parser/parser.cc"
    break;

  case 74:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8849 "Parser/parser.cc"
    break;

  case 77:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 8855 "Parser/parser.cc"
    break;

  case 79:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8861 "Parser/parser.cc"
    break;

  case 80:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8867 "Parser/parser.cc"
    break;

  case 81:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8873 "Parser/parser.cc"
    break;

  case 82:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8879 "Parser/parser.cc"
    break;

  case 83:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8885 "Parser/parser.cc"
    break;

  case 84:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8891 "Parser/parser.cc"
    break;

  case 85:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8897 "Parser/parser.cc"
    break;

  case 86:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8903 "Parser/parser.cc"
    break;

  case 87:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8911 "Parser/parser.cc"
    break;

  case 88:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8917 "Parser/parser.cc"
    break;

  case 89:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8926 "Parser/parser.cc"
    break;

  case 92:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8932 "Parser/parser.cc"
    break;

  case 93:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8938 "Parser/parser.cc"
    break;

  case 94:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].oper) ) {
			case OperKinds::AddressOf:
				(yyval.expr) = new ExpressionNode( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].expr) ) ) );
				break;
			case OperKinds::PointTo:
				(yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) );
				break;
			case OperKinds::And:
				(yyval.expr) = new ExpressionNode( new ast::AddressExpr( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].expr) ) ) ) );
				break;
			default:
				assert( false );
			}
		}
#line 8958 "Parser/parser.cc"
    break;

  case 95:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 96:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 97:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8976 "Parser/parser.cc"
    break;

  case 98:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8982 "Parser/parser.cc"
    break;

  case 99:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8988 "Parser/parser.cc"
    break;

  case 100:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 101:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 102:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 103:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 104:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 105:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 9027 "Parser/parser.cc"
    break;

  case 106:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 9033 "Parser/parser.cc"
    break;

  case 107:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "countof for expressions is currently unimplemented. "); (yyval.expr) = nullptr; }
#line 9039 "Parser/parser.cc"
    break;

  case 108:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 9045 "Parser/parser.cc"
    break;

  case 109:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 9051 "Parser/parser.cc"
    break;

  case 110:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 9057 "Parser/parser.cc"
    break;

  case 111:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 9063 "Parser/parser.cc"
    break;

  case 112:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 9069 "Parser/parser.cc"
    break;

  case 113:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 9075 "Parser/parser.cc"
    break;

  case 114:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 9081 "Parser/parser.cc"
    break;

  case 116:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 9087 "Parser/parser.cc"
    break;

  case 117:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9093 "Parser/parser.cc"
    break;

  case 118:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 9099 "Parser/parser.cc"
    break;

  case 119:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 9105 "Parser/parser.cc"
    break;

  case 120:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 9111 "Parser/parser.cc"
    break;

  case 121:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 122:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9123 "Parser/parser.cc"
    break;

  case 123:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9129 "Parser/parser.cc"
    break;

  case 131:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 133:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 134:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 135:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 137:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9159 "Parser/parser.cc"
    break;

  case 138:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9165 "Parser/parser.cc"
    break;

  case 140:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9171 "Parser/parser.cc"
    break;

  case 141:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9177 "Parser/parser.cc"
    break;

  case 143:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9183 "Parser/parser.cc"
    break;

  case 144:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9189 "Parser/parser.cc"
    break;

  case 145:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 146:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9201 "Parser/parser.cc"
    break;

  case 148:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9207 "Parser/parser.cc"
    break;

  case 149:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9213 "Parser/parser.cc"
    break;

  case 151:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9219 "Parser/parser.cc"
    break;

  case 153:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9225 "Parser/parser.cc"
    break;

  case 155:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9231 "Parser/parser.cc"
    break;

  case 157:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 9237 "Parser/parser.cc"
    break;

  case 159:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 9243 "Parser/parser.cc"
    break;

  case 161:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 9249 "Parser/parser.cc"
    break;

  case 162:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 9255 "Parser/parser.cc"
    break;

  case 165:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 9267 "Parser/parser.cc"
    break;

  case 166:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9273 "Parser/parser.cc"
    break;

  case 167:
#line 1157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9279 "Parser/parser.cc"
    break;

  case 171:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 9285 "Parser/parser.cc"
    break;

  case 172:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 9291 "Parser/parser.cc"
    break;

  case 173:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 9297 "Parser/parser.cc"
    break;

  case 174:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 9303 "Parser/parser.cc"
    break;

  case 175:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 9309 "Parser/parser.cc"
    break;

  case 176:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 9315 "Parser/parser.cc"
    break;

  case 177:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 9321 "Parser/parser.cc"
    break;

  case 178:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 9327 "Parser/parser.cc"
    break;

  case 179:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 9333 "Parser/parser.cc"
    break;

  case 180:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 9339 "Parser/parser.cc"
    break;

  case 181:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 9345 "Parser/parser.cc"
    break;

  case 182:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 9351 "Parser/parser.cc"
    break;

  case 183:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 9357 "Parser/parser.cc"
    break;

  case 184:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (new ExpressionNode( nullptr ))->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9363 "Parser/parser.cc"
    break;

  case 185:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) ); }
#line 9369 "Parser/parser.cc"
    break;

  case 187:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9375 "Parser/parser.cc"
    break;

  case 188:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 9381 "Parser/parser.cc"
    break;

  case 189:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9387 "Parser/parser.cc"
    break;

  case 191:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9393 "Parser/parser.cc"
    break;

  case 192:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9399 "Parser/parser.cc"
    break;

  case 207:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9405 "Parser/parser.cc"
    break;

  case 209:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 210:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 9417 "Parser/parser.cc"
    break;

  case 211:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 9428 "Parser/parser.cc"
    break;

  case 212:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 9434 "Parser/parser.cc"
    break;

  case 213:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 215:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9446 "Parser/parser.cc"
    break;

  case 216:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9452 "Parser/parser.cc"
    break;

  case 217:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9458 "Parser/parser.cc"
    break;

  case 218:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9464 "Parser/parser.cc"
    break;

  case 219:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 222:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 9476 "Parser/parser.cc"
    break;

  case 223:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 9483 "Parser/parser.cc"
    break;

  case 224:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 9489 "Parser/parser.cc"
    break;

  case 225:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9495 "Parser/parser.cc"
    break;

  case 226:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9501 "Parser/parser.cc"
    break;

  case 227:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9507 "Parser/parser.cc"
    break;

  case 228:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9521 "Parser/parser.cc"
    break;

  case 229:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9527 "Parser/parser.cc"
    break;

  case 230:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 9533 "Parser/parser.cc"
    break;

  case 231:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 9542 "Parser/parser.cc"
    break;

  case 232:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9548 "Parser/parser.cc"
    break;

  case 233:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9554 "Parser/parser.cc"
    break;

  case 234:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9560 "Parser/parser.cc"
    break;

  case 235:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9566 "Parser/parser.cc"
    break;

  case 236:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9572 "Parser/parser.cc"
    break;

  case 237:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9578 "Parser/parser.cc"
    break;

  case 238:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 240:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 241:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 242:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 9602 "Parser/parser.cc"
    break;

  case 243:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9608 "Parser/parser.cc"
    break;

  case 244:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9614 "Parser/parser.cc"
    break;

  case 245:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 246:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9626 "Parser/parser.cc"
    break;

  case 248:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 249:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 250:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9644 "Parser/parser.cc"
    break;

  case 252:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 253:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 254:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 255:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9671 "Parser/parser.cc"
    break;

  case 256:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9677 "Parser/parser.cc"
    break;

  case 257:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9683 "Parser/parser.cc"
    break;

  case 258:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9689 "Parser/parser.cc"
    break;

  case 259:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9698 "Parser/parser.cc"
    break;

  case 260:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 261:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 262:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9716 "Parser/parser.cc"
    break;

  case 263:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9725 "Parser/parser.cc"
    break;

  case 264:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9731 "Parser/parser.cc"
    break;

  case 265:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9737 "Parser/parser.cc"
    break;

  case 267:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].forctl)->init->set_last( (yyvsp[0].forctl)->init );
			if ( (yyvsp[-2].forctl)->condition ) {
				if ( (yyvsp[0].forctl)->condition ) {
					(yyvsp[-2].forctl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].forctl)->condition->expr.release(), (yyvsp[0].forctl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].forctl)->condition = (yyvsp[0].forctl)->condition;
			if ( (yyvsp[-2].forctl)->change ) {
				if ( (yyvsp[0].forctl)->change ) {
					(yyvsp[-2].forctl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].forctl)->change->expr.release(), (yyvsp[0].forctl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].forctl)->change = (yyvsp[0].forctl)->change;
			(yyval.forctl) = (yyvsp[-2].forctl);
		}
#line 9756 "Parser/parser.cc"
    break;

  case 268:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9762 "Parser/parser.cc"
    break;

  case 269:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9771 "Parser/parser.cc"
    break;

  case 270:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9777 "Parser/parser.cc"
    break;

  case 271:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9783 "Parser/parser.cc"
    break;

  case 272:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9789 "Parser/parser.cc"
    break;

  case 273:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9795 "Parser/parser.cc"
    break;

  case 274:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9801 "Parser/parser.cc"
    break;

  case 275:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9807 "Parser/parser.cc"
    break;

  case 276:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9816 "Parser/parser.cc"
    break;

  case 277:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9825 "Parser/parser.cc"
    break;

  case 278:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9831 "Parser/parser.cc"
    break;

  case 279:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9840 "Parser/parser.cc"
    break;

  case 280:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9849 "Parser/parser.cc"
    break;

  case 281:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9855 "Parser/parser.cc"
    break;

  case 282:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9861 "Parser/parser.cc"
    break;

  case 283:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9867 "Parser/parser.cc"
    break;

  case 284:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9873 "Parser/parser.cc"
    break;

  case 285:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9879 "Parser/parser.cc"
    break;

  case 286:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9885 "Parser/parser.cc"
    break;

  case 287:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9891 "Parser/parser.cc"
    break;

  case 288:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9897 "Parser/parser.cc"
    break;

  case 289:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9906 "Parser/parser.cc"
    break;

  case 290:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9916 "Parser/parser.cc"
    break;

  case 291:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9922 "Parser/parser.cc"
    break;

  case 292:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9928 "Parser/parser.cc"
    break;

  case 293:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9937 "Parser/parser.cc"
    break;

  case 294:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9947 "Parser/parser.cc"
    break;

  case 295:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9953 "Parser/parser.cc"
    break;

  case 296:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9962 "Parser/parser.cc"
    break;

  case 297:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9972 "Parser/parser.cc"
    break;

  case 298:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9978 "Parser/parser.cc"
    break;

  case 299:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9984 "Parser/parser.cc"
    break;

  case 300:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9990 "Parser/parser.cc"
    break;

  case 301:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9996 "Parser/parser.cc"
    break;

  case 302:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10005 "Parser/parser.cc"
    break;

  case 303:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 10015 "Parser/parser.cc"
    break;

  case 304:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 10021 "Parser/parser.cc"
    break;

  case 305:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10030 "Parser/parser.cc"
    break;

  case 306:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 10040 "Parser/parser.cc"
    break;

  case 307:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 10046 "Parser/parser.cc"
    break;

  case 308:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10055 "Parser/parser.cc"
    break;

  case 309:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 10065 "Parser/parser.cc"
    break;

  case 310:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 10071 "Parser/parser.cc"
    break;

  case 311:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctl) = enumRangeCtrl( (yyvsp[-2].expr), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->buildType() ) ) );
		}
#line 10079 "Parser/parser.cc"
    break;

  case 312:
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 10090 "Parser/parser.cc"
    break;

  case 313:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10096 "Parser/parser.cc"
    break;

  case 314:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false ); }
#line 10102 "Parser/parser.cc"
    break;

  case 315:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false ); }
#line 10108 "Parser/parser.cc"
    break;

  case 316:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10114 "Parser/parser.cc"
    break;

  case 317:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10120 "Parser/parser.cc"
    break;

  case 318:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10126 "Parser/parser.cc"
    break;

  case 319:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 10132 "Parser/parser.cc"
    break;

  case 320:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 10138 "Parser/parser.cc"
    break;

  case 322:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 10144 "Parser/parser.cc"
    break;

  case 323:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 10150 "Parser/parser.cc"
    break;

  case 324:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 10156 "Parser/parser.cc"
    break;

  case 325:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 10162 "Parser/parser.cc"
    break;

  case 326:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 10168 "Parser/parser.cc"
    break;

  case 327:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 10174 "Parser/parser.cc"
    break;

  case 328:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 10180 "Parser/parser.cc"
    break;

  case 329:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 10186 "Parser/parser.cc"
    break;

  case 330:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 331:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 332:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 333:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 334:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10216 "Parser/parser.cc"
    break;

  case 335:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 10222 "Parser/parser.cc"
    break;

  case 336:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 10228 "Parser/parser.cc"
    break;

  case 337:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 10234 "Parser/parser.cc"
    break;

  case 338:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 10240 "Parser/parser.cc"
    break;

  case 339:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 10246 "Parser/parser.cc"
    break;

  case 340:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 10252 "Parser/parser.cc"
    break;

  case 341:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 10258 "Parser/parser.cc"
    break;

  case 342:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 10264 "Parser/parser.cc"
    break;

  case 343:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10270 "Parser/parser.cc"
    break;

  case 346:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10276 "Parser/parser.cc"
    break;

  case 347:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 10285 "Parser/parser.cc"
    break;

  case 348:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10291 "Parser/parser.cc"
    break;

  case 349:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10297 "Parser/parser.cc"
    break;

  case 352:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10303 "Parser/parser.cc"
    break;

  case 353:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10309 "Parser/parser.cc"
    break;

  case 356:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10315 "Parser/parser.cc"
    break;

  case 357:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 10321 "Parser/parser.cc"
    break;

  case 358:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 359:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 360:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 361:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10345 "Parser/parser.cc"
    break;

  case 362:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 10351 "Parser/parser.cc"
    break;

  case 363:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10357 "Parser/parser.cc"
    break;

  case 364:
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 10363 "Parser/parser.cc"
    break;

  case 367:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10369 "Parser/parser.cc"
    break;

  case 368:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 369:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 10381 "Parser/parser.cc"
    break;

  case 370:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10387 "Parser/parser.cc"
    break;

  case 371:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 372:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 10399 "Parser/parser.cc"
    break;

  case 373:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 10405 "Parser/parser.cc"
    break;

  case 374:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10411 "Parser/parser.cc"
    break;

  case 375:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 10417 "Parser/parser.cc"
    break;

  case 376:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 10423 "Parser/parser.cc"
    break;

  case 377:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10429 "Parser/parser.cc"
    break;

  case 378:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 10435 "Parser/parser.cc"
    break;

  case 379:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 10441 "Parser/parser.cc"
    break;

  case 380:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 381:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 382:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 383:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10465 "Parser/parser.cc"
    break;

  case 384:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10471 "Parser/parser.cc"
    break;

  case 385:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10477 "Parser/parser.cc"
    break;

  case 386:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 10483 "Parser/parser.cc"
    break;

  case 387:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 10489 "Parser/parser.cc"
    break;

  case 388:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 10495 "Parser/parser.cc"
    break;

  case 389:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 391:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 392:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10513 "Parser/parser.cc"
    break;

  case 393:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10519 "Parser/parser.cc"
    break;

  case 398:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 10525 "Parser/parser.cc"
    break;

  case 399:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10531 "Parser/parser.cc"
    break;

  case 400:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10537 "Parser/parser.cc"
    break;

  case 401:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 10543 "Parser/parser.cc"
    break;

  case 402:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10549 "Parser/parser.cc"
    break;

  case 403:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10555 "Parser/parser.cc"
    break;

  case 404:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10561 "Parser/parser.cc"
    break;

  case 405:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10567 "Parser/parser.cc"
    break;

  case 408:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10573 "Parser/parser.cc"
    break;

  case 409:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10579 "Parser/parser.cc"
    break;

  case 410:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10588 "Parser/parser.cc"
    break;

  case 411:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10594 "Parser/parser.cc"
    break;

  case 412:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10600 "Parser/parser.cc"
    break;

  case 413:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10606 "Parser/parser.cc"
    break;

  case 414:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10615 "Parser/parser.cc"
    break;

  case 415:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10624 "Parser/parser.cc"
    break;

  case 416:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10630 "Parser/parser.cc"
    break;

  case 419:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 420:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10642 "Parser/parser.cc"
    break;

  case 422:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10648 "Parser/parser.cc"
    break;

  case 423:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 433:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 434:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 438:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10672 "Parser/parser.cc"
    break;

  case 440:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 441:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 442:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 443:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 444:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 445:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 446:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10714 "Parser/parser.cc"
    break;

  case 447:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10720 "Parser/parser.cc"
    break;

  case 449:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10726 "Parser/parser.cc"
    break;

  case 450:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 451:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10738 "Parser/parser.cc"
    break;

  case 452:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10749 "Parser/parser.cc"
    break;

  case 453:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10755 "Parser/parser.cc"
    break;

  case 454:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 455:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 456:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 457:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10779 "Parser/parser.cc"
    break;

  case 458:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) ) ); }
#line 10785 "Parser/parser.cc"
    break;

  case 459:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10794 "Parser/parser.cc"
    break;

  case 460:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10803 "Parser/parser.cc"
    break;

  case 461:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10812 "Parser/parser.cc"
    break;

  case 462:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10823 "Parser/parser.cc"
    break;

  case 463:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10832 "Parser/parser.cc"
    break;

  case 464:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10838 "Parser/parser.cc"
    break;

  case 465:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10844 "Parser/parser.cc"
    break;

  case 466:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10850 "Parser/parser.cc"
    break;

  case 467:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10858 "Parser/parser.cc"
    break;

  case 468:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10866 "Parser/parser.cc"
    break;

  case 469:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 472:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "syntax error, useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "syntax error, useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 10887 "Parser/parser.cc"
    break;

  case 473:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 474:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10899 "Parser/parser.cc"
    break;

  case 475:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10905 "Parser/parser.cc"
    break;

  case 476:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10911 "Parser/parser.cc"
    break;

  case 477:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10917 "Parser/parser.cc"
    break;

  case 483:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 10927 "Parser/parser.cc"
    break;

  case 496:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10933 "Parser/parser.cc"
    break;

  case 499:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10939 "Parser/parser.cc"
    break;

  case 500:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 10945 "Parser/parser.cc"
    break;

  case 502:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 10951 "Parser/parser.cc"
    break;

  case 503:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 10957 "Parser/parser.cc"
    break;

  case 504:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 10963 "Parser/parser.cc"
    break;

  case 505:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 10969 "Parser/parser.cc"
    break;

  case 506:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 507:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10981 "Parser/parser.cc"
    break;

  case 509:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 510:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 512:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 513:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 11005 "Parser/parser.cc"
    break;

  case 514:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 11011 "Parser/parser.cc"
    break;

  case 515:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 11017 "Parser/parser.cc"
    break;

  case 516:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 11023 "Parser/parser.cc"
    break;

  case 517:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 11029 "Parser/parser.cc"
    break;

  case 518:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 11035 "Parser/parser.cc"
    break;

  case 519:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 11041 "Parser/parser.cc"
    break;

  case 520:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 11047 "Parser/parser.cc"
    break;

  case 521:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 11053 "Parser/parser.cc"
    break;

  case 522:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 523:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 11065 "Parser/parser.cc"
    break;

  case 524:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 11071 "Parser/parser.cc"
    break;

  case 525:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 11077 "Parser/parser.cc"
    break;

  case 526:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 11083 "Parser/parser.cc"
    break;

  case 527:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 11089 "Parser/parser.cc"
    break;

  case 528:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 529:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 11101 "Parser/parser.cc"
    break;

  case 530:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 11107 "Parser/parser.cc"
    break;

  case 531:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat80 ); }
#line 11113 "Parser/parser.cc"
    break;

  case 532:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 11119 "Parser/parser.cc"
    break;

  case 533:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat16 ); }
#line 11125 "Parser/parser.cc"
    break;

  case 534:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32 ); }
#line 11131 "Parser/parser.cc"
    break;

  case 535:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat32x ); }
#line 11137 "Parser/parser.cc"
    break;

  case 536:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64 ); }
#line 11143 "Parser/parser.cc"
    break;

  case 537:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat64x ); }
#line 11149 "Parser/parser.cc"
    break;

  case 538:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uFloat128 ); }
#line 11155 "Parser/parser.cc"
    break;

  case 539:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11161 "Parser/parser.cc"
    break;

  case 540:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11167 "Parser/parser.cc"
    break;

  case 541:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11173 "Parser/parser.cc"
    break;

  case 542:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 11179 "Parser/parser.cc"
    break;

  case 543:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 11185 "Parser/parser.cc"
    break;

  case 544:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 11191 "Parser/parser.cc"
    break;

  case 545:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 11197 "Parser/parser.cc"
    break;

  case 546:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 11203 "Parser/parser.cc"
    break;

  case 547:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 11209 "Parser/parser.cc"
    break;

  case 548:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 11215 "Parser/parser.cc"
    break;

  case 549:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 11221 "Parser/parser.cc"
    break;

  case 551:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11227 "Parser/parser.cc"
    break;

  case 553:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 554:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 11239 "Parser/parser.cc"
    break;

  case 555:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 11245 "Parser/parser.cc"
    break;

  case 557:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 558:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 559:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 560:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 562:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 564:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 565:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 566:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 11293 "Parser/parser.cc"
    break;

  case 567:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11299 "Parser/parser.cc"
    break;

  case 568:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 11305 "Parser/parser.cc"
    break;

  case 569:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 11311 "Parser/parser.cc"
    break;

  case 570:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 11317 "Parser/parser.cc"
    break;

  case 571:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 572:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 574:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 575:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 576:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 578:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 11353 "Parser/parser.cc"
    break;

  case 579:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 580:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 11368 "Parser/parser.cc"
    break;

  case 582:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11374 "Parser/parser.cc"
    break;

  case 583:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 584:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11386 "Parser/parser.cc"
    break;

  case 586:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 587:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11398 "Parser/parser.cc"
    break;

  case 589:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 590:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11410 "Parser/parser.cc"
    break;

  case 591:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11416 "Parser/parser.cc"
    break;

  case 592:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 11422 "Parser/parser.cc"
    break;

  case 593:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11428 "Parser/parser.cc"
    break;

  case 594:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11434 "Parser/parser.cc"
    break;

  case 595:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 11440 "Parser/parser.cc"
    break;

  case 596:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 11446 "Parser/parser.cc"
    break;

  case 597:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 599:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 11458 "Parser/parser.cc"
    break;

  case 600:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 11464 "Parser/parser.cc"
    break;

  case 601:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 11470 "Parser/parser.cc"
    break;

  case 602:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 11476 "Parser/parser.cc"
    break;

  case 603:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11482 "Parser/parser.cc"
    break;

  case 608:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 11488 "Parser/parser.cc"
    break;

  case 609:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11494 "Parser/parser.cc"
    break;

  case 610:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 11503 "Parser/parser.cc"
    break;

  case 611:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11511 "Parser/parser.cc"
    break;

  case 612:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 11520 "Parser/parser.cc"
    break;

  case 613:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-5].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11529 "Parser/parser.cc"
    break;

  case 614:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 11538 "Parser/parser.cc"
    break;

  case 615:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-5].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11547 "Parser/parser.cc"
    break;

  case 617:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11553 "Parser/parser.cc"
    break;

  case 618:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11559 "Parser/parser.cc"
    break;

  case 619:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11569 "Parser/parser.cc"
    break;

  case 620:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( (yyvsp[0].type)->kind == TypeData::SymbolicInst && ! (yyvsp[0].type)->symbolic.isTypedef ) {
				(yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].type)->symbolic.name, (yyvsp[0].type)->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].type)->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].type)->symbolic.actuals = nullptr;
				delete (yyvsp[0].type);
			}
		}
#line 11588 "Parser/parser.cc"
    break;

  case 623:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11594 "Parser/parser.cc"
    break;

  case 624:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11600 "Parser/parser.cc"
    break;

  case 625:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11606 "Parser/parser.cc"
    break;

  case 626:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11612 "Parser/parser.cc"
    break;

  case 627:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11618 "Parser/parser.cc"
    break;

  case 628:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11624 "Parser/parser.cc"
    break;

  case 629:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11633 "Parser/parser.cc"
    break;

  case 630:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11639 "Parser/parser.cc"
    break;

  case 631:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11648 "Parser/parser.cc"
    break;

  case 632:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11654 "Parser/parser.cc"
    break;

  case 633:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11663 "Parser/parser.cc"
    break;

  case 634:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11669 "Parser/parser.cc"
    break;

  case 635:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11675 "Parser/parser.cc"
    break;

  case 636:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11688 "Parser/parser.cc"
    break;

  case 637:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntax error, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 11697 "Parser/parser.cc"
    break;

  case 638:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11703 "Parser/parser.cc"
    break;

  case 639:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11709 "Parser/parser.cc"
    break;

  case 640:
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11722 "Parser/parser.cc"
    break;

  case 641:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11728 "Parser/parser.cc"
    break;

  case 644:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11734 "Parser/parser.cc"
    break;

  case 645:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11740 "Parser/parser.cc"
    break;

  case 648:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11746 "Parser/parser.cc"
    break;

  case 650:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11752 "Parser/parser.cc"
    break;

  case 651:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11758 "Parser/parser.cc"
    break;

  case 652:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 653:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11770 "Parser/parser.cc"
    break;

  case 654:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 655:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11782 "Parser/parser.cc"
    break;

  case 657:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11788 "Parser/parser.cc"
    break;

  case 659:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11794 "Parser/parser.cc"
    break;

  case 660:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11800 "Parser/parser.cc"
    break;

  case 662:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11806 "Parser/parser.cc"
    break;

  case 663:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11812 "Parser/parser.cc"
    break;

  case 665:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11818 "Parser/parser.cc"
    break;

  case 666:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11829 "Parser/parser.cc"
    break;

  case 667:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl) && ((yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-4].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "syntax error, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11843 "Parser/parser.cc"
    break;

  case 668:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type 1" ); }
#line 11849 "Parser/parser.cc"
    break;

  case 669:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 670:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 671:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 11872 "Parser/parser.cc"
    break;

  case 672:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 673:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 675:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11890 "Parser/parser.cc"
    break;

  case 676:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11896 "Parser/parser.cc"
    break;

  case 677:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11902 "Parser/parser.cc"
    break;

  case 678:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11908 "Parser/parser.cc"
    break;

  case 679:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11917 "Parser/parser.cc"
    break;

  case 680:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11926 "Parser/parser.cc"
    break;

  case 681:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11932 "Parser/parser.cc"
    break;

  case 682:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 11942 "Parser/parser.cc"
    break;

  case 683:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11948 "Parser/parser.cc"
    break;

  case 684:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 11954 "Parser/parser.cc"
    break;

  case 686:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11960 "Parser/parser.cc"
    break;

  case 687:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11966 "Parser/parser.cc"
    break;

  case 688:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11972 "Parser/parser.cc"
    break;

  case 689:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11978 "Parser/parser.cc"
    break;

  case 690:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11984 "Parser/parser.cc"
    break;

  case 691:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11990 "Parser/parser.cc"
    break;

  case 693:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 11996 "Parser/parser.cc"
    break;

  case 696:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12002 "Parser/parser.cc"
    break;

  case 697:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12008 "Parser/parser.cc"
    break;

  case 698:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 12014 "Parser/parser.cc"
    break;

  case 699:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12020 "Parser/parser.cc"
    break;

  case 702:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12026 "Parser/parser.cc"
    break;

  case 703:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12032 "Parser/parser.cc"
    break;

  case 704:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 12038 "Parser/parser.cc"
    break;

  case 706:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12044 "Parser/parser.cc"
    break;

  case 707:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12050 "Parser/parser.cc"
    break;

  case 708:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 12056 "Parser/parser.cc"
    break;

  case 710:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12062 "Parser/parser.cc"
    break;

  case 711:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12068 "Parser/parser.cc"
    break;

  case 712:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12074 "Parser/parser.cc"
    break;

  case 713:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12080 "Parser/parser.cc"
    break;

  case 714:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 12086 "Parser/parser.cc"
    break;

  case 716:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12092 "Parser/parser.cc"
    break;

  case 717:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 12098 "Parser/parser.cc"
    break;

  case 718:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12104 "Parser/parser.cc"
    break;

  case 723:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12110 "Parser/parser.cc"
    break;

  case 725:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12116 "Parser/parser.cc"
    break;

  case 726:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 12122 "Parser/parser.cc"
    break;

  case 729:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12128 "Parser/parser.cc"
    break;

  case 732:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12134 "Parser/parser.cc"
    break;

  case 733:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 12140 "Parser/parser.cc"
    break;

  case 734:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 12146 "Parser/parser.cc"
    break;

  case 735:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12152 "Parser/parser.cc"
    break;

  case 736:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 12158 "Parser/parser.cc"
    break;

  case 737:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 12164 "Parser/parser.cc"
    break;

  case 738:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 12170 "Parser/parser.cc"
    break;

  case 740:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 12176 "Parser/parser.cc"
    break;

  case 741:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 12182 "Parser/parser.cc"
    break;

  case 742:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 12188 "Parser/parser.cc"
    break;

  case 744:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 12194 "Parser/parser.cc"
    break;

  case 746:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 12200 "Parser/parser.cc"
    break;

  case 747:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 12206 "Parser/parser.cc"
    break;

  case 748:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12212 "Parser/parser.cc"
    break;

  case 749:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12218 "Parser/parser.cc"
    break;

  case 750:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 12224 "Parser/parser.cc"
    break;

  case 751:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 12230 "Parser/parser.cc"
    break;

  case 753:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 12236 "Parser/parser.cc"
    break;

  case 754:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12242 "Parser/parser.cc"
    break;

  case 755:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12248 "Parser/parser.cc"
    break;

  case 756:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 12259 "Parser/parser.cc"
    break;

  case 757:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12265 "Parser/parser.cc"
    break;

  case 758:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 12271 "Parser/parser.cc"
    break;

  case 759:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 12277 "Parser/parser.cc"
    break;

  case 760:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 12286 "Parser/parser.cc"
    break;

  case 761:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 12292 "Parser/parser.cc"
    break;

  case 762:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 12302 "Parser/parser.cc"
    break;

  case 763:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12308 "Parser/parser.cc"
    break;

  case 764:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12314 "Parser/parser.cc"
    break;

  case 765:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 12320 "Parser/parser.cc"
    break;

  case 766:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12326 "Parser/parser.cc"
    break;

  case 767:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 12332 "Parser/parser.cc"
    break;

  case 768:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 12338 "Parser/parser.cc"
    break;

  case 769:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 12344 "Parser/parser.cc"
    break;

  case 770:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 12350 "Parser/parser.cc"
    break;

  case 771:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12356 "Parser/parser.cc"
    break;

  case 774:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12362 "Parser/parser.cc"
    break;

  case 775:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12368 "Parser/parser.cc"
    break;

  case 776:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12374 "Parser/parser.cc"
    break;

  case 777:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12380 "Parser/parser.cc"
    break;

  case 779:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 12386 "Parser/parser.cc"
    break;

  case 780:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12392 "Parser/parser.cc"
    break;

  case 781:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 12398 "Parser/parser.cc"
    break;

  case 782:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12404 "Parser/parser.cc"
    break;

  case 783:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 12410 "Parser/parser.cc"
    break;

  case 784:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 12416 "Parser/parser.cc"
    break;

  case 785:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 12422 "Parser/parser.cc"
    break;

  case 786:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 12431 "Parser/parser.cc"
    break;

  case 787:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 12440 "Parser/parser.cc"
    break;

  case 788:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 12449 "Parser/parser.cc"
    break;

  case 789:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 12455 "Parser/parser.cc"
    break;

  case 790:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 12464 "Parser/parser.cc"
    break;

  case 791:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 12470 "Parser/parser.cc"
    break;

  case 793:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 12476 "Parser/parser.cc"
    break;

  case 798:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 12482 "Parser/parser.cc"
    break;

  case 799:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12488 "Parser/parser.cc"
    break;

  case 800:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 12494 "Parser/parser.cc"
    break;

  case 802:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 12500 "Parser/parser.cc"
    break;

  case 803:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12506 "Parser/parser.cc"
    break;

  case 804:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 12512 "Parser/parser.cc"
    break;

  case 805:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12518 "Parser/parser.cc"
    break;

  case 807:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 12524 "Parser/parser.cc"
    break;

  case 808:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 12530 "Parser/parser.cc"
    break;

  case 809:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 12536 "Parser/parser.cc"
    break;

  case 810:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == ast::Linkage::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static &&
				 (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) {
					SemanticError( yylloc, "extern anonymous aggregate is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 12552 "Parser/parser.cc"
    break;

  case 811:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 12558 "Parser/parser.cc"
    break;

  case 812:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 12564 "Parser/parser.cc"
    break;

  case 813:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12570 "Parser/parser.cc"
    break;

  case 814:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12576 "Parser/parser.cc"
    break;

  case 815:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12582 "Parser/parser.cc"
    break;

  case 816:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12588 "Parser/parser.cc"
    break;

  case 818:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12597 "Parser/parser.cc"
    break;

  case 819:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12603 "Parser/parser.cc"
    break;

  case 820:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12612 "Parser/parser.cc"
    break;

  case 821:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12622 "Parser/parser.cc"
    break;

  case 822:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12631 "Parser/parser.cc"
    break;

  case 823:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12641 "Parser/parser.cc"
    break;

  case 824:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12652 "Parser/parser.cc"
    break;

  case 825:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12662 "Parser/parser.cc"
    break;

  case 826:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12673 "Parser/parser.cc"
    break;

  case 827:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12683 "Parser/parser.cc"
    break;

  case 828:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12694 "Parser/parser.cc"
    break;

  case 829:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12704 "Parser/parser.cc"
    break;

  case 831:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12710 "Parser/parser.cc"
    break;

  case 832:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12716 "Parser/parser.cc"
    break;

  case 833:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12722 "Parser/parser.cc"
    break;

  case 834:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12734 "Parser/parser.cc"
    break;

  case 835:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12745 "Parser/parser.cc"
    break;

  case 836:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12754 "Parser/parser.cc"
    break;

  case 837:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12763 "Parser/parser.cc"
    break;

  case 838:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 839:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 840:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 841:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12790 "Parser/parser.cc"
    break;

  case 842:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12796 "Parser/parser.cc"
    break;

  case 843:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12802 "Parser/parser.cc"
    break;

  case 844:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12808 "Parser/parser.cc"
    break;

  case 849:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12814 "Parser/parser.cc"
    break;

  case 850:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12820 "Parser/parser.cc"
    break;

  case 851:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12830 "Parser/parser.cc"
    break;

  case 852:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12836 "Parser/parser.cc"
    break;

  case 855:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 856:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12848 "Parser/parser.cc"
    break;

  case 857:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12854 "Parser/parser.cc"
    break;

  case 858:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12860 "Parser/parser.cc"
    break;

  case 860:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 861:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12872 "Parser/parser.cc"
    break;

  case 862:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 863:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12884 "Parser/parser.cc"
    break;

  case 865:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12890 "Parser/parser.cc"
    break;

  case 866:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12896 "Parser/parser.cc"
    break;

  case 867:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12902 "Parser/parser.cc"
    break;

  case 868:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12908 "Parser/parser.cc"
    break;

  case 869:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 871:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12920 "Parser/parser.cc"
    break;

  case 872:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 873:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12932 "Parser/parser.cc"
    break;

  case 874:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 875:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 876:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 877:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 878:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 879:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 880:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12974 "Parser/parser.cc"
    break;

  case 881:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 882:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12986 "Parser/parser.cc"
    break;

  case 883:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 884:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 885:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 886:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13010 "Parser/parser.cc"
    break;

  case 887:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 888:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 890:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 891:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13034 "Parser/parser.cc"
    break;

  case 892:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13040 "Parser/parser.cc"
    break;

  case 893:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13046 "Parser/parser.cc"
    break;

  case 894:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13052 "Parser/parser.cc"
    break;

  case 895:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13058 "Parser/parser.cc"
    break;

  case 896:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13064 "Parser/parser.cc"
    break;

  case 897:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 898:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13076 "Parser/parser.cc"
    break;

  case 899:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13082 "Parser/parser.cc"
    break;

  case 900:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13088 "Parser/parser.cc"
    break;

  case 901:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13094 "Parser/parser.cc"
    break;

  case 902:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13100 "Parser/parser.cc"
    break;

  case 903:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13106 "Parser/parser.cc"
    break;

  case 904:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13112 "Parser/parser.cc"
    break;

  case 905:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13118 "Parser/parser.cc"
    break;

  case 909:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 13124 "Parser/parser.cc"
    break;

  case 910:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13130 "Parser/parser.cc"
    break;

  case 911:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13136 "Parser/parser.cc"
    break;

  case 912:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13142 "Parser/parser.cc"
    break;

  case 913:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13148 "Parser/parser.cc"
    break;

  case 914:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13154 "Parser/parser.cc"
    break;

  case 915:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13160 "Parser/parser.cc"
    break;

  case 916:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13166 "Parser/parser.cc"
    break;

  case 917:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 918:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 919:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 920:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 921:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 922:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13202 "Parser/parser.cc"
    break;

  case 923:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 924:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 13217 "Parser/parser.cc"
    break;

  case 925:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13223 "Parser/parser.cc"
    break;

  case 926:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13229 "Parser/parser.cc"
    break;

  case 928:
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13235 "Parser/parser.cc"
    break;

  case 929:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13241 "Parser/parser.cc"
    break;

  case 930:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13247 "Parser/parser.cc"
    break;

  case 931:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13253 "Parser/parser.cc"
    break;

  case 932:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13259 "Parser/parser.cc"
    break;

  case 933:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13265 "Parser/parser.cc"
    break;

  case 934:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13271 "Parser/parser.cc"
    break;

  case 935:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13277 "Parser/parser.cc"
    break;

  case 936:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13283 "Parser/parser.cc"
    break;

  case 937:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13289 "Parser/parser.cc"
    break;

  case 938:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13295 "Parser/parser.cc"
    break;

  case 939:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13301 "Parser/parser.cc"
    break;

  case 940:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13307 "Parser/parser.cc"
    break;

  case 941:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13313 "Parser/parser.cc"
    break;

  case 942:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13319 "Parser/parser.cc"
    break;

  case 943:
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13325 "Parser/parser.cc"
    break;

  case 944:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13331 "Parser/parser.cc"
    break;

  case 945:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13337 "Parser/parser.cc"
    break;

  case 947:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13343 "Parser/parser.cc"
    break;

  case 948:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13349 "Parser/parser.cc"
    break;

  case 949:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13355 "Parser/parser.cc"
    break;

  case 950:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 13361 "Parser/parser.cc"
    break;

  case 951:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13367 "Parser/parser.cc"
    break;

  case 952:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13373 "Parser/parser.cc"
    break;

  case 953:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13379 "Parser/parser.cc"
    break;

  case 954:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13385 "Parser/parser.cc"
    break;

  case 955:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13391 "Parser/parser.cc"
    break;

  case 956:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13397 "Parser/parser.cc"
    break;

  case 957:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13403 "Parser/parser.cc"
    break;

  case 958:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13409 "Parser/parser.cc"
    break;

  case 959:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13415 "Parser/parser.cc"
    break;

  case 960:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 13421 "Parser/parser.cc"
    break;

  case 961:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13427 "Parser/parser.cc"
    break;

  case 962:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13433 "Parser/parser.cc"
    break;

  case 963:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13439 "Parser/parser.cc"
    break;

  case 964:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13446 "Parser/parser.cc"
    break;

  case 966:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13452 "Parser/parser.cc"
    break;

  case 967:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13458 "Parser/parser.cc"
    break;

  case 968:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13464 "Parser/parser.cc"
    break;

  case 969:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13470 "Parser/parser.cc"
    break;

  case 970:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13476 "Parser/parser.cc"
    break;

  case 971:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13482 "Parser/parser.cc"
    break;

  case 972:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13488 "Parser/parser.cc"
    break;

  case 973:
#line 3857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13494 "Parser/parser.cc"
    break;

  case 974:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13500 "Parser/parser.cc"
    break;

  case 975:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13506 "Parser/parser.cc"
    break;

  case 976:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13512 "Parser/parser.cc"
    break;

  case 977:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13518 "Parser/parser.cc"
    break;

  case 978:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13524 "Parser/parser.cc"
    break;

  case 979:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13531 "Parser/parser.cc"
    break;

  case 981:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 982:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13543 "Parser/parser.cc"
    break;

  case 983:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13549 "Parser/parser.cc"
    break;

  case 984:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13555 "Parser/parser.cc"
    break;

  case 985:
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 986:
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 987:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 988:
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13579 "Parser/parser.cc"
    break;

  case 989:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13585 "Parser/parser.cc"
    break;

  case 990:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13591 "Parser/parser.cc"
    break;

  case 991:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13597 "Parser/parser.cc"
    break;

  case 993:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13603 "Parser/parser.cc"
    break;

  case 994:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13609 "Parser/parser.cc"
    break;

  case 995:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13615 "Parser/parser.cc"
    break;

  case 996:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13621 "Parser/parser.cc"
    break;

  case 997:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13627 "Parser/parser.cc"
    break;

  case 998:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13633 "Parser/parser.cc"
    break;

  case 999:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13639 "Parser/parser.cc"
    break;

  case 1001:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13645 "Parser/parser.cc"
    break;

  case 1002:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13651 "Parser/parser.cc"
    break;

  case 1003:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13657 "Parser/parser.cc"
    break;

  case 1004:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13663 "Parser/parser.cc"
    break;

  case 1005:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13669 "Parser/parser.cc"
    break;

  case 1006:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13675 "Parser/parser.cc"
    break;

  case 1007:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13681 "Parser/parser.cc"
    break;

  case 1008:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13687 "Parser/parser.cc"
    break;

  case 1009:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13693 "Parser/parser.cc"
    break;

  case 1010:
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13699 "Parser/parser.cc"
    break;

  case 1012:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13705 "Parser/parser.cc"
    break;

  case 1013:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 13711 "Parser/parser.cc"
    break;

  case 1015:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13717 "Parser/parser.cc"
    break;

  case 1016:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 13723 "Parser/parser.cc"
    break;

  case 1018:
#line 4014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13729 "Parser/parser.cc"
    break;

  case 1019:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13735 "Parser/parser.cc"
    break;

  case 1020:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13741 "Parser/parser.cc"
    break;

  case 1021:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13747 "Parser/parser.cc"
    break;

  case 1022:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13753 "Parser/parser.cc"
    break;

  case 1023:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13759 "Parser/parser.cc"
    break;

  case 1024:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13765 "Parser/parser.cc"
    break;

  case 1027:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13772 "Parser/parser.cc"
    break;

  case 1028:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13778 "Parser/parser.cc"
    break;

  case 1029:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13784 "Parser/parser.cc"
    break;

  case 1030:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13790 "Parser/parser.cc"
    break;

  case 1031:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13796 "Parser/parser.cc"
    break;

  case 1032:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13802 "Parser/parser.cc"
    break;

  case 1033:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13808 "Parser/parser.cc"
    break;

  case 1034:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13814 "Parser/parser.cc"
    break;

  case 1036:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13820 "Parser/parser.cc"
    break;

  case 1037:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13826 "Parser/parser.cc"
    break;

  case 1038:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13832 "Parser/parser.cc"
    break;

  case 1039:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 13838 "Parser/parser.cc"
    break;

  case 1040:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13844 "Parser/parser.cc"
    break;

  case 1041:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13850 "Parser/parser.cc"
    break;

  case 1043:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13856 "Parser/parser.cc"
    break;

  case 1045:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13862 "Parser/parser.cc"
    break;

  case 1046:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13868 "Parser/parser.cc"
    break;

  case 1047:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13874 "Parser/parser.cc"
    break;

  case 1048:
#line 4131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13880 "Parser/parser.cc"
    break;

  case 1049:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13886 "Parser/parser.cc"
    break;

  case 1050:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13892 "Parser/parser.cc"
    break;

  case 1052:
#line 4150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13898 "Parser/parser.cc"
    break;

  case 1053:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13904 "Parser/parser.cc"
    break;

  case 1054:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 1055:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 1056:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13922 "Parser/parser.cc"
    break;

  case 1057:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13928 "Parser/parser.cc"
    break;

  case 1058:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13934 "Parser/parser.cc"
    break;

  case 1060:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13940 "Parser/parser.cc"
    break;

  case 1061:
#line 4173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13946 "Parser/parser.cc"
    break;

  case 1062:
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13952 "Parser/parser.cc"
    break;

  case 1063:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 13958 "Parser/parser.cc"
    break;

  case 1064:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13964 "Parser/parser.cc"
    break;

  case 1067:
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13970 "Parser/parser.cc"
    break;

  case 1070:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 1071:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13982 "Parser/parser.cc"
    break;

  case 1072:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13988 "Parser/parser.cc"
    break;

  case 1073:
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13994 "Parser/parser.cc"
    break;

  case 1074:
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14000 "Parser/parser.cc"
    break;

  case 1075:
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14006 "Parser/parser.cc"
    break;

  case 1076:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14012 "Parser/parser.cc"
    break;

  case 1077:
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14018 "Parser/parser.cc"
    break;

  case 1078:
#line 4224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14024 "Parser/parser.cc"
    break;

  case 1079:
#line 4226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14030 "Parser/parser.cc"
    break;

  case 1080:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14036 "Parser/parser.cc"
    break;

  case 1081:
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14042 "Parser/parser.cc"
    break;

  case 1082:
#line 4233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14048 "Parser/parser.cc"
    break;

  case 1083:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14054 "Parser/parser.cc"
    break;

  case 1084:
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 14060 "Parser/parser.cc"
    break;

  case 1085:
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14066 "Parser/parser.cc"
    break;

  case 1086:
#line 4244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 14072 "Parser/parser.cc"
    break;

  case 1087:
#line 4246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 14078 "Parser/parser.cc"
    break;

  case 1088:
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 14084 "Parser/parser.cc"
    break;

  case 1089:
#line 4253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 14090 "Parser/parser.cc"
    break;

  case 1091:
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14096 "Parser/parser.cc"
    break;

  case 1095:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14102 "Parser/parser.cc"
    break;

  case 1096:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14108 "Parser/parser.cc"
    break;

  case 1097:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14114 "Parser/parser.cc"
    break;

  case 1098:
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14120 "Parser/parser.cc"
    break;

  case 1099:
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14126 "Parser/parser.cc"
    break;

  case 1100:
#line 4301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 14132 "Parser/parser.cc"
    break;

  case 1101:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14138 "Parser/parser.cc"
    break;

  case 1102:
#line 4310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14144 "Parser/parser.cc"
    break;

  case 1103:
#line 4312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14150 "Parser/parser.cc"
    break;

  case 1104:
#line 4314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14156 "Parser/parser.cc"
    break;

  case 1105:
#line 4316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 14162 "Parser/parser.cc"
    break;

  case 1106:
#line 4318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 14168 "Parser/parser.cc"
    break;

  case 1107:
#line 4323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 14174 "Parser/parser.cc"
    break;

  case 1108:
#line 4325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14180 "Parser/parser.cc"
    break;

  case 1109:
#line 4327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 14186 "Parser/parser.cc"
    break;

  case 1110:
#line 4332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 14192 "Parser/parser.cc"
    break;

  case 1111:
#line 4334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14198 "Parser/parser.cc"
    break;

  case 1112:
#line 4336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 14204 "Parser/parser.cc"
    break;

  case 1115:
#line 4360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 14210 "Parser/parser.cc"
    break;

  case 1116:
#line 4362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 14216 "Parser/parser.cc"
    break;


#line 14220 "Parser/parser.cc"

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
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
