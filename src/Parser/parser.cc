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
#line 34 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
#include "AST/Attribute.hpp"         // for Attribute
#include "AST/Print.hpp"             // for print
#include "Common/Iterate.hpp"        // for reverseIterate

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

DeclarationNode * distTypeSpec( DeclarationNode * typeSpec, DeclarationNode * declList ) {
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
} // distTypeSpec

void distAttr( DeclarationNode * attributes, DeclarationNode * declaration ) {
	// distribute attributes across all declaring list
	for ( DeclarationNode * attr = attributes; attr != nullptr ; attr = attr->next ) {
		for ( DeclarationNode * decl = declaration ; decl != nullptr ; decl = decl->next ) {
			decl->attributes.insert( decl->attributes.begin(), attr->attributes.begin(), attr->attributes.end() );
		} // for
	} // for
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode * decl = declaration ; decl != nullptr ; decl = decl->next ) {
		decl->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute INLINE across all declarations
	for ( DeclarationNode *decl = declaration ; decl != nullptr ; decl = decl->next ) {
		decl->set_inLine( true );
	} // for
} // distInl

void distQual( DeclarationNode * declaration, DeclarationNode * qualifiers ) {
	// distribute qualifiers across all non-variable declarations in a distribution statemement
	for ( DeclarationNode * decl = declaration ; decl != nullptr ; decl = decl->next ) {
		// SKULLDUGGERY: Distributions are parsed inside out, so qualifiers are added to declarations inside out. Since
		// addQualifiers appends to the back of the list, the forall clauses are in the wrong order (right to left). To
		// get the qualifiers in the correct order and still use addQualifiers (otherwise, 90% of addQualifiers has to
		// be copied to add to front), the appropriate forall pointers are interchanged before calling addQualifiers.
		DeclarationNode * clone = qualifiers->clone();
		if ( qualifiers->type ) {						// forall clause ? (handles SC)
			if ( decl->type->kind == TypeData::Aggregate ) { // struct/union ?
				swap( clone->type->forall, decl->type->aggregate.params );
				decl->addQualifiers( clone );
			} else if ( decl->type->kind == TypeData::AggregateInst && decl->type->aggInst.aggregate->aggregate.body ) { // struct/union ?
				// Create temporary node to hold aggregate, call addQualifiers as above, then put nodes back together.
				DeclarationNode newnode;
				swap( newnode.type, decl->type->aggInst.aggregate );
				swap( clone->type->forall, newnode.type->aggregate.params );
				newnode.addQualifiers( clone );
				swap( newnode.type, decl->type->aggInst.aggregate );
			} else if ( decl->type->kind == TypeData::Function ) { // routines ?
				swap( clone->type->forall, decl->type->forall );
				decl->addQualifiers( clone );
			} // if
		} else {										// just SC qualifiers
			decl->addQualifiers( clone );
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
	if ( fieldList == nullptr ) {
		if ( !( typeSpec->type && typeSpec->type->kind == TypeData::Aggregate ) ) { // int; no fieldList
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr ); // struct S { ... } no fieldList
	} // if

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distTypeSpec( typeSpec, fieldList ); // mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( yylloc, *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( yylloc, *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan || compop == OperKinds::Neq ? left : right)
#define MISSING_ANON_FIELD "illegal syntax, missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "illegal syntax, missing low value for ascanding range so index is uninitialized."
#define MISSING_HIGH "illegal syntax, missing high value for descending range so index is uninitialized."

static ForCtrl * makeForCtrl( const CodeLocation & location, DeclarationNode * init, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val( location,
		compop,
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val( location,
		// choose += or -= for upto/downto
		UPDOWN( compop, OperKinds::PlusAssn, OperKinds::MinusAssn ),
		new ExpressionNode( build_varref( location, new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( const CodeLocation & location, DeclarationNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "illegal syntax, direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "illegal syntax, multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, string * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ast::ConstantExpr * constant = dynamic_cast<ast::ConstantExpr *>(type->expr.get());
	if ( constant && (constant->rep == "0" || constant->rep == "1") ) {
		type = new ExpressionNode( new ast::CastExpr( location, maybeMoveBuild(type), new ast::BasicType( ast::BasicKind::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distTypeSpec(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( location, initDecl, compop, comp, inc );
} // forCtrl

#define MISSING_LOOP_INDEX "illegal syntax, only a single identifier or declaration allowed in initialization, e.g., for ( i; ... ) or for ( int i; ... ). Expression disallowed."

ForCtrl * forCtrl( const CodeLocation & location, ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index->expr.get()) ) {
		return forCtrl( location, type, new string( identifier->name ), start, compop, comp, inc );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // forCtrl

ForCtrl * enumRangeCtrl( ExpressionNode * index_expr, OperKinds compop, ExpressionNode * range_over_expr, DeclarationNode * type ) {
	assert( compop == OperKinds::LEThan || compop == OperKinds::GEThan );
	if ( auto identifier = dynamic_cast<ast::NameExpr *>(index_expr->expr.get()) ) {
		DeclarationNode * indexDecl =
			DeclarationNode::newName( new std::string(identifier->name) )->addType( type );
		return new ForCtrl( new StatementNode( indexDecl ), range_over_expr, compop );
	} else {
		SemanticError( yylloc, MISSING_LOOP_INDEX ); return nullptr;
	} // if
} // enumRangeCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, adjacent identifiers \"%s\" and \"%s\" are not meaningful in an %s.\n"
				   "Possible cause is misspelled type name or missing generic parameter.",
				   identifier1.c_str(), identifier2.c_str(), kind );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, "illegal syntax, identifier \"%s\" cannot appear before a %s.\n"
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

#line 360 "Parser/parser.cc"

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
    FLOAT80 = 291,
    uuFLOAT128 = 292,
    FLOAT16 = 293,
    FLOAT32 = 294,
    FLOAT32X = 295,
    FLOAT64 = 296,
    FLOAT64X = 297,
    FLOAT128 = 298,
    FLOAT128X = 299,
    FLOAT32X4 = 300,
    FLOAT64X2 = 301,
    SVFLOAT32 = 302,
    SVFLOAT64 = 303,
    SVBOOL = 304,
    DECIMAL32 = 305,
    DECIMAL64 = 306,
    DECIMAL128 = 307,
    ZERO_T = 308,
    ONE_T = 309,
    SIZEOF = 310,
    TYPEOF = 311,
    VA_LIST = 312,
    VA_ARG = 313,
    AUTO_TYPE = 314,
    COUNTOF = 315,
    OFFSETOF = 316,
    BASETYPEOF = 317,
    TYPEID = 318,
    ENUM = 319,
    STRUCT = 320,
    UNION = 321,
    EXCEPTION = 322,
    GENERATOR = 323,
    COROUTINE = 324,
    MONITOR = 325,
    THREAD = 326,
    OTYPE = 327,
    FTYPE = 328,
    DTYPE = 329,
    TTYPE = 330,
    TRAIT = 331,
    LABEL = 332,
    SUSPEND = 333,
    ATTRIBUTE = 334,
    EXTENSION = 335,
    IF = 336,
    ELSE = 337,
    SWITCH = 338,
    CASE = 339,
    DEFAULT = 340,
    DO = 341,
    WHILE = 342,
    FOR = 343,
    BREAK = 344,
    CONTINUE = 345,
    GOTO = 346,
    RETURN = 347,
    CHOOSE = 348,
    FALLTHROUGH = 349,
    WITH = 350,
    WHEN = 351,
    WAITFOR = 352,
    WAITUNTIL = 353,
    CORUN = 354,
    COFOR = 355,
    DISABLE = 356,
    ENABLE = 357,
    TRY = 358,
    THROW = 359,
    THROWRESUME = 360,
    AT = 361,
    ASM = 362,
    ALIGNAS = 363,
    ALIGNOF = 364,
    GENERIC = 365,
    STATICASSERT = 366,
    IDENTIFIER = 367,
    TYPEDIMname = 368,
    TYPEDEFname = 369,
    TYPEGENname = 370,
    TIMEOUT = 371,
    WAND = 372,
    WOR = 373,
    CATCH = 374,
    RECOVER = 375,
    CATCHRESUME = 376,
    FIXUP = 377,
    FINALLY = 378,
    INTEGERconstant = 379,
    CHARACTERconstant = 380,
    STRINGliteral = 381,
    DIRECTIVE = 382,
    C23_ATTRIBUTE = 383,
    FLOATING_DECIMALconstant = 384,
    FLOATING_FRACTIONconstant = 385,
    FLOATINGconstant = 386,
    ARROW = 387,
    ICR = 388,
    DECR = 389,
    LS = 390,
    RS = 391,
    LE = 392,
    GE = 393,
    EQ = 394,
    NE = 395,
    ANDAND = 396,
    OROR = 397,
    ATTR = 398,
    ELLIPSIS = 399,
    EXPassign = 400,
    MULTassign = 401,
    DIVassign = 402,
    MODassign = 403,
    PLUSassign = 404,
    MINUSassign = 405,
    LSassign = 406,
    RSassign = 407,
    ANDassign = 408,
    ERassign = 409,
    ORassign = 410,
    ErangeUpLt = 411,
    ErangeUpLe = 412,
    ErangeEq = 413,
    ErangeNe = 414,
    ErangeDownGt = 415,
    ErangeDownGe = 416,
    ErangeDownEq = 417,
    ErangeDownNe = 418,
    ATassign = 419,
    THEN = 420
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
#define FLOAT80 291
#define uuFLOAT128 292
#define FLOAT16 293
#define FLOAT32 294
#define FLOAT32X 295
#define FLOAT64 296
#define FLOAT64X 297
#define FLOAT128 298
#define FLOAT128X 299
#define FLOAT32X4 300
#define FLOAT64X2 301
#define SVFLOAT32 302
#define SVFLOAT64 303
#define SVBOOL 304
#define DECIMAL32 305
#define DECIMAL64 306
#define DECIMAL128 307
#define ZERO_T 308
#define ONE_T 309
#define SIZEOF 310
#define TYPEOF 311
#define VA_LIST 312
#define VA_ARG 313
#define AUTO_TYPE 314
#define COUNTOF 315
#define OFFSETOF 316
#define BASETYPEOF 317
#define TYPEID 318
#define ENUM 319
#define STRUCT 320
#define UNION 321
#define EXCEPTION 322
#define GENERATOR 323
#define COROUTINE 324
#define MONITOR 325
#define THREAD 326
#define OTYPE 327
#define FTYPE 328
#define DTYPE 329
#define TTYPE 330
#define TRAIT 331
#define LABEL 332
#define SUSPEND 333
#define ATTRIBUTE 334
#define EXTENSION 335
#define IF 336
#define ELSE 337
#define SWITCH 338
#define CASE 339
#define DEFAULT 340
#define DO 341
#define WHILE 342
#define FOR 343
#define BREAK 344
#define CONTINUE 345
#define GOTO 346
#define RETURN 347
#define CHOOSE 348
#define FALLTHROUGH 349
#define WITH 350
#define WHEN 351
#define WAITFOR 352
#define WAITUNTIL 353
#define CORUN 354
#define COFOR 355
#define DISABLE 356
#define ENABLE 357
#define TRY 358
#define THROW 359
#define THROWRESUME 360
#define AT 361
#define ASM 362
#define ALIGNAS 363
#define ALIGNOF 364
#define GENERIC 365
#define STATICASSERT 366
#define IDENTIFIER 367
#define TYPEDIMname 368
#define TYPEDEFname 369
#define TYPEGENname 370
#define TIMEOUT 371
#define WAND 372
#define WOR 373
#define CATCH 374
#define RECOVER 375
#define CATCHRESUME 376
#define FIXUP 377
#define FINALLY 378
#define INTEGERconstant 379
#define CHARACTERconstant 380
#define STRINGliteral 381
#define DIRECTIVE 382
#define C23_ATTRIBUTE 383
#define FLOATING_DECIMALconstant 384
#define FLOATING_FRACTIONconstant 385
#define FLOATINGconstant 386
#define ARROW 387
#define ICR 388
#define DECR 389
#define LS 390
#define RS 391
#define LE 392
#define GE 393
#define EQ 394
#define NE 395
#define ANDAND 396
#define OROR 397
#define ATTR 398
#define ELLIPSIS 399
#define EXPassign 400
#define MULTassign 401
#define DIVassign 402
#define MODassign 403
#define PLUSassign 404
#define MINUSassign 405
#define LSassign 406
#define RSassign 407
#define ANDassign 408
#define ERassign 409
#define ORassign 410
#define ErangeUpLt 411
#define ErangeUpLe 412
#define ErangeEq 413
#define ErangeNe 414
#define ErangeDownGt 415
#define ErangeDownGe 416
#define ErangeDownEq 417
#define ErangeDownNe 418
#define ATassign 419
#define THEN 420

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	CondCtrl * ifctrl;
	ForCtrl * forctrl;
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

#line 775 "Parser/parser.cc"

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
#define YYFINAL  29
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   31971

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  193
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1156
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2270

#define YYUNDEFTOK  2
#define YYMAXUTOK   420


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
       2,     2,     2,   182,     2,     2,     2,   186,   179,     2,
     167,   169,   178,   180,   173,   181,   170,   185,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   174,   192,
     187,   191,   188,   190,   168,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   171,   184,   172,   177,     2,   176,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   175,   189,   166,   183,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   653,   653,   657,   664,   665,   666,   667,   668,   672,
     673,   674,   675,   676,   677,   678,   679,   683,   684,   688,
     689,   694,   695,   696,   700,   704,   705,   716,   718,   720,
     722,   723,   725,   727,   729,   731,   741,   743,   745,   747,
     749,   751,   756,   757,   768,   773,   778,   779,   784,   790,
     792,   794,   800,   802,   804,   806,   808,   828,   831,   833,
     835,   837,   839,   841,   843,   845,   847,   849,   851,   853,
     862,   863,   867,   868,   870,   872,   874,   876,   878,   883,
     885,   887,   895,   896,   904,   907,   908,   910,   915,   931,
     933,   935,   937,   939,   941,   943,   945,   950,   952,   955,
     957,   962,   964,   969,   970,   972,   976,   977,   978,   979,
     983,   984,   986,   988,   990,   992,   994,   996,   998,  1005,
    1006,  1007,  1008,  1012,  1013,  1017,  1018,  1023,  1024,  1026,
    1028,  1033,  1034,  1036,  1041,  1042,  1044,  1049,  1050,  1052,
    1054,  1056,  1061,  1062,  1064,  1069,  1070,  1075,  1076,  1081,
    1082,  1087,  1088,  1093,  1094,  1099,  1100,  1102,  1107,  1112,
    1113,  1117,  1119,  1124,  1127,  1130,  1135,  1136,  1144,  1150,
    1151,  1155,  1156,  1160,  1161,  1165,  1166,  1167,  1168,  1169,
    1170,  1171,  1172,  1173,  1174,  1175,  1181,  1184,  1186,  1188,
    1190,  1195,  1196,  1198,  1200,  1205,  1206,  1212,  1213,  1219,
    1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,  1228,  1229,
    1230,  1231,  1232,  1234,  1235,  1241,  1243,  1253,  1255,  1263,
    1264,  1269,  1271,  1273,  1275,  1277,  1282,  1284,  1286,  1292,
    1321,  1324,  1326,  1328,  1338,  1340,  1342,  1347,  1352,  1354,
    1356,  1358,  1366,  1367,  1369,  1373,  1375,  1379,  1381,  1382,
    1384,  1386,  1391,  1392,  1396,  1401,  1402,  1406,  1408,  1413,
    1415,  1420,  1422,  1424,  1426,  1431,  1433,  1435,  1437,  1442,
    1444,  1449,  1450,  1472,  1474,  1478,  1481,  1483,  1486,  1488,
    1491,  1493,  1498,  1504,  1506,  1511,  1516,  1518,  1520,  1522,
    1524,  1529,  1531,  1534,  1536,  1541,  1547,  1550,  1553,  1555,
    1560,  1566,  1568,  1573,  1579,  1582,  1584,  1587,  1589,  1594,
    1601,  1604,  1606,  1611,  1617,  1619,  1624,  1630,  1633,  1637,
    1648,  1653,  1658,  1669,  1671,  1673,  1675,  1680,  1682,  1686,
    1688,  1690,  1692,  1697,  1699,  1704,  1706,  1708,  1710,  1713,
    1717,  1720,  1724,  1726,  1728,  1730,  1732,  1734,  1736,  1738,
    1740,  1742,  1744,  1749,  1755,  1763,  1768,  1769,  1773,  1774,
    1779,  1783,  1784,  1787,  1789,  1794,  1797,  1799,  1801,  1804,
    1806,  1811,  1816,  1817,  1821,  1826,  1828,  1833,  1835,  1840,
    1842,  1844,  1849,  1854,  1859,  1864,  1866,  1868,  1873,  1875,
    1881,  1882,  1886,  1887,  1888,  1889,  1893,  1898,  1899,  1901,
    1903,  1905,  1909,  1913,  1914,  1918,  1920,  1922,  1924,  1926,
    1932,  1933,  1939,  1940,  1944,  1945,  1950,  1952,  1961,  1962,
    1964,  1969,  1971,  1979,  1980,  1984,  1986,  1992,  1993,  1997,
    1999,  2003,  2005,  2009,  2010,  2014,  2015,  2019,  2020,  2021,
    2025,  2027,  2042,  2043,  2044,  2045,  2047,  2051,  2053,  2057,
    2064,  2066,  2068,  2070,  2078,  2080,  2085,  2086,  2088,  2090,
    2092,  2102,  2104,  2116,  2119,  2124,  2126,  2132,  2137,  2142,
    2153,  2160,  2165,  2167,  2169,  2175,  2177,  2182,  2184,  2185,
    2186,  2202,  2204,  2207,  2209,  2212,  2217,  2218,  2222,  2223,
    2224,  2225,  2234,  2235,  2236,  2245,  2246,  2247,  2251,  2252,
    2253,  2262,  2263,  2264,  2269,  2270,  2279,  2281,  2286,  2291,
    2293,  2295,  2297,  2304,  2309,  2314,  2315,  2317,  2327,  2329,
    2334,  2336,  2338,  2340,  2342,  2344,  2347,  2349,  2351,  2356,
    2362,  2364,  2366,  2368,  2370,  2372,  2374,  2376,  2378,  2380,
    2382,  2384,  2386,  2388,  2390,  2392,  2395,  2397,  2399,  2401,
    2403,  2405,  2407,  2409,  2411,  2413,  2415,  2417,  2419,  2421,
    2423,  2425,  2427,  2429,  2434,  2435,  2439,  2445,  2446,  2452,
    2453,  2455,  2457,  2459,  2464,  2467,  2469,  2474,  2475,  2477,
    2479,  2484,  2486,  2488,  2490,  2492,  2494,  2499,  2500,  2502,
    2504,  2509,  2511,  2510,  2514,  2522,  2523,  2525,  2527,  2532,
    2533,  2535,  2540,  2542,  2544,  2546,  2551,  2553,  2555,  2560,
    2562,  2564,  2566,  2567,  2569,  2574,  2576,  2578,  2583,  2584,
    2588,  2589,  2596,  2595,  2600,  2599,  2609,  2608,  2619,  2618,
    2628,  2633,  2634,  2639,  2645,  2663,  2664,  2668,  2670,  2672,
    2677,  2679,  2681,  2683,  2688,  2690,  2695,  2697,  2706,  2707,
    2712,  2714,  2719,  2721,  2723,  2732,  2734,  2735,  2736,  2738,
    2740,  2741,  2746,  2747,  2751,  2752,  2757,  2759,  2762,  2765,
    2772,  2773,  2774,  2779,  2784,  2786,  2792,  2793,  2799,  2800,
    2804,  2812,  2819,  2832,  2831,  2835,  2838,  2837,  2846,  2850,
    2854,  2856,  2862,  2863,  2868,  2873,  2882,  2883,  2885,  2891,
    2893,  2898,  2899,  2905,  2906,  2907,  2916,  2917,  2919,  2920,
    2925,  2926,  2928,  2929,  2931,  2933,  2939,  2940,  2942,  2943,
    2944,  2946,  2948,  2955,  2956,  2958,  2960,  2965,  2966,  2975,
    2977,  2982,  2984,  2989,  2990,  2992,  2995,  2997,  3001,  3002,
    3003,  3005,  3007,  3015,  3017,  3022,  3023,  3025,  3029,  3030,
    3032,  3033,  3039,  3040,  3041,  3042,  3046,  3047,  3052,  3053,
    3054,  3055,  3056,  3070,  3071,  3076,  3077,  3082,  3084,  3086,
    3088,  3090,  3113,  3114,  3120,  3121,  3127,  3126,  3131,  3130,
    3134,  3140,  3143,  3153,  3154,  3156,  3160,  3165,  3167,  3169,
    3171,  3177,  3178,  3182,  3183,  3188,  3190,  3197,  3199,  3200,
    3202,  3207,  3209,  3211,  3216,  3218,  3223,  3228,  3236,  3241,
    3243,  3248,  3253,  3254,  3259,  3260,  3264,  3265,  3266,  3272,
    3274,  3276,  3282,  3284,  3290,  3291,  3295,  3297,  3302,  3306,
    3310,  3312,  3324,  3326,  3328,  3330,  3332,  3334,  3336,  3337,
    3342,  3345,  3344,  3356,  3355,  3368,  3367,  3381,  3380,  3394,
    3393,  3406,  3411,  3417,  3419,  3425,  3426,  3437,  3444,  3449,
    3455,  3458,  3461,  3465,  3471,  3474,  3477,  3482,  3483,  3484,
    3485,  3489,  3497,  3498,  3510,  3511,  3515,  3516,  3521,  3523,
    3525,  3527,  3532,  3533,  3539,  3540,  3542,  3547,  3548,  3550,
    3585,  3587,  3590,  3595,  3597,  3598,  3600,  3605,  3607,  3609,
    3611,  3613,  3618,  3620,  3622,  3624,  3626,  3628,  3630,  3635,
    3637,  3639,  3641,  3650,  3652,  3653,  3658,  3660,  3662,  3664,
    3666,  3671,  3673,  3675,  3677,  3679,  3684,  3686,  3688,  3690,
    3692,  3694,  3706,  3707,  3708,  3712,  3714,  3716,  3718,  3720,
    3725,  3727,  3729,  3731,  3733,  3738,  3740,  3742,  3744,  3746,
    3748,  3760,  3765,  3770,  3772,  3773,  3775,  3780,  3782,  3784,
    3786,  3788,  3793,  3795,  3797,  3799,  3801,  3803,  3805,  3810,
    3812,  3814,  3816,  3825,  3827,  3828,  3833,  3835,  3837,  3839,
    3841,  3846,  3848,  3850,  3852,  3854,  3859,  3861,  3863,  3865,
    3867,  3869,  3879,  3881,  3884,  3885,  3887,  3892,  3894,  3896,
    3898,  3903,  3905,  3907,  3909,  3914,  3916,  3918,  3932,  3934,
    3937,  3938,  3940,  3945,  3947,  3952,  3954,  3956,  3958,  3963,
    3965,  3970,  3972,  3989,  3990,  3992,  3997,  3999,  4001,  4003,
    4005,  4007,  4012,  4013,  4015,  4017,  4022,  4024,  4026,  4032,
    4034,  4037,  4044,  4046,  4055,  4057,  4059,  4060,  4062,  4064,
    4068,  4070,  4075,  4077,  4079,  4081,  4116,  4117,  4121,  4122,
    4125,  4127,  4132,  4134,  4136,  4138,  4140,  4145,  4146,  4148,
    4150,  4155,  4157,  4159,  4165,  4166,  4168,  4177,  4180,  4182,
    4185,  4187,  4189,  4203,  4204,  4206,  4211,  4213,  4215,  4217,
    4219,  4224,  4225,  4227,  4229,  4234,  4236,  4244,  4245,  4246,
    4251,  4252,  4253,  4259,  4261,  4263,  4265,  4267,  4269,  4271,
    4278,  4280,  4282,  4284,  4286,  4288,  4290,  4292,  4294,  4296,
    4299,  4301,  4303,  4305,  4307,  4312,  4314,  4316,  4321,  4347,
    4348,  4350,  4354,  4355,  4359,  4361,  4363,  4365,  4367,  4369,
    4371,  4378,  4380,  4382,  4384,  4386,  4388,  4393,  4395,  4397,
    4402,  4404,  4406,  4424,  4426,  4431,  4432
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
  "INT128", "UINT128", "FLOAT80", "uuFLOAT128", "FLOAT16", "FLOAT32",
  "FLOAT32X", "FLOAT64", "FLOAT64X", "FLOAT128", "FLOAT128X", "FLOAT32X4",
  "FLOAT64X2", "SVFLOAT32", "SVFLOAT64", "SVBOOL", "DECIMAL32",
  "DECIMAL64", "DECIMAL128", "ZERO_T", "ONE_T", "SIZEOF", "TYPEOF",
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "COUNTOF", "OFFSETOF", "BASETYPEOF",
  "TYPEID", "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR",
  "COROUTINE", "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE",
  "TRAIT", "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE",
  "SWITCH", "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE",
  "GOTO", "RETURN", "CHOOSE", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "WAITUNTIL", "CORUN", "COFOR", "DISABLE", "ENABLE", "TRY", "THROW",
  "THROWRESUME", "AT", "ASM", "ALIGNAS", "ALIGNOF", "GENERIC",
  "STATICASSERT", "IDENTIFIER", "TYPEDIMname", "TYPEDEFname",
  "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH", "RECOVER",
  "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE", "C23_ATTRIBUTE",
  "FLOATING_DECIMALconstant", "FLOATING_FRACTIONconstant",
  "FLOATINGconstant", "ARROW", "ICR", "DECR", "LS", "RS", "LE", "GE", "EQ",
  "NE", "ANDAND", "OROR", "ATTR", "ELLIPSIS", "EXPassign", "MULTassign",
  "DIVassign", "MODassign", "PLUSassign", "MINUSassign", "LSassign",
  "RSassign", "ANDassign", "ERassign", "ORassign", "ErangeUpLt",
  "ErangeUpLe", "ErangeEq", "ErangeNe", "ErangeDownGt", "ErangeDownGe",
  "ErangeDownEq", "ErangeDownNe", "ATassign", "THEN", "'}'", "'('", "'@'",
  "')'", "'.'", "'['", "']'", "','", "':'", "'{'", "'`'", "'^'", "'*'",
  "'&'", "'+'", "'-'", "'!'", "'~'", "'\\\\'", "'/'", "'%'", "'<'", "'>'",
  "'|'", "'?'", "'='", "';'", "$accept", "push", "pop", "constant",
  "quasi_keyword", "identifier", "identifier_at",
  "identifier_or_type_name", "string_literal", "string_literal_list",
  "primary_expression", "generic_assoc_list", "generic_association",
  "postfix_expression", "field_name_list", "field", "field_name",
  "fraction_constants_opt", "unary_expression", "ptrref_operator",
  "unary_operator", "cast_expression", "qualifier_cast_list",
  "cast_modifier", "exponential_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "AND_expression", "exclusive_OR_expression",
  "inclusive_OR_expression", "logical_AND_expression",
  "logical_OR_expression", "conditional_expression", "constant_expression",
  "argument_expression_list_opt", "argument_expression_list",
  "argument_expression", "assignment_expression",
  "assignment_expression_opt", "assignment_operator",
  "simple_assignment_operator", "compound_assignment_operator", "tuple",
  "tuple_expression_list", "comma_expression", "comma_expression_opt",
  "statement", "labelled_statement", "compound_statement",
  "statement_decl_list", "statement_decl", "statement_list_nodecl",
  "expression_statement", "selection_statement", "conditional_declaration",
  "case_value", "case_value_list", "case_label", "case_label_list",
  "case_clause", "switch_clause_list_opt", "switch_clause_list",
  "iteration_statement", "for_control_expression_list",
  "for_control_expression", "enum_key", "updown", "updownS", "updownEq",
  "jump_statement", "with_statement", "mutex_statement", "when_clause",
  "when_clause_opt", "cast_expression_list", "timeout", "wor", "waitfor",
  "wor_waitfor_clause", "waitfor_statement", "wand", "waituntil",
  "waituntil_clause", "wand_waituntil_clause", "wor_waituntil_clause",
  "waituntil_statement", "corun_statement", "cofor_statement",
  "exception_statement", "handler_clause", "handler_predicate_opt",
  "handler_key", "finally_clause", "exception_declaration",
  "enable_disable_statement", "enable_disable_key", "asm_statement",
  "asm_volatile_opt", "asm_operands_opt", "asm_operands_list",
  "asm_operand", "asm_clobbers_list_opt", "asm_label_list",
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
  "field_declaration", "field_declaring_list_opt", "field_declaring_list",
  "field_declarator", "field_abstract_list_opt", "field_abstract",
  "cfa_field_declaring_list", "cfa_field_abstract_list",
  "bit_subrange_size_opt", "bit_subrange_size", "enum_type", "$@6", "$@7",
  "enumerator_type", "hide_opt", "enum_type_nobody", "enumerator_list",
  "visible_hide_opt", "enumerator_value_opt",
  "parameter_list_ellipsis_opt", "parameter_list",
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
  "external_definition_list_opt", "external_definition_list", "up", "down",
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
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   125,    40,    64,    41,
      46,    91,    93,    44,    58,   123,    96,    94,    42,    38,
      43,    45,    33,   126,    92,    47,    37,    60,    62,   124,
      63,    61,    59
};
# endif

#define YYPACT_NINF (-1926)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1155)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     141,   -80, -1926,  3224,   119,   493, -1926,   213, -1926,  2407,
   -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926,   173, -1926,     9, -1926,
   -1926, 13103, -1926,  3224,   303, -1926,  3224,  7407, 13103,  4268,
      75, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926,    47,  1178,   133, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926, -1926,   741,
     206, -1926, -1926, -1926, -1926, -1926, -1926,  7706,  7706, 13103,
     146,   284, 31797, -1926,   361, -1926, -1926,  3061, -1926,   867,
   15671, -1926, -1926,  3510, -1926, -1926, -1926, 18822, -1926,   212,
     377,   409,   505,    37, -1926,  7442,   425,   462,   497,   503,
    7571,   738,  1017, 13293,   213, -1926,   740, 19160,  2946,   213,
   -1926, -1926, -1926,  2935,   806, 28803, 12418,  1651,  2935,   847,
     655, -1926, -1926, -1926, -1926,   213, -1926, -1926, -1926, -1926,
     736, -1926, -1926, -1926, -1926,   666,   749,   213, -1926,   213,
   22510, -1926, -1926, -1926, 26222,  7706, -1926, -1926,  7706,   331,
   -1926, -1926, 31155,   760, 31232,   767,   781, 31309, 31386,   784,
   31856, -1926, -1926, -1926, -1926, -1926, -1926, -1926, 31463, 31463,
   22844, 28917,  5897, -1926, -1926, -1926, -1926,  3510,   390, -1926,
     439,   842, -1926,  1542,  6949, 31309, 31309, -1926,   805,   700,
     535,   956,   457,   977,   792,   820,   828,   888,    32, -1926,
     866,   864, -1926, -1926, -1926,   872, -1926,   860, 26281,   874,
    3498, -1926, -1926, -1926, -1926,   360, 20628,   213,  3771, -1926,
   -1926,   918, -1926,   912,   952, -1926,  1000, 31309, -1926, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926, 23015,  4687,  3061,   728,
     962,   967,   986,  1010,  1012,  1020, -1926, -1926,   213, 14591,
   24944,  1031, 23357,  1042, -1926,  6801,  4021,  1056, 21332, 17880,
    2935,  2935,  1076,  2935,   174,  2935,   589,   872, -1926, -1926,
     213, -1926,  1251,  1258, -1926, -1926, -1926, -1926, 26448,  7706,
   -1926, -1926, 26507,  7386, -1926, -1926, 15851,  1069, -1926, 11466,
   -1926,  1146,   847, 18991, -1926, -1926, 26674, -1926, -1926,  1105,
   -1926, -1926, -1926,  1092, -1926, 28994,  1201, 10689, -1926,  1110,
    7706,   749,  1115,  1125, -1926,   213,   213,  3510, -1926, -1926,
   -1926,  6125,  5791,  1112,  1187,   547,  1187, -1926,   213,   213,
      44, 22282,   559,  1187, -1926,   213,   213,    44,   213, -1926,
     213, -1926,  4967, -1926, -1926,  1137,  1152,   847, 18047, 20804,
   18822, -1926,  7442,   213,  2935, -1926,  3226,   655,  1149,  1231,
   22282,  7706, -1926,  7706,   505, -1926, 13855, -1926,  1146,   847,
    1171,  1231, 22282,  7706,   213, -1926, 28745, -1926, -1926, -1926,
   -1926,  1146, -1926, -1926, -1926, -1926,   847, -1926,  1349,  1019,
    6157,  7706, -1926, 24452,  1198, -1926, -1926, -1926,  4268,   749,
   22396,  1183,  7599, 24393, 18047, 16751, 25269, -1926, 27859, -1926,
    7706,  1187,     6,  1200, 23186, -1926,  5897, 23699, -1926, 26733,
   25269, -1926, -1926, 23870, -1926, 31309, -1926, -1926, -1926, -1926,
   -1926, -1926, 23699, -1926, -1926, 25770, 26733, 26733, 14771,  1736,
    1883, 23528,   830,  2017, -1926,   513,  1209,   421, 27859,  1060,
    1212, -1926, -1926,  1211,  1215,  1223, 29225,  1224,  1214, 31309,
    3510, 31309,  3510, -1926, -1926,  3040, -1926, -1926,  7407,  4381,
   31309,  7407,  3510, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926, -1926,  1228, 31309, -1926, -1926, -1926, -1926,
   31309, 31309, 31309, 31309, 31309, 31309, 31309, 31309, 31309, 31309,
   31309, 31309, 31309, 31309, 31309, 31309, 31309, 31309, 31309, 29302,
   -1926,  7407,  3709, -1926, 31309, -1926, -1926,  7599, 27909, -1926,
    1276,  1269, -1926, -1926, -1926, -1926,  7706,  5193,   180,   677,
   -1926,  7706,   912, -1926,  1273, -1926, 16031, 25436,  1154, 21332,
   -1926,  1146,   847,  1284, -1926,  1105,  5744,   473,   880, -1926,
     626,   655,  1289,   213,  3498,  1277,   912,  3498,  1304, -1926,
     733, -1926, 14951, -1926, -1926, -1926,   825, 25269, -1926,  5970,
    3061,  1334,  1337,  1339,  1352,  1355,  1364, -1926, -1926,   668,
    1343, -1926,   743,  1343,  6875, 24610,  1288, 16931, 24041,  1360,
   31540,  1372, -1926, 28069, 26674, -1926, -1926, 15131, -1926, 26900,
   -1926,  1146,  1146, 28447, -1926, -1926,  1105, -1926, -1926, -1926,
   13668, 29379,  1524, 31309,  3986,   824,  1353, -1926,   213,   213,
    1353,  1050, -1926,   213,   213,  1379,  1353, -1926,   213,   213,
   -1926,  1343, -1926, 29456, 16211, 26959, -1926, -1926,  7706, 21920,
    1146,  1146, -1926, -1926,  6875, -1926, 21508, -1926, 21508, -1926,
   27699, -1926, -1926,  1353, 17111, -1926, 26448, -1926, -1926, -1926,
      -6, 25829, 21684, -1926, -1926, -1926, -1926, -1926, 12144, -1926,
   -1926, 31617, -1926,  8122,  6093, 28917, 28994,  1384,  1386, -1926,
   -1926,  1389, 10689,   372, -1926, -1926, -1926, 23528,  1399, -1926,
    1000, -1926,  3510,  7599,  1395,  6125,   794,  1431,  1443,  1448,
     807,  1454,  1456,  1458,  1460,  1464,  1468,  5152,  6125, -1926,
   -1926, -1926,   213,  1467, 28112, -1926, -1926,  1379,   505, -1926,
   -1926,   749,  1231, 24786, -1926, -1926,   505, -1926, -1926,   749,
   -1926, -1926,  6887,  6317,  4967, -1926,  1132, -1926, -1926, -1926,
   -1926, 23528, 23528, -1926,  1146,   213,  7599, 16391,  3187, -1926,
   -1926, -1926, -1926, -1926, -1926, -1926,   749,  1231,  1440,  1476,
   -1926, -1926,  2935,  1480,  1231, 22282, -1926, -1926,   749,  1231,
   -1926, -1926, 11394, -1926, -1926,  1146,  1146, -1926, -1926, -1926,
     486,   698,   486,   655,  1503, -1926, -1926, -1926, 21920,  1526,
    1546, -1926, -1926,   881, 25120, -1926,  1572, 18047, -1926,  1523,
   -1926, -1926, -1926, 27135,  1549,  1553,  1319, 28596, 27187,  7706,
    1187, -1926, -1926, -1926, -1926,  1559, 25603,  1568,  1574,  1586,
   16571,  1584,  1590,  1594,  1608,  1609,  1612, 31309,  1614,  1615,
    1620, 27247, 31309, -1926, -1926,  2148, -1926, -1926, -1926, 31309,
   -1926, 20100,  2259, -1926, -1926,   213,   213, -1926,  1623,  1627,
   29071, 29533,  1626, -1926, 29148,  7407, 31309,  1629, -1926,  1631,
   -1926, -1926,  6951, -1926,  1640, -1926,  6951, -1926, -1926,  1327,
    1644, -1926, 28994, -1926, -1926,   805,   805,   805,   700,   700,
     535,   535,   956,   956,   956,   956,   457,   457,   977,   792,
     820,   828,   888, 31309,  1308, -1926,  6951, -1926, -1926, -1926,
   21508,  1187, 13103, -1926,  7706,  1639, 18655,  1647, -1926, -1926,
   -1926, -1926, -1926,  3498, -1926, -1926,  1731, 25996, 20276,  1801,
    2679, -1926, -1926,   213,  1648,   213, -1926,    90,  1642,   934,
   25269,   955,  1630, -1926,  1000, -1926, 23528, -1926, -1926, -1926,
    1199,  1343, -1926,  1001,  1343, 24786, -1926, -1926,  1379, 24786,
   -1926,  1379, -1926, 20452, -1926, 26448, -1926, -1926, 15311,  1656,
   24212,  1657,   872,  1658, 17291, -1926, -1926, -1926, -1926, -1926,
   -1926, 17880, 20452, 13668,  1665,   859,  1666,  1667,  1668,  1680,
    1683,  1684,  1686, -1926,  1672,  4498, -1926,  2759, -1926,  7173,
   -1926, -1926, -1926, 24786, -1926, -1926, -1926, -1926, -1926, -1926,
   24786, -1926, -1926, -1926, -1926, -1926, -1926, -1926,  1379, -1926,
    1669, 26507, 16931, -1926, -1926, -1926,  1353,  1146, -1926,  1345,
   -1926, -1926, -1926, -1926, -1926, -1926, -1926, 20452, -1926,  7706,
    6951, -1926,  1688,   326,  1685,  1389, -1926, 28994,  1690, -1926,
    2632, 31309, -1926, -1926,  1004, -1926,  1695, 20452, -1926, -1926,
   31309,  1003,  1698,  1699,  1704,  1028,  1711,  1713,  1714,  1717,
    1718,  1721,  1230,  1343, -1926, -1926,  1239,  1343, -1926, -1926,
    1255,  1343, -1926, -1926, -1926, -1926, -1926, -1926,  7599,  1869,
    1343,  1156, -1926,   872,  1358, -1926, -1926,   749,  1735, -1926,
   -1926,  6887,   931,  4967,  6887, -1926, 24786,  1029,  1737,  1037,
    1738, -1926,  1360,   208, -1926,   749, 17820, -1926,   749,  1231,
     208, -1926,   749, -1926, -1926, -1926, -1926, -1926,   849, -1926,
    3510, -1926, -1926,  7706,   213,  1825, -1926, -1926, -1926, 21684,
    1187, -1926, 20452,   333,  1739, -1926, 21920,   333,  3510, -1926,
     333, -1926, 26055,   333, -1926, 31309, 31309, 31309, -1926, -1926,
   -1926, -1926,  1740,  1742,  1743,  1747,  1170, -1926,  1066, -1926,
   -1926, -1926, 31309, 31309,  1744, 28994, -1926, -1926, -1926,  1745,
   -1926, -1926,  1745,  1751, -1926, -1926, -1926, -1926,  5451, -1926,
   -1926,  1396, -1926,   198, -1926,  1406, -1926, 29533, -1926,  1389,
   -1926, 31309,  1409,  1748, -1926,   208,  1753, -1926, -1926, -1926,
   -1926,  7599, 27414, 17987, -1926,   -13,   341, 23528,  1741, -1926,
    1741, -1926, -1926,   213,  1378, -1926,    90,  1642,  1642,   360,
   -1926, -1926,  1754,  7706,  1756, -1926, -1926,  1760, -1926,  1757,
   -1926, -1926, 24786, -1926, -1926,  1379, 24786, -1926,  1379,  1767,
    1769, -1926,  1771,  1768,  1773, -1926, -1926, -1926, -1926, -1926,
   -1926,  1770, 20452, 20452, -1926,  1779, -1926,  1261,  1343, -1926,
    1274,  1309,  1343, -1926,  1146,  7267,  1858, -1926,   213,   213,
   -1926, -1926, -1926,  5409,  1932,  6567, -1926, -1926,  1784,  1785,
   -1926, -1926, -1926, 21508, -1926,   505,  1416, 31309, -1926, 31309,
   -1926,  1789, -1926, 10689, -1926,   213, 20452,   213, -1926, -1926,
    1392,  1343, -1926,  1447,  1343, -1926, -1926,  1473,  1343, 24786,
   -1926, -1926,  1379, 24786, -1926, -1926,  1379, 24786, -1926, -1926,
    1379,  1187, -1926,  1379, -1926, 31309, -1926, 31309, -1926, 28276,
   -1926, -1926,  1055, -1926, -1926, -1926, -1926, -1926,   664, -1926,
   -1926, 18154,   208, -1926,   749, -1926, -1926,  1782,  1783,  1786,
     671, -1926, 21920, -1926, -1926,   391,   961, -1926, 12533,  7706,
   -1926, -1926, -1926,  1299,  1790,  1795,  1097, -1926,  1799, -1926,
   -1926, -1926, -1926,  1485,  1343, -1926, -1926, -1926, -1926, -1926,
   28994,  1389, 29610,  1791,  1810, -1926,  1854,  6951, -1926,  1854,
    1854, -1926,  6951,  6259,  7293, -1926,  1420,  1819, -1926, -1926,
   -1926, -1926,  7706, -1926, -1926, -1926,  7706, -1926,  7599, -1926,
    1118, 25269,   912,   912,  1642,  1754,  1811,  1813,   655,    59,
    1824,  1800,    90, 18321, -1926,  1823,  1826, -1926, -1926, -1926,
   20980, 21156, -1926, -1926, -1926,  1827,   213, 24786, -1926, -1926,
    1379, 24786, -1926, -1926, 24786, -1926, -1926,  1379, 31309, 31309,
    1822,  1831, -1926,  1829, -1926, -1926,  5409,  5127,  5320,  7173,
   -1926, -1926, -1926,  1838, -1926, -1926,  1836, -1926, -1926, -1926,
   -1926, -1926, -1926,  1841, 24786, -1926, -1926,  1379, 24786, -1926,
   -1926,  1379, 24786, -1926, -1926,  1379,  1842,  1846,  1848,   505,
    1425, -1926,    23, -1926,   872,  1852, -1926, -1926, -1926,  1853,
   -1926, -1926, -1926,  1856, 19864, -1926, -1926,  7706, -1926,  1855,
   -1926,   257,    69, 14411,  1871,  1873, 22092,  1874,  1875,  2437,
    3467,  4162, 29687,  1880,  2212,  1885,  1888, 22092,  1891, -1926,
   -1926,   749, 31309, 31309,  2006,  1862,   357, -1926, 22673, 15491,
    1887,  1889,  1886, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926,  1505,   161, -1926,   192, -1926,   161, -1926,
   -1926, -1926, -1926, -1926,  3510, -1926, -1926, 13481, 19329, -1926,
     395,  1896,  1897, -1926, -1926, 31309, -1926, 26055, 31309, 24786,
   -1926, -1926,  1379,  1389,  1915, -1926, -1926, -1926, -1926, -1926,
    1438, -1926,  6951, -1926,  6951, -1926, -1926, 29610, -1926,  1919,
     180, -1926, -1926, -1926, -1926, -1926, -1926,  1912,  1923,    90,
      90,   360,  7706,   213, 29764, -1926,  1754, -1926, 18488, -1926,
   -1926, -1926,  1917, -1926,  1922,  1924, -1926,  1926,  1927,  1928,
   -1926, -1926,  1920, -1926,  1929, -1926, -1926,  1930,   213,  1934,
    1936,  1939, -1926, -1926, -1926, -1926, -1926, 31309, -1926,  1953,
   -1926,  1009,  1087,  1177, 23528,   213,   213, 18047,   213, 26733,
    1931,   397,   417,  2276,  9654, -1926,   454,  7706, -1926, -1926,
    7407,    36,   175, -1926, -1926, -1926, -1926, 14411, 31309,  1957,
    2043, 14230, 12723, -1926,  1944, -1926,  1947, 31309,  1948, 28994,
    1949, 31309,  1952, -1926,  1954, 23528, 31309, -1926, 12913,  1611,
   -1926,  1955,    -9, -1926,    29,  2050,    40,   213, -1926,  1982,
    1983, 22092, 22092, -1926, -1926,  2061, -1926, -1926,   107,   107,
     746, 14042, -1926,   213, -1926, -1926, -1926, -1926,  1990,  2002,
   -1926, -1926,  1474,  1498, -1926,  1741,    90,   213,  1754,  1754,
     655,  1800, -1926, 28994, -1926,  2003, -1926,   213,   213, -1926,
   -1926, -1926,  1998,  1999, -1926, -1926, -1926, -1926, -1926, -1926,
   -1926, -1926, -1926,  1856,  1856,  1856,  1128, -1926,  4831, 26733,
    4831,   508, -1926, -1926, -1926,  7490, 31309,  6599,   275, -1926,
   -1926, -1926,   770,  2004,  2004,  2004,  7706, -1926, -1926,  2007,
   -1926, -1926, -1926, -1926,  1889,  2010, 31309,   377,  2000,   503,
   20041, 26281,  1143,  2005, 22092,  2011, -1926, -1926, -1926, -1926,
    1123, 22092, 31309,  1702,   483, -1926, 31309, 28835, -1926, -1926,
     578, -1926,  1389, -1926,  1190, -1926, -1926,  1192,  1207,   570,
   -1926, -1926, -1926, -1926,   749,  1611,  2015, -1926, -1926, 31309,
   -1926,  2016,  1000, -1926, 12208, 31309, 31309, -1926, -1926,   592,
     107, -1926,   420, -1926, -1926, -1926, -1926, -1926, -1926, -1926,
     912,  1754, -1926,  2021,  2033, -1926,  1389,   213, -1926, -1926,
   -1926, -1926,   213,   213,   213, -1926,   584,  1326,  2009,   614,
   -1926,   618, -1926,  7490,   787, -1926,  7033,  7490, -1926,   213,
   -1926, -1926, -1926, -1926, -1926, -1926, 22092, 22092,  1889, 21860,
      93, 29841,  2120, 22092, -1926, 31309, -1926, 29918,  2121,  2012,
    9247, 29995, 22092, 12913,  1889,   606,  2816,  2013, 31309, -1926,
    2040,   110, 22092, -1926, 22092, -1926,  2046, -1926, 27473,  2018,
    1000,   631, -1926, -1926,  2039,  1518,  1242, 22092,  2047, 22092,
   22092, 22092, -1926,  2049,   213,   213,  2051, -1926, -1926, -1926,
   -1926, -1926,  1326,  2971,   667, -1926, -1926, -1926, -1926,   213,
     213, -1926, -1926, -1926, -1926,  2045,  4831, -1926,  2136,  6227,
      76, 17474, -1926, 21962, -1926,    35,  1243, 22092,  2137,   678,
    2038,   261, 22092, 31309,   606,  2816,  2030, 30077,  1098,  1146,
    2042,   353,  2144, -1926, 30154, -1926, -1926, -1926, -1926, 30231,
   31309, 31309,  1889,  2036, 17653, -1926, -1926, -1926, 27473,  2037,
    5241, 27640,  3510, -1926,  2056,  2044,   126, -1926, 31309,  7407,
   -1926, -1926, 31309,   161, -1926, -1926,   213, -1926, -1926, -1926,
    2064,  2068,  2069,  2139, -1926, -1926,   213, -1926, -1926, -1926,
   -1926, 22092, -1926,    74, -1926,    80, -1926, -1926, -1926,  2073,
    1370, -1926, -1926, 22092, -1926,    66, -1926, 22092, 31309,  2072,
   30308, -1926, -1926, 30385, 30462, 31309, 31309,  6875,  1889, -1926,
     872, 30539, 30616, 22092,  2058,   511,  2059,   573,  1889, -1926,
   -1926,  2078,  1370,  2037, 31309,  2076,  5608,  4347, -1926, -1926,
   -1926,  2074, -1926,  2134,  2082,   660,  2077, -1926, -1926,  2084,
    1265,   560, -1926,  1492,  1343, -1926, -1926,  1326, -1926, 31309,
   -1926, 31309, -1926, -1926,  1617, 19517, 19694, -1926, 22092, -1926,
   -1926,  1889, -1926, -1926,  1889,  2071,   601,  2075,   708,  1889,
   -1926, -1926,   655, -1926,  1889, -1926,  1889, -1926,  2088, 30693,
   30770, 30847, -1926,  1617,  2092, -1926,   749,  3731,  6887,   126,
    2085, 31309,  2079,   126,   126, -1926, -1926, 22092,  2181, 24786,
   -1926, -1926,  1379, -1926, -1926, -1926,  1410, -1926,  1617, -1926,
   -1926, -1926,  2097, 30924, 31001, 31078, -1926, -1926,  1889, -1926,
    1889, -1926,  1889, -1926,   749, -1926,  2095,  1000,  2101, -1926,
     713, -1926, -1926, 22092,  2109, 11812, 22092,  2118,  1410, -1926,
   -1926,  1889, -1926,  1889, -1926,  1889,  2124, -1926,  1000,  2119,
   -1926,  2099,  1000, -1926, -1926, -1926, 22092, -1926, -1926, 11970,
   -1926, -1926,  1550, 31309, -1926,  1275, -1926,  1000,  7706,  2123,
    2102, -1926, -1926,  1290, -1926, -1926,  2103,  7706, -1926, -1926
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     874,     0,   881,   884,     0,   874,     2,   875,   876,   884,
     889,   888,    17,    22,    23,     9,    10,    11,    12,    13,
      14,    15,    16,    18,    21,   887,     0,   882,   885,     1,
       2,   504,   877,   884,     0,   880,   884,   159,   504,   874,
     520,   521,   522,   523,   524,   525,   526,   527,   528,   509,
     511,   510,   512,     0,     0,     0,   530,   532,   559,   533,
     560,   536,   537,   557,   558,   531,   555,   556,   534,   535,
     538,   539,   540,   541,   542,   543,   544,   545,   546,   547,
     548,   549,   550,   551,   552,   553,   554,   561,   562,   874,
     564,   638,   639,   642,   644,   640,   646,     0,     0,   504,
       0,     0,    17,   609,   615,   830,   105,     0,    20,     0,
     504,   103,   104,     0,   851,    19,   890,   504,   831,     0,
       0,   442,   752,   444,   456,   872,   443,   478,   479,     0,
       0,     0,     0,   592,   874,   508,   513,   504,   515,   874,
     577,   529,   563,   488,   569,   874,   490,   587,   489,   874,
     606,   612,   591,   618,   630,   874,   635,   636,   619,   689,
     445,   446,     3,   838,   852,     0,     0,   874,   914,   874,
     504,   932,   933,   934,   504,     0,  1132,  1133,     0,     0,
     879,   883,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    29,     4,     8,    25,     5,     6,     7,     0,     0,
     504,     0,     0,   106,   107,   108,   109,   163,    85,    28,
      86,    24,    46,    84,   110,     0,     0,   125,   127,   131,
     134,   137,   142,   145,   147,   149,   151,   153,   155,   166,
       0,   160,   161,   165,    30,     0,     3,     0,   504,   841,
       0,   641,   643,   645,   647,     0,   504,   874,   692,   637,
     565,   806,   801,   791,     0,   839,     0,     0,   520,   832,
     836,   837,   833,   513,   834,   835,   504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   610,   613,   874,   504,
     504,   103,   504,     0,   737,     0,  1155,     0,   505,   504,
     515,   495,   569,   496,   595,   497,   874,   606,   599,   620,
     874,   621,     0,     0,   733,   738,   723,   727,   504,   739,
    1100,  1101,   504,   740,   742,   891,   504,     0,  1134,   592,
     498,   499,   874,   504,   921,   940,   504,  1139,  1131,  1129,
    1137,   439,   438,     0,   174,   758,   173,     0,   447,     0,
       0,     0,     0,     0,   454,   874,   874,     0,   437,  1013,
    1014,     0,     0,   477,   872,   874,   872,   894,   874,   874,
     487,   504,   874,   872,   954,   874,   874,   486,   874,   974,
     874,   951,     0,   585,   586,     0,     0,   504,   504,   504,
     504,   457,   872,   874,   516,   578,     0,   607,     0,   855,
     504,     0,   506,     0,   752,   458,   592,   570,   588,   874,
       0,   855,   504,     0,   874,   518,   874,   579,   580,   574,
     491,   589,   493,   494,   492,   594,   874,   608,   602,     0,
     622,     0,   826,   504,     2,   853,   913,   915,   874,     0,
     504,     0,     0,   592,   504,   504,   504,  1143,   592,  1146,
       0,   872,   872,     0,   504,    92,     0,   504,   101,   504,
     504,   110,    87,   504,    95,     0,    36,    40,    41,    37,
      38,    39,   504,    90,    91,   504,   504,   504,   504,   106,
     107,   504,     0,     0,   195,     0,     0,   745,   592,   636,
       0,   747,  1129,  1153,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    26,    59,     0,    65,    66,   159,     0,
       0,   159,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,   184,   185,   173,     0,   171,   172,    88,    89,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     886,     0,     0,   827,     0,   467,   468,     0,   592,   828,
       0,     0,   787,   789,   788,   790,     0,     0,   783,     0,
     772,     0,   781,   793,     0,   690,   504,   504,  1155,   505,
     569,   595,   874,     0,   739,   740,   692,   609,   615,   693,
     694,   695,     0,   874,     0,   804,   792,     0,     0,   158,
       0,   616,   504,   798,   748,   797,     0,   504,   750,     0,
       0,     0,     0,     0,     0,     0,     0,   892,   919,   874,
     930,   938,   943,   949,     0,   504,     0,   505,   504,   609,
       0,     0,  1141,   592,   504,  1144,  1053,   504,  1103,   505,
     501,   502,   503,   504,  1108,  1097,  1098,  1106,  1052,     2,
     504,     2,   104,     0,   874,   874,  1155,   994,   874,   874,
    1155,   874,  1010,   874,   874,  1076,  1155,  1058,   874,   874,
    1067,  1074,   731,     0,   504,   504,   600,  1102,   741,   505,
     596,   597,   601,   602,     0,   465,   504,  1147,   504,  1118,
     505,  1124,  1119,  1155,   504,  1112,   504,  1121,  1113,     2,
    1155,   504,   504,   923,   942,  1130,   500,  1135,   592,   922,
     941,     0,     2,    27,     0,     0,   758,    28,     0,   756,
     759,  1153,     0,     0,   765,   754,   753,   504,     0,   857,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   897,
     957,   981,   874,   483,     0,   893,   902,  1043,   752,   895,
     896,     0,   855,   504,   953,   962,   752,   955,   956,     0,
     973,   975,     0,     0,     0,   473,   874,   867,   869,   868,
     870,   504,   504,   576,   505,   575,     0,   504,     0,  1136,
    1140,  1138,   455,   507,   593,   828,     0,   855,     0,     0,
     448,   459,   517,     0,   855,   504,   603,   828,     0,   855,
     802,   519,   572,   573,   571,   590,   605,   604,   611,   614,
     609,   615,   633,   634,     0,   803,   707,   743,   505,     0,
     708,   710,   712,     0,   504,   217,   431,   504,   854,     0,
     429,   487,   486,   592,   103,     0,     0,   504,   504,     0,
     872,   450,     2,   451,   878,     0,   504,     0,     0,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   504,     0,   124,   123,     0,   120,   119,    31,     0,
      32,   504,   874,   746,  1023,   874,   874,  1032,     0,     0,
       0,  1154,     0,   186,     0,   159,     0,     0,    55,     0,
      56,    63,     0,    62,     0,    58,     0,    57,    61,     0,
       0,    54,   758,   167,   126,   128,   129,   130,   132,   133,
     135,   136,   140,   141,   138,   139,   143,   144,   146,   148,
     150,   152,   154,     0,     0,   162,     0,    33,   475,   470,
     504,   872,   504,   828,     0,     0,     0,     0,   786,   785,
     784,   778,   514,     0,   776,   794,   567,   504,   504,   104,
     874,   741,   691,   874,     0,   874,   683,   692,   692,     0,
     504,     0,     0,   441,     0,   617,   504,   749,   751,   920,
     874,   931,   939,   944,   950,   504,   924,   926,   928,   504,
     945,   947,   694,   504,  1110,   504,  1120,  1111,   504,   103,
     504,     0,   607,     0,   505,     2,     2,  1142,  1145,  1099,
    1104,   505,   504,   504,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1077,     0,   874,  1156,  1063,  1062,   875,
     997,  1015,  1064,   504,   992,  1001,   729,   995,   996,   730,
     504,  1008,  1019,  1011,  1012,   732,  1060,  1061,  1075,  1148,
       0,   504,   505,  1105,  1109,  1107,  1155,   598,   633,     0,
     725,   724,   728,   734,  1116,  1123,  1117,   504,   735,     0,
       0,   767,   158,     0,     0,  1153,   764,  1154,     0,   760,
       0,     0,   763,   766,     0,     2,     0,   504,   469,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   874,   907,   911,   952,   874,   967,   971,   979,
     874,   990,   899,   959,   983,   898,   958,   982,     0,     0,
    1039,     0,  1044,  1045,     0,   481,   858,     0,     0,   482,
     859,     0,     0,     0,     0,   474,   504,     0,     0,     0,
       0,   472,     0,   874,   860,     0,     0,   828,     0,   855,
     874,   861,     0,   626,   628,   624,   648,   916,   874,   935,
       0,   711,   713,     0,   874,   432,   430,  1055,  1054,   504,
     872,   452,   504,    93,     0,    97,   504,   102,     0,   100,
      96,    98,   504,     0,   114,     0,     0,     0,   118,   122,
     121,   196,     0,     0,     0,     0,  1027,  1026,   875,  1028,
    1024,  1025,     0,     0,     0,   758,   111,   192,   191,  1153,
     188,   187,  1153,     0,   164,    49,    50,    82,     0,    82,
      82,     0,    70,    72,    52,     0,    48,     0,    51,  1153,
     157,     0,     0,     0,   829,   874,     0,   780,   821,   816,
     817,     0,   505,     0,   812,     0,     0,   504,   774,   773,
     774,   568,   566,   874,  1063,   686,   692,   692,   692,     0,
     702,   701,  1153,     0,     0,   807,   805,     0,   840,     0,
     800,   799,   504,   925,   927,   929,   504,   946,   948,     0,
       0,   717,     0,   718,   719,  1114,  1122,  1115,  1125,  1126,
    1127,     0,   504,   504,     3,     0,  1071,   874,  1004,  1007,
     874,   874,  1070,  1073,   504,     3,     0,  1059,   874,   874,
     999,  1017,  1065,     0,   104,     0,   998,  1016,     0,     0,
    1149,   736,   466,   504,     3,   752,     0,     0,   768,     0,
     769,     0,   761,     0,   755,   874,   504,   874,     3,   476,
     874,   908,   912,   874,   968,   972,   980,   874,   991,   504,
     900,   903,   905,   504,   960,   963,   965,   504,   984,   986,
     988,   872,   484,  1040,  1051,     0,  1050,     0,  1042,     0,
     863,   976,     0,   582,   581,   584,   583,   829,   874,   864,
     809,     0,   874,   862,     0,   829,   865,     0,     0,     0,
     874,   709,   504,   744,   435,     0,   874,   219,   504,     0,
     453,     3,    94,  1056,     0,     0,     0,    42,     0,   115,
     117,   116,  1036,   874,  1035,  1038,  1030,  1029,   113,   112,
     758,  1153,  1154,     0,     0,    69,    79,     0,    73,    80,
      81,    64,     0,     0,     0,    60,     0,     0,   156,    34,
     842,   829,     0,   819,   796,   813,     0,   814,     0,   815,
       0,   504,   791,   791,   692,  1153,     0,     0,   698,   692,
       0,   703,   692,     0,   440,     0,     0,   917,   936,  1150,
     504,   504,  1128,     3,     3,     0,   874,   504,  1000,  1002,
    1003,   504,  1018,  1020,   504,  1066,  1068,  1069,     0,     0,
     103,     0,     3,     0,   993,  1009,     0,     0,     0,     0,
    1005,  1021,   726,     0,   449,   771,     0,   871,   757,   762,
     856,     3,   873,     0,   504,   901,   904,   906,   504,   961,
     964,   966,   504,   985,   987,   989,     0,     0,     0,   752,
       0,  1046,     0,  1047,  1048,     0,   811,   829,   866,     0,
     648,   648,   648,   631,   504,   714,   715,     0,   433,     0,
     220,     0,     0,   504,     0,     0,   356,     0,     0,     0,
       0,     0,   197,     0,     0,     0,     0,   356,     0,   404,
     403,     0,   169,   169,   410,   609,   615,   214,   504,   504,
       0,   198,     0,   225,   199,   200,   201,   202,   203,   204,
     205,   206,   357,     0,   371,   207,   377,   379,   382,   208,
     209,   210,   211,   212,     0,   213,   221,   592,   504,   223,
       0,     0,     0,  1057,    99,     0,    35,   504,     0,   504,
    1031,  1033,  1034,  1153,     0,   194,   193,   190,   189,    83,
       0,    71,     0,    77,     0,    75,    47,     0,   168,     0,
     783,   818,   820,   795,   775,   779,   777,     0,     0,   692,
     692,     0,     0,   874,     0,   697,  1153,   808,     0,   918,
     937,   721,   720,   722,     0,     0,  1152,     0,     0,     0,
       3,     3,     0,  1079,     0,  1151,   770,     0,   874,     0,
       0,     0,   909,   969,   977,   485,  1041,     0,   846,     0,
     848,   874,   874,   874,   504,   874,   874,   504,   874,   504,
       0,     0,     0,   662,   592,   649,     0,     0,   436,   218,
     159,     0,     0,   344,   345,   222,   224,   504,     0,     0,
       0,   504,   504,   340,     0,   338,     0,     0,     0,   758,
       0,     0,     0,   335,     0,   504,     0,   383,   504,     0,
     170,     0,     0,   411,     0,     0,     0,   874,   229,     0,
       0,   356,   356,   362,   361,   356,   373,   372,   356,   356,
       0,   592,   434,   874,    53,    45,    43,    44,     0,     0,
      67,    74,     0,     0,   844,   774,   692,   874,  1153,  1153,
     700,   703,   681,   758,   704,     0,   810,   874,   874,  1006,
    1022,  1072,     0,     0,  1078,  1080,   460,   464,   910,   970,
     978,  1049,   850,   631,   631,   631,     0,   623,   662,   504,
     662,     0,   661,   660,   656,     0,     0,     0,     0,   663,
     664,   666,   874,   678,   678,   678,     0,   657,   674,     0,
     348,   349,   346,   347,   238,     0,     0,   240,   444,   239,
     592,   504,     0,     0,   356,     0,   323,   325,   324,   326,
       0,   356,   197,   278,     0,   271,     0,   197,   341,   339,
       0,   333,  1153,   342,     0,   337,   336,     0,     0,     0,
     392,   393,   394,   395,     0,   385,     0,   386,   350,     0,
     351,     0,     0,   376,     0,     0,     0,   365,   375,     0,
     356,   378,     0,   380,   402,   463,  1037,    68,    78,    76,
     791,  1153,   682,     0,     0,   699,  1153,   874,   462,   461,
    1081,  1082,   874,   874,   874,   632,     0,   670,   636,     0,
     676,     0,   658,     0,     0,   680,     0,     0,   651,   874,
     650,   667,   679,   668,   669,   675,   356,   356,   241,   592,
       0,     0,   259,   356,   327,     0,   328,     0,   267,     0,
     197,     0,   356,   504,   279,     0,   305,     0,     0,   334,
       0,     0,   356,   355,   356,   396,     0,   387,   504,     0,
       0,     0,   216,   215,   358,     0,     0,   356,     0,   356,
     356,   356,   782,     0,   874,   874,     0,   685,   627,   629,
     625,   653,     0,   874,     0,   671,  1091,   673,  1083,   874,
     874,   655,   677,   659,   652,     0,     0,   354,   230,     0,
       0,     0,   252,   356,   232,     0,     0,   356,   261,   276,
     287,   281,   356,   197,     0,   291,     0,     0,     0,   318,
     282,   280,   269,   272,     0,   329,   330,   331,   332,     0,
       0,   197,   306,     0,     0,   235,   353,   384,   504,   390,
     397,   505,   401,   352,     0,     0,   412,   363,     0,   159,
     374,   367,     0,   368,   366,   381,   874,   688,   684,   705,
       0,     0,     0,  1087,  1086,  1088,   874,   654,  1084,  1085,
     665,   356,   247,   242,   245,     0,   244,   251,   250,     0,
     874,   254,   253,   356,   263,     0,   260,   356,     0,     0,
       0,   268,   273,     0,     0,     0,   197,     0,   292,   319,
     320,     0,     0,   356,     0,   308,   309,   307,   310,   275,
     343,     0,   874,   390,     0,     0,     0,   874,   398,   399,
     400,     0,   405,     0,     0,     0,   413,   414,   359,     0,
       0,     0,   687,   874,  1094,  1096,  1089,     0,   231,     0,
     249,     0,   248,   234,   255,   504,   504,   264,   356,   265,
     262,   277,   290,   288,   284,   296,   294,   295,   293,   297,
     274,   321,   322,   289,   285,   286,   283,   270,     0,     0,
       0,     0,   237,   255,     0,   391,     0,  1087,   875,   412,
       0,     0,     0,   412,     0,   364,   360,   356,     0,   504,
    1090,  1092,  1093,   672,   243,   246,   874,     3,   256,   426,
     425,   266,     0,     0,     0,     0,   317,   315,   312,   316,
     313,   314,   311,     3,     0,   388,     0,     0,     0,   406,
       0,   415,   369,   356,     0,     0,   356,     0,   874,   304,
     302,   299,   303,   300,   301,   298,     0,   389,   418,     0,
     416,     0,   418,   370,  1095,   228,   356,   226,   233,     0,
     236,   419,     0,     0,   407,     0,   227,     0,     0,     0,
       0,   420,   421,     0,   417,   408,     0,     0,   409,   422
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1926,   101,   -55, -1926,    -3,   640,  2594,   237,   -57, -1926,
    -141, -1926,   680, -1926,  -770,  -942, -1926,   531,  3452,  6493,
   -1926,   179, -1926,  1821,   989,  1218,  1235,  1176,  1233,  1764,
    1765,  1766,  1772,  1763, -1926,  -194,  -179,  -482, -1926,  1762,
    9993,   742, -1926,  2090, -1926, -1926,  -786,  3948, -1472,  2820,
   -1926,  1681, -1926,   921,    83, -1926, -1926,   602,   167, -1926,
   -1921, -1858,   364,   147, -1926, -1926,   591,   378, -1926, -1505,
   -1776,   321, -1926, -1926, -1926,   209, -1516, -1926, -1926, -1558,
     463, -1926, -1926, -1926, -1926, -1926,    27, -1452, -1926, -1926,
   -1926, -1926, -1926,   230,   479,   480,   308, -1926, -1926, -1926,
   -1926,  -912, -1926,   163,   106, -1926,   239, -1926,  -255, -1926,
   -1926, -1926,   963, -1191,   821,  -360, -1926,  -123,   -20,   836,
    1733,   823,   827, -1926,  -131, -1926, -1926,    12, -1926,  -157,
     908,  1774,  -356,  3914,  9712,  -432,   -28,   540,   114,   858,
    4776, -1926, -1926,  2272, -1926,   177,  4714, -1926,  2219, -1926,
     481, -1926, -1926,  2229,   555,  5514,  3089,   -96,  1981,  -146,
   -1926, -1926, -1926, -1926, -1926,  -152,  8696,  8353, -1926,  -191,
     130, -1926,  -795, -1926,   374, -1926,   231,   682, -1926,   -36,
    -133, -1926, -1926, -1926, -1926,  -203,  9174, -1190,   923,   604,
    1621, -1926,  -797,   -19,  2041,  3592,  1808,  -585,  -226,   779,
     628,  -375,  -365,  -299,  -704,  1311, -1926,  1670,   137, -1209,
    1437, -1926, -1926,   754, -1926, -1385,  -252,  -223,  -705, -1926,
      85, -1926, -1926, -1099, -1208, -1926, -1926, -1926, -1041,  2385,
    -731, -1231,   -15, -1926, -1926, -1926, -1926, -1926, -1926,  -239,
   -1300,  -404, -1925,   -61,  1705,  8483,    20,   389,  2365, -1926,
    6189,   -71,  -343,  -336,  -317,    34,  -103,  -102,  -100,   -25,
     -47,   -37,   -34,  -309,   -40,  -304,  -298,  -289,   199,  -283,
    -268,  -265,  -237,  -618,  -596,  -594,  -231,   -31,  -592, -1926,
   -1926,  -732,  1532,  1534,  1535,  2173, -1926,   886,  7610, -1926,
    -612,  -564,  -538,  -486,  -542, -1926, -1662, -1881, -1827, -1825,
    -614,   597,  -183,  -170, -1926,     7,   -14,  -134, -1926, 10683,
    2980,  1167,  -497
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    31,   333,   208,   209,   115,   116,  1570,   210,   211,
     212,  1396,  1397,   213,  1211,  1212,  1213,  1416,   214,   215,
     216,   217,   472,   473,   218,   219,   220,   221,   222,   223,
     224,   225,   226,   227,   228,   229,  1063,   230,   231,   232,
     474,  1731,   515,   337,   517,   234,  1199,  1571,  1572,  1573,
    1574,  1575,  1386,  1387,  2225,  1576,  1577,  1825,  2074,  2075,
    2002,  2003,  2004,  2197,  2198,  1578,  1844,  1845,  2099,  1936,
    1937,  2030,  1579,  1580,  1581,  1582,  1583,  1965,  1969,  1749,
    1741,  1584,  1585,  1748,  1742,  1586,  1587,  1588,  1589,  1590,
    1591,  1592,  1865,  2115,  1866,  1867,  2039,  1593,  1594,  1595,
    1734,  2125,  2126,  2127,  2252,  2263,  2144,  2145,   429,   430,
    1154,  1155,  1385,   118,   119,   120,   121,   122,  1828,   284,
     317,   126,   127,   128,   129,   353,   354,   432,   410,   286,
     477,   287,   132,   478,   134,   135,   263,   289,   290,   139,
     140,   141,   249,   142,  1242,   291,   320,   145,   377,   146,
     321,   386,   293,   571,   295,   322,   235,   151,   152,   298,
     153,   814,  1379,  1377,  1378,  1685,   299,   300,   156,   157,
    1380,  1695,  1808,  1809,  1810,  1984,  1985,  1696,  1909,  1921,
    1811,   158,  1248,  1444,   247,  1251,   301,  1252,  1253,  1645,
    1004,   820,  1272,   302,   303,   821,   305,   306,   307,   823,
     594,   595,   338,   710,   711,   712,   713,   714,   559,  1442,
     560,  1240,  1238,   941,   561,   585,   562,   563,   596,   160,
     252,   253,   161,  1233,  1234,  1235,  1236,     4,  1367,  1368,
     932,  1430,   162,   549,   550,   388,   400,   793,   163,   341,
     164,   765,  1064,   782,     6,     7,     8,    26,    27,    28,
     165,   767,   357,   358,   359,   768,   167,   168,   169,   170,
     171,   172,   173,   362,   769,   364,   365,   366,   770,   368,
     369,   370,  1020,   647,   648,   649,  1021,   371,   652,   653,
     654,   873,   874,   875,   876,   746,  1114,  1357,   308,  1602,
     656,   657,   658,   659,   660,   661,  1987,  1988,  1989,  1990,
     634,   309,   310,   311,   312,   481,   328,   176,   177,   178,
     314,   882,   662
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      23,   586,  1065,   136,   270,   271,    23,   272,   727,   479,
     136,   123,  1074,   277,   394,   728,   894,   175,   123,   900,
     573,   773,  1006,   236,   175,  1435,  1745,    32,    23,   790,
      23,  1443,  1022,    23,   729,    23,    23,  1371,   716,   431,
     439,   867,   730,   130,  1007,   582,  1008,   731,  1009,   646,
     130,  1044,  1735,   732,  1133,   650,  1445,  1635,  1636,   356,
     273,   487,   733,   589,   344,   166,  1140,  1941,   734,  1641,
     274,   136,   166,   275,  2076,   852,  1010,  2077,   590,   123,
    1720,  2140,  2082,   735,   255,   175,   736,     9,  1599,   568,
     363,  1050,   325,  1051,    23,    23,    23,  1869,  1202,  1375,
    1249,  2060,  1011,  1025,    23,   361,   752,   422,   390,  1032,
      23,   130,   402,   342,    23,   545,  1736,  2083,   667,    29,
    1871,   751,    23,   175,   327,   681,  1215,    23,   759,   687,
      23,    38,   339,   166,    23,   787,  1525,  1701,  1702,   339,
    1189,  -822,   666,   929,  1529,   138,   625,   799,  2148,  1026,
     786,   324,   138,  1029,  1012,  2061,  1222,  2062,  1743,  1035,
    1436,   842,   798,  1435,   360,   601,   602,   389,   603,  2024,
    2029,   401,    23,   842,   538,    23,    37,  1999,  2000,  1437,
    1354,   543,  1744,  1870,  1431,   643,  1053,    49,    50,    51,
      52,    53,   439,  1058,  1999,  2000,  1872,  1596,  1219,   588,
    1629,   239,  1225,  1556,    23,  1284,  1356,  1846,   143,  1873,
     340,   424,  -855,   138,   240,   143,  2076,   583,  2139,  -855,
       1,   604,   539,  1846,   175, -1154,    55,  2084,  1820,  1879,
     431,   605,  1735,  1882,   606,  2060,  1250,    23,  2093,  2094,
      25,   579,   325,  1706,   424,    23,    25,   384,   270,   271,
    2078,   272,   194,  2141,  2142,   651,  -696,  1319,  2149,   431,
    1314,  1703,  1646,  -696,    23,    23,  1418,  1250,  2001,     2,
      25,   431,   579,    25,  1880,  2082,   143,    23,   568,  1743,
    1328,   739,    23,    23,     3,  2034,  2196,     1,    32,  2061,
    1316,  2062,     1,   743,   694,   748,  1679,  2123,   700,   829,
     245,   324,   756,  1744,   273,   848,    23,  2082,   898,  1746,
      23,   568,   740,   256,   274,  2196,    23,   275,  -502,  1079,
      23,  2065,   241,   809,   938,   242,   243,   325,   244,   367,
    1423,  1527,   707,  1747,   251,   254,     2,    23,  1022,   945,
    2228,     2,  1948,    32,    23,    35,    36,   143,    23,    23,
     424,     3,  1705,   693,  1648,   325,     3,   699,   939,   940,
    1125,   356,   343,   945,  1735,  1391,   452,  1822,  1424,    23,
    1939,   795,  1131,   954,  -824,  1947,  1044,    23,  2119,    23,
     841,   843,  1082,  1115,  1010,  1006,   324,   780,    23,  1083,
      23,  1119,   363,    23,   518,   519,  1128,  1130,    34,   667,
      23,  2136,   384,  1203,   331,  1302,  1372,  1007,  1084,  1008,
    1011,  1285,   441,  1069,   324,   442,  1085,    23,    23,   727,
      23,  1086,   179,   666,  1700,    23,   728,  1087,  1736,    23,
     794,  1426,   986,  1180,   869,  2017,  1088,    23,  1846,  1010,
    1435,  1918,  1089,   327,  2090,   729,   809,   667,  1919,  1768,
    1769,   257,  1052,   730,  1406,  2065,  1407,  1090,   731,   667,
    1091,   518,  1012,  1025,   732,  1011,   831,  1920,  2016,   589,
    1317,   666,   180,   733,   103,   104,    36,   558,   327,   734,
    1621,  1623,  1625,   666,  1040,  1463,  1464,    23,   800,    23,
     998,  1411,    23,  -823,   735,  2131,    23,   736,  1318,    23,
     443,  1194,  1971,  1055,    36,  1972,   815,   568,  1195,  1319,
     792,  1062,   147,  1117,  1438,  2136,  1826,  1012,  1740,   147,
    1826,  1847,   343,   666,   266,   826,   869,   568,   266,  1501,
     109,   -23,  1440,  1439,   666,   568,  2102,  1847,   143,    23,
    1139,  2092,  1070,  1071,    23,   955,   683,   384,  1135,  1311,
     690,   741, -1013,    23,    23,  1138,  1890,   625,    23,  2109,
    1142,   489,   106,  1072,  1537,     1,   490,   143,  1537,   332,
     345,   137,     1,    23,   694,   700,  1891,   718,   137,   143,
     147,    23,    -3,  1538,    23,  1256,   148,  1752,   871,  1803,
     346,  1261,   744,   148,   528,   529,    23,    23,   345,   111,
     112, -1013,    49,    50,    51,    52,    53,   143,   277,  1804,
     491,    23,    23,   651,     2,   492, -1013,    32,   398,   343,
      32,     2,   601,   602,  2160,   603,     1,  1816,   788,     3,
     789,   832,  1302,   693,   699,   346,     3,    23,     1,   137,
     251,    23,  2187,    24,   530,   531,  1817,  1620, -1013,    24,
     518,   147,  1942,    32,   148, -1013,  1556,  1943,   251,   270,
     271,  -874,   272,  1076,   431,    23,  1102,  1105,     1,   334,
     347,    23,   666,    24,  1967,     2,    24,   840,   604,   237,
     335,  1816,   868,   384,   869,  1006,   869,     2,   605,  1739,
       3,   606,   739,   568,  2169,   348,   336,  1103,  1106,   904,
    1912,    23,     3,   707,  1351,     1,  1613,  1007,  1968,  1008,
     434,  1009,   694,   700,   423,   524,   525,     2,   744,    23,
      23,   959,    23,   740,   961,   148,   753,   625,  1492,  1220,
     744,   384,     3,  -503,    23,    23,   897,    24,    24,  1954,
    1002,   372,  1014,     1,  1943,    32,   869,     1,  1473,  1476,
       1,   869,  1847,   315,     2,  1254,  2171,  1919,    32,    23,
      23,    23,  1836,  1837,   986,    24,  1838,  1839,  1322,     3,
    1949,   693,   699,    23,   869,    23,  1981,     1,  1082,  1161,
    1300,  1603,  1306,   792,  2203,  1083,  1301,  1992,  1307,  1934,
    1057,  1919,     2,   935,   937,    32,     2,   324,   944,     2,
    2045,  -874,  1276,  1059,  1084,  2046,  1993,     3,  -874,  -498,
    1994,     3,  1085,  1229,     3,    24,   393,  1086,    24,   618,
       1,   558,  1077,  1087,   558,   419,     2,  1433,   480,  2182,
    -825,   568,  1088,   423,  2183,   975,    23,  1533,  1089,   744,
    2066,     3,   147,   574,    32,   780,   942,   488,   666,     1,
     943,   869,  1762,  1090,  1763,   666,  1091,    32,   986,  2067,
      49,    50,    51,    52,    53,   266,    32,   124,  1006,     2,
    2088,   147,   667,  -874,   124,  1050,  1051,   589,   521,   635,
      24,   869,  2241,   147,     3,   522,   523,  2242,   580,    23,
    1007,  2205,  1008,    23,  1285,   423,   666,   607,     2,   707,
    1374,   434,   963,     1,   136,  1046,   964,  1259,   246,   421,
     979,   147,   123,     3,   744,   398,   148,  1224,   175,  1150,
     621,   424,   175,    23,   424,    24,     1,   446,     1,    23,
     434,    23,   384,  1052,   449,   124,  1104,  1107,  1478,   131,
      23,   744,   434,  1162,   130,   148,   131,   792,   450,    24,
    1494,   455,     2,    24,  2199,  2200,   607,   148,   744, -1014,
     264,   423,   741,   607,  1273,   744,   166,     3,   493,   381,
     434,   534,   143,   395,   753,     2,  1095,     2,   744,  1796,
      24,   276,   104,  1273,  1299,   148,   651,   724,   651,   520,
       3,  1023,     3,  1381,   965,   641,   404,   535,   966,   862,
      23,   406,   568,  1906,   411,  1911,   416,   131, -1014,  1393,
     863,   864,    23,  1229,    23,  1852,    23,   536,   601,   602,
    1857,   603,    24, -1014,  1499,   318,  1023,  1428,   607,   537,
     641,    24,   574,    24,  1632,   540,  1174,   541,  1273,    32,
       1,  1178,   542,    24,  1446,  1447,   138,   266,   460,  -843,
    1149,   544,  1102,  1105,  1150, -1014,    23,    23,  1273,  1196,
     812,    24, -1014,   817,   707,   574,  1634,    23,   237,  1896,
     373,   374,   845,   375,   546,   849,  1160,   851,   131,   376,
      24,   853,   437,  1103,  1106,   584,   666,  1052,     1,     2,
     856,   526,   527,   858,   859,   860,  1282,  1283,  1126,  1390,
     607,   557,   744,  1255,     3,    23,   618,   943,   792,   143,
    1229,    49,    50,    51,    52,    53,   532,   533,    23,   587,
      23,    23,   175,   589,  1257,   589,   194,    -3,   943,     1,
     888,   608,   890,   808,   104,   893,   609,     2,  1496,    24,
    1497,    32,   901,  1273,    32,     1,   547,    23,   404,   406,
      23,   671,     3,   416,  1675,   610,   384,   693,   699,    49,
      50,    51,    52,    53,   635,    23,     1,   618,  1266,   518,
     423,  1226,   744,  1325,   744,  1793,  1326,   966,     2,   611,
     558,   612,   927,    49,    50,    51,    52,    53,   622,   613,
     586,   586,   707,     3,     2,   753,    24,    24,  1363,   744,
     663,    24,   869,   626,   618,    23,  1365,   106,    32,     3,
     869,     1,   103,   104,   638,     2,   953,  1030,  1819,   175,
    -501,   641,  1126,   715,    24,   967,   744,    24,    23,  1465,
       3,   697,   791,   871,   437,  1052,   639,   744,   878,   879,
    1481,  1637,   404,   241,   111,   112,   242,   243,  1229,   244,
      23,  1300,  1306,  1794,   982,   621,     1,  1301,  1307,  1493,
       2,   574,   635,  1273,  1273,   702,  1606,  2216,   109,   131,
    1607,  2220,   689,  1503,   651,     3,   147,   717,     1,  1836,
    1837,   574,   720,  1838,  1839,   742,   776,  1633,   779,   574,
    1519,   966,   721,    23,   342,   106,  1315,  1905,   131,  1126,
      23,   966,    23,   744,   771,     2,  1934,  1273,    24,     1,
     131,   106,  1930,  1354,  1048,  1935,   869,   618,     1,   772,
       3,   948,  1104,  1107,   785,   641,   339,     2,   638,  1355,
    1229,  1539,   111,   949,     1,   434,  1601,   871,   131,  1356,
       1,   744,     3,  1795,   318,   643,   797,  1827,   111,   112,
     148,  1827,  -500,     1,  1399,  1400,  1401,   175,     2,  1951,
     136,  1952,  1078,   869,   825,   966,  1262,     2,   123,   844,
     744,  1408,  1409,     3,   175,   830,  1953,   479,   870,   318,
     869,   880,     3,     2,   881,   707,    23,   883,     1,     2,
    1384,    49,    50,    51,    52,    53,   884,  1339,     3,   885,
     130,   744,     2,   902,     3,   886,  1343,   707,  1654,  1655,
     744,  2050,  2085,   147,    23,   869,   869,     3,   621,    23,
      23,    23,  1347,   675,   676,  1662,   744,  1664,  1467,    23,
     677,   678,   744,    23,  2186,    23,   934,     2,   869,   175,
     106,  1471,   946,   419,  2260,   641,  1667,   574,  2257,     1,
     589,   933,     3,   952,  -423,  -423,  1299,   651,   651,  2266,
     638,   884,   727,  2267,   957,  1774,   948,   106,   960,   728,
     641,     1,   137,   962,  1164,   394,  1474,   111,   949,    24,
     641,   869,  1221,    23,    23,    23,    23,   148,   729,     1,
    1451,   677,  1159,  1982,  1999,  2000,   730,   744,     2,  1216,
    1217,   731,   138,   969,   111,   112,   970,   732,   971,    32,
     905,   906,   907,     3,   620,   136,   733,  1312,  1313,   106,
       2,   972,   734,   123,   973,  1229,     1,   995,   618,   175,
    1358,  1359,   997,   974,    23,     3,  -423,   735,     2,   996,
     736,  1697,  1015,   707,   643,   948,    23,    23,    23,   641,
     701,    23,     1,     3,   707,   130,   111,   949,   -18,  1504,
    1066,   460,  1067,   744,     1,   143,  1075,  2129,  1421,  1422,
    1082,     1,   390,   402,    24,     2,  1829,  1083,  1425,  1422,
    1829,  1429,  1422,    24,   175,   574,  1080,   635,  1495,  1422,
       3,    23,  1626,  1627,    23,    23,  1084,  1676,   869,   622,
    1092,     2,  1739,  1740,  1085,  1782,  1783,   842,  1908,  1086,
    1761,  1422,  1093,     2,  1508,  1087,     3,  1094,   744,    23,
       2,    23,  1813,  1096,  1088,  1097,  1384,  1098,     3,  1099,
    1089,   389,   401,  1100,   175,     3,  2118,  1101,   586,    23,
    1512,  1902,  1903,  1904,   744,  1090,  1888,  1422,  1091,  1153,
     404,  1136,  1609,  1814,  -499,  1137,   744,   138,  1109,  2189,
    1681,  1682,  1683,   744,    49,    50,    51,    52,    53,  1630,
    1889,  1422,   494,  1631,   495,   496,   497,  1294,  1146,   136,
    1922,  1922,  1922,   136,   136,    49,    50,    51,    52,    53,
      23,  2048,  2049,   175,    23,  1147,  1697,   175,   175,    24,
     136,  1999,  2000,   131,   912,   913,   914,   915,   394,   498,
      30,   384,   499,   500,   175,  1156,   707,   501,   502,  1148,
     143,  1157,   452,  2257,  2258,  1158,   795,  1813,  1163,  1813,
    1860,  1861,  1862,  1863,  1864,   547,   739,  1165,   707,   707,
    1419,  1420,   908,   909,   238,   697,   779,  1166,    23,  -124,
    -124,  -124,  -124,  -124,  -124,  1167,   574,  1168,  1814,  1169,
    1814,   910,   911,  1170,   125,   916,   917,   740,   124,   622,
     707,   125,  1230,   727,  1698,  1881,  1883,  1171,  1923,  1924,
     728,  1173,  1172,  1175,  1176,   794,  1714,  1716,  1718,  1177,
    1383,  1724,  1192,    24,   248,    23,  1193,    23,  1200,   729,
    1398,  1205,    23,  1206,    23,   589,  1910,   730,  1394,  1214,
    1218,  1227,   731,    23,  1237,  1961,  1241,   175,   732,  1243,
    2073,   138,  1258,  1246,   579,   138,   138,   733,  1278,  1279,
    1280,   707,   125,   734,  1286,  1287,  1288,  1289,   707,   392,
     131,  1310,   138,   285,   405,  1102,  1105,   425,   735,  1290,
     409,   736,  1291,  1292,   418,  1293,  1324,  1320,  1836,  1837,
     420,  -166,  1838,  1839,  1327,   792,   382,  1330,  1331,   147,
     125,   707,   426,  1332,   427,   869,  1103,  1106,  1068,  1771,
    1333,   476,  1334,  1335,   143,  1934,  1336,  1337,   143,   143,
    1338,  1352,  1815,    24,  1940,   666,  -123,  -123,  -123,  -123,
    -123,  -123,  1389,  2044,  1361,   143,  1364,  1366,  1392,  1402,
      23,  1403,  1404,    23,    23,   136,  1405,  1415,  1412,  1410,
    2175,   678,  1432,   707,   707,  1813,  1454,  1449,  1598,   175,
     707,  1452,  1441,    32,  1818,  1453,  1457,    32,  1458,   707,
    1459,  1460,  1462,   148,   384,   589,  1461,   589,  1466,   707,
    1487,   707,   576,  1490,  1491,  1498,  1814,  1530,  1531,  1604,
    2194,  1532,  2073,  1617,   707,  2042,   707,   707,   707,  1605,
      12,   125,  1230,  1608,    15,    16,    17,    18,    19,    20,
      21,    22,  1618,   614,  1619,  1628,  1639,   589,  1640,  2124,
    1643,  1644,  1649,    23,    -3,  1650,  1656,  1815,   607,  1815,
     707,   673,  2218,  1663,   707,   674,   741,  1665,  1666,   707,
    1668,  1672,   285,   270,   271,  1673,   272,  1674,  1678,  1680,
    1733,  1699,   719,  1684,   147,  1296,   108,   696,    24,    24,
      49,    50,    51,    52,    53,   866,   -22,    23,  1707,    23,
    1708,  1711,  1712,   792,   819,  2042,   739,  1721,   113,   285,
     722,   723,  1725,  1925,  1964,  1726,   628,   138,  1728,   589,
     745,  1737,   869,   749,   750,  1753,  1754,   754,   707,  1230,
     757,   758,    24,   760,  2259,   761,    24,   740,  1738,   398,
     707,  1760,   679,  1598,   707,  1764,   685,  1766,   783,  1767,
    1313,  1777,  1784,  1778,    23,  1779,  1780,  1781,   148,  1786,
     707,  1785,  2080,  1788,   796,  1789,  1102,  1105,  1790,   801,
     828,   804,   285,    23,    23,  1104,  1107,   136,   136,  1792,
     143,   807,  2124,  1802,  1700,   476,  2124,  2124,   476,   382,
    1833,   175,   175,   827,   476,  2112,  1848,  1103,  1106,  1849,
    1851,  1853,  2227,   476,  1855,   707,  1856,  1868,  1740,  1875,
    1876,   324,    49,    50,    51,    52,    53,  1556,  2236,  1886,
    2239,    49,    50,    51,    52,    53,  1179,   476,  1887,  1897,
    1900,  1901,  1931,   340,    23,    23,  1926,    24,  1806,  1927,
    1933,  2251,  1958,  1960,   707,  2251,    24,  1974,   147,    24,
      24,    24,   147,   147,    24,  1815,   480,    24,    32,  1975,
    2261,  1991,  2007,  2012,  2013,  2031,  2033,  1230,  2047,   147,
    2043,   693,   699,  2038,  2052,  2056,   744,  2059,  2071,  2087,
     707,  2089,  2096,   707,   124,  2101,  2103,  2128,  2110,  2114,
    2121,   822,  1321,  2133,   817,  1398,  2122,  2134,  2135,  2143,
    2152,  2168,  2170,   707,  2172,  2176,  2180,  1831,  2179,  2181,
    2184,  1831,  1831,  2185,  2202,    23,  2206,  2217,  2204,   138,
     138,  2214,   148,  2223,    23,  2229,   148,   148,  1831,  2238,
    2240,  2219,    49,    50,    51,    52,    53,   673,  2244,   372,
     106,   931,    24,   148,  2248,   956,  2253,  1756,   958,  1230,
    2250,  2254,  2264,   865,  2265,  2268,   131,  1722,   918,   285,
     919,   922,   920,   925,   516,  1732,  1982,  1540,  2195,   921,
     744,  2249,   398,  1835,   976,  2035,   741,   111,   112,  1859,
    2213,  2023,   143,   143,    12,   285,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,  2095,    24,     1,   294,
    2188,   628,  1970,  2174,  1956,  1957,  2113,  2221,  2255,  1018,
    1024,  2173,  1600,  1027,  1028,  1690,  1031,  1691,  1033,  1034,
     285,  1692,   250,  1036,  1037,   413,  1413,   784,  2193,  1414,
    2070,  1801,  1642,  1750,  1118,  1895,  1104,  1107,  1323,   124,
    1239,    24,    24,  1073,  1765,     5,  1427,     2,    12,   984,
     349,   350,    15,    16,    17,    18,    19,    20,    21,    22,
     106,   181,     3,  1183,  1723,  1184,  1185,  1000,  1677,   285,
       0,   285,     0,     0,     0,     0,     0,   106,     0,  1450,
      10,     0,     0,     0,   147,     0,   871,     0,     0,     0,
     744,     0,  1116,   381,   395,     0,     0,   111,   112,  1043,
    1120,     0,  1693,  1805,   108,     0,     0,  1108,   822,     0,
    1806,   131,     0,     0,   111,   112,    24,     0,     0,     0,
    1054,     0,     0,     0,     0,     0,   113,  1134,     0,     0,
       0,   745,     0,     0,    24,     0,   836,     0,     0,  1141,
       0,    24,     0,  1831,  1230,     0,     0,     0,     0,     0,
       0,     0,  1182,     0,     0,  2262,     0,     0,   148,     0,
       0,    11,     0,     0,  2269,     0,     0,     0,   294,   836,
     285,   631,     0,     0,    24,  1143,  1144,  1145,   670,    12,
       0,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,   755,     0,   631,     0,     0,
       0,   631,     0,   124,     0,   294,     0,   124,   124,    12,
       0,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,   822,     0,     0,   124,     0,    24,    24,     0,     0,
       0,     0,     0,    24,    33,     0,     0,  1187,  1614,     0,
    1190,  1191,    24,     0,     0,     0,     0,   791,     0,     0,
       0,     0,    24,     0,    24,  1798,  1269,  1800,     0,     0,
    1270,     0,     0,     0,     0,     0,     0,    24,   294,    24,
      24,    24,  1638,     0,     0,   131,     0,     0,     0,   131,
     131,     0,     0,     0,     0,     0,   147,   147,     0,  1713,
       0,     0,  1152,     0,     0,     0,   131,     0,     0,     0,
       0,     0,     0,    24,  1308,     0,     0,    24,     0,     0,
     877,  1309,    24,     0,     0,  1018,     0,     0,  1245,     0,
    1247,     0,     0,   285,   294,   125,   381,   395,     0,   125,
       0,     0,     0,     0,     0,  1263,     0,     0,     0,   822,
       0,     0,  2120,     0,     0,  1831,  1831,     0,     0,     0,
       0,     0,    49,    50,    51,    52,    53,   294,     0,     0,
     148,   148,     0,     0,     0,   836,     0,  1907,     0,     0,
    1231,    24,     0,     0,     0,     0,   285,  1049,     0,     0,
    1297,   984,     0,    24,     0,   836,     0,    24,     0,     0,
       0,     0,     0,   836,     0,   285,     0,  2161,     0,     0,
       0,     0,     0,    24,    12,     0,     0,   819,    15,    16,
      17,    18,    19,    20,    21,    22,   822,     0,     1,  1275,
       0,     0,     0,     0,  1043,   791,     0,     0,     0,     0,
       0,     0,    49,    50,    51,    52,    53,     0,     0,   124,
    1759,     0,   977,   822,     0,   980,     0,   822,    24,     0,
     285,     0,     0,     0,     0,   294,   631,  1340,  1360,     0,
     108,  1344,     0,  1060,     0,  1348,     0,     2,     0,     0,
     285,   822,     0,  1775,     0,   984,  1369,     0,     0,  1373,
     106,   294,     3,  1376,     0,     0,     0,    24,     0,     0,
       0,   822,     0,     0,     0,     0,     0,     0,   822,     0,
       0,     0,     0,     0,   631,     0,   948,   670,     0,     0,
     641,   131,     0,  1382,     0,     0,   294,   111,   949,  1388,
       0,     0,   631,    24,     0,     0,    24,     0,     0,   125,
       0,    12,     0,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,  1455,     0,     0,    24,  1456,     0,     0,
       0,   836,     0,   294,   631,   285,     0,     0,    24,     0,
     106,     0,     0,   755,     0,     0,     0,    24,     0,     0,
    1231,     0,     0,   294,     0,   631,     0,     0,     0,     0,
       0,   294,     0,     0,     0,     0,   640,   108,     0,   708,
     641,     0,     0,     0,   822,  1893,  1894,   111,   642,   258,
      41,    42,    43,    44,    45,    46,    47,    48,  1297,   113,
     258,    41,    42,    43,    44,    45,    46,    47,    48,     0,
    1516,     0,     0,     0,  1517,   931,   125,     0,  1518,     0,
       0,  1223,  1836,  1837,  2025,  2026,  1838,  1839,  2027,  2028,
       0,   124,   124,     0,    49,    50,    51,    52,    53,   869,
       0,     0,  1468,     0,     0,  1472,  1475,     0,     0,  1934,
       0,     0,     0,  1484,  1485,     0,   294,  1231,  -198,     0,
       0,     0,     0,     0,     0,   285,   285,     0,   403,  1950,
       0,     0,     0,     0,  1274,     0,     0,     0,     0,   836,
    1500,     0,  1502,     0,     0,  1505,     0,     0,  1509,     0,
       0,     0,  1513,  1274,   877,   877,   285,     0,     0,     0,
       1,     0,     0,   131,   131,  1528,     0,     0,  1973,   285,
       0,     0,     0,  1976,     0,     0,     0,     0,     0,     0,
     822,     0,     0,    30,   822,     0,     0,     0,     0,   294,
       0,     0,     0,     0,     0,  1534,     0,     0,  1657,     0,
       0,  1388,  1658,     0,     0,  1659,     0,   330,  1274,     2,
       0,     0,     0,     0,   125,     0,     0,     0,  1610,     0,
       0,     0,   106,     0,     3,     0,     0,     0,  1274,     0,
     150,   125,     0,     0,     0,  1669,     0,   150,     0,  1670,
       0,     0,     0,  1671,     0,     0,     0,     0,  1982,     0,
       1,     0,   744,  1264,     0,  1231,  1267,   822,     0,   111,
     112,   822,    12,     0,     0,   822,    15,    16,    17,    18,
      19,    20,    21,    22,   891,   631,     0,     0,     0,     0,
       0,  1472,     0,    12,     0,     0,   631,    15,    16,    17,
      18,    19,    20,    21,    22,     0,   125,     0,   150,     2,
    1536,     0,     0,   285,   285,     0,     0,     0,     0,   297,
       0,     0,   106,  1274,     3,     0,   150,     0,     0,     0,
       0,   892,     0,     0,   631,     0,     0,   294,     0,   631,
       0,     0,   387,  1704,     0,     0,   150,  1231,   107,   108,
    1758,     0,     0,     0,     0,     0,     0,    10,     0,   111,
     112,     0,  1729,     0,    54,     0,   598,     0,     0,   476,
       0,   113,     0,     0,     0,     0,     0,     0,   755,   150,
       0,     0,   637,   150,     0,  1341,     0,     0,     0,  1345,
     631,     0,     0,  1349,     0,   822,   125,     0,     0,   822,
       0,     0,   822,     0,     0,     0,     0,     0,     0,   297,
      89,    90,    91,    92,    93,    94,    95,    96,  1061,    12,
     708,  1132,   285,    15,    16,    17,    18,    19,    20,    21,
      22,     0,   822,     0,     0,     0,   822,     0,    11,     0,
     822,     0,     0,  1274,  1274,     0,     0,   150,     0,     0,
     382,   125,     0,     0,   564,   150,    12,   581,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,  1772,     0,
       0,     0,     0,     0,     0,   297,     0,     0,     0,   877,
     781,   877,     0,     0,     0,   631,     0,  1274,   297,   150,
       0,   297,     0,  1787,     0,     0,     0,   387,   150,     0,
       0,   125,  1821,  1823,     0,     0,  1534,  1534,  1534,     0,
    1797,   238,     0,  1799,     0,     0,     0,   150,     0,     0,
       0,   150,     0,     0,     0,   297,     0,     0,   387,     0,
       0,     0,   150,     0,     0,   150,   330,   822,     0,     0,
       0,     0,  1231,     0,   847,     0,     0,     0,     0,     0,
     598,  1884,     0,   854,     0,     0,     0,     0,     0,     0,
     125,     0,  1874,     0,   125,   125,     0,     0,     0,     0,
     150,   330,     0,     0,     0,     0,     0,     0,  1885,     0,
    1469,   125,   631,     0,     0,     0,     0,   150,   150,   150,
       0,     0,  1892,     0,     0,     0,     0,     0,     0,   150,
       0,     0,  1898,  1899,   382,   387,  1210,     0,     0,     0,
    1210,   150,     0,     0,     0,     0,   708,     0,     0,     0,
       0,  1652,     0,  1506,     0,     0,  1510,     0,     0,   813,
    1514,     0,   150,     0,     0,     0,     0,   745,     0,   150,
    1210,     0,   387,   150,   297,   150,     0,   387,     0,     0,
       0,     0,     0,   297,     0,     0,   297,     0,   150,   150,
       0,     0,   297,     0,     0,  1955,     0,   637,     0,     0,
       0,   297,     0,     0,   150,   150,   150,   297,     0,     0,
     297,     0,   551,   382,   125,     0,     0,   387,     0,     0,
     552,   553,   554,   555,     0,     0,  1611,   968,     0,    12,
       0,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     631,     0,  1977,     0,     0,     0,     0,  1978,  1979,  1980,
      12,     0,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    12,     0,  1996,     0,    15,    16,    17,    18,
      19,    20,    21,    22,   445,     0,   448,   387,     0,   451,
     454,     0,     0,     0,     0,  1045,     0,     0,     0,     0,
     463,   464,     0,     0,  1210,   297,   150,     0,   387,  1715,
       0,   708,   382,     0,  1061,     0,     0,   451,   451,   556,
       0,     0,     0,     0,     0,     0,   125,     0,     0,  2057,
    2058,   297,   631,     0,     0,     0,   150,   557,  2064,     0,
       0,     0,     0,     0,  2068,  2069,     0,   598,     0,     0,
       0,     0,   304,   581,   150,     0,   992,   297,     0,   451,
       0,     0,   387,   150,     0,     0,   297,     0,   387,     0,
       0,     0,   150,     0,     0,     0,     0,     0,     0,   150,
       0,     0,  2040,     0,   451,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,     0,
       0,   598,   598,   297,   150,     0,     0,     0,   387,     0,
       0,  2132,     0,   813,     0,   150,     0,   150,     0,   387,
       0,  2137,     0,   297,     0,   150,     0,     0,     0,     0,
     150,   150,     0,     0,     0,  2146,     0,   387,     0,   708,
       0,     0,     0,     0,     0,     0,     0,     0,   294,     0,
       0,     0,  1210,     0,     0,     0,   297,     0,     0,     0,
    2224,     0,  2040,     0,     0,     0,     0,  2146,   781,     0,
       0,    12,  2064,   808,   104,    15,    16,    17,    18,    19,
      20,    21,    22,  1113,     0,     0,     0,     0,  2190,     0,
       0,     0,   150,    12,     0,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,  2215,     0,     0,
     297,   297,     0,     0,     0,     0,   297,     0,     0,     0,
       0,   304,   106,     0,     0,     0,     0,   631,   125,   125,
     926,     0,     0,    12,   150,   577,   578,    15,    16,    17,
      18,    19,    20,    21,    22,  2237,     0,     0,  2116,   108,
       0,  2226,   744,     0,     0,     0,     0,   387,   304,   111,
     112,     0,     0,   150,     0,     0,   150,     0,     0,     0,
       0,   113,   387,   451,     0,     0,   150,   150,     0,     0,
    2246,     0,     0,  2226,     0,   150,     0,     0,     0,   297,
     598,   109,     0,     0,     0,   133,   598,     0,     0,     0,
     150,     0,   133,   579,  2246,     0,     0,     0,     0,     0,
     150,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1045,   304,   451,   451,   451,   451,   451,   451,   451,   451,
     451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
     451,     0,     0,     0,     0,     0,     0,   822,     0,    49,
      50,    51,    52,    53,   708,     0,     0,     0,     0,     0,
       0,  1210,     0,   133,     0,     0,  1210,  1210,  1210,   150,
       0,   150,     0,     0,   288,   150,     0,     0,     0,     0,
       0,   319,     0,     0,     0,     0,   150,   150,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   150,
       0,   396,     0,     0,     0,   297,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     1,     0,     0,   150,     0,
       0,     0,   150,     0,   150,     0,     0,   297,     0,   297,
    1986,     0,     0,   992,   433,     0,     0,     0,   438,     0,
     387,   150,   150,     0,     0,     0,     0,     0,    12,     0,
     349,   350,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,   150,     0,     2,   451,     0,     0,     0,   150,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
     150,   992,     0,    12,     0,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,   150,     0,   475,     0,
       0,     0,   548,   640,   108,  1986,  1986,   641,   304,     0,
     569,     0,   106,     0,   111,   642,   150,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,   599,     0,     0,   304,     0,     0,   631,   640,   108,
       0,     0,   641,   617,   623,     0,   629,     0,     0,   111,
     642,     0,     0,   669,     0,     0,     0,     0,     0,     0,
       0,   113,   643,  1986,     0,   150,  1210,   598,  1210,   304,
       0,     0,   680,     0,     0,   150,   680,     0,     0,     0,
     288,     0,     0,     0,     0,     0,  1986,   698,     0,     0,
     623,     0,     0,     0,     0,     0,     0,     0,   150,     0,
       0,   150,     0,     0,     0,   150,     0,     0,     0,     0,
       0,   150,     0,     0,     0,     0,   737,   631,   304,     0,
     304,     0,     0,     0,    12,   433,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,   764,     0,     0,  1986,
    1986,   774,   698,   288,   319,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   433,     0,  2191,     0,     0,   451,
    1986,     0,     0,   708,   451,     0,   433,     0,     0,     0,
     802,   387,   150,     0,     0,   805,   297,     0,     0,     0,
     806,     0,   451,     0,     0,     0,     0,   818,  1448,     0,
    1717,     0,     0,     0,   433,     0,     0,     1,   833,   569,
    1986,   150,     0,     0,     0,   150,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,  1710,   708,     0,   304,
       0,   150,   150,     0,     0,   451,     0,  1727,     0,     0,
      12,     0,   569,     0,    15,    16,    17,    18,    19,    20,
      21,    22,   475,     0,     0,   475,     2,     0,     0,    54,
       0,   475,   150,     0,     0,     0,     0,     0,     0,     0,
     475,     3,     0,     0,     0,   150,  1151,     0,     0,     0,
       0,   598,     0,     0,     0,     0,     1,     0,   150,     0,
       0,     0,   150,     0,   475,     0,   150,     0,     0,     0,
       0,     0,   451,     0,     0,     0,     0,     0,  1524,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,    12,
     150,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,   150,     0,     0,     0,     2,     0,   297,     0,     0,
     617,   629,     0,     0,     0,     0,     0,   924,   106,     0,
       3,     0,     0,    12,     0,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,   895,   288,     0,     0,     0,
       0,     0,     0,     0,  2116,   108,     0,     0,   744,     0,
       0,     0,   304,   451,     0,   111,   112,     0,     0,   680,
     150,     0,   994,     0,     0,     0,     0,   113,   623,     0,
       0,   617,   150,     0,     0,     0,     0,  1001,     0,   150,
     150,     0,   896,     0,   818,     0,   150,     0,  1017,     0,
     150,  1877,  1878,   150,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   304,     0,     1,  1042,   629,
       0,     0,     0,     0,     0,  1047,     0,     0,     0,     0,
     288,     0,   288,   150,   304,     0,     0,   150,   569,     0,
     680,   150,     0,     0,     0,   623,   569,     0,     0,     0,
      12,     0,   349,   350,    15,    16,    17,    18,    19,    20,
      21,    22,     0,   150,     0,     0,     2,   451,   451,   451,
       0,     0,   297,     0,     0,     0,     0,     0,     0,     0,
       0,     3,     0,     0,   451,   451,     0,     0,     0,   304,
       0,     0,     0,     0,  1932,     0,     0,   297,   297,     0,
       0,  1938,     0,     0,   598,  1296,   108,   818,     0,   304,
       0,     0,     0,   451,     0,     0,     0,  1123,     0,     0,
       0,     0,     0,     0,     0,     0,   387,   150,   113,     0,
       0,   617,     0,     0,  1963,     0,   150,     0,   150,     0,
      49,    50,    51,    52,    53,   598,     0,     0,     0,   433,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1127,
    1129,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1770,     0,     0,     0,     0,     0,     0,   150,   818,     0,
       0,   698,     0,     0,     0,   144,  1997,  1998,     0,     0,
       0,   698,   144,  2008,   304,     0,     0,     0,     0,     0,
       0,     0,  2022,     0,   569,     0,     1,     0,     0,   451,
       0,   451,  2036,   297,  2037,   623,   150,     0,   150,     0,
       0,     0,     0,   387,     0,   818,  1186,  2051,     0,  2053,
    2054,  2055,     0,     0,     0,     0,   297,     0,     0,    12,
     297,   297,     0,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   144,   297,     2,     0,   297,     0,     0,
       0,     0,     0,  2081,   292,     0,     0,  2086,   106,     0,
       3,     0,  2091,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,   288,     0,   133,     0,     0,     0,
    1232,   397,     0,     0,   107,   108,     0,     0,     0,     0,
       0,   680,   818,     0,  1244,   111,   112,     0,     0,     0,
       0,     0,     0,     0,   304,   304,     0,   113,   265,     0,
       0,     0,     0,     0,   144,     0,     0,     0,   150,   818,
       0,  2138,     0,   818,     0,     0,     0,   288,     0,   680,
       0,     0,  1042,  2147,   629,   304,     0,  2150,     0,   385,
       0,     0,     0,     0,     0,     0,   288,   818,   304,   387,
     150,   408,   412,  2167,     0,     0,     0,     0,  1295,     0,
       0,     0,     0,     0,     0,     0,     0,   818,     0,     0,
       0,     0,     0,    12,   818,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,   680,     0,     0,     0,     0,
     570,     0,     0,     0,     0,     0,   461,     0,  2201,     0,
       0,   288,   106,     0,  1535,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,     0,     0,     0,     0,
       0,   288,     0,   292,     0,   451,   630,     0,  1805,   108,
       0,     0,     0,   397,     0,  1806,     0,  2222,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,   113,   630,     0,     0,     0,   630,     0,     0,   297,
     292,     0,   297,     0,     0,     0,     0,     0,     0,     0,
     818,     0,     0,  2243,     0,     0,  2247,   150,     0,     0,
    1232,     0,   304,   304,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,     0,  2256,     0,     0,     0,
       0,     0,     0,   569,     0,   144,   288,     0,     0,    12,
     818,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   292,     0,   385,   451,     0,     0,     0,
       0,     0,     0,     0,   144,     0,  2100,   387,   106,     0,
       0,     0,     0,     0,     0,     0,   144,     0,     0,     0,
     803,     0,     0,     0,     0,     0,     0,   150,     0,     0,
     387,     0,     0,     0,   762,   108,     0,   570,     0,     0,
       0,     0,     0,     0,   144,   111,   112,  1232,   397,   292,
       0,     0,     0,     0,     0,     0,     0,   113,     0,     0,
       0,   304,     0,     0,     0,    49,    50,    51,    52,    53,
       0,     0,   385,     0,     0,     0,   818,     0,     0,     0,
     818,     0,   292,     0,     0,     0,  2162,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   288,   288,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   774,   385,
       0,     0,     0,     0,   385,     0,     0,     0,     0,  1488,
       0,     0,     0,     0,     0,     0,     0,   288,     0,     0,
       0,     0,     0,     0,   150,   150,     0,     0,     0,    12,
     288,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   818,   385,     0,     0,   818,     0,     0,
       0,   818,     0,     0,    12,     0,   349,   350,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,   150,     0,
     292,   630,     0,     0,     0,  1232,     0,     0,     0,     0,
       0,     0,     0,   106,  1296,   108,   818,     0,     0,     0,
       0,     0,  1597,  1520,     0,    12,   292,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,   113,     0,   351,
     108,     0,     0,     0,   385,     0,     0,   451,     0,   630,
     111,   112,   397,    49,    50,    51,    52,    53,     0,     0,
       0,   292,   113,     0,     0,   385,     0,   630,     0,     0,
       0,     0,     0,    12,   570,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,  1232,   936,     0,
       0,     0,     0,     0,   288,   288,     0,     0,   292,   630,
       0,   818,   106,     0,     0,   818,     0,     0,   818,     0,
     570,     0,   570,   385,     0,     0,     0,     0,   292,   385,
     630,     0,     0,     0,     0,   385,   292,     0,  2116,   108,
       0,     0,   744,     0,     0,     0,     0,     0,   818,   111,
     112,     0,   818,     0,     0,     0,   818,     0,     0,     0,
       0,   113,    12,     0,   349,   350,    15,    16,    17,    18,
      19,    20,    21,    22,     0,   385,     0,     0,  1694,     0,
       0,   451,     0,     0,     0,     0,   385,  1597,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   570,     0,     0,
       0,     0,     0,     0,   385,     0,     0,     0,     0,   461,
       0,     0,     0,   288,     0,     0,     0,  1303,   108,     0,
       0,   292,     0,     0,     0,     0,     0,     0,   111,  1304,
     451,     0,     0,     0,     0,     0,     0,     0,     0,   144,
     113,     0,  1751,     0,     0,     0,   475,     0,     0,     0,
    1112,    12,     0,   818,     0,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     0,   570,     0,
       0,     0,     0,     0,     0,   149,     0,     0,     0,     0,
     106,     0,   149,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1232,    12,   292,     0,   451,    15,    16,    17,
      18,    19,    20,    21,    22,  1207,  1486,   108,   385,     0,
    1208,     0,  1209,     0,     0,   570,     0,   111,   112,     0,
       0,   451,     0,   451,   385,     0,     0,     0,     0,   113,
       0,   698,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,   149,     0,     0,     0,     0,     0,   108,
       0,  1830,  1417,     0,   296,  1830,  1830,     0,     0,     0,
       0,     0,     0,   451,     0,     0,     0,     0,     0,     0,
       0,     0,  1830,     0,   570,     0,   144,     0,     0,     0,
     630,   399,     0,     0,     0,  1824,  1832,     0,     0,  1824,
    1843,   630,   570,     0,     0,  1850,     0,     0,     0,  1854,
       0,     0,     0,     0,  1858,     0,  1843,     0,     0,     0,
       0,     0,     0,     0,   149,     0,     0,     1,     0,   570,
       0,     0,     0,   570,     0,     0,     0,   570,     0,   630,
       0,     0,   292,     0,   630,   451,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   570,   570,     0,     0,
      12,  1916,   349,   350,    15,    16,    17,    18,    19,    20,
      21,    22,     0,     0,     0,     0,     2,   570,     0,     0,
       0,     0,     0,     0,   570,  1929,     0,     0,     0,   106,
       0,     3,     0,     0,     0,   630,     0,     0,     0,     0,
     572,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,   570,     0,     0,  1928,  2116,   108,   385,     0,   744,
       0,     0,     0,     0,     0,     0,   111,   112,     0,     0,
       0,   570,     0,   296,  1944,  1946,   632,     0,   113,     0,
       0,     0,     0,   399,    49,    50,    51,    52,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
       0,     0,   632,     0,  1966,     0,   632,     0,     0,     0,
     296,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     630,     0,     0,     0,  2018,     0,    12,  1830,   349,   350,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       1,     0,  2041,   570,     0,   149,   570,     0,     0,  2006,
     570,     0,     0,  2009,     0,  2011,     0,     0,  2015,  2021,
       0,  1843,     0,   296,     0,     0,  2032,  2063,     0,     0,
       0,     0,     0,    12,   149,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,   149,     0,     0,     2,
       0,     0,     0,     0,     0,     0,   579,     0,     0,     0,
       0,     0,   106,     0,     3,     0,     0,   572,     0,     0,
       0,     0,     0,     0,   149,     0,     0,   630,   399,   296,
       0,     0,  2041,     0,     0,     0,     0,     0,   351,   108,
       0,     0,     0,     0,     0,  2098,     0,     0,     0,   111,
     112,     0,  2105,     0,     0,     0,   570,  2107,  2108,     0,
     570,   113,   296,    49,    50,    51,    52,    53,     0,     0,
       0,     0,     0,     0,     0,     0,   570,   570,     0,     0,
    2130,     0,     0,     0,     0,     0,     0,   189,   385,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,   570,     0,     0,
       0,  2177,     0,     0,     0,     0,  2151,     0,  2154,     0,
     570,  2156,  2158,  2159,     0,     0,     0,     0,     0,  2164,
    2166,     0,     0,   570,     0,     0,     0,   570,     0,  1830,
    1830,   570,     0,     0,   486,     0,     0,   109,   201,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     296,   632,    12,     0,     0,   630,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,   570,     0,     0,     0,
       0,     0,   144,   818,     0,     0,   296,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,  2208,  2210,  2212,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   632,
       0,     0,   399,     0,     0,  1523,     0,   107,   108,     0,
       0,   296,     0,     0,     0,     0,     0,   632,   111,   112,
       0,  2231,  2233,  2235,   572,     0,     0,     0,     0,     0,
     113,     0,     0,     0,     0,     0,     0,   630,     0,     0,
       0,     0,     0,     0,   570,   570,     0,     0,   296,   632,
       0,   570,     0,     0,     0,   570,     0,     0,   570,     0,
     572,     0,   572,     0,     0,     0,     0,     0,   296,     0,
     632,     0,     0,     0,     1,    12,   296,   276,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,   570,     0,
       0,     0,   570,     0,     0,     0,   570,     0,  2072,     0,
       0,     0,     0,     0,     0,     0,     0,    12,     0,   349,
     350,    15,    16,    17,    18,    19,    20,    21,    22,     0,
       0,     0,     0,     2,     0,     0,     0,   144,     0,     0,
       0,   108,     0,     0,  1060,     0,   106,   572,     3,    12,
       0,   810,   811,    15,    16,    17,    18,    19,    20,    21,
      22,     0,   182,   292,     0,   183,     0,   184,   185,     0,
     186,   296,   351,   108,     0,     0,   269,     0,     0,     0,
       0,     0,     0,   111,   112,     0,     0,   187,     0,   149,
       0,     0,   397,     0,     0,   113,     0,     0,     0,   355,
       0,     0,     0,   570,     0,     0,     0,   109,     0,     0,
      49,    50,    51,    52,    53,     0,   188,   189,   572,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,     0,
     198,   199,   630,     0,   296,     0,     0,     0,   106,     0,
       0,    12,     0,   385,     0,    15,    16,    17,    18,    19,
      20,    21,    22,  1207,     0,   572,     0,     0,  1208,     0,
    1209,     0,     0,     0,   200,     0,     1,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   144,     0,     0,     0,   144,   144,   108,     0,    12,
    1622,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,   144,     0,   572,     2,   149,     0,     0,     0,
     632,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       3,   632,   572,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,   645,     0,     0,     0,     0,
       0,     0,     0,     0,   762,   108,     0,     0,     0,   572,
       0,     0,     0,   572,     0,   111,   112,   572,     0,   632,
       0,     0,   296,     0,   632,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,   572,   572,     0,     0,
       0,     0,     0,     0,   117,     0,     0,   385,     0,     0,
       0,   117,     0,     0,     0,     0,     0,   572,     0,     0,
     726,   355,     0,     0,   572,   397,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   632,     0,     0,     0,     0,
       0,   766,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   572,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,     0,     0,     0,     0,
       0,   572,   117,     0,     0,     0,     0,     0,     0,     0,
     267,     0,     0,   282,     0,     0,   385,     0,     0,     0,
     117,     0,    49,    50,    51,    52,    53,     0,     0,     0,
       0,   766,     0,   352,     0,     0,   380,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     572,     0,     0,     0,     0,     0,     1,     0,     0,     0,
     632,     0,     0,     0,     0,     0,     0,   144,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   436,     0,     0,
       0,     0,   630,   572,     0,     0,   572,     0,     1,    12,
     572,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   471,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,   106,     0,
       3,    12,     0,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     2,     0,     0,
       0,   436,     0,     0,  1303,   108,   766,     0,     0,   567,
     106,     0,     3,     0,     0,   111,  1304,   632,     0,     0,
       0,     0,   630,     0,     0,     0,     0,   113,     0,   471,
     267,   267,     0,     0,     0,     0,  1805,   108,     0,     0,
       0,     0,   282,   436,     0,   282,   572,   111,   112,   644,
     572,   665,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,   572,   572,     0,     0,
       0,   567,     0,     0,     0,   567,     0,     0,     0,   282,
       0,     0,   380,     0,     0,     0,   267,   385,     0,   436,
       0,     0,     0,     0,     0,     0,     0,   572,     0,  1005,
       0,     0,     0,   645,     0,     0,     0,     0,     0,     0,
     572,     0,     0,     0,   352,   352,     0,     0,     0,     0,
       0,     0,     0,   572,     0,     0,     0,   572,     0,   144,
     144,   572,     0,     0,     0,   763,     0,     0,     0,     0,
       0,     0,   567,   117,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   632,     0,     0,     0,   380,
       0,     0,     0,     0,     0,     0,   572,     0,     0,     0,
       0,     0,   149,   570,     0,     0,     0,     0,     0,     0,
       0,     0,   766,    12,  1081,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,   763,   355,   355,   282,   436,
       0,   838,     0,     0,     0,     0,     0,   471,     0,     0,
     471,     0,   436,   436,     0,     0,   471,     0,     0,     0,
       0,  1122,   766,   766,     0,   471,     0,     0,   436,   436,
     436,   282,     0,     0,   471,   766,     1,   632,   639,     0,
     872,   838,     0,     0,   572,   572,     0,     0,     0,     0,
       0,   572,     0,     0,     0,   572,     0,    12,   572,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,    12,
       0,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     2,     0,     0,   572,     0,
       0,     0,   572,     0,     0,     0,   572,     0,   106,     0,
       3,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     763,   838,     0,     0,     0,   109,    49,    50,    51,    52,
      53,     0,     0,     0,   762,   108,     0,   149,     0,   282,
     567,   950,   665,    12,     0,   111,   112,    15,    16,    17,
      18,    19,    20,    21,    22,  1207,     0,   113,     0,     0,
    1208,     0,  1209,   296,     0,   282,     0,     0,     0,     0,
     436,     0,   267,   267,   503,   504,   505,   506,   507,   508,
     509,   510,   511,   512,   513,     0,     0,     0,   567,     0,
     990,     0,   399,   334,     0,     0,   838,   436,     0,   108,
     282,     0,   665,   572,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   644,     0,     0,     0,   644,     0,     0,
     514,     0,     0,     0,     0,    12,     0,   349,   350,    15,
      16,    17,    18,    19,    20,    21,    22,   282,   567,     0,
       0,     0,   632,     0,     0,     0,     0,     0,     0,   567,
       0,   567,     0,   665,   106,     0,     0,   282,     0,   567,
       0,     0,     0,     0,   436,   567,     0,     0,     0,     0,
       0,     0,  1005,     0,     0,     0,     0,     0,     0,     0,
    1805,   108,     0,     0,  1298,     0,   645,     0,   645,     0,
     471,   111,   112,     0,     0,     0,   763,     0,   352,     0,
       0,   149,     0,   113,     0,   149,   149,     0,     0,     0,
     352,   352,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   149,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,   763,   763,   763,     0,     0,
       0,     0,     0,     0,   471,   471,     0,     0,     0,   763,
     282,     0,  1479,     0,     0,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,    12,     0,   349,   350,    15,
      16,    17,    18,    19,    20,    21,    22,   766,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
    1362,     0,   766,   766,   106,     0,     3,     0,     0,     0,
       0,     0,   182,     0,     0,   183,     0,   184,   185,     0,
     186,   436,     0,     0,     0,     0,     0,     0,     0,   436,
    1303,   108,     0,   282,     0,   399,     0,   187,     0,     0,
       0,   111,  1304,     0,   436,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   872,   872,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,     0,
     198,   199,     0,     0,     0,    12,     0,     0,   106,    15,
      16,    17,    18,    19,    20,    21,    22,  1207,     0,     0,
     766,     0,  1208,   567,  1209,   117,     0,     0,     0,   436,
       0,     0,     0,     0,   200,     0,     0,   109,   201,     0,
     567,   950,     0,   950,   202,  1480,   112,   203,   204,   205,
     206,     0,     0,   436,  2019,     0,     0,   149,     0,   471,
       0,   108,   182,     0,  1624,   183,     0,   184,   185,     0,
     186,     0,   632,     0,     0,     0,   567,     0,   567,     0,
       0,   282,     0,   282,     0,  1483,     0,   187,     0,     0,
       0,     0,  1005,     0,   645,   567,   644,     0,    12,     0,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     644,     0,  1305,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,   567,   990,   195,   196,   197,     0,
     198,   199,     0,     0,     0,     0,     0,     0,   106,   342,
     567,     0,   632,   689,    12,     0,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     1,
     567,     0,     0,     0,   200,     0,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,     0,     0,     0,     0,   207,     0,     0,
       0,   763,    12,     0,   349,   350,    15,    16,    17,    18,
      19,    20,    21,    22,   763,     0,   763,   763,     2,     0,
       0,     0,     0,     0,     0,     0,     0,   766,     0,   436,
       0,   106,     0,     3,     0,     0,     0,     0,     0,     0,
       0,   174,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,   567,     0,     0,   567,     0,  1805,   108,   149,
     149,     0,     0,     0,     0,   436,     0,     0,   111,   112,
       0,     0,     0,     0,     0,  1005,  1298,   645,   645,   872,
     113,   872,     0,    12,     0,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,   572,     0,     0,     0,     0,     0,   174,
       0,    12,   106,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,     0,   763,   838,   436,   326,     0,     0,
     471,     0,     0,     0,     0,     0,     0,   950,   351,   108,
     106,     0,     0,     0,     0,     0,     0,   174,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,     0,     0,   762,   108,     0,     0,
       0,     0,     0,     0,     0,   567,   567,   111,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,  1305,     0,  1305,     0,
       0,     0,     0,     0,     0,     0,   567,     0,     0,     0,
     326,     0,     0,     0,     0,     0,     0,     0,    12,   567,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   436,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   326,     0,     0,     0,
       0,   471,  1812,     0,     0,     0,   950,     0,     0,     0,
     624,     0,     0,     0,     0,     0,   655,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   686,     0,     0,     0,     0,     0,     0,     0,
       0,   763,     0,     0,   436,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   436,     0,     0,     0,
       0,     0,     0,   567,   567,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   747,     0,     0,     0,     0,
       0,     0,   747,     0,     0,     0,     0,     0,     0,  1305,
       0,  1305,  1305,     0,     0,     0,     0,  1812,     0,  1812,
     326,     0,     0,     0,  1914,     0,  1812,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   436,     0,     0,
       0,     0,     0,     0,     0,     0,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   326,     0,     0,     0,
       0,     0,     0,     0,   326,     0,     0,   326,     0,   326,
     326,   471,   282,   326,     0,     0,     0,     0,     0,     0,
       0,     0,   326,     0,     0,   326,   326,   326,     0,     0,
       0,   326,     0,     0,     0,     0,     0,   747,     0,     0,
     380,   117,     0,     0,     0,     0,     0,     0,     0,     0,
     436,     0,  1995,     0,     0,  1812,  1812,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,   436,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   471,   655,    87,
       0,    88,   436,     0,     0,  1812,  1807,   838,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,     0,   471,   471,     0,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   471,   747,
     978,   471,   747,   981,     0,   985,     0,     0,     0,   766,
       0,     0,     0,     0,   456,     0,   457,   458,     0,     0,
       0,     0,     0,     0,   380,     0,     0,     0,     0,     0,
     655,     0,     0,     0,   655,   655,     0,     0,     0,     0,
       0,   655,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1038,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1807,   436,  1807,     0,     0,   -17,     0,  1807,     0,
    1807,   624,     0,     0,     0,  1122,   766,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   838,   436,     0,     0,   326,     0,     0,
       0,     0,     0,     0,     0,     0,   747,     0,     0,     0,
     747,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   766,   766,     0,     0,
       0,     0,     0,     0,     0,     0,   747,     0,     0,     0,
       0,   326,   326,     0,   155,     0,     0,     0,     0,     0,
       0,   155,     0,     0,     0,     0,     0,     0,     0,     0,
    1983,     0,     0,     0,     0,     0,  1807,     0,     0,  1807,
    1807,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   838,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   326,     0,
       0,   436,   155,     0,     0,     0,   326,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,   624,     0,     0,     0,  1983,  1983,     0,     0,     0,
       0,   747,   747,     0,     0,     0,     0,     0,     0,  1807,
     155,     0,     0,     0,     0,     0,     0,     0,     0,   414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   155,     0,     0,     0,   155,     0,     0,
       0,   436,     0,  2117,   838,     0,     0,     0,     0,     0,
       0,     0,   174,     0,     0,     0,   174,     0,     0,     0,
       0,     0,     0,   155,     0,     0,  1983,   985,   655,     0,
     655,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     326,     0,     0,     0,     0,     0,   326,     0,     0,     0,
     747,  1265,     0,   747,  1268,     0,     0,     0,     0,     0,
     268,   155,     0,     0,     0,     0,     0,     0,     0,     0,
     323,     0,     0,     0,     0,     0,     0,     0,     0,  2117,
    2117,     0,     0,   655,     0,   655,     0,     0,     0,   155,
       0,     0,     0,     0,     0,     0,     0,   655,     0,     0,
    1983,     0,     0,   155,     0,     0,     0,     0,   436,   436,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   985,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2117,   763,     0,     0,     0,     0,   155,     0,     0,   155,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   747,     0,     0,     0,   747,     0,     0,     0,     0,
       0,     0,   747,  1342,     0,     0,   747,  1346,     0,     0,
     747,  1350,     0,     0,   155,     0,     0,     0,     0,     0,
    1353,     0,     0,     0,     0,     0,     0,   154,     0,     0,
       0,   155,   747,   155,   154,     0,     0,     0,     0,   155,
       0,     0,     0,   155,     0,     0,   174,     0,     0,   597,
     600,     0,     0,     0,     0,   155,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   633,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   326,   155,     0,     0,     0,   155,     0,   155,
       0,     0,     0,     0,     0,   154,   747,   155,   747,     0,
     155,     0,   155,   155,     0,     0,   155,     0,     0,     0,
       0,     0,     0,   154,     0,   155,     0,     0,   155,   155,
     155,     0,     0,     0,   155,     0,     0,     0,     0,     0,
       0,     0,     0,   154,   725,   738,     0,     0,     0,     0,
       0,     0,     0,   174,     0,     0,     0,   326,     0,     0,
       0,     0,     0,     0,   655,     0,     0,     0,     0,     0,
     775,     0,     0,   600,     0,     0,   154,     0,     0,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   154,   747,  1470,     0,
     655,   655,  1477,     0,     0,     0,   824,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   837,
       0,     0,     0,     0,     0,     0,     0,   846,     0,     0,
       0,     0,     0,   597,   154,     0,     0,     0,     0,     0,
     747,  1507,     0,   747,  1511,     0,     0,   747,  1515,     0,
     155,     0,     0,     0,   837,     0,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   747,     0,     0,     0,   154,   155,     0,     0,
       0,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
       0,     0,     0,   655,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   747,  1612,     0,     0,     0,     0,   154,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   155,     0,     0,     0,     0,     0,
     633,   326,     0,     0,     0,     0,     0,   154,     0,     0,
       0,     0,     0,   174,     0,     0,     0,     0,     0,     0,
     155,     0,     0,     0,   154,     0,   154,     0,     0,     0,
       0,     0,   154,     0,     0,     0,   154,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   824,   155,   155,   154,  1019,     0,     0,
     154,     0,   154,     0,     0,     0,     0,     0,     0,     0,
     154,     0,     0,   154,   326,   154,   154,     0,   155,   154,
       0,     0,     0,   174,     0,     0,     0,     0,   154,     0,
       0,   154,   154,   154,     0,     0,     0,   154,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   326,     0,
     155,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,   155,     0,     0,     0,     0,     0,     0,     0,   155,
     597,     0,     0,     0,     0,   159,     0,     0,   174,     0,
       0,     0,   159,     0,   155,     0,     0,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   824,     0,     0,     0,
       0,     0,     0,     0,     0,  1121,  1124,     0,     0,     0,
       0,     0,     0,     0,   597,   597,     0,     0,   174,     0,
      49,    50,    51,    52,    53,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   155,     0,     0,     0,     0,
       0,   159,     0,   154,   326,     0,     0,     0,     0,   326,
       0,     0,   182,     0,     0,   183,     0,   184,   185,     0,
     186,   159,     0,   155,     0,     0,     0,   174,     0,   155,
     154,   174,   174,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,   326,     0,     0,   174,     0,
       0,     0,     0,     0,   159,     0,     0,     0,   159,     0,
       0,     0,     0,     0,   824,  1188,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,   159,     0,   195,   196,   197,     0,
     198,   199,     0,     0,     0,     0,     0,   154,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1836,  1837,     0,     0,  1838,  1839,   326,
       0,     0,   159,   154,   200,  2014,     0,   109,   201,     0,
       0,     0,   747,     0,   202,   111,   112,   203,   204,   205,
     206,   824,     0,     0,     0,     0,     0,     0,     0,     0,
     159,   174,     0,   597,     0,     0,     0,     0,     0,   597,
       0,     0,     0,     0,   159,     0,     0,     0,   824,     0,
       0,     0,   824,     0,     0,     0,     0,   154,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   824,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,   159,     0,     0,
     159,     0,     0,     0,     0,     0,   824,     0,     0,     0,
       0,     0,     0,   824,     0,     0,     0,   747,     0,     0,
       0,     0,     0,   154,   747,   155,     0,     0,     0,     0,
       0,     0,     0,   154,   154,   159,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   159,   174,   159,     0,     0,   154,     0,     0,
     159,     0,     0,     0,   159,     0,     0,     0,   326,     0,
       0,     0,     0,     0,     0,     0,   159,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,     0,   747,   747,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,   747,     0,     0,   159,   824,
     159,     0,     0,     0,     0,     0,     0,     0,   159,     0,
       0,   159,     0,   159,   159,     0,     0,   159,   154,     0,
       0,     0,     0,     0,     0,     0,   159,     0,     0,   159,
     159,   159,     0,     0,     0,   159,     0,     0,   326,     0,
     747,     0,     0,     0,     0,     0,   154,   378,     0,     0,
       0,     0,   154,     0,     0,     0,     0,    49,    50,    51,
      52,    53,     0,   747,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,  -505,  -505,     0,
    -505,    87,     0,    88,     0,     0,  -505,     0,     0,     0,
     597,     0,     0,     0,     0,     0,   747,   747,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,   747,  2192,   824,     0,   747,     0,   824,
       0,     0,     0,     0,     0,   174,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,   159,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   747,  1489,     0,
       0,     0,     0,     0,   155,   106,     0,     0,   159,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   262,     0,     0,     0,     0,     0,
       0,     0,   824,     0,   109,   692,   824,     0,     0,     0,
     824,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   407,     0,   415,
       0,   417,     0,     0,     0,   159,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   155,     0,     0,
       0,   159,     0,     0,     0,     0,   155,     0,     0,     0,
       0,     0,   459,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,     0,   597,     0,     0,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   159,   159,     0,     0,     0,
     824,   155,     0,     0,   824,     0,     0,   824,     0,     0,
     155,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   824,     0,     0,
       0,   824,     0,     0,     0,   824,     0,     0,     0,     0,
     383,   159,     0,     0,     0,     0,   672,     0,   417,     0,
       0,   159,   159,     0,     0,     0,     0,     0,     0,     0,
     159,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     233,   383,     0,   415,   417,   159,     0,   155,     0,     0,
     155,     0,   155,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,     0,     0,     0,   155,   155,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   155,     0,
       0,   155,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,   824,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   283,     0,     0,   159,     0,   383,     0,
     415,   417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,     0,   154,     0,     0,
     159,     0,     0,     0,     0,   383,     0,     0,     0,     0,
     383,     0,   155,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   597,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   155,     0,     0,     0,     0,     0,
     383,     0,     0,     0,   485,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   597,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   593,
     383,     0,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,   616,     0,     0,     0,     0,     0,     0,     0,
       0,   383,     0,   672,   417,     0,     0,     0,  1913,     0,
    1917,     0,     0,     0,   154,     0,   155,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,   283,
       0,   383,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   709,   383,
     709,     0,     0,     0,     0,   383,     0,     0,     0,     0,
       0,   383,     0,   672,   417,     0,   159,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,   383,   672,   154,     0,   154,     0,     0,     0,     0,
       0,     0,   383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,   154,   154,     0,
     383,   159,     0,     0,     0,   459,     0,     0,     0,     0,
       0,   154,     0,     0,   154,     0,     0,     0,   835,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   855,   383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   616,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   383,     0,     0,     0,
       0,     0,   887,     0,   889,     0,   383,     0,     0,     0,
       0,   233,     0,   899,   233,   154,     0,     0,   155,   155,
       0,     0,     0,     0,     0,     0,     0,     0,   903,     0,
       0,     0,     0,     0,   383,     0,     0,   383,   383,     0,
       0,     0,     0,     0,     0,     0,     0,   154,     0,     0,
     383,     0,     0,     0,   233,     0,     0,   928,     0,     0,
       0,     0,     0,     0,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   616,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1121,
    2178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     991,   993,     0,   283,     0,   159,     0,     0,     0,     0,
     616,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   283,     0,  1016,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   824,     0,     0,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   835,     0,     0,     0,   485,   709,
       0,     0,     0,     0,     0,   709,   383,     0,   159,     0,
     593,     0,     0,   383,     0,     0,     0,   159,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   383,
       0,     0,     0,     0,     0,     0,     0,  1111,     0,     0,
       0,     0,   159,     0,   182,     0,     0,   183,     0,   184,
     185,     0,   186,     0,   383,     0,     0,     0,     0,   383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
     283,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,   159,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   313,     0,     0,     0,     0,   188,   189,
     329,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,   391,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,   383,     0,     0,     0,     0,
       0,   154,   154,   283,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   440,   159,   109,
     201,   159,  1181,   159,   706,     0,   202,   111,   112,   203,
     204,   205,   206,     0,  1198,     0,     0,  1198,   233,  1204,
       0,   159,     0,   482,     0,   159,   159,     0,     0,     0,
       0,     0,     0,     0,     0,   709,     0,     0,   383,   159,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   575,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   383,     0,     0,     0,     0,   329,
       0,     0,     0,     0,     0,     0,   383,     0,     0,  1260,
       0,     0,   313,     0,     0,   636,     0,     0,     0,     0,
       0,   668,     0,   159,     0,     0,     0,     0,     0,     0,
       0,   616,     0,     0,     0,     0,     0,  1281,     0,     0,
       0,   682,     0,     0,     0,   688,     0,     0,     0,   313,
       0,     0,   695,     0,     0,   159,     0,   383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   991,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     709,     0,   313,   329,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1329,     0,     0,     0,     0,     0,   391,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,   575,   329,
       0,   839,     0,     0,     0,     0,     0,   329,     0,     0,
     482,     0,   482,   329,     0,     0,   329,     0,     0,     0,
       0,     0,     0,     0,     0,   482,     0,     0,   482,   482,
     482,   575,     0,     0,   329,     0,     0,     0,     0,     0,
       0,   695,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   709,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     383,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     593,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
     636,     0,   951,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   313,     0,     0,     0,     0,
     329,     0,     0,     0,     0,     0,     0,     0,  1482,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   987,     0,
     668,     0,     0,     0,     0,     0,     0,     0,     0,   383,
     313,     0,   999,     0,     0,     0,   709,     0,     0,   159,
     159,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   575,   636,     0,
    1521,     0,  1522,     0,     0,     0,     0,     0,     0,   313,
       0,   313,     0,     0,     0,     0,     0,   575,     0,  1056,
       0,     0,     0,     0,     0,   575,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     329,     0,     0,   709,     0,  1616,   383,    49,    50,    51,
      52,    53,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,  -505,  -505,     0,
    -505,    87,     0,    88,   329,   329,  -505,     0,     0,     0,
     313,     0,     0,   383,     0,     0,     0,     0,     0,     0,
       0,  1660,  1661,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,  -505,
    -505,   329,  -505,    87,     0,    88,     0,     0,  -505,   482,
       0,     0,     0,   575,     0,     0,     0,     0,     0,     0,
       0,     0,   383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1730,  1730,     0,     0,     0,
       0,     0,   616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     0,     0,     0,     0,  1755,     0,
       0,  1757,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   313,     0,     0,     0,     0,     0,     0,
    1616,     0,     0,     0,     0,     0,     0,     0,   383,     0,
     987,     0,     0,   107,   108,     0,   109,   692,     0,     0,
       0,   383,     0,   329,   111,   112,     0,     0,     0,   329,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   313,     0,  1277,     0,
    1791,   575,     0,   636,     0,     0,     0,   593,     0,     0,
       0,     0,     0,     0,     0,   313,     0,     0,     0,     0,
       0,     0,     0,   233,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   709,     0,     0,     0,     0,     0,   593,     0,
       0,     0,     0,     0,   987,   951,     0,     0,     0,     0,
     383,   417,     0,     0,     0,     0,     0,     0,     0,     0,
     313,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   383,     0,     0,     0,     0,     0,     0,
     313,     0,     0,     0,     0,     0,   709,     0,     0,     0,
       0,     0,     0,     0,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1915,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2245,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -874,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   575,     0,     0,   313,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   482,     0,     0,     0,     0,
       0,     0,  1959,     0,     0,     0,     0,  -874,     0,     0,
    -874,     0,  -874,  -874,     0,  -874,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   383,
    -874,     1,  -874,  -874,     0,  -874,  -257,  -257,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,     0,  -874,
     329,  -874,  -874,     0,  -874,  -874,  -874,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,
       2,  -874,  -874,  -874,     0,  -874,  -874,     0,     0,     0,
       0,     0,     0,  -874,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   313,   313,     0,     0,     0,
       0,  2245,     0,     0,     0,     0,     0,     0,  -257,  -874,
       0,     0,  -874,  -874,     0,     0,     0,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,   313,     0,     0,     0,
       0,     0,     0,     0,  -874,     0,     0,     0,     0,   313,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -874,     0,     0,  -874,     0,
    -874,  -874,     0,  -874,     0,     0,     0,     0,     0,     0,
       0,     0,   233,     0,     0,     0,     0,     0,  -874,     1,
    -874,  -874,     0,  -874,  -258,  -258,  -874,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,     0,  -874,     0,  -874,
    -874,     0,  -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,  -874,  -874,  -874,  -874,     2,  -874,
    -874,  -874,     0,  -874,  -874,     0,     0,     0,     0,     0,
       0,  -874,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -258,  -874,     0,     0,
    -874,  -874,     0,   313,   313,  -874,     0,  -874,  -874,  -874,
    -874,  -874,  -874,  -874,     0,     0,     0,    49,    50,    51,
      52,    53,  -874,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,  -505,  -505,     0,
    -505,    87,     0,    88,     0,     0,  -505,     0,     0,  1962,
       0,     0,     0,     0,     0,     0,     0,   482,     0,     0,
       0,     0,     0,     0,     0,     0,  1709,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   482,   313,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,   182,     0,     0,   183,     0,   184,   185,
       0,   186,     0,     0,     0,     0,     0,     0,     0,     0,
     391,     0,     0,     0,     0,     0,  1542,     0,   187,  1544,
     482,  1545,     0,     0,  1546,  1547,  1548,  1549,  1550,  1551,
    1552,  1553,  1554,  1555,  1556,  -356,  -356,  1557,  1558,  1559,
    1560,  1561,  1562,  1563,   109,  1564,     0,   188,   189,     0,
     703,   191,  1565,  1566,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,  1567,     0,   195,   196,   197,
       0,   198,   199,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   329,     0,     0,
       0,     0,   482,     0,     0,  1568,     0,   695,   109,   201,
       0,     0,     0,   424,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,     0,     0,     0,     0,     0,     0,
    -197,     0,     0,     0,     0,     0,     0,     0,   329,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   258,    41,    42,    43,    44,    45,    46,    47,
      48,     0,     0,     0,   391,     0,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,   373,   374,     0,   375,    87,     0,    88,     0,     0,
     376,     0,   482,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,  1541,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,  -480,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,    97,     0,     0,     0,    98,
    -480,  1542,   391,  1543,  1544,     0,  1545,     0,     0,  1546,
    1547,  1548,  1549,  1550,  1551,  1552,  1553,  1554,  1555,  1556,
    -356,  -356,  1557,  1558,  1559,  1560,  1561,  1562,  1563,     0,
    1564,   482,   188,   189,   101,   703,   191,  1565,  1566,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
    1567,     0,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1568,     0,     0,   109,  1569,     0,     0,     0,   424,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,     0,
       0,   482,     0,     0,   695,  -197,    39,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,    97,     0,     0,     0,    98,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   101,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1836,
    1837,     0,     0,  1838,  1839,     0,     0,     0,     0,     0,
     200,  1840,  1841,   109,  1569,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,     0,
       0,     0,     0,     0,     0,  1842,    39,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,    97,     0,     0,     0,    98,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   101,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1836,
    1837,     0,     0,  1838,  1839,     0,     0,     0,     0,     0,
     200,  1840,     0,   109,  1569,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,     0,
       0,     0,     0,     0,     0,  1842,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,    97,     0,     0,     0,    98,
       0,     0,     0,    99,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     100,     0,     0,     0,   101,   102,     0,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
     105,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,   108,     0,   109,   110,     0,     0,     0,     0,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,   114,   378,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -505,  -505,     0,  -505,
      87,     0,    88,     0,     0,  -505,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    12,     0,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,   108,     0,   109,   379,     0,     0,     0,  -845,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   378,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -505,  -505,     0,  -505,    87,     0,
      88,     0,     0,  -505,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    12,     0,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   108,
       0,   109,   379,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,   258,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      12,     0,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     3,   816,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1003,   108,  -706,   109,   641,
       0,     0,     0,     0,     0,     0,   111,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,   258,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -505,  -505,
       0,  -505,    87,     0,    88,     0,     0,  -505,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    12,     0,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,   108,     0,   109,   379,     0,     0,     0,
    -849,     0,     0,   111,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   113,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,  -505,  -505,     0,  -505,    87,
       0,    88,     0,     0,  -505,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    12,     0,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
     108,     0,   109,   379,     0,     0,     0,     0,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,    39,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   182,     0,    87,   183,    88,
     184,   185,     0,   186,    89,    90,    91,    92,    93,    94,
      95,    96,    97,     0,     0,     0,    98,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   101,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,     0,   198,   199,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,  1834,
     109,  1569,     0,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,    39,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,    89,    90,    91,    92,    93,
      94,    95,    96,    97,     0,     0,     0,    98,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   101,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,  1569,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   279,   615,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   627,   861,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   279,   691,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   279,   947,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   627,  1041,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   279,   280,   484,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   279,   280,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   279,   691,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   279,   947,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   627,  1041,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   279,   615,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   627,   861,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   627,     0,     0,     0,     0,     0,   202,   834,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   988,     0,     0,     0,     0,     0,   202,   989,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   627,     0,     0,     0,     0,     0,   202,   281,
     112,   203,   204,   205,   206,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,  2079,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,  2111,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,     0,    -2,     0,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,  1228,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -504,  -504,     0,  -504,    87,     0,    88,
       0,     0,  -504,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,   103,   104,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1370,     0,  1228,     0,
     109,   110,     0,     0,   103,   104,     0,     0,   111,   112,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    -504,  -504,     0,  -504,    87,     0,    88,     0,     0,  -504,
     109,   278,    90,    91,    92,    93,    94,    95,    96,     0,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,   103,   104,     0,    87,     0,    88,     0,     0,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1434,     0,  1228,     0,   109,   110,     0,
       0,   103,   104,     0,     0,   111,   112,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,  -504,  -504,     0,
    -504,    87,     0,    88,     0,     0,  -504,   109,   278,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1526,     0,  1228,     0,   109,   110,     0,     0,     0,     0,
       0,     0,   111,   112,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -504,  -504,     0,  -504,    87,     0,
      88,     0,     0,  -504,     0,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1647,     0,  1228,
       0,   109,   110,     0,     0,     0,     0,     0,     0,   111,
     112,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,  -504,  -504,     0,  -504,    87,     0,    88,     0,     0,
    -504,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1776,     0,  1228,     0,   109,   110,
       0,     0,     0,     0,     0,     0,   111,   112,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -504,  -504,
       0,  -504,    87,     0,    88,     0,     0,  -504,     0,   278,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   110,     0,     0,     0,
       0,     0,     0,   111,   112,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    12,     0,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
     108,     0,   109,   316,     0,     0,     0,     0,     0,     0,
     111,   112,     0,     0,    49,    50,    51,    52,    53,    54,
       0,    55,   113,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    12,     0,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     3,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   108,
       0,   109,     0,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,    49,    50,    51,    52,    53,    54,     0,
      55,   113,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    12,     0,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,   108,     0,
     109,   110,     0,     0,     0,  -847,     0,     0,   111,   112,
       0,     0,    49,    50,    51,    52,    53,    54,     0,    55,
     113,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    12,     0,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,   108,     0,   109,
     110,     0,     0,     0,     0,     0,     0,   111,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
      39,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,    97,
       0,     0,     0,    98,     0,     0,     0,     0,     0,     0,
       0,  -424,  -424,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -424,     0,     0,     0,   109,   110,     0,
       0,     0,     0,     0,     0,   111,   112,    39,   258,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   110,     0,  1686,     0,  1687,
       0,     0,   111,   112,  1688,     0,     0,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,  1689,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   850,     0,     0,     0,     0,
       0,     0,   111,   112,   378,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -505,  -505,     0,  -505,    87,     0,
      88,     0,     0,  -505,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   103,   104,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   379,     0,   103,   104,     0,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     3,   816,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   871,     0,  -706,
     109,   744,     0,     0,     0,     0,     0,     0,   111,   112,
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     278,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
     816,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   948,     0,  -706,   109,   641,     0,     0,
       0,     0,     0,     0,   111,   112,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,  1271,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -716,   109,   777,     0,     0,     0,     0,     0,     0,
     111,   112,   258,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   565,   109,   566,
       0,     0,     0,     0,     0,     0,   111,   112,   258,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,   278,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   777,   778,     0,     0,     0,
       0,     0,   111,   112,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,  1651,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   777,     0,     0,     0,     0,     0,     0,   111,   112,
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     278,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
    1653,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   777,     0,     0,
       0,     0,     0,     0,   111,   112,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   664,     0,     0,     0,     0,     0,     0,
     111,   112,   258,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   777,
       0,     0,     0,     0,     0,     0,   111,   112,   258,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,   278,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   566,     0,     0,     0,     0,
       0,     0,   111,   112,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -505,  -505,     0,  -505,    87,     0,    88,
       0,     0,  -505,     0,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,   103,   104,     0,    87,     0,    88,
    1709,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,     0,     0,
     183,     0,   184,   185,     0,   186,     0,     0,     0,     0,
     109,   379,     0,     0,   103,   104,     0,     0,   111,   112,
    1542,     0,   187,  1544,     0,  1545,  1999,  2000,  1546,  1547,
    1548,  1549,  1550,  1551,  1552,  1553,  1554,  1555,  1556,     0,
       0,  1557,  1558,  1559,  1560,  1561,  1562,  1563,     0,  1564,
       0,   188,   189,     0,   703,   191,  1565,  1566,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,  1567,
     109,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
    1709,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1568,
       0,     0,   109,   201,     0,     0,     0,   424,     0,   202,
     111,   112,   203,   204,   205,   206,     0,   182,     0,     0,
     183,     0,   184,   185,  -197,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1542,     0,   187,  1544,     0,  1545,     0,     0,  1546,  1547,
    1548,  1549,  1550,  1551,  1552,  1553,  1554,  1555,  1556,     0,
       0,  1557,  1558,  1559,  1560,  1561,  1562,  1563,     0,  1564,
       0,   188,   189,     0,   703,   191,  1565,  1566,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,  1567,
       0,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1568,
       0,     0,   109,   201,     0,     0,     0,   424,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,     0,     0,
       0,     0,     0,     0,  -197,   428,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -427,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,   428,
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,   109,    87,     0,    88,     0,  -427,     0,     0,
      89,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -428,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,   428,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,   109,    87,     0,    88,
       0,  -428,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,  -427,    49,    50,    51,    52,
      53,    54,   465,    55,   466,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   467,     0,     0,     0,  1556,
       0,  -356,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1568,     0,     0,   109,   468,     0,     0,     0,   424,     0,
     202,   111,   112,   469,   470,   205,   206,    49,    50,    51,
      52,    53,    54,   465,    55,   466,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,   182,
       0,    87,   183,    88,   184,   185,     0,   186,    89,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,     0,   198,   199,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   109,   468,     0,     0,     0,   424,
       0,   202,   111,   112,   469,   470,   205,   206,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     182,     0,    87,   183,    88,   184,   185,     0,   186,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     1,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     2,   195,   196,   197,     0,   198,   199,
       0,     0,     0,     0,     0,     0,   106,     0,     3,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,   591,   109,   592,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,   182,     0,    87,   183,    88,   184,   185,     0,   186,
      89,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     1,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     2,   195,   196,   197,     0,   198,
     199,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   592,     0,     0,
       0,   424,     0,   202,   111,   112,   203,   204,   205,   206,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     2,   195,   196,   197,     0,
     198,   199,     0,     0,     0,     0,     0,     0,   106,     0,
       3,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   627,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,    89,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     1,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     2,   195,   196,   197,
       0,   198,   199,     0,     0,     0,     0,     0,     0,   106,
       0,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,   109,   592,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   109,
     468,     0,     0,     0,   424,     0,   202,   111,   112,   203,
     204,   205,   206,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   182,     0,    87,   183,    88,
     184,   185,     0,   186,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,     0,   198,   199,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
     109,   592,     0,     0,     0,   424,     0,   202,   111,   112,
     203,   204,   205,   206,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   109,   627,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,   378,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -505,  -505,     0,  -505,
      87,     0,    88,     0,     0,  -505,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   103,   104,    87,
       0,    88,     0,     0,     0,     0,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,    12,     0,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     3,   816,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -706,   109,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    12,     0,   619,   104,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   983,     0,     0,
     109,   684,     0,     0,     0,     0,     0,     0,   111,   112,
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     278,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     3,
     816,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -706,   109,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    12,     0,   619,   104,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   620,     0,     0,     0,     0,
       0,     0,   111,   112,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,    51,    52,    53,    54,     0,    55,
     109,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     3,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     316,     0,     0,     0,     0,     0,     0,   111,   112,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     278,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   566,     0,     0,
       0,     0,     0,     0,   111,   112,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     3,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   850,     0,     0,     0,     0,     0,
       0,   111,   112,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   857,
     109,   850,     0,   103,   104,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   983,     0,     0,   109,
     620,     0,     0,     0,     0,     0,     0,   111,   112,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     278,    90,    91,    92,    93,    94,    95,    96,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     103,   104,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
    1395,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   983,     0,     0,   109,   684,     0,   103,
     104,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   850,     0,     0,     0,
       0,     0,     0,   111,   112,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   103,   104,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   435,     0,   103,   104,     0,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,     0,     0,     0,     0,     0,   111,
     112,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,   278,    90,    91,    92,    93,    94,    95,    96,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   103,   104,    87,     0,    88,     0,     0,     0,
       0,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   435,
       0,   103,   104,     0,     0,     0,   111,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   684,     0,
       0,     0,     0,     0,     0,   111,   112,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   701,     0,   103,   104,     0,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   850,     0,     0,     0,     0,     0,
       0,   111,   112,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   664,     0,   103,   104,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     566,     0,     0,     0,     0,     0,     0,   111,   112,   258,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -505,  -505,
       0,  -505,    87,     0,    88,     0,     0,  -505,     0,     0,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,   103,
     104,    89,    90,    91,    92,    93,    94,    95,    96,     0,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,   103,   104,     0,    87,   109,    88,     0,     0,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   316,     0,
       0,   103,   104,     0,     0,   111,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   620,     0,
       0,     0,     0,     0,     0,   111,   112,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,   278,    90,
      91,    92,    93,    94,    95,    96,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,     0,    88,     0,     0,     0,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   930,     0,   103,   104,     0,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   850,     0,     0,     0,     0,     0,
       0,   111,   112,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   278,    90,    91,    92,    93,    94,
      95,    96,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   692,     0,   103,   104,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,    49,    50,    51,    52,    53,   111,   112,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -505,  -505,     0,  -505,    87,     0,    88,     0,
       0,  -505,    49,    50,    51,    52,    53,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -505,  -505,     0,  -505,    87,     0,    88,     0,
       0,  -505,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,   109,
     692,     0,     0,     0,     0,     0,     0,   111,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     930,     0,    49,    50,    51,    52,    53,   111,   112,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -505,  -505,     0,  -505,    87,     0,    88,     0,
       0,  -505,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,   109,
       0,   195,   196,   197,     0,   198,   199,   111,   112,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   109,   201,  1110,     0,     0,     0,     0,   202,
     281,   112,   203,   204,   205,   206,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,   182,     0,    87,   183,    88,   184,   185,     0,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,     0,   198,
     199,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   201,     0,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       3,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,    54,     0,    55,   109,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
      89,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
      51,    52,    53,     0,     0,    55,   109,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -504,  -504,
       0,  -504,    87,     0,    88,     0,     0,  -504,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,    51,    52,
      53,     0,     0,    55,     1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     3,     0,
     182,     0,     0,   183,     0,   184,   185,     0,   186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     3,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,     0,   198,   199,
       0,     0,   182,     0,     0,   183,   106,   184,   185,     0,
     186,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1836,  1837,     0,     0,  1838,  1839,   187,     0,     0,
       0,     0,   200,  1945,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,   483,     0,   109,   201,     0,
     484,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   703,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,   108,     0,   704,   705,     0,     0,     0,   706,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,  1194,
       0,   109,   201,     0,     0,     0,  1195,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,  1197,     0,   109,   201,
    1201,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   109,   201,     0,     0,     0,
     424,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   109,   201,     0,     0,   923,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   109,
     201,  1013,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   201,  1039,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,  1197,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,  1615,     0,
     109,   201,     0,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   201,     0,
       0,     0,  1719,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   109,   201,     0,     0,     0,  1773,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
    2005,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,  2010,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,     0,   198,   199,
       0,     0,   182,     0,     0,   183,   106,   184,   185,     0,
     186,  2097,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,   200,  2020,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,  2104,     0,   109,   201,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,  2106,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,  2153,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,  2155,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
    2157,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,  2163,     0,   109,
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,  2165,     0,   109,   201,     0,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,  2207,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,  2209,     0,
     109,   201,     0,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,  2211,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,  2230,     0,   109,   201,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,  2232,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,  2234,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   444,     0,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   447,
       0,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   109,
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,     0,   109,   201,     0,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     462,     0,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
     109,   201,     0,     0,     0,     0,     0,   202,   281,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,     0,
     198,   199,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   834,   112,   203,   204,   205,
     206,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,   259,
       0,   260,   261,    87,     0,    88,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   456,     0,
     457,   458
};

static const yytype_int16 yycheck[] =
{
       3,   253,   706,    31,   107,   107,     9,   107,   351,   200,
      38,    31,   717,   109,   137,   351,   498,    31,    38,   501,
     246,   377,   640,    38,    38,  1233,  1584,     7,    31,   394,
      33,  1240,   644,    36,   351,    38,    39,  1136,   337,   170,
     174,   473,   351,    31,   640,   248,   640,   351,   640,   286,
      38,   665,  1568,   351,   785,   286,  1246,  1442,  1443,   130,
     107,   202,   351,   257,   125,    31,   797,  1843,   351,    10,
     107,    99,    38,   107,  1999,   450,   640,     1,   257,    99,
    1552,     1,  2003,   351,    99,    99,   351,   167,  1388,   246,
     130,   676,   117,   678,    97,    98,    99,   106,   884,  1140,
      10,  1982,   640,   645,   107,   130,   361,   162,   133,   651,
     113,    99,   137,   107,   117,   238,  1568,    82,   288,     0,
      91,   360,   125,   137,   117,   308,   896,   130,   367,   312,
     133,    30,    95,    99,   137,   390,  1367,    68,    69,    95,
     872,     0,   288,   547,  1375,    31,   280,   402,    82,   646,
     389,   117,    38,   650,   640,  1982,   926,  1982,   118,   656,
     173,   167,   401,  1371,   130,   268,   268,   133,   268,  1945,
    1946,   137,   175,   167,   142,   178,   167,    84,    85,   192,
     157,   236,   142,   192,  1225,   191,   683,    13,    14,    15,
      16,    17,   326,   690,    84,    85,   167,  1388,   902,   256,
    1431,   126,   933,    96,   207,  1002,   183,  1712,    31,   169,
     173,   175,   175,    99,   167,    38,  2141,   248,   144,   175,
      79,   268,   190,  1728,   238,   166,    20,   192,   192,  1745,
     361,   268,  1748,  1749,   268,  2116,   177,   240,  2014,  2015,
       3,   182,   267,  1543,   175,   248,     9,   133,   351,   351,
     174,   351,   126,   173,   174,   286,   166,   183,   192,   390,
    1057,   192,  1452,   173,   267,   268,  1208,   177,   175,   128,
      33,   402,   182,    36,   167,  2196,    99,   280,   435,   118,
    1077,   352,   285,   286,   143,   175,  2144,    79,   268,  2116,
    1060,  2116,    79,   354,   319,   356,  1527,   171,   323,   430,
     167,   267,   363,   142,   351,   446,   309,  2228,   499,   117,
     313,   468,   352,   167,   351,  2173,   319,   351,   144,   723,
     323,  1983,    65,   419,   144,    68,    69,   352,    71,   130,
     132,  1372,   335,   141,    97,    98,   128,   340,   950,   562,
    2198,   128,  1847,   323,   347,   172,   173,   170,   351,   352,
     175,   143,  1543,   319,  1453,   380,   143,   323,   178,   179,
     764,   432,   125,   586,  1880,  1162,   187,   192,   170,   372,
    1842,   396,   776,   576,   166,  1847,   990,   380,  2040,   382,
     441,   442,   725,   748,   948,  1003,   352,   380,   391,   725,
     393,   756,   432,   396,   215,   216,   771,   772,     9,   569,
     403,  2063,   288,   885,   192,  1017,  1137,  1003,   725,  1003,
     948,  1003,   175,   712,   380,   178,   725,   420,   421,   762,
     423,   725,    33,   569,   167,   428,   762,   725,  1880,   432,
     396,  1217,   615,   865,   173,  1940,   725,   440,  1943,  1003,
    1648,   166,   725,   436,   183,   762,   542,   617,   173,  1639,
    1640,   167,   678,   762,  1186,  2117,  1188,   725,   762,   629,
     725,   282,   948,  1005,   762,  1003,   432,   192,  1940,   663,
     144,   617,   169,   762,   114,   115,   173,   240,   471,   762,
    1422,  1423,  1424,   629,   663,  1282,  1283,   490,   403,   492,
     624,  1195,   495,     0,   762,  2053,   499,   762,   172,   502,
     169,   168,    82,   686,   173,  1890,   421,   664,   175,   183,
     396,   705,    31,   752,   173,  2177,  1707,  1003,    98,    38,
    1711,  1712,   285,   669,   167,   424,   173,   684,   167,  1326,
     170,   174,  1237,   192,   680,   692,   183,  1728,   361,   542,
     795,  2013,   170,   171,   547,   576,   309,   433,   787,  1046,
     313,   352,    79,   556,   557,   794,  1765,   691,   561,  2031,
     799,   171,   141,   191,   173,    79,   176,   390,   173,   192,
     173,    31,    79,   576,   599,   600,  1766,   340,    38,   402,
      99,   584,   173,   192,   587,   960,    31,   192,   167,   192,
     173,   966,   171,    38,   137,   138,   599,   600,   173,   178,
     179,   128,    13,    14,    15,    16,    17,   430,   704,   192,
     171,   614,   615,   644,   128,   176,   143,   597,   137,   382,
     600,   128,   725,   725,  2096,   725,    79,   173,   391,   143,
     393,   432,  1244,   599,   600,   173,   143,   640,    79,    99,
     403,   644,    82,     3,   187,   188,   192,  1417,   175,     9,
     471,   170,   169,   633,    99,   182,    96,   174,   421,   762,
     762,   175,   762,   720,   795,   668,   737,   738,    79,   164,
     173,   674,   818,    33,    82,   128,    36,   440,   725,    39,
     175,   173,   169,   569,   173,  1303,   173,   128,   725,    97,
     143,   725,   763,   850,   183,   192,   191,   737,   738,   520,
     192,   704,   143,   706,  1108,    79,  1410,  1303,   116,  1303,
     170,  1303,   737,   738,   167,   180,   181,   128,   171,   722,
     723,   584,   725,   763,   587,   170,   167,   861,  1313,   923,
     171,   617,   143,   144,   737,   738,   499,    97,    98,   169,
     639,     3,   641,    79,   174,   725,   173,    79,  1290,  1291,
      79,   173,  1943,   113,   128,   958,   183,   173,   738,   762,
     763,   764,   156,   157,   947,   125,   160,   161,  1067,   143,
     192,   737,   738,   776,   173,   778,   192,    79,  1121,   840,
    1017,  1393,  1019,   669,   183,  1121,  1017,   173,  1019,   183,
     689,   173,   128,   556,   557,   775,   128,   763,   561,   128,
     169,   175,   985,   702,  1121,   174,   192,   143,   182,     3,
     192,   143,  1121,   936,   143,   175,    76,  1121,   178,   279,
      79,   584,   721,  1121,   587,   170,   128,  1231,   200,   169,
     166,   988,  1121,   167,   174,   167,   839,   166,  1121,   171,
     173,   143,   361,   246,   824,   838,   169,   207,   994,    79,
     173,   173,  1622,  1121,  1624,  1001,  1121,   837,  1041,   192,
      13,    14,    15,    16,    17,   167,   846,    31,  1486,   128,
     192,   390,  1042,   175,    38,  1460,  1461,  1071,   178,   282,
     240,   173,   169,   402,   143,   185,   186,   174,   248,   892,
    1486,   183,  1486,   896,  1486,   167,  1042,   169,   128,   902,
    1139,   361,   169,    79,   932,   668,   173,   964,   167,   173,
     167,   430,   932,   143,   171,   434,   361,   932,   932,   173,
     280,   175,   936,   926,   175,   285,    79,   167,    79,   932,
     390,   934,   818,  1159,   167,    99,   737,   738,  1294,    31,
     943,   171,   402,   842,   932,   390,    38,   833,   167,   309,
    1315,   167,   128,   313,  2145,  2146,   169,   402,   171,    79,
     102,   167,   763,   169,   983,   171,   932,   143,   126,   133,
     430,   179,   795,   137,   167,   128,   169,   128,   171,  1684,
     340,   114,   115,  1002,  1015,   430,  1017,   347,  1019,   184,
     143,   167,   143,   144,   169,   171,   138,   177,   173,   169,
    1003,   143,  1159,  1798,   146,  1800,   148,    99,   128,  1166,
     180,   181,  1015,  1136,  1017,  1719,  1019,   189,  1121,  1121,
    1725,  1121,   382,   143,  1323,   117,   167,  1221,   169,   141,
     171,   391,   435,   393,  1438,   169,   857,   173,  1057,  1019,
      79,   862,   170,   403,  1247,  1248,   932,   167,   190,   175,
     169,   191,  1123,  1124,   173,   175,  1059,  1060,  1077,   880,
     420,   421,   182,   423,  1067,   468,  1441,  1070,   428,  1773,
      53,    54,   444,    56,   238,   447,   839,   449,   170,    62,
     440,   453,   174,  1123,  1124,   167,  1232,  1313,    79,   128,
     462,   135,   136,   465,   466,   467,   995,   996,   167,  1160,
     169,   189,   171,   169,   143,  1108,   566,   173,   994,   932,
    1233,    13,    14,    15,    16,    17,   139,   140,  1121,   167,
    1123,  1124,  1136,  1317,   169,  1319,   126,   166,   173,    79,
     490,   169,   492,   114,   115,   495,   169,   128,  1317,   499,
    1319,  1121,   502,  1162,  1124,    79,   238,  1150,   290,   291,
    1153,   293,   143,   295,  1519,   169,  1042,  1123,  1124,    13,
      14,    15,    16,    17,   567,  1168,    79,   627,   167,   990,
     167,   934,   171,   169,   171,   166,  1075,   173,   128,   169,
     943,   169,   542,    13,    14,    15,    16,    17,   280,   169,
    1442,  1443,  1195,   143,   128,   167,   556,   557,   169,   171,
     144,   561,   173,   172,   664,  1208,   169,   141,  1188,   143,
     173,    79,   114,   115,   172,   128,   576,   167,  1700,  1233,
     144,   171,   167,    22,   584,   597,   171,   587,  1231,  1284,
     143,   323,   396,   167,   326,  1461,   167,   171,   178,   179,
    1295,  1444,   384,    65,   178,   179,    68,    69,  1371,    71,
    1253,  1488,  1489,   166,   614,   615,    79,  1488,  1489,  1314,
     128,   664,   665,  1282,  1283,   173,   169,  2179,   170,   361,
     173,  2183,   167,  1328,  1305,   143,   795,   167,    79,   156,
     157,   684,   167,   160,   161,   173,   378,   169,   380,   692,
    1351,   173,   167,  1296,   107,   141,  1059,   169,   390,   167,
    1303,   173,  1305,   171,   167,   128,   183,  1326,   668,    79,
     402,   141,   169,   157,   674,   192,   173,   777,    79,   167,
     143,   167,  1123,  1124,   175,   171,    95,   128,   172,   173,
    1453,  1386,   178,   179,    79,   795,  1391,   167,   430,   183,
      79,   171,   143,   166,   436,   191,   175,  1707,   178,   179,
     795,  1711,     3,    79,  1175,  1176,  1177,  1371,   128,   169,
    1388,   169,   722,   173,   166,   173,   167,   128,  1388,   169,
     171,  1192,  1193,   143,  1388,   192,   169,  1568,   169,   471,
     173,   169,   143,   128,   173,  1388,  1389,   172,    79,   128,
    1153,    13,    14,    15,    16,    17,   173,   167,   143,   175,
    1388,   171,   128,   175,   143,   191,   167,  1410,  1463,  1464,
     171,   169,   169,   932,  1417,   173,   173,   143,   778,  1422,
    1423,  1424,   167,   172,   173,  1480,   171,  1482,   167,  1432,
     172,   173,   171,  1436,   169,  1438,   167,   128,   173,  1453,
     141,   167,   169,   170,   169,   171,  1501,   850,   173,    79,
    1644,   175,   143,   169,    84,    85,  1487,  1488,  1489,   169,
     172,   173,  1805,   173,   175,  1644,   167,   141,   191,  1805,
     171,    79,   932,   169,   846,  1598,   167,   178,   179,   839,
     171,   173,   174,  1486,  1487,  1488,  1489,   932,  1805,    79,
    1253,   172,   173,   167,    84,    85,  1805,   171,   128,   172,
     173,  1805,  1388,   169,   178,   179,   169,  1805,   169,  1489,
     521,   522,   523,   143,   171,  1543,  1805,   172,   173,   141,
     128,   169,  1805,  1543,   169,  1648,    79,   167,   988,  1543,
     172,   173,   624,   169,  1537,   143,   166,  1805,   128,   167,
    1805,  1534,    18,  1546,   191,   167,  1549,  1550,  1551,   171,
     171,  1554,    79,   143,  1557,  1543,   178,   179,   174,   167,
     174,   703,   173,   171,    79,  1388,   167,  2049,   172,   173,
    1913,    79,  1597,  1598,   934,   128,  1707,  1913,   172,   173,
    1711,   172,   173,   943,  1598,   988,   191,   990,   172,   173,
     143,  1594,   172,   173,  1597,  1598,  1913,   172,   173,   691,
     169,   128,    97,    98,  1913,  1660,  1661,   167,  1799,  1913,
     172,   173,   169,   128,   167,  1913,   143,   169,   171,  1622,
     128,  1624,  1693,   169,  1913,   169,  1389,   169,   143,   169,
    1913,  1597,  1598,   169,  1648,   143,  2040,   169,  1890,  1642,
     167,  1793,  1794,  1795,   171,  1913,   172,   173,  1913,    77,
     792,   175,   167,  1693,     3,   175,   171,  1543,   191,   167,
    1530,  1531,  1532,   171,    13,    14,    15,    16,    17,  1432,
     172,   173,   130,  1436,   132,   133,   134,     5,   175,  1707,
    1813,  1814,  1815,  1711,  1712,    13,    14,    15,    16,    17,
    1693,   173,   174,  1707,  1697,   169,  1689,  1711,  1712,  1059,
    1728,    84,    85,   795,   528,   529,   530,   531,  1831,   167,
       5,  1597,   170,   171,  1728,   192,  1719,   175,   176,   173,
    1543,   172,  1543,   173,   174,   172,  1751,  1798,   169,  1800,
     119,   120,   121,   122,   123,   827,  1807,   169,  1741,  1742,
    1209,  1210,   524,   525,    39,   837,   838,   173,  1751,    13,
      14,    15,    16,    17,    18,   169,  1159,   173,  1798,   169,
    1800,   526,   527,   169,    31,   532,   533,  1807,   932,   861,
    1773,    38,   936,  2116,  1537,  1748,  1749,   169,  1814,  1815,
    2116,   169,   173,   169,   169,  1751,  1549,  1550,  1551,   169,
    1150,  1554,   169,  1153,    89,  1798,   169,  1800,   172,  2116,
    1172,   172,  1805,   172,  1807,  1999,  1799,  2116,  1168,   169,
     166,   172,  2116,  1816,   167,  1872,    85,  1831,  2116,    18,
    1999,  1707,   192,   175,   182,  1711,  1712,  2116,   172,   172,
     172,  1834,    99,  2116,   169,   169,   169,   169,  1841,   134,
     932,   172,  1728,   110,   139,  1916,  1917,   166,  2116,   169,
     145,  2116,   169,   169,   149,   169,   166,   172,   156,   157,
     155,   173,   160,   161,   169,  1751,   133,   169,   169,  1388,
     137,  1874,   167,   169,   169,   173,  1916,  1917,   711,  1642,
     169,   200,   169,   169,  1707,   183,   169,   169,  1711,  1712,
     169,    22,  1693,  1253,   192,  2041,    13,    14,    15,    16,
      17,    18,    77,  1960,   169,  1728,   169,   169,   169,   169,
    1913,   169,   169,  1916,  1917,  1943,   169,   166,   173,   175,
    2114,   173,   169,  1926,  1927,  1996,   169,   173,  1388,  1943,
    1933,   175,   191,  1913,  1697,   175,   169,  1917,   169,  1942,
     169,   173,   172,  1388,  1830,  2139,   173,  2141,   169,  1952,
      18,  1954,   247,   169,   169,   166,  1996,   175,   175,   169,
    2139,   175,  2141,   172,  1967,  1958,  1969,  1970,  1971,   174,
     112,   238,  1136,   174,   116,   117,   118,   119,   120,   121,
     122,   123,   172,   278,   130,   166,   175,  2181,   175,  2046,
     166,   191,   169,  1996,   172,   169,   169,  1798,   169,  1800,
    2003,   296,  2181,   172,  2007,   300,  1807,   169,   172,  2012,
     169,   169,   279,  2116,  2116,   169,  2116,   169,   166,   166,
      14,   166,   341,   167,  1543,   167,   168,   322,  1388,  1389,
      13,    14,    15,    16,    17,    18,   174,  2040,   167,  2042,
     167,   167,   167,  1929,   423,  2038,  2117,   167,   190,   316,
     345,   346,   167,  1816,  1875,   167,   282,  1943,   167,  2253,
     355,   174,   173,   358,   359,   169,   169,   362,  2071,  1233,
     365,   366,  1432,   368,  2253,   370,  1436,  2117,   192,  1598,
    2083,   166,   308,  1543,  2087,   166,   312,   175,   383,   166,
     173,   169,   172,   169,  2097,   169,   169,   169,  1543,   169,
    2103,   172,  2001,   169,   399,   169,  2177,  2178,   169,   404,
     429,   406,   379,  2116,  2117,  1916,  1917,  2145,  2146,   166,
    1943,   416,  2179,   192,   167,   444,  2183,  2184,   447,   396,
      87,  2145,  2146,   428,   453,  2034,   192,  2177,  2178,   192,
     192,   192,  2197,   462,   192,  2148,   192,   192,    98,   167,
     167,  2117,    13,    14,    15,    16,    17,    96,  2213,   169,
    2217,    13,    14,    15,    16,    17,    18,   486,   166,   166,
     172,   172,   167,   173,  2177,  2178,   169,  1537,   174,   169,
     169,  2238,   167,   167,  2187,  2242,  1546,   166,  1707,  1549,
    1550,  1551,  1711,  1712,  1554,  1996,  1568,  1557,  2178,   166,
    2257,   192,    82,    82,   192,   192,   166,  1371,   169,  1728,
     192,  2177,  2178,   167,   167,   166,   171,   166,    82,    82,
    2223,   183,   192,  2226,  1388,   183,    82,  2048,   192,   192,
     174,   423,  1065,   169,  1594,  1607,   192,   169,   169,   166,
     168,   183,   183,  2246,   166,   169,   112,  1707,   174,   167,
     173,  1711,  1712,   169,   183,  2258,   168,   172,   183,  2145,
    2146,   169,  1707,    82,  2267,   168,  1711,  1712,  1728,   174,
     169,   192,    13,    14,    15,    16,    17,   572,   169,     3,
     141,   548,  1642,  1728,   166,   580,   167,  1607,   583,  1453,
     166,   192,   169,   472,   192,   192,  1388,    85,   534,   566,
     535,   538,   536,   541,   214,  1563,   167,  1386,  2141,   537,
     171,  2228,  1831,  1711,   609,  1951,  2117,   178,   179,  1728,
    2173,  1943,  2145,  2146,   112,   592,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,  2015,  1697,    79,   110,
    2131,   567,  1879,  2113,  1865,  1865,  2038,  2184,  2242,   644,
     645,  2112,  1389,   648,   649,  1534,   651,  1534,   653,   654,
     627,  1534,    90,   658,   659,   146,  1199,   386,  2137,  1202,
    1996,  1689,  1449,  1594,   753,  1771,  2177,  2178,  1067,  1543,
     943,  1741,  1742,   713,  1630,     0,  1219,   128,   112,   615,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     141,    36,   143,   871,   192,   871,   871,   633,  1522,   676,
      -1,   678,    -1,    -1,    -1,    -1,    -1,   141,    -1,  1252,
      13,    -1,    -1,    -1,  1943,    -1,   167,    -1,    -1,    -1,
     171,    -1,   751,  1597,  1598,    -1,    -1,   178,   179,   665,
     759,    -1,  1534,   167,   168,    -1,    -1,   742,   640,    -1,
     174,  1543,    -1,    -1,   178,   179,  1816,    -1,    -1,    -1,
     686,    -1,    -1,    -1,    -1,    -1,   190,   786,    -1,    -1,
      -1,   766,    -1,    -1,  1834,    -1,   435,    -1,    -1,   798,
      -1,  1841,    -1,  1943,  1648,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   871,    -1,    -1,  2258,    -1,    -1,  1943,    -1,
      -1,    94,    -1,    -1,  2267,    -1,    -1,    -1,   279,   468,
     777,   282,    -1,    -1,  1874,   810,   811,   812,   289,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   362,    -1,   308,    -1,    -1,
      -1,   312,    -1,  1707,    -1,   316,    -1,  1711,  1712,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   753,    -1,    -1,  1728,    -1,  1926,  1927,    -1,    -1,
      -1,    -1,    -1,  1933,   167,    -1,    -1,   872,  1411,    -1,
     875,   876,  1942,    -1,    -1,    -1,    -1,  1751,    -1,    -1,
      -1,    -1,  1952,    -1,  1954,  1687,   975,  1689,    -1,    -1,
     979,    -1,    -1,    -1,    -1,    -1,    -1,  1967,   379,  1969,
    1970,  1971,  1445,    -1,    -1,  1707,    -1,    -1,    -1,  1711,
    1712,    -1,    -1,    -1,    -1,    -1,  2145,  2146,    -1,   192,
      -1,    -1,   824,    -1,    -1,    -1,  1728,    -1,    -1,    -1,
      -1,    -1,    -1,  2003,  1023,    -1,    -1,  2007,    -1,    -1,
     477,  1030,  2012,    -1,    -1,   950,    -1,    -1,   953,    -1,
     955,    -1,    -1,   930,   435,   932,  1830,  1831,    -1,   936,
      -1,    -1,    -1,    -1,    -1,   970,    -1,    -1,    -1,   871,
      -1,    -1,  2042,    -1,    -1,  2145,  2146,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,   468,    -1,    -1,
    2145,  2146,    -1,    -1,    -1,   664,    -1,  1799,    -1,    -1,
     936,  2071,    -1,    -1,    -1,    -1,   983,   676,    -1,    -1,
    1015,   947,    -1,  2083,    -1,   684,    -1,  2087,    -1,    -1,
      -1,    -1,    -1,   692,    -1,  1002,    -1,  2097,    -1,    -1,
      -1,    -1,    -1,  2103,   112,    -1,    -1,  1126,   116,   117,
     118,   119,   120,   121,   122,   123,   948,    -1,    79,   985,
      -1,    -1,    -1,    -1,   990,  1929,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,  1943,
    1613,    -1,   609,   975,    -1,   612,    -1,   979,  2148,    -1,
    1057,    -1,    -1,    -1,    -1,   566,   567,  1092,  1117,    -1,
     168,  1096,    -1,   171,    -1,  1100,    -1,   128,    -1,    -1,
    1077,  1003,    -1,  1646,    -1,  1041,  1135,    -1,    -1,  1138,
     141,   592,   143,  1142,    -1,    -1,    -1,  2187,    -1,    -1,
      -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,  1030,    -1,
      -1,    -1,    -1,    -1,   615,    -1,   167,   618,    -1,    -1,
     171,  1943,    -1,  1148,    -1,    -1,   627,   178,   179,  1154,
      -1,    -1,   633,  2223,    -1,    -1,  2226,    -1,    -1,  1136,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,  1262,    -1,    -1,  2246,  1266,    -1,    -1,
      -1,   850,    -1,   664,   665,  1162,    -1,    -1,  2258,    -1,
     141,    -1,    -1,   730,    -1,    -1,    -1,  2267,    -1,    -1,
    1136,    -1,    -1,   684,    -1,   686,    -1,    -1,    -1,    -1,
      -1,   692,    -1,    -1,    -1,    -1,   167,   168,    -1,   335,
     171,    -1,    -1,    -1,  1126,  1768,  1769,   178,   179,     4,
       5,     6,     7,     8,     9,    10,    11,    12,  1243,   190,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
    1339,    -1,    -1,    -1,  1343,  1232,  1233,    -1,  1347,    -1,
      -1,   930,   156,   157,   158,   159,   160,   161,   162,   163,
      -1,  2145,  2146,    -1,    13,    14,    15,    16,    17,   173,
      -1,    -1,  1287,    -1,    -1,  1290,  1291,    -1,    -1,   183,
      -1,    -1,    -1,  1298,  1299,    -1,   777,  1233,   192,    -1,
      -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    72,  1852,
      -1,    -1,    -1,    -1,   983,    -1,    -1,    -1,    -1,   988,
    1325,    -1,  1327,    -1,    -1,  1330,    -1,    -1,  1333,    -1,
      -1,    -1,  1337,  1002,   871,   872,  1313,    -1,    -1,    -1,
      79,    -1,    -1,  2145,  2146,  1374,    -1,    -1,  1891,  1326,
      -1,    -1,    -1,  1896,    -1,    -1,    -1,    -1,    -1,    -1,
    1262,    -1,    -1,  1368,  1266,    -1,    -1,    -1,    -1,   850,
      -1,    -1,    -1,    -1,    -1,  1380,    -1,    -1,  1467,    -1,
      -1,  1386,  1471,    -1,    -1,  1474,    -1,   117,  1057,   128,
      -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,  1403,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,  1077,    -1,
      31,  1388,    -1,    -1,    -1,  1504,    -1,    38,    -1,  1508,
      -1,    -1,    -1,  1512,    -1,    -1,    -1,    -1,   167,    -1,
      79,    -1,   171,   970,    -1,  1371,   973,  1339,    -1,   178,
     179,  1343,   112,    -1,    -1,  1347,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   936,    -1,    -1,    -1,    -1,
      -1,  1466,    -1,   112,    -1,    -1,   947,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,  1453,    -1,    99,   128,
    1382,    -1,    -1,  1460,  1461,    -1,    -1,    -1,    -1,   110,
      -1,    -1,   141,  1162,   143,    -1,   117,    -1,    -1,    -1,
      -1,   171,    -1,    -1,   985,    -1,    -1,   988,    -1,   990,
      -1,    -1,   133,  1542,    -1,    -1,   137,  1453,   167,   168,
    1609,    -1,    -1,    -1,    -1,    -1,    -1,    13,    -1,   178,
     179,    -1,  1561,    -1,    18,    -1,   266,    -1,    -1,  1568,
      -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,  1085,   170,
      -1,    -1,   282,   174,    -1,  1092,    -1,    -1,    -1,  1096,
    1041,    -1,    -1,  1100,    -1,  1467,  1543,    -1,    -1,  1471,
      -1,    -1,  1474,    -1,    -1,    -1,    -1,    -1,    -1,   200,
      64,    65,    66,    67,    68,    69,    70,    71,   704,   112,
     706,   114,  1569,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,  1504,    -1,    -1,    -1,  1508,    -1,    94,    -1,
    1512,    -1,    -1,  1282,  1283,    -1,    -1,   238,    -1,    -1,
    1597,  1598,    -1,    -1,   245,   246,   112,   248,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,  1643,    -1,
      -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,  1186,
     380,  1188,    -1,    -1,    -1,  1136,    -1,  1326,   279,   280,
      -1,   282,    -1,  1668,    -1,    -1,    -1,   288,   289,    -1,
      -1,  1648,  1701,  1702,    -1,    -1,  1681,  1682,  1683,    -1,
    1685,  1686,    -1,  1688,    -1,    -1,    -1,   308,    -1,    -1,
      -1,   312,    -1,    -1,    -1,   316,    -1,    -1,   319,    -1,
      -1,    -1,   323,    -1,    -1,   326,   436,  1609,    -1,    -1,
      -1,    -1,  1648,    -1,   444,    -1,    -1,    -1,    -1,    -1,
     450,  1750,    -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,
    1707,    -1,  1737,    -1,  1711,  1712,    -1,    -1,    -1,    -1,
     361,   471,    -1,    -1,    -1,    -1,    -1,    -1,  1753,    -1,
    1287,  1728,  1233,    -1,    -1,    -1,    -1,   378,   379,   380,
      -1,    -1,  1767,    -1,    -1,    -1,    -1,    -1,    -1,   390,
      -1,    -1,  1777,  1778,  1751,   396,   892,    -1,    -1,    -1,
     896,   402,    -1,    -1,    -1,    -1,   902,    -1,    -1,    -1,
      -1,  1460,    -1,  1330,    -1,    -1,  1333,    -1,    -1,   420,
    1337,    -1,   423,    -1,    -1,    -1,    -1,  1812,    -1,   430,
     926,    -1,   433,   434,   435,   436,    -1,   438,    -1,    -1,
      -1,    -1,    -1,   444,    -1,    -1,   447,    -1,   449,   450,
      -1,    -1,   453,    -1,    -1,  1864,    -1,   567,    -1,    -1,
      -1,   462,    -1,    -1,   465,   466,   467,   468,    -1,    -1,
     471,    -1,    64,  1830,  1831,    -1,    -1,   478,    -1,    -1,
      72,    73,    74,    75,    -1,    -1,  1403,   597,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1371,    -1,  1897,    -1,    -1,    -1,    -1,  1902,  1903,  1904,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   112,    -1,  1919,    -1,   116,   117,   118,   119,
     120,   121,   122,   123,   182,    -1,   184,   548,    -1,   187,
     188,    -1,    -1,    -1,    -1,   665,    -1,    -1,    -1,    -1,
     198,   199,    -1,    -1,  1060,   566,   567,    -1,   569,   192,
      -1,  1067,  1929,    -1,  1070,    -1,    -1,   215,   216,   171,
      -1,    -1,    -1,    -1,    -1,    -1,  1943,    -1,    -1,  1974,
    1975,   592,  1453,    -1,    -1,    -1,   597,   189,  1983,    -1,
      -1,    -1,    -1,    -1,  1989,  1990,    -1,   717,    -1,    -1,
      -1,    -1,   110,   614,   615,    -1,   617,   618,    -1,   257,
      -1,    -1,   623,   624,    -1,    -1,   627,    -1,   629,    -1,
      -1,    -1,   633,    -1,    -1,    -1,    -1,    -1,    -1,   640,
      -1,    -1,  1958,    -1,   282,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,   771,   772,   664,   665,    -1,    -1,    -1,   669,    -1,
      -1,  2056,    -1,   674,    -1,   676,    -1,   678,    -1,   680,
      -1,  2066,    -1,   684,    -1,   686,    -1,    -1,    -1,    -1,
     691,   692,    -1,    -1,    -1,  2080,    -1,   698,    -1,  1195,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1569,    -1,
      -1,    -1,  1208,    -1,    -1,    -1,   717,    -1,    -1,    -1,
    2189,    -1,  2038,    -1,    -1,    -1,    -1,  2112,   838,    -1,
      -1,   112,  2117,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   744,    -1,    -1,    -1,    -1,  2133,    -1,
      -1,    -1,   753,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,  2176,    -1,    -1,
     771,   772,    -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,   279,   141,    -1,    -1,    -1,    -1,  1648,  2145,  2146,
     171,    -1,    -1,   112,   795,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,  2214,    -1,    -1,   167,   168,
      -1,  2196,   171,    -1,    -1,    -1,    -1,   818,   316,   178,
     179,    -1,    -1,   824,    -1,    -1,   827,    -1,    -1,    -1,
      -1,   190,   833,   471,    -1,    -1,   837,   838,    -1,    -1,
    2225,    -1,    -1,  2228,    -1,   846,    -1,    -1,    -1,   850,
     960,   170,    -1,    -1,    -1,    31,   966,    -1,    -1,    -1,
     861,    -1,    38,   182,  2249,    -1,    -1,    -1,    -1,    -1,
     871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     990,   379,   520,   521,   522,   523,   524,   525,   526,   527,
     528,   529,   530,   531,   532,   533,   534,   535,   536,   537,
     538,    -1,    -1,    -1,    -1,    -1,    -1,  2189,    -1,    13,
      14,    15,    16,    17,  1410,    -1,    -1,    -1,    -1,    -1,
      -1,  1417,    -1,    99,    -1,    -1,  1422,  1423,  1424,   930,
      -1,   932,    -1,    -1,   110,   936,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,   947,   948,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   960,
      -1,   137,    -1,    -1,    -1,   966,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   975,    79,    -1,    -1,   979,    -1,
      -1,    -1,   983,    -1,   985,    -1,    -1,   988,    -1,   990,
    1907,    -1,    -1,   994,   170,    -1,    -1,    -1,   174,    -1,
    1001,  1002,  1003,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,  1023,    -1,   128,   663,    -1,    -1,    -1,  1030,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
    1041,  1042,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,  1057,    -1,   200,    -1,
      -1,    -1,   238,   167,   168,  1982,  1983,   171,   566,    -1,
     246,    -1,   141,    -1,   178,   179,  1077,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,
      -1,   267,    -1,    -1,   592,    -1,    -1,  1958,   167,   168,
      -1,    -1,   171,   279,   280,    -1,   282,    -1,    -1,   178,
     179,    -1,    -1,   289,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,   191,  2040,    -1,  1126,  1622,  1237,  1624,   627,
      -1,    -1,   308,    -1,    -1,  1136,   312,    -1,    -1,    -1,
     316,    -1,    -1,    -1,    -1,    -1,  2063,   323,    -1,    -1,
     326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1159,    -1,
      -1,  1162,    -1,    -1,    -1,  1166,    -1,    -1,    -1,    -1,
      -1,  1172,    -1,    -1,    -1,    -1,   352,  2038,   676,    -1,
     678,    -1,    -1,    -1,   112,   361,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   372,    -1,    -1,  2116,
    2117,   377,   378,   379,   380,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   390,    -1,  2133,    -1,    -1,   857,
    2137,    -1,    -1,  1719,   862,    -1,   402,    -1,    -1,    -1,
     406,  1232,  1233,    -1,    -1,   411,  1237,    -1,    -1,    -1,
     416,    -1,   880,    -1,    -1,    -1,    -1,   423,  1249,    -1,
     178,    -1,    -1,    -1,   430,    -1,    -1,    79,   434,   435,
    2177,  1262,    -1,    -1,    -1,  1266,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,  1546,  1773,    -1,   777,
      -1,  1282,  1283,    -1,    -1,   923,    -1,  1557,    -1,    -1,
     112,    -1,   468,    -1,   116,   117,   118,   119,   120,   121,
     122,   123,   444,    -1,    -1,   447,   128,    -1,    -1,    18,
      -1,   453,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     462,   143,    -1,    -1,    -1,  1326,   824,    -1,    -1,    -1,
      -1,  1441,    -1,    -1,    -1,    -1,    79,    -1,  1339,    -1,
      -1,    -1,  1343,    -1,   486,    -1,  1347,    -1,    -1,    -1,
      -1,    -1,   990,    -1,    -1,    -1,    -1,    -1,  1359,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,   112,
    1371,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,  1382,    -1,    -1,    -1,   128,    -1,  1388,    -1,    -1,
     566,   567,    -1,    -1,    -1,    -1,    -1,   539,   141,    -1,
     143,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   592,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,   171,    -1,
      -1,    -1,   930,  1071,    -1,   178,   179,    -1,    -1,   615,
    1441,    -1,   618,    -1,    -1,    -1,    -1,   190,   624,    -1,
      -1,   627,  1453,    -1,    -1,    -1,    -1,   633,    -1,  1460,
    1461,    -1,   171,    -1,   640,    -1,  1467,    -1,   644,    -1,
    1471,  1741,  1742,  1474,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   983,    -1,    79,   664,   665,
      -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,    -1,    -1,
     676,    -1,   678,  1504,  1002,    -1,    -1,  1508,   684,    -1,
     686,  1512,    -1,    -1,    -1,   691,   692,    -1,    -1,    -1,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,  1534,    -1,    -1,   128,  1175,  1176,  1177,
      -1,    -1,  1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   143,    -1,    -1,  1192,  1193,    -1,    -1,    -1,  1057,
      -1,    -1,    -1,    -1,  1834,    -1,    -1,  1568,  1569,    -1,
      -1,  1841,    -1,    -1,  1684,   167,   168,   753,    -1,  1077,
      -1,    -1,    -1,  1221,    -1,    -1,    -1,   763,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1597,  1598,   190,    -1,
      -1,   777,    -1,    -1,  1874,    -1,  1607,    -1,  1609,    -1,
      13,    14,    15,    16,    17,  1725,    -1,    -1,    -1,   795,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   771,
     772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1641,    -1,    -1,    -1,    -1,    -1,    -1,  1648,   824,    -1,
      -1,   827,    -1,    -1,    -1,    31,  1926,  1927,    -1,    -1,
      -1,   837,    38,  1933,  1162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1942,    -1,   850,    -1,    79,    -1,    -1,  1317,
      -1,  1319,  1952,  1684,  1954,   861,  1687,    -1,  1689,    -1,
      -1,    -1,    -1,  1694,    -1,   871,   872,  1967,    -1,  1969,
    1970,  1971,    -1,    -1,    -1,    -1,  1707,    -1,    -1,   112,
    1711,  1712,    -1,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    99,  1725,   128,    -1,  1728,    -1,    -1,
      -1,    -1,    -1,  2003,   110,    -1,    -1,  2007,   141,    -1,
     143,    -1,  2012,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1751,    -1,    -1,    -1,   930,    -1,   932,    -1,    -1,    -1,
     936,   137,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   947,   948,    -1,   950,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1282,  1283,    -1,   190,   102,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,  1799,   975,
      -1,  2071,    -1,   979,    -1,    -1,    -1,   983,    -1,   985,
      -1,    -1,   988,  2083,   990,  1313,    -1,  2087,    -1,   133,
      -1,    -1,    -1,    -1,    -1,    -1,  1002,  1003,  1326,  1830,
    1831,   145,   146,  2103,    -1,    -1,    -1,    -1,  1014,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,
      -1,    -1,    -1,   112,  1030,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,  1041,    -1,    -1,    -1,    -1,
     246,    -1,    -1,    -1,    -1,    -1,   190,    -1,  2148,    -1,
      -1,  1057,   141,    -1,  1382,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,  1077,    -1,   279,    -1,  1543,   282,    -1,   167,   168,
      -1,    -1,    -1,   289,    -1,   174,    -1,  2187,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,   190,   308,    -1,    -1,    -1,   312,    -1,    -1,  1940,
     316,    -1,  1943,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1126,    -1,    -1,  2223,    -1,    -1,  2226,  1958,    -1,    -1,
    1136,    -1,  1460,  1461,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   288,    -1,  2246,    -1,    -1,    -1,
      -1,    -1,    -1,  1159,    -1,   361,  1162,    -1,    -1,   112,
    1166,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,   379,    -1,   319,  1644,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   390,    -1,  2017,  2018,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,
     406,    -1,    -1,    -1,    -1,    -1,    -1,  2038,    -1,    -1,
    2041,    -1,    -1,    -1,   167,   168,    -1,   423,    -1,    -1,
      -1,    -1,    -1,    -1,   430,   178,   179,  1233,   434,   435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,
      -1,  1569,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,   396,    -1,    -1,    -1,  1262,    -1,    -1,    -1,
    1266,    -1,   468,    -1,    -1,    -1,  2097,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1294,   433,
      -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,  1305,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,
      -1,    -1,    -1,    -1,  2145,  2146,    -1,    -1,    -1,   112,
    1326,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,  1339,   478,    -1,    -1,  1343,    -1,    -1,
      -1,  1347,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,  2189,    -1,
     566,   567,    -1,    -1,    -1,  1371,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,   167,   168,  1382,    -1,    -1,    -1,
      -1,    -1,  1388,  1355,    -1,   112,   592,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   190,    -1,   167,
     168,    -1,    -1,    -1,   548,    -1,    -1,  1875,    -1,   615,
     178,   179,   618,    13,    14,    15,    16,    17,    -1,    -1,
      -1,   627,   190,    -1,    -1,   569,    -1,   633,    -1,    -1,
      -1,    -1,    -1,   112,   640,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,  1453,   175,    -1,
      -1,    -1,    -1,    -1,  1460,  1461,    -1,    -1,   664,   665,
      -1,  1467,   141,    -1,    -1,  1471,    -1,    -1,  1474,    -1,
     676,    -1,   678,   617,    -1,    -1,    -1,    -1,   684,   623,
     686,    -1,    -1,    -1,    -1,   629,   692,    -1,   167,   168,
      -1,    -1,   171,    -1,    -1,    -1,    -1,    -1,  1504,   178,
     179,    -1,  1508,    -1,    -1,    -1,  1512,    -1,    -1,    -1,
      -1,   190,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   669,    -1,    -1,  1534,    -1,
      -1,  1999,    -1,    -1,    -1,    -1,   680,  1543,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,   753,    -1,    -1,
      -1,    -1,    -1,    -1,   698,    -1,    -1,    -1,    -1,   703,
      -1,    -1,    -1,  1569,    -1,    -1,    -1,   167,   168,    -1,
      -1,   777,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
    2048,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,
     190,    -1,  1598,    -1,    -1,    -1,  1568,    -1,    -1,    -1,
     744,   112,    -1,  1609,    -1,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,    -1,   824,    -1,
      -1,    -1,    -1,    -1,    -1,    31,    -1,    -1,    -1,    -1,
     141,    -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1648,   112,   850,    -1,  2114,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   167,   168,   802,    -1,
     129,    -1,   131,    -1,    -1,   871,    -1,   178,   179,    -1,
      -1,  2139,    -1,  2141,   818,    -1,    -1,    -1,    -1,   190,
      -1,  1687,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,   168,
      -1,  1707,   171,    -1,   110,  1711,  1712,    -1,    -1,    -1,
      -1,    -1,    -1,  2181,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1728,    -1,   930,    -1,   932,    -1,    -1,    -1,
     936,   137,    -1,    -1,    -1,  1707,  1708,    -1,    -1,  1711,
    1712,   947,   948,    -1,    -1,  1717,    -1,    -1,    -1,  1721,
      -1,    -1,    -1,    -1,  1726,    -1,  1728,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    79,    -1,   975,
      -1,    -1,    -1,   979,    -1,    -1,    -1,   983,    -1,   985,
      -1,    -1,   988,    -1,   990,  2253,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1002,  1003,    -1,    -1,
     112,  1807,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,   128,  1023,    -1,    -1,
      -1,    -1,    -1,    -1,  1030,  1831,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,  1041,    -1,    -1,    -1,    -1,
     246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     994,  1057,    -1,    -1,  1826,   167,   168,  1001,    -1,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    -1,
      -1,  1077,    -1,   279,  1846,  1847,   282,    -1,   190,    -1,
      -1,    -1,    -1,   289,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1042,    -1,
      -1,    -1,   308,    -1,  1876,    -1,   312,    -1,    -1,    -1,
     316,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1136,    -1,    -1,    -1,  1940,    -1,   112,  1943,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      79,    -1,  1958,  1159,    -1,   361,  1162,    -1,    -1,  1931,
    1166,    -1,    -1,  1935,    -1,  1937,    -1,    -1,  1940,  1941,
      -1,  1943,    -1,   379,    -1,    -1,  1948,  1983,    -1,    -1,
      -1,    -1,    -1,   112,   390,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,   402,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,   182,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,   423,    -1,    -1,
      -1,    -1,    -1,    -1,   430,    -1,    -1,  1233,   434,   435,
      -1,    -1,  2038,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,  2017,    -1,    -1,    -1,   178,
     179,    -1,  2024,    -1,    -1,    -1,  1262,  2029,  2030,    -1,
    1266,   190,   468,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,
    2052,    -1,    -1,    -1,    -1,    -1,    -1,   110,  1232,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,
      -1,  2117,    -1,    -1,    -1,    -1,  2088,    -1,  2090,    -1,
    1326,  2093,  2094,  2095,    -1,    -1,    -1,    -1,    -1,  2101,
    2102,    -1,    -1,  1339,    -1,    -1,    -1,  1343,    -1,  2145,
    2146,  1347,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     566,   567,   112,    -1,    -1,  1371,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,  1382,    -1,    -1,    -1,
      -1,    -1,  1388,  2189,    -1,    -1,   592,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,  2169,  2170,  2171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   615,
      -1,    -1,   618,    -1,    -1,  1359,    -1,   167,   168,    -1,
      -1,   627,    -1,    -1,    -1,    -1,    -1,   633,   178,   179,
      -1,  2203,  2204,  2205,   640,    -1,    -1,    -1,    -1,    -1,
     190,    -1,    -1,    -1,    -1,    -1,    -1,  1453,    -1,    -1,
      -1,    -1,    -1,    -1,  1460,  1461,    -1,    -1,   664,   665,
      -1,  1467,    -1,    -1,    -1,  1471,    -1,    -1,  1474,    -1,
     676,    -1,   678,    -1,    -1,    -1,    -1,    -1,   684,    -1,
     686,    -1,    -1,    -1,    79,   112,   692,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,  1504,    -1,
      -1,    -1,  1508,    -1,    -1,    -1,  1512,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,  1543,    -1,    -1,
      -1,   168,    -1,    -1,   171,    -1,   141,   753,   143,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    55,  1569,    -1,    58,    -1,    60,    61,    -1,
      63,   777,   167,   168,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,    -1,    -1,    80,    -1,   795,
      -1,    -1,  1598,    -1,    -1,   190,    -1,    -1,    -1,   130,
      -1,    -1,    -1,  1609,    -1,    -1,    -1,   170,    -1,    -1,
      13,    14,    15,    16,    17,    -1,   109,   110,   824,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    -1,
     133,   134,  1648,    -1,   850,    -1,    -1,    -1,   141,    -1,
      -1,   112,    -1,  1597,    -1,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   871,    -1,    -1,   129,    -1,
     131,    -1,    -1,    -1,   167,    -1,    79,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1707,    -1,    -1,    -1,  1711,  1712,   168,    -1,   112,
     171,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,  1728,    -1,   930,   128,   932,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,   947,   948,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1694,    -1,    -1,    -1,    -1,   286,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,   975,
      -1,    -1,    -1,   979,    -1,   178,   179,   983,    -1,   985,
      -1,    -1,   988,    -1,   990,    -1,    -1,   190,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1002,  1003,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    -1,    -1,  1751,    -1,    -1,
      -1,    38,    -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,
     351,   352,    -1,    -1,  1030,  1831,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1041,    -1,    -1,    -1,    -1,
      -1,   372,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,  1077,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,   110,    -1,    -1,  1830,    -1,    -1,    -1,
     117,    -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,   432,    -1,   130,    -1,    -1,   133,    -1,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1126,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
    1136,    -1,    -1,    -1,    -1,    -1,    -1,  1943,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,  1958,  1159,    -1,    -1,  1162,    -1,    79,   112,
    1166,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,   200,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1929,    -1,    -1,   141,    -1,
     143,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,   238,    -1,    -1,   167,   168,   547,    -1,    -1,   246,
     141,    -1,   143,    -1,    -1,   178,   179,  1233,    -1,    -1,
      -1,    -1,  2038,    -1,    -1,    -1,    -1,   190,    -1,   266,
     267,   268,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,   279,   280,    -1,   282,  1262,   178,   179,   286,
    1266,   288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,
      -1,   308,    -1,    -1,    -1,   312,    -1,    -1,    -1,   316,
      -1,    -1,   319,    -1,    -1,    -1,   323,  2041,    -1,   326,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,   640,
      -1,    -1,    -1,   644,    -1,    -1,    -1,    -1,    -1,    -1,
    1326,    -1,    -1,    -1,   351,   352,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1339,    -1,    -1,    -1,  1343,    -1,  2145,
    2146,  1347,    -1,    -1,    -1,   372,    -1,    -1,    -1,    -1,
      -1,    -1,   379,   380,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,   396,
      -1,    -1,    -1,    -1,    -1,    -1,  1382,    -1,    -1,    -1,
      -1,    -1,  1388,  2189,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   723,   112,   725,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   432,   737,   738,   435,   436,
      -1,   438,    -1,    -1,    -1,    -1,    -1,   444,    -1,    -1,
     447,    -1,   449,   450,    -1,    -1,   453,    -1,    -1,    -1,
      -1,   762,   763,   764,    -1,   462,    -1,    -1,   465,   466,
     467,   468,    -1,    -1,   471,   776,    79,  1453,   167,    -1,
     477,   478,    -1,    -1,  1460,  1461,    -1,    -1,    -1,    -1,
      -1,  1467,    -1,    -1,    -1,  1471,    -1,   112,  1474,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,  1504,    -1,
      -1,    -1,  1508,    -1,    -1,    -1,  1512,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     547,   548,    -1,    -1,    -1,   170,    13,    14,    15,    16,
      17,    -1,    -1,    -1,   167,   168,    -1,  1543,    -1,   566,
     567,   568,   569,   112,    -1,   178,   179,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   190,    -1,    -1,
     129,    -1,   131,  1569,    -1,   592,    -1,    -1,    -1,    -1,
     597,    -1,   599,   600,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,    -1,    -1,    -1,   615,    -1,
     617,    -1,  1598,   164,    -1,    -1,   623,   624,    -1,   168,
     627,    -1,   629,  1609,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   640,    -1,    -1,    -1,   644,    -1,    -1,
     191,    -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   664,   665,    -1,
      -1,    -1,  1648,    -1,    -1,    -1,    -1,    -1,    -1,   676,
      -1,   678,    -1,   680,   141,    -1,    -1,   684,    -1,   686,
      -1,    -1,    -1,    -1,   691,   692,    -1,    -1,    -1,    -1,
      -1,    -1,  1003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,  1015,    -1,  1017,    -1,  1019,    -1,
     717,   178,   179,    -1,    -1,    -1,   723,    -1,   725,    -1,
      -1,  1707,    -1,   190,    -1,  1711,  1712,    -1,    -1,    -1,
     737,   738,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1728,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,   762,   763,   764,    -1,    -1,
      -1,    -1,    -1,    -1,   771,   772,    -1,    -1,    -1,   776,
     777,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,  1108,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1121,    -1,  1123,  1124,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,   838,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,
     167,   168,    -1,   850,    -1,  1831,    -1,    80,    -1,    -1,
      -1,   178,   179,    -1,   861,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,   871,   872,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,   112,    -1,    -1,   141,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
    1231,    -1,   129,   930,   131,   932,    -1,    -1,    -1,   936,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
     947,   948,    -1,   950,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,   960,  1940,    -1,    -1,  1943,    -1,   966,
      -1,   168,    55,    -1,   171,    58,    -1,    60,    61,    -1,
      63,    -1,  1958,    -1,    -1,    -1,   983,    -1,   985,    -1,
      -1,   988,    -1,   990,    -1,  1296,    -1,    80,    -1,    -1,
      -1,    -1,  1303,    -1,  1305,  1002,  1003,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
    1017,    -1,  1019,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,  1041,  1042,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,   107,
    1057,    -1,  2038,   167,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    79,
    1077,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,
      -1,  1108,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,  1121,    -1,  1123,  1124,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1438,    -1,  1136,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,
      -1,    -1,  1159,    -1,    -1,  1162,    -1,   167,   168,  2145,
    2146,    -1,    -1,    -1,    -1,  1172,    -1,    -1,   178,   179,
      -1,    -1,    -1,    -1,    -1,  1486,  1487,  1488,  1489,  1186,
     190,  1188,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2189,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   112,   141,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,  1231,  1232,  1233,   117,    -1,    -1,
    1237,    -1,    -1,    -1,    -1,    -1,    -1,  1244,   167,   168,
     141,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1282,  1283,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,    -1,    -1,  1303,    -1,  1305,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1313,    -1,    -1,    -1,
     200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,  1326,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   238,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   266,    -1,    -1,    -1,
      -1,  1388,  1693,    -1,    -1,    -1,  1393,    -1,    -1,    -1,
     280,    -1,    -1,    -1,    -1,    -1,   286,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1438,    -1,    -1,  1441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1453,    -1,    -1,    -1,
      -1,    -1,    -1,  1460,  1461,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,
      -1,    -1,   362,    -1,    -1,    -1,    -1,    -1,    -1,  1486,
      -1,  1488,  1489,    -1,    -1,    -1,    -1,  1798,    -1,  1800,
     380,    -1,    -1,    -1,  1805,    -1,  1807,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1534,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1543,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   444,    -1,    -1,   447,    -1,   449,
     450,  1568,  1569,   453,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   462,    -1,    -1,   465,   466,   467,    -1,    -1,
      -1,   471,    -1,    -1,    -1,    -1,    -1,   477,    -1,    -1,
    1597,  1598,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1607,    -1,  1913,    -1,    -1,  1916,  1917,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,  1648,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,  1684,   568,    57,
      -1,    59,  1689,    -1,    -1,  1996,  1693,  1694,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1707,    -1,    -1,    -1,  1711,  1712,    -1,   597,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1725,   609,
     610,  1728,   612,   613,    -1,   615,    -1,    -1,    -1,  2040,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,  1751,    -1,    -1,    -1,    -1,    -1,
     640,    -1,    -1,    -1,   644,   645,    -1,    -1,    -1,    -1,
      -1,   651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   661,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1798,  1799,  1800,    -1,    -1,   174,    -1,  1805,    -1,
    1807,   691,    -1,    -1,    -1,  2116,  2117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1830,  1831,    -1,    -1,   717,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   726,    -1,    -1,    -1,
     730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2177,  2178,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   766,    -1,    -1,    -1,
      -1,   771,   772,    -1,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1907,    -1,    -1,    -1,    -1,    -1,  1913,    -1,    -1,  1916,
    1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,
      -1,  1958,    99,    -1,    -1,    -1,   846,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   861,    -1,    -1,    -1,  1982,  1983,    -1,    -1,    -1,
      -1,   871,   872,    -1,    -1,    -1,    -1,    -1,    -1,  1996,
     137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,    -1,    -1,    -1,   174,    -1,    -1,
      -1,  2038,    -1,  2040,  2041,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   932,    -1,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,   200,    -1,    -1,  2063,   947,   948,    -1,
     950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     960,    -1,    -1,    -1,    -1,    -1,   966,    -1,    -1,    -1,
     970,   971,    -1,   973,   974,    -1,    -1,    -1,    -1,    -1,
     107,   238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2116,
    2117,    -1,    -1,  1003,    -1,  1005,    -1,    -1,    -1,   266,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,    -1,    -1,
    2137,    -1,    -1,   280,    -1,    -1,    -1,    -1,  2145,  2146,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1041,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2177,  2178,    -1,    -1,    -1,    -1,   323,    -1,    -1,   326,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1081,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,
      -1,    -1,  1092,  1093,    -1,    -1,  1096,  1097,    -1,    -1,
    1100,  1101,    -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,
    1110,    -1,    -1,    -1,    -1,    -1,    -1,    31,    -1,    -1,
      -1,   378,  1122,   380,    38,    -1,    -1,    -1,    -1,   386,
      -1,    -1,    -1,   390,    -1,    -1,  1136,    -1,    -1,   266,
     267,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1172,   430,    -1,    -1,    -1,   434,    -1,   436,
      -1,    -1,    -1,    -1,    -1,    99,  1186,   444,  1188,    -1,
     447,    -1,   449,   450,    -1,    -1,   453,    -1,    -1,    -1,
      -1,    -1,    -1,   117,    -1,   462,    -1,    -1,   465,   466,
     467,    -1,    -1,    -1,   471,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,   351,   352,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1233,    -1,    -1,    -1,  1237,    -1,    -1,
      -1,    -1,    -1,    -1,  1244,    -1,    -1,    -1,    -1,    -1,
     377,    -1,    -1,   380,    -1,    -1,   170,    -1,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   200,  1287,  1288,    -1,
    1290,  1291,  1292,    -1,    -1,    -1,   423,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   444,    -1,    -1,
      -1,    -1,    -1,   450,   238,    -1,    -1,    -1,    -1,    -1,
    1330,  1331,    -1,  1333,  1334,    -1,    -1,  1337,  1338,    -1,
     597,    -1,    -1,    -1,   471,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   266,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1362,    -1,    -1,    -1,   280,   624,    -1,    -1,
      -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1388,    -1,
      -1,    -1,    -1,  1393,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1403,  1404,    -1,    -1,    -1,    -1,   323,
      -1,    -1,   326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   691,    -1,    -1,    -1,    -1,    -1,
     567,  1441,    -1,    -1,    -1,    -1,    -1,   361,    -1,    -1,
      -1,    -1,    -1,  1453,    -1,    -1,    -1,    -1,    -1,    -1,
     717,    -1,    -1,    -1,   378,    -1,   380,    -1,    -1,    -1,
      -1,    -1,   386,    -1,    -1,    -1,   390,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   640,   771,   772,   430,   644,    -1,    -1,
     434,    -1,   436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     444,    -1,    -1,   447,  1534,   449,   450,    -1,   795,   453,
      -1,    -1,    -1,  1543,    -1,    -1,    -1,    -1,   462,    -1,
      -1,   465,   466,   467,    -1,    -1,    -1,   471,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1568,    -1,
     827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     837,   838,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,
     717,    -1,    -1,    -1,    -1,    31,    -1,    -1,  1598,    -1,
      -1,    -1,    38,    -1,   861,    -1,    -1,  1607,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   753,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   762,   763,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   771,   772,    -1,    -1,  1648,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   932,    -1,    -1,    -1,    -1,
      -1,   117,    -1,   597,  1684,    -1,    -1,    -1,    -1,  1689,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,   137,    -1,   960,    -1,    -1,    -1,  1707,    -1,   966,
     624,  1711,  1712,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1725,    -1,    -1,  1728,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,   871,   872,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   200,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,   691,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    -1,    -1,   160,   161,  1799,
      -1,    -1,   238,   717,   167,   168,    -1,   170,   171,    -1,
      -1,    -1,  1812,    -1,   177,   178,   179,   180,   181,   182,
     183,   948,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     266,  1831,    -1,   960,    -1,    -1,    -1,    -1,    -1,   966,
      -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,   975,    -1,
      -1,    -1,   979,    -1,    -1,    -1,    -1,   771,   772,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1003,    -1,    -1,    -1,
      -1,   795,    -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,
     326,    -1,    -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,
      -1,    -1,    -1,  1030,    -1,    -1,    -1,  1907,    -1,    -1,
      -1,    -1,    -1,   827,  1914,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   837,   838,   361,    -1,    -1,    -1,    -1,
      -1,    -1,   846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   378,  1943,   380,    -1,    -1,   861,    -1,    -1,
     386,    -1,    -1,    -1,   390,    -1,    -1,    -1,  1958,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1237,    -1,  1982,  1983,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   430,  1995,    -1,    -1,   434,  1126,
     436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   444,    -1,
      -1,   447,    -1,   449,   450,    -1,    -1,   453,   932,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,   465,
     466,   467,    -1,    -1,    -1,   471,    -1,    -1,  2038,    -1,
    2040,    -1,    -1,    -1,    -1,    -1,   960,     3,    -1,    -1,
      -1,    -1,   966,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,  2063,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,
    1237,    -1,    -1,    -1,    -1,    -1,  2116,  2117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1388,    -1,  2133,  2134,  1262,    -1,  2137,    -1,  1266,
      -1,    -1,    -1,    -1,    -1,  2145,  2146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,   597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2177,  1305,    -1,
      -1,    -1,    -1,    -1,  1441,   141,    -1,    -1,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1339,    -1,   170,   171,  1343,    -1,    -1,    -1,
    1347,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,    -1,   147,
      -1,   149,    -1,    -1,    -1,   691,    -1,    -1,  1172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1534,    -1,    -1,
      -1,   717,    -1,    -1,    -1,    -1,  1543,    -1,    -1,    -1,
      -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1568,    -1,    -1,  1441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1237,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   771,   772,    -1,    -1,    -1,
    1467,  1598,    -1,    -1,  1471,    -1,    -1,  1474,    -1,    -1,
    1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1504,    -1,    -1,
      -1,  1508,    -1,    -1,    -1,  1512,    -1,    -1,    -1,    -1,
     288,   827,    -1,    -1,    -1,    -1,   294,    -1,   296,    -1,
      -1,   837,   838,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,   319,    -1,   321,   322,   861,    -1,  1684,    -1,    -1,
    1687,    -1,  1689,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1707,    -1,    -1,    -1,  1711,  1712,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1725,    -1,
      -1,  1728,    -1,    -1,  1388,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,    -1,    -1,   932,    -1,   396,    -1,
     398,   399,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   960,    -1,    -1,  1441,    -1,    -1,
     966,    -1,    -1,    -1,    -1,   433,    -1,    -1,    -1,    -1,
     438,    -1,  1799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1684,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1831,    -1,    -1,    -1,    -1,    -1,
     478,    -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1725,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1543,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,
     548,    -1,    -1,    -1,  1568,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   569,    -1,   571,   572,    -1,    -1,    -1,  1805,    -1,
    1807,    -1,    -1,    -1,  1598,    -1,  1943,    -1,    -1,    -1,
      -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,    -1,   316,
      -1,   599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   335,   617,
     337,    -1,    -1,    -1,    -1,   623,    -1,    -1,    -1,    -1,
      -1,   629,    -1,   631,   632,    -1,  1172,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1684,   669,   670,  1687,    -1,  1689,    -1,    -1,    -1,    -1,
      -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1707,    -1,    -1,    -1,  1711,  1712,    -1,
     698,  1237,    -1,    -1,    -1,   703,    -1,    -1,    -1,    -1,
      -1,  1725,    -1,    -1,  1728,    -1,    -1,    -1,   435,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,   737,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   468,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   764,    -1,    -1,    -1,
      -1,    -1,   489,    -1,   491,    -1,   774,    -1,    -1,    -1,
      -1,   498,    -1,   500,   501,  1799,    -1,    -1,  2145,  2146,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   515,    -1,
      -1,    -1,    -1,    -1,   802,    -1,    -1,   805,   806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1831,    -1,    -1,
     818,    -1,    -1,    -1,   541,    -1,    -1,   544,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   833,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,
      -1,    -1,  1388,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2116,
    2117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     617,   618,    -1,   620,    -1,  1441,    -1,    -1,    -1,    -1,
     627,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   641,    -1,   643,    -1,    -1,  1943,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2189,    -1,    -1,    -1,    -1,   684,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   701,    -1,    -1,    -1,   705,   706,
      -1,    -1,    -1,    -1,    -1,   712,   994,    -1,  1534,    -1,
     717,    -1,    -1,  1001,    -1,    -1,    -1,  1543,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   744,    -1,    -1,
      -1,    -1,  1568,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,  1042,    -1,    -1,    -1,    -1,  1047,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
     777,    -1,  1598,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   110,    -1,    -1,    -1,    -1,   109,   110,
     117,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   133,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,  1123,    -1,    -1,    -1,    -1,
      -1,  2145,  2146,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   174,  1684,   170,
     171,  1687,   869,  1689,   175,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,   881,    -1,    -1,   884,   885,   886,
      -1,  1707,    -1,   200,    -1,  1711,  1712,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   902,    -1,    -1,  1186,  1725,
      -1,    -1,  1728,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   246,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1232,    -1,    -1,    -1,    -1,   266,
      -1,    -1,    -1,    -1,    -1,    -1,  1244,    -1,    -1,   966,
      -1,    -1,   279,    -1,    -1,   282,    -1,    -1,    -1,    -1,
      -1,   288,    -1,  1799,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   988,    -1,    -1,    -1,    -1,    -1,   994,    -1,    -1,
      -1,   308,    -1,    -1,    -1,   312,    -1,    -1,    -1,   316,
      -1,    -1,   319,    -1,    -1,  1831,    -1,  1295,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1042,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1067,    -1,   379,   380,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1080,    -1,    -1,    -1,    -1,    -1,   396,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1943,   435,   436,
      -1,   438,    -1,    -1,    -1,    -1,    -1,   444,    -1,    -1,
     447,    -1,   449,   450,    -1,    -1,   453,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,   465,   466,
     467,   468,    -1,    -1,   471,    -1,    -1,    -1,    -1,    -1,
      -1,   478,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1195,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1217,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,
     567,    -1,   569,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,    -1,    -1,
     597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1295,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   615,    -1,
     617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1597,
     627,    -1,   629,    -1,    -1,    -1,  1323,    -1,    -1,  2145,
    2146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   664,   665,    -1,
    1357,    -1,  1359,    -1,    -1,    -1,    -1,    -1,    -1,   676,
      -1,   678,    -1,    -1,    -1,    -1,    -1,   684,    -1,   686,
      -1,    -1,    -1,    -1,    -1,   692,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     717,    -1,    -1,  1410,    -1,  1412,  1694,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,   771,   772,    62,    -1,    -1,    -1,
     777,    -1,    -1,  1751,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1478,  1479,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,   838,    56,    57,    -1,    59,    -1,    -1,    62,   846,
      -1,    -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1562,  1563,    -1,    -1,    -1,
      -1,    -1,  1569,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,
      -1,  1608,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,   930,    -1,    -1,    -1,    -1,    -1,    -1,
    1627,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1916,    -1,
     947,    -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,
      -1,  1929,    -1,   960,   178,   179,    -1,    -1,    -1,   966,
      -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   983,    -1,   985,    -1,
    1677,   988,    -1,   990,    -1,    -1,    -1,  1684,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1002,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1719,    -1,    -1,    -1,    -1,    -1,  1725,    -1,
      -1,    -1,    -1,    -1,  1041,  1042,    -1,    -1,    -1,    -1,
    2018,  2019,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2041,    -1,    -1,    -1,    -1,    -1,    -1,
    1077,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2063,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1806,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1159,    -1,    -1,  1162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1172,    -1,    -1,    -1,    -1,
      -1,    -1,  1869,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2177,
      78,    79,    80,    81,    -1,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,    -1,   107,
    1237,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,   166,   167,
      -1,    -1,   170,   171,    -1,    -1,    -1,   175,    18,   177,
     178,   179,   180,   181,   182,   183,  1313,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,  1326,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,
      60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2049,    -1,    -1,    -1,    -1,    -1,    78,    79,
      80,    81,    -1,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,    -1,   107,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1441,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
     170,   171,    -1,  1460,  1461,   175,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,    -1,    13,    14,    15,
      16,    17,   192,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1534,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1568,  1569,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1597,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,    81,
    1607,    83,    -1,    -1,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   170,   107,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1684,    -1,    -1,
      -1,    -1,  1689,    -1,    -1,   167,    -1,  1694,   170,   171,
      -1,    -1,    -1,   175,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1725,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,  1751,    -1,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,  1799,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1830,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,   169,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
     192,    78,  1929,    80,    81,    -1,    83,    -1,    -1,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,    -1,
     107,  1958,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,    -1,
      -1,  2038,    -1,    -1,  2041,   192,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   192,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   192,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,   111,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
     127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,    -1,   192,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
      -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,
     175,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   190,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   190,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,
     180,   181,   182,   183,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,   172,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    79,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    -1,    -1,    76,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,     1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,   114,   115,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,     1,    -1,
     170,   171,    -1,    -1,   114,   115,    -1,    -1,   178,   179,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
     170,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,   114,   115,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,    -1,     1,    -1,   170,   171,    -1,
      -1,   114,   115,    -1,    -1,   178,   179,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,   170,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,    -1,     1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,     1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,    -1,     1,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   190,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   190,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,    -1,    -1,    -1,   175,    -1,    -1,   178,   179,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     190,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,     3,    -1,     5,
      -1,    -1,   178,   179,    10,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,   114,   115,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,   114,   115,    -1,    -1,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,   169,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,   114,   115,    -1,    57,    -1,    59,
      18,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
     170,   171,    -1,    -1,   114,   115,    -1,    -1,   178,   179,
      78,    -1,    80,    81,    -1,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    -1,
      -1,    99,   100,   101,   102,   103,   104,   105,    -1,   107,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     170,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,   192,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    80,    81,    -1,    83,    -1,    -1,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    -1,
      -1,    99,   100,   101,   102,   103,   104,   105,    -1,   107,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   192,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,   170,    57,    -1,    59,    -1,   175,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,   170,    57,    -1,    59,
      -1,   175,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,   175,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    96,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
     177,   178,   179,   180,   181,   182,   183,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,
      -1,   177,   178,   179,   180,   181,   182,   183,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,   128,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,   128,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,
      -1,   175,    -1,   177,   178,   179,   180,   181,   182,   183,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,   128,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,   180,
     181,   182,   183,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,
     170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,
     180,   181,   182,   183,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,   114,   115,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   112,    -1,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   169,   170,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   169,   170,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     170,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   114,   115,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,
     170,   171,    -1,   114,   115,    -1,    -1,    -1,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
     114,   115,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,   114,
     115,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   114,   115,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   170,   171,    -1,   114,   115,    -1,    -1,    -1,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,   114,   115,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,
      -1,   114,   115,    -1,    -1,    -1,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   114,   115,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,   114,   115,    -1,
      -1,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   114,   115,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,   171,    -1,   114,   115,    -1,    -1,    -1,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,   114,
     115,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,   114,   115,    -1,    57,   170,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,
      -1,   114,   115,    -1,    -1,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   114,   115,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,    -1,   114,   115,    -1,
      -1,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   114,   115,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,   171,    -1,   114,   115,    -1,    -1,    -1,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
      -1,    -1,    13,    14,    15,    16,    17,   178,   179,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
     171,    -1,    13,    14,    15,    16,    17,   178,   179,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   170,
      -1,   129,   130,   131,    -1,   133,   134,   178,   179,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,   177,   178,   179,   180,   181,   182,   183,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   170,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,   170,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    79,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,   143,    -1,
      55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,   143,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    55,    -1,    -1,    58,   141,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,    -1,    -1,   160,   161,    80,    -1,    -1,
      -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,    -1,
     173,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,   175,
      -1,   177,   178,   179,   180,   181,   182,   183,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,
     172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,
     175,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,    -1,    -1,   174,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,   172,    -1,
      -1,    -1,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,   175,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,
      -1,   177,   178,   179,   180,   181,   182,   183,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    -1,   133,   134,
      -1,    -1,    55,    -1,    -1,    58,   141,    60,    61,    -1,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,   177,   178,   179,   180,   181,   182,   183,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
     168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,    55,   133,   134,    58,
      -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,   177,   178,   179,   180,   181,   182,   183,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    55,   133,   134,    58,    -1,    60,    61,
      -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    55,   133,   134,    58,    -1,    60,    61,    -1,    63,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,    55,   133,
     134,    58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,   182,
     183,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,   112,
      -1,   114,   115,    57,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    79,   128,   143,   420,   422,   437,   438,   439,   167,
      13,    94,   112,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   197,   198,   200,   440,   441,   442,     0,
     437,   194,   439,   167,   440,   172,   173,   167,   194,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    20,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    57,    59,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    76,    80,
     107,   111,   112,   114,   115,   127,   141,   167,   168,   170,
     171,   178,   179,   190,   192,   198,   199,   212,   306,   307,
     308,   309,   310,   311,   312,   313,   314,   315,   316,   317,
     320,   323,   325,   326,   327,   328,   329,   330,   331,   332,
     333,   334,   336,   338,   339,   340,   342,   343,   347,   348,
     349,   350,   351,   353,   359,   360,   361,   362,   374,   379,
     412,   415,   425,   431,   433,   443,   448,   449,   450,   451,
     452,   453,   454,   455,   481,   499,   500,   501,   502,   440,
     169,   441,    55,    58,    60,    61,    63,    80,   109,   110,
     112,   113,   124,   125,   126,   129,   130,   131,   133,   134,
     167,   171,   177,   180,   181,   182,   183,   190,   196,   197,
     201,   202,   203,   206,   211,   212,   213,   214,   217,   218,
     219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
     230,   231,   232,   233,   238,   349,   425,   198,   437,   126,
     167,    65,    68,    69,    71,   167,   167,   377,   437,   335,
     336,   200,   413,   414,   200,   425,   167,   167,     4,   112,
     114,   115,   327,   329,   332,   333,   167,   212,   438,   443,
     449,   450,   451,   453,   454,   455,   114,   350,    64,   171,
     172,   178,   212,   233,   312,   313,   322,   324,   326,   330,
     331,   338,   339,   345,   346,   347,   348,   349,   352,   359,
     360,   379,   386,   387,   388,   389,   390,   391,   481,   494,
     495,   496,   497,   502,   503,   198,   171,   313,   323,   326,
     339,   343,   348,   438,   448,   452,   481,   498,   499,   502,
     503,   192,   192,   195,   164,   175,   191,   236,   395,    95,
     173,   432,   107,   200,   436,   173,   173,   173,   192,   114,
     115,   167,   212,   318,   319,   443,   444,   445,   446,   447,
     448,   452,   456,   457,   458,   459,   460,   461,   462,   463,
     464,   470,     3,    53,    54,    56,    62,   341,     3,   171,
     212,   312,   313,   327,   331,   333,   344,   349,   428,   448,
     452,   502,   437,    76,   310,   312,   326,   339,   343,   348,
     429,   448,   452,    72,   332,   437,   332,   327,   333,   437,
     321,   332,   333,   341,   360,   327,   332,   327,   437,   170,
     437,   173,   195,   167,   175,   244,   437,   437,     3,   301,
     302,   317,   320,   326,   330,   171,   212,   323,   326,   500,
     502,   200,   200,   169,   167,   211,   167,   167,   211,   167,
     167,   211,   214,   167,   211,   167,   112,   114,   115,   327,
     332,   333,   167,   211,   211,    19,    21,    92,   171,   180,
     181,   212,   215,   216,   233,   240,   244,   323,   326,   362,
     393,   498,   502,   168,   173,   233,   167,   203,   198,   171,
     176,   171,   176,   126,   130,   132,   133,   134,   167,   170,
     171,   175,   176,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,   155,   191,   235,   236,   237,   214,   214,
     184,   178,   185,   186,   180,   181,   135,   136,   137,   138,
     187,   188,   139,   140,   179,   177,   189,   141,   142,   190,
     169,   173,   170,   195,   191,   310,   312,   323,   326,   426,
     427,    64,    72,    73,    74,    75,   171,   189,   200,   401,
     403,   407,   409,   410,   349,   169,   171,   212,   322,   326,
     339,   346,   348,   391,   494,   502,   437,   114,   115,   182,
     198,   349,   378,   470,   167,   408,   409,   167,   201,   228,
     229,   169,   171,   233,   393,   394,   411,   438,   503,   326,
     438,   449,   450,   451,   453,   454,   455,   169,   169,   169,
     169,   169,   169,   169,   437,   172,   233,   326,   330,   114,
     171,   198,   323,   326,   481,   500,   172,   171,   324,   326,
     339,   346,   348,   438,   493,   494,   502,   503,   172,   167,
     167,   171,   179,   191,   212,   443,   465,   466,   467,   468,
     469,   470,   471,   472,   473,   481,   483,   484,   485,   486,
     487,   488,   505,   144,   171,   212,   352,   496,   502,   326,
     346,   332,   327,   437,   437,   172,   173,   172,   173,   324,
     326,   495,   502,   200,   171,   324,   481,   495,   502,   167,
     200,   172,   171,   448,   452,   502,   437,   323,   326,   448,
     452,   171,   173,   112,   170,   171,   175,   197,   199,   233,
     396,   397,   398,   399,   400,    22,   396,   167,   200,   244,
     167,   167,   437,   437,   198,   438,   443,   445,   446,   447,
     456,   458,   459,   460,   462,   463,   464,   326,   438,   444,
     457,   461,   173,   436,   171,   437,   478,   481,   436,   437,
     437,   432,   301,   167,   437,   478,   436,   437,   437,   432,
     437,   437,   167,   212,   326,   434,   443,   444,   448,   457,
     461,   167,   167,   325,   326,   438,   323,   171,   172,   323,
     498,   503,   436,   437,   351,   175,   432,   301,   200,   200,
     395,   312,   331,   430,   448,   452,   437,   175,   432,   301,
     413,   437,   326,   339,   437,   326,   326,   437,   114,   350,
     114,   115,   198,   349,   354,   413,   144,   198,   326,   383,
     384,   388,   389,   392,   438,   166,   194,   437,   244,   317,
     192,   448,   461,   326,   178,   233,   387,   438,   212,   502,
     200,   436,   167,   436,   169,   393,   438,   503,   203,   393,
     171,   393,   394,   393,   503,   233,   393,   169,   393,   393,
     393,   172,   169,   180,   181,   216,    18,   328,   169,   173,
     169,   167,   212,   474,   475,   476,   477,   478,   178,   179,
     169,   173,   504,   172,   173,   175,   191,   233,   198,   233,
     198,   124,   171,   198,   230,   124,   171,   200,   362,   233,
     230,   198,   175,   233,   214,   217,   217,   217,   218,   218,
     219,   219,   220,   220,   220,   220,   221,   221,   222,   223,
     224,   225,   226,   174,   240,   232,   171,   198,   233,   434,
     171,   313,   423,   175,   167,   200,   175,   200,   144,   178,
     179,   406,   169,   173,   200,   410,   169,   172,   167,   179,
     212,   502,   169,   198,   378,   470,   437,   175,   437,   401,
     191,   401,   169,   169,   173,   169,   173,   393,   503,   169,
     169,   169,   169,   169,   169,   167,   437,   478,   481,   167,
     478,   481,   198,   167,   324,   481,   495,   502,   171,   178,
     212,   233,   349,   233,   326,   167,   167,   323,   500,   502,
     324,   326,   194,   167,   383,   443,   466,   467,   468,   471,
     484,   485,   486,   172,   194,    18,   233,   326,   437,   438,
     465,   469,   483,   167,   437,   487,   505,   437,   437,   505,
     167,   437,   487,   437,   437,   505,   437,   437,   481,   172,
     229,   172,   326,   324,   493,   503,   200,   326,   198,   387,
     390,   390,   391,   505,   324,   495,   502,   194,   505,   194,
     171,   199,   228,   229,   435,   397,   174,   173,   504,   396,
     170,   171,   191,   400,   411,   167,   201,   194,   198,   434,
     191,   443,   445,   446,   447,   456,   458,   459,   460,   462,
     463,   464,   169,   169,   169,   169,   169,   169,   169,   169,
     169,   169,   444,   457,   461,   444,   457,   461,   437,   191,
     172,   233,   333,   349,   479,   395,   244,   432,   383,   395,
     244,   438,   443,   326,   438,   434,   167,   240,   394,   240,
     394,   434,   114,   423,   244,   432,   175,   175,   432,   301,
     423,   244,   432,   437,   437,   437,   175,   169,   173,   169,
     173,   388,   389,    77,   303,   304,   192,   172,   172,   173,
     200,   436,   194,   169,   393,   169,   173,   169,   173,   169,
     169,   169,   173,   169,   214,   169,   169,   169,   214,    18,
     328,   233,   383,   475,   476,   477,   326,   437,   438,   474,
     437,   437,   169,   169,   168,   175,   214,   168,   233,   239,
     172,   172,   239,   230,   233,   172,   172,   124,   129,   131,
     199,   207,   208,   209,   169,   207,   172,   173,   166,   397,
     228,   174,   207,   387,   425,   423,   200,   172,     1,   310,
     312,   324,   326,   416,   417,   418,   419,   167,   405,   403,
     404,    85,   337,    18,   326,   437,   175,   437,   375,    10,
     177,   378,   380,   381,   378,   169,   394,   169,   192,   201,
     233,   394,   167,   437,   478,   481,   167,   478,   481,   383,
     383,   144,   385,   386,   387,   324,   495,   502,   172,   172,
     172,   233,   194,   194,   385,   471,   169,   169,   169,   169,
     169,   169,   169,   169,     5,   326,   167,   437,   443,   470,
     465,   469,   483,   167,   179,   212,   465,   469,   383,   383,
     172,   505,   172,   173,   385,   200,   207,   144,   172,   183,
     172,   504,   396,   398,   166,   169,   194,   169,   385,   233,
     169,   169,   169,   169,   169,   169,   169,   169,   169,   167,
     437,   478,   481,   167,   437,   478,   481,   167,   437,   478,
     481,   434,    22,   481,   157,   173,   183,   480,   172,   173,
     244,   169,   443,   169,   169,   169,   169,   421,   422,   244,
     166,   416,   423,   244,   432,   421,   244,   356,   357,   355,
     363,   144,   437,   198,   200,   305,   245,   246,   437,    77,
     436,   385,   169,   322,   198,    85,   204,   205,   393,   214,
     214,   214,   169,   169,   169,   169,   474,   474,   214,   214,
     175,   397,   173,   504,   504,   166,   210,   171,   208,   210,
     210,   172,   173,   132,   170,   172,   239,   504,   228,   172,
     424,   421,   169,   434,   166,   417,   173,   192,   173,   192,
     411,   191,   402,   402,   376,   380,   378,   378,   349,   173,
     504,   200,   175,   175,   169,   383,   383,   169,   169,   169,
     173,   173,   172,   385,   385,   195,   169,   167,   437,   478,
     481,   167,   437,   487,   167,   437,   487,   481,   325,     5,
     178,   195,   233,   443,   437,   437,   167,    18,   326,   438,
     169,   169,   390,   195,   395,   172,   229,   229,   166,   396,
     437,   385,   437,   195,   167,   437,   478,   481,   167,   437,
     478,   481,   167,   437,   478,   481,   383,   383,   383,   436,
     240,   233,   233,   333,   349,   424,   166,   421,   244,   424,
     175,   175,   175,   166,   437,   388,   389,   173,   192,   195,
     246,    18,    78,    80,    81,    83,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    99,   100,   101,
     102,   103,   104,   105,   107,   114,   115,   127,   167,   171,
     200,   240,   241,   242,   243,   244,   248,   249,   258,   265,
     266,   267,   268,   269,   274,   275,   278,   279,   280,   281,
     282,   283,   284,   290,   291,   292,   306,   326,   330,   433,
     305,   195,   482,   483,   169,   174,   169,   173,   174,   167,
     437,   478,   481,   397,   504,   168,   233,   172,   172,   130,
     207,   208,   171,   208,   171,   208,   172,   173,   166,   424,
     200,   200,   434,   169,   394,   408,   408,   378,   504,   175,
     175,    10,   381,   166,   191,   382,   380,   166,   416,   169,
     169,   144,   387,   144,   195,   195,   169,   383,   383,   383,
     233,   233,   195,   172,   195,   169,   172,   195,   169,   383,
     383,   383,   169,   169,   169,   395,   172,   480,   166,   424,
     166,   363,   363,   363,   167,   358,     3,     5,    10,    80,
     307,   314,   315,   323,   326,   364,   370,   498,   200,   166,
     167,    68,    69,   192,   244,   306,   433,   167,   167,    18,
     242,   167,   167,   192,   200,   192,   200,   178,   200,   175,
     241,   167,    85,   192,   200,   167,   167,   242,   167,   244,
     233,   234,   234,    14,   293,   269,   280,   174,   192,    97,
      98,   273,   277,   118,   142,   272,   117,   141,   276,   272,
     392,   326,   192,   169,   169,   233,   205,   233,   383,   504,
     166,   172,   207,   207,   166,   406,   175,   166,   380,   380,
     349,   200,   437,   175,   229,   504,   166,   169,   169,   169,
     169,   169,   195,   195,   172,   172,   169,   437,   169,   169,
     169,   233,   166,   166,   166,   166,   411,   437,   323,   437,
     323,   370,   192,   192,   192,   167,   174,   212,   365,   366,
     367,   373,   443,   444,   457,   461,   173,   192,   200,   230,
     192,   244,   192,   244,   240,   250,   306,   308,   311,   317,
     326,   330,   240,    87,   169,   250,   156,   157,   160,   161,
     168,   169,   192,   240,   259,   260,   262,   306,   192,   192,
     240,   192,   397,   192,   240,   192,   192,   411,   240,   259,
     119,   120,   121,   122,   123,   285,   287,   288,   192,   106,
     192,    91,   167,   169,   437,   167,   167,   242,   242,   269,
     167,   279,   269,   279,   244,   437,   169,   166,   172,   172,
     402,   380,   437,   504,   504,   382,   397,   166,   437,   437,
     172,   172,   358,   358,   358,   169,   365,   323,   362,   371,
     498,   365,   192,   438,   443,   233,   326,   438,   166,   173,
     192,   372,   373,   372,   372,   200,   169,   169,   240,   326,
     169,   167,   242,   169,   183,   192,   262,   263,   242,   241,
     192,   263,   169,   174,   240,   168,   240,   241,   262,   192,
     504,   169,   169,   169,   169,   244,   287,   288,   167,   233,
     167,   201,     1,   242,   214,   270,   240,    82,   116,   271,
     273,    82,   408,   504,   166,   166,   504,   437,   437,   437,
     437,   192,   167,   212,   368,   369,   478,   489,   490,   491,
     492,   192,   173,   192,   192,   443,   437,   242,   242,    84,
      85,   175,   253,   254,   255,   169,   240,    82,   242,   240,
     168,   240,    82,   192,   168,   240,   241,   262,   326,   348,
     168,   240,   242,   260,   263,   158,   159,   162,   163,   263,
     264,   192,   240,   166,   175,   255,   242,   242,   167,   289,
     324,   326,   498,   192,   201,   169,   174,   169,   173,   174,
     169,   242,   167,   242,   242,   242,   166,   437,   437,   166,
     490,   491,   492,   326,   437,   489,   173,   192,   437,   437,
     367,    82,     1,   229,   251,   252,   435,     1,   174,     1,
     194,   242,   253,    82,   192,   169,   242,    82,   192,   183,
     183,   242,   241,   263,   263,   264,   192,    64,   240,   261,
     349,   183,   183,    82,   168,   240,   168,   240,   240,   241,
     192,     1,   194,   289,   192,   286,   167,   212,   434,   489,
     198,   174,   192,   171,   201,   294,   295,   296,   214,   230,
     240,   272,   437,   169,   169,   169,   489,   437,   242,   144,
       1,   173,   174,   166,   299,   300,   437,   242,    82,   192,
     242,   240,   168,   168,   240,   168,   240,   168,   240,   240,
     241,   198,   349,   168,   240,   168,   240,   242,   183,   183,
     183,   183,   166,   299,   286,   228,   169,   326,   438,   174,
     112,   167,   169,   174,   173,   169,   169,    82,   268,   167,
     437,   478,   481,   369,   229,   251,   254,   256,   257,   306,
     306,   242,   183,   183,   183,   183,   168,   168,   240,   168,
     240,   168,   240,   256,   169,   244,   294,   172,   229,   192,
     294,   296,   242,    82,   383,   247,   437,   195,   254,   168,
     168,   240,   168,   240,   168,   240,   195,   244,   174,   201,
     169,   169,   174,   242,   169,     1,   437,   242,   166,   247,
     166,   201,   297,   167,   192,   297,   242,   173,   174,   229,
     169,   201,   200,   298,   169,   192,   169,   173,   192,   200
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   193,   194,   195,   196,   196,   196,   196,   196,   197,
     197,   197,   197,   197,   197,   197,   197,   198,   198,   199,
     199,   200,   200,   200,   201,   202,   202,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   204,   204,   205,   205,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     207,   207,   208,   208,   208,   208,   208,   208,   208,   209,
     209,   209,   210,   210,   211,   211,   211,   211,   211,   211,
     211,   211,   211,   211,   211,   211,   211,   211,   211,   211,
     211,   211,   211,   212,   212,   212,   213,   213,   213,   213,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   215,
     215,   215,   215,   216,   216,   217,   217,   218,   218,   218,
     218,   219,   219,   219,   220,   220,   220,   221,   221,   221,
     221,   221,   222,   222,   222,   223,   223,   224,   224,   225,
     225,   226,   226,   227,   227,   228,   228,   228,   229,   230,
     230,   231,   231,   232,   232,   232,   233,   233,   233,   234,
     234,   235,   235,   236,   236,   237,   237,   237,   237,   237,
     237,   237,   237,   237,   237,   237,   238,   238,   238,   238,
     238,   239,   239,   239,   239,   240,   240,   241,   241,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   243,   243,   244,   244,   245,
     245,   246,   246,   246,   246,   246,   247,   247,   247,   248,
     249,   249,   249,   249,   249,   249,   249,   249,   250,   250,
     250,   250,   251,   251,   251,   252,   252,   253,   253,   253,
     253,   253,   254,   254,   255,   256,   256,   257,   257,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   259,   259,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     261,   261,   261,   262,   262,   262,   262,   263,   263,   264,
     264,   264,   264,   265,   265,   265,   265,   265,   265,   265,
     265,   265,   265,   265,   265,   265,   265,   265,   265,   265,
     265,   265,   265,   266,   267,   268,   269,   269,   270,   270,
     271,   272,   272,   273,   273,   274,   274,   274,   274,   274,
     274,   275,   276,   276,   277,   278,   278,   279,   279,   280,
     280,   280,   281,   282,   283,   284,   284,   284,   285,   285,
     286,   286,   287,   287,   287,   287,   288,   289,   289,   289,
     289,   289,   290,   291,   291,   292,   292,   292,   292,   292,
     293,   293,   294,   294,   295,   295,   296,   296,   297,   297,
     297,   298,   298,   299,   299,   300,   300,   301,   301,   302,
     302,   303,   303,   304,   304,   305,   305,   306,   306,   306,
     307,   307,   308,   308,   308,   308,   308,   309,   309,   309,
     310,   310,   310,   310,   310,   310,   311,   311,   311,   311,
     311,   312,   312,   312,   312,   313,   313,   314,   314,   314,
     315,   315,   315,   315,   315,   316,   316,   317,   317,   317,
     317,   318,   318,   318,   318,   318,   319,   319,   320,   320,
     320,   320,   321,   321,   321,   322,   322,   322,   323,   323,
     323,   324,   324,   324,   325,   325,   326,   326,   327,   328,
     328,   328,   328,   328,   329,   330,   330,   330,   331,   331,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   333,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   334,   335,   335,   336,   337,   337,   338,
     338,   338,   338,   338,   339,   339,   339,   340,   340,   340,
     340,   341,   341,   341,   341,   341,   341,   342,   342,   342,
     342,   343,   344,   343,   343,   345,   345,   345,   345,   346,
     346,   346,   347,   347,   347,   347,   348,   348,   348,   349,
     349,   349,   349,   349,   349,   350,   350,   350,   351,   351,
     352,   352,   354,   353,   355,   353,   356,   353,   357,   353,
     353,   358,   358,   359,   359,   360,   360,   361,   361,   361,
     362,   362,   362,   362,   362,   362,   362,   362,   363,   363,
     364,   364,   364,   364,   364,   364,   364,   364,   364,   364,
     364,   364,   365,   365,   366,   366,   367,   367,   367,   367,
     368,   368,   368,   369,   370,   370,   371,   371,   372,   372,
     373,   374,   374,   375,   374,   374,   376,   374,   374,   374,
     377,   377,   378,   378,   379,   379,   380,   380,   380,   380,
     380,   381,   381,   382,   382,   382,   383,   383,   383,   383,
     384,   384,   384,   384,   384,   384,   385,   385,   385,   385,
     385,   385,   385,   386,   386,   386,   386,   387,   387,   388,
     388,   389,   389,   390,   390,   390,   390,   390,   391,   391,
     391,   391,   391,   392,   392,   393,   393,   393,   394,   394,
     394,   394,   395,   395,   395,   395,   396,   396,   397,   397,
     397,   397,   397,   398,   398,   399,   399,   400,   400,   400,
     400,   400,   401,   401,   402,   402,   404,   403,   405,   403,
     403,   403,   403,   406,   406,   406,   406,   407,   407,   407,
     407,   408,   408,   409,   409,   410,   410,   411,   411,   411,
     411,   412,   412,   412,   413,   413,   414,   414,   415,   415,
     415,   415,   416,   416,   417,   417,   418,   418,   418,   419,
     419,   419,   420,   420,   421,   421,   422,   422,   423,   424,
     425,   425,   425,   425,   425,   425,   425,   425,   425,   425,
     425,   426,   425,   427,   425,   428,   425,   429,   425,   430,
     425,   425,   431,   431,   431,   432,   432,   433,   433,   433,
     433,   433,   433,   433,   433,   433,   433,   434,   434,   434,
     434,   435,   436,   436,   437,   437,   438,   438,   439,   439,
     439,   439,   440,   440,   441,   441,   441,   442,   442,   442,
     443,   443,   443,   444,   444,   444,   444,   445,   445,   445,
     445,   445,   446,   446,   446,   446,   446,   446,   446,   447,
     447,   447,   447,   448,   448,   448,   449,   449,   449,   449,
     449,   450,   450,   450,   450,   450,   451,   451,   451,   451,
     451,   451,   452,   452,   452,   453,   453,   453,   453,   453,
     454,   454,   454,   454,   454,   455,   455,   455,   455,   455,
     455,   456,   456,   457,   457,   457,   457,   458,   458,   458,
     458,   458,   459,   459,   459,   459,   459,   459,   459,   460,
     460,   460,   460,   461,   461,   461,   462,   462,   462,   462,
     462,   463,   463,   463,   463,   463,   464,   464,   464,   464,
     464,   464,   465,   465,   465,   465,   465,   466,   466,   466,
     466,   467,   467,   467,   467,   468,   468,   468,   469,   469,
     469,   469,   469,   470,   470,   471,   471,   471,   471,   472,
     472,   473,   473,   474,   474,   474,   475,   475,   475,   475,
     475,   475,   476,   476,   476,   476,   477,   477,   477,   478,
     478,   478,   478,   478,   479,   479,   479,   479,   479,   479,
     480,   480,   481,   481,   481,   481,   482,   482,   483,   483,
     483,   483,   484,   484,   484,   484,   484,   485,   485,   485,
     485,   486,   486,   486,   487,   487,   487,   488,   488,   488,
     488,   488,   488,   489,   489,   489,   490,   490,   490,   490,
     490,   491,   491,   491,   491,   492,   492,   493,   493,   493,
     494,   494,   494,   495,   495,   495,   495,   495,   495,   495,
     496,   496,   496,   496,   496,   496,   496,   496,   496,   496,
     496,   496,   496,   496,   496,   497,   497,   497,   497,   498,
     498,   498,   499,   499,   500,   500,   500,   500,   500,   500,
     500,   501,   501,   501,   501,   501,   501,   502,   502,   502,
     503,   503,   503,   504,   504,   505,   505
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
       1,     3,     1,     2,     4,     3,     5,     3,     5,     2,
       2,     2,     0,     2,     1,     1,     1,     2,     2,     2,
       2,     2,     2,     4,     5,     2,     4,     4,     4,     6,
       4,     2,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     5,     5,     4,     5,     5,     5,     4,     2,
       2,     3,     3,     1,     1,     1,     3,     1,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
       3,     3,     1,     3,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     5,     4,     1,     0,
       1,     1,     3,     1,     4,     1,     1,     3,     6,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     4,     6,
       6,     1,     1,     3,     3,     1,     3,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     4,     2,     6,     1,
       2,     2,     3,     2,     3,     2,     2,     3,     2,     2,
       5,     7,     5,    10,     7,     5,    10,     7,     1,     1,
       1,     2,     1,     3,     1,     1,     3,     2,     3,     3,
       2,     2,     1,     2,     2,     0,     1,     2,     3,     4,
       6,     5,     7,     6,     7,     7,     8,     4,     6,     5,
       7,     1,     3,     4,     5,     4,     3,     5,     1,     2,
       3,     3,     3,     5,     5,     5,     5,     3,     5,     5,
       5,     3,     4,     5,     5,     5,     5,     5,     7,     7,
       7,     7,     7,     7,     7,     2,     3,     4,     4,     4,
       4,     6,     6,     6,     6,     6,     6,     6,     3,     4,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     2,     3,     3,     2,     3,
       2,     3,     3,     6,     2,     2,     3,     3,     3,     3,
       3,     3,     5,     5,     5,     4,     0,     1,     1,     3,
       4,     1,     1,     4,     6,     3,     5,     5,     5,     8,
       9,     1,     1,     1,     4,     3,     3,     1,     3,     1,
       3,     5,     1,     2,     5,     3,     3,     4,     6,     7,
       0,     2,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     1,     1,     6,     8,    10,    12,    14,
       0,     1,     0,     1,     1,     3,     4,     7,     0,     1,
       3,     1,     3,     0,     1,     2,     2,     0,     1,     2,
       3,     0,     1,     3,     4,     1,     3,     2,     2,     2,
       6,     4,     1,     1,     1,     1,     1,     2,     3,     6,
       3,     3,     4,     5,     2,     3,     1,     2,     2,     3,
       8,     9,     9,     8,     8,     3,     5,     3,     3,     4,
       4,     4,     4,     3,     4,     4,     5,     2,     1,     1,
       1,     3,     3,     2,     4,     6,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     0,     1,     2,     3,     1,     1,
       1,     1,     1,     1,     4,     1,     2,     3,     2,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     5,     0,     1,     1,
       2,     3,     3,     3,     2,     3,     3,     1,     2,     2,
       2,     4,     4,     4,     4,     1,     1,     1,     2,     2,
       3,     1,     0,     3,     2,     1,     2,     2,     3,     1,
       2,     2,     2,     3,     3,     3,     1,     2,     2,     1,
       2,     3,     1,     2,     3,     1,     3,     4,     1,     1,
       1,     1,     0,     8,     0,    10,     0,    10,     0,    10,
       1,     0,     3,     3,     3,     1,     1,     2,     1,     1,
       1,     2,     1,     2,     1,     2,     1,     2,     0,     3,
       3,     3,     4,     4,     5,     4,     2,     2,     3,     4,
       2,     2,     0,     1,     1,     4,     1,     2,     2,     2,
       0,     1,     4,     1,     2,     3,     1,     2,     0,     1,
       2,     8,     9,     0,    11,    10,     0,    12,    11,     1,
       2,     3,     0,     1,     3,     3,     0,     3,     2,     5,
       4,     1,     1,     0,     2,     5,     0,     1,     1,     3,
       1,     2,     1,     2,     4,     4,     0,     1,     1,     1,
       3,     3,     3,     1,     3,     3,     5,     1,     3,     3,
       3,     2,     3,     1,     3,     3,     4,     1,     1,     1,
       1,     2,     1,     1,     3,     1,     2,     1,     1,     2,
       1,     2,     0,     2,     2,     4,     1,     4,     0,     1,
       2,     3,     4,     2,     2,     1,     2,     2,     3,     3,
       5,     4,     1,     3,     0,     2,     0,     5,     0,     5,
       3,     1,     8,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     4,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,     5,
       8,     6,     1,     2,     2,     2,     1,     1,     3,     2,
       3,     1,     0,     1,     0,     1,     4,     5,     0,     0,
       1,     1,     2,     2,     2,     2,     2,     2,     1,     2,
       5,     0,     6,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     1,     2,     3,     0,     5,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     4,
       3,     1,     1,     3,     0,     1,     4,     1,     1,     1,
       1,     2,     3,     2,     1,     2,     2,     2,     3,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     6,
       7,     3,     4,     2,     1,     2,     4,     6,     7,     3,
       4,     2,     3,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     1,     1,     1,     4,     6,     7,     3,     4,
       2,     3,     3,     3,     4,     4,     5,     4,     5,     3,
       4,     1,     3,     2,     1,     2,     2,     2,     3,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     6,
       7,     3,     4,     2,     1,     2,     4,     6,     7,     3,
       4,     2,     3,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     2,     4,     1,     2,     2,     2,     3,     3,
       4,     2,     4,     4,     3,     4,     6,     3,     2,     4,
       1,     2,     2,     1,     1,     2,     3,     3,     4,     2,
       4,     4,     6,     1,     2,     2,     2,     2,     2,     3,
       3,     4,     1,     4,     4,     3,     3,     6,     3,     2,
       3,     5,     3,     1,     1,     1,     3,     3,     3,     5,
       1,     1,     3,     3,     4,     4,     0,     1,     1,     3,
       2,     2,     2,     2,     2,     3,     4,     1,     4,     4,
       3,     3,     6,     3,     1,     2,     1,     2,     6,     5,
       6,     7,     7,     1,     2,     2,     2,     2,     2,     3,
       4,     1,     4,     4,     3,     6,     3,     1,     1,     2,
       1,     1,     2,     2,     3,     3,     2,     3,     2,     3,
       3,     3,     2,     2,     4,     4,     3,     3,     2,     2,
       3,     2,     4,     3,     2,     4,     4,     4,     5,     1,
       2,     1,     1,     1,     2,     3,     3,     2,     3,     2,
       3,     3,     4,     2,     3,     4,     2,     3,     4,     5,
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
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 9698 "Parser/parser.cc"
    break;

  case 3:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 9704 "Parser/parser.cc"
    break;

  case 4:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 5:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9716 "Parser/parser.cc"
    break;

  case 6:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9722 "Parser/parser.cc"
    break;

  case 7:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 9728 "Parser/parser.cc"
    break;

  case 8:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 20:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 9740 "Parser/parser.cc"
    break;

  case 24:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 9746 "Parser/parser.cc"
    break;

  case 25:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 9752 "Parser/parser.cc"
    break;

  case 26:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 9762 "Parser/parser.cc"
    break;

  case 27:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9768 "Parser/parser.cc"
    break;

  case 28:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 9774 "Parser/parser.cc"
    break;

  case 29:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 9780 "Parser/parser.cc"
    break;

  case 31:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9786 "Parser/parser.cc"
    break;

  case 32:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 9792 "Parser/parser.cc"
    break;

  case 33:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9798 "Parser/parser.cc"
    break;

  case 34:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9804 "Parser/parser.cc"
    break;

  case 35:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 9814 "Parser/parser.cc"
    break;

  case 36:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 9820 "Parser/parser.cc"
    break;

  case 37:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 9826 "Parser/parser.cc"
    break;

  case 38:
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 9832 "Parser/parser.cc"
    break;

  case 39:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9838 "Parser/parser.cc"
    break;

  case 40:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9844 "Parser/parser.cc"
    break;

  case 41:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 9850 "Parser/parser.cc"
    break;

  case 43:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 9862 "Parser/parser.cc"
    break;

  case 44:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 9871 "Parser/parser.cc"
    break;

  case 45:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 9877 "Parser/parser.cc"
    break;

  case 47:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) ) ); }
#line 9883 "Parser/parser.cc"
    break;

  case 48:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9889 "Parser/parser.cc"
    break;

  case 49:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9895 "Parser/parser.cc"
    break;

  case 50:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 51:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 9911 "Parser/parser.cc"
    break;

  case 52:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9917 "Parser/parser.cc"
    break;

  case 53:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 9923 "Parser/parser.cc"
    break;

  case 54:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9929 "Parser/parser.cc"
    break;

  case 55:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9935 "Parser/parser.cc"
    break;

  case 56:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 9941 "Parser/parser.cc"
    break;

  case 57:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 58:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 59:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9959 "Parser/parser.cc"
    break;

  case 60:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 61:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 9971 "Parser/parser.cc"
    break;

  case 62:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 9977 "Parser/parser.cc"
    break;

  case 63:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 9983 "Parser/parser.cc"
    break;

  case 64:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 9989 "Parser/parser.cc"
    break;

  case 65:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 9995 "Parser/parser.cc"
    break;

  case 66:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10001 "Parser/parser.cc"
    break;

  case 67:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10007 "Parser/parser.cc"
    break;

  case 68:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10013 "Parser/parser.cc"
    break;

  case 69:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10023 "Parser/parser.cc"
    break;

  case 71:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10029 "Parser/parser.cc"
    break;

  case 73:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 74:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10041 "Parser/parser.cc"
    break;

  case 75:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10047 "Parser/parser.cc"
    break;

  case 76:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10053 "Parser/parser.cc"
    break;

  case 77:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10059 "Parser/parser.cc"
    break;

  case 78:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10065 "Parser/parser.cc"
    break;

  case 79:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 80:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 81:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10085 "Parser/parser.cc"
    break;

  case 82:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10091 "Parser/parser.cc"
    break;

  case 83:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10100 "Parser/parser.cc"
    break;

  case 86:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10106 "Parser/parser.cc"
    break;

  case 87:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10112 "Parser/parser.cc"
    break;

  case 88:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10132 "Parser/parser.cc"
    break;

  case 89:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 90:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10144 "Parser/parser.cc"
    break;

  case 91:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10150 "Parser/parser.cc"
    break;

  case 92:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10156 "Parser/parser.cc"
    break;

  case 93:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10162 "Parser/parser.cc"
    break;

  case 94:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ) ) ) ); }
#line 10168 "Parser/parser.cc"
    break;

  case 95:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10174 "Parser/parser.cc"
    break;

  case 96:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10180 "Parser/parser.cc"
    break;

  case 97:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10186 "Parser/parser.cc"
    break;

  case 98:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 99:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 100:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10207 "Parser/parser.cc"
    break;

  case 101:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10213 "Parser/parser.cc"
    break;

  case 102:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10219 "Parser/parser.cc"
    break;

  case 103:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10225 "Parser/parser.cc"
    break;

  case 104:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10231 "Parser/parser.cc"
    break;

  case 105:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10237 "Parser/parser.cc"
    break;

  case 106:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10243 "Parser/parser.cc"
    break;

  case 107:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10249 "Parser/parser.cc"
    break;

  case 108:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10255 "Parser/parser.cc"
    break;

  case 109:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10261 "Parser/parser.cc"
    break;

  case 111:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10267 "Parser/parser.cc"
    break;

  case 112:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10273 "Parser/parser.cc"
    break;

  case 113:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10279 "Parser/parser.cc"
    break;

  case 114:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 115:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10291 "Parser/parser.cc"
    break;

  case 116:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 10297 "Parser/parser.cc"
    break;

  case 117:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10303 "Parser/parser.cc"
    break;

  case 118:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10309 "Parser/parser.cc"
    break;

  case 126:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10315 "Parser/parser.cc"
    break;

  case 128:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10321 "Parser/parser.cc"
    break;

  case 129:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 130:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 132:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 133:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10345 "Parser/parser.cc"
    break;

  case 135:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10351 "Parser/parser.cc"
    break;

  case 136:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10357 "Parser/parser.cc"
    break;

  case 138:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10363 "Parser/parser.cc"
    break;

  case 139:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 140:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 141:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 143:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 144:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 146:
#line 1071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10399 "Parser/parser.cc"
    break;

  case 148:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10405 "Parser/parser.cc"
    break;

  case 150:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10411 "Parser/parser.cc"
    break;

  case 152:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10417 "Parser/parser.cc"
    break;

  case 154:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10423 "Parser/parser.cc"
    break;

  case 156:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10429 "Parser/parser.cc"
    break;

  case 157:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 10435 "Parser/parser.cc"
    break;

  case 159:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10441 "Parser/parser.cc"
    break;

  case 162:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 163:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 164:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10459 "Parser/parser.cc"
    break;

  case 167:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 10471 "Parser/parser.cc"
    break;

  case 168:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10477 "Parser/parser.cc"
    break;

  case 169:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10483 "Parser/parser.cc"
    break;

  case 173:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 10489 "Parser/parser.cc"
    break;

  case 174:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 10495 "Parser/parser.cc"
    break;

  case 175:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 10501 "Parser/parser.cc"
    break;

  case 176:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 10507 "Parser/parser.cc"
    break;

  case 177:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 10513 "Parser/parser.cc"
    break;

  case 178:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 10519 "Parser/parser.cc"
    break;

  case 179:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 10525 "Parser/parser.cc"
    break;

  case 180:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 10531 "Parser/parser.cc"
    break;

  case 181:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 10537 "Parser/parser.cc"
    break;

  case 182:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 10543 "Parser/parser.cc"
    break;

  case 183:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 10549 "Parser/parser.cc"
    break;

  case 184:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 10555 "Parser/parser.cc"
    break;

  case 185:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 10561 "Parser/parser.cc"
    break;

  case 186:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 10567 "Parser/parser.cc"
    break;

  case 187:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 10573 "Parser/parser.cc"
    break;

  case 188:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10579 "Parser/parser.cc"
    break;

  case 189:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 10585 "Parser/parser.cc"
    break;

  case 190:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10591 "Parser/parser.cc"
    break;

  case 192:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10597 "Parser/parser.cc"
    break;

  case 193:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10603 "Parser/parser.cc"
    break;

  case 194:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10609 "Parser/parser.cc"
    break;

  case 196:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10615 "Parser/parser.cc"
    break;

  case 197:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10621 "Parser/parser.cc"
    break;

  case 212:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 10627 "Parser/parser.cc"
    break;

  case 214:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 215:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 10639 "Parser/parser.cc"
    break;

  case 216:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 10650 "Parser/parser.cc"
    break;

  case 217:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 218:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 220:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 10668 "Parser/parser.cc"
    break;

  case 221:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10674 "Parser/parser.cc"
    break;

  case 222:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 223:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 224:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 225:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 226:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 227:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-2].stmt) ); (yyvsp[-2].stmt)->set_last( (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ) ); (yyval.stmt) = (yyvsp[-2].stmt); }
#line 10710 "Parser/parser.cc"
    break;

  case 228:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 10717 "Parser/parser.cc"
    break;

  case 229:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 230:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 231:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 232:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 233:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10755 "Parser/parser.cc"
    break;

  case 234:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10761 "Parser/parser.cc"
    break;

  case 235:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 236:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 10776 "Parser/parser.cc"
    break;

  case 237:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 10782 "Parser/parser.cc"
    break;

  case 238:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 239:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10794 "Parser/parser.cc"
    break;

  case 240:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 10800 "Parser/parser.cc"
    break;

  case 241:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 242:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 10812 "Parser/parser.cc"
    break;

  case 243:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 245:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 246:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 247:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 10836 "Parser/parser.cc"
    break;

  case 248:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 10842 "Parser/parser.cc"
    break;

  case 249:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 10848 "Parser/parser.cc"
    break;

  case 250:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 251:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 10860 "Parser/parser.cc"
    break;

  case 253:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 254:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 255:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 10878 "Parser/parser.cc"
    break;

  case 257:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 258:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 259:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 260:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10905 "Parser/parser.cc"
    break;

  case 261:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10911 "Parser/parser.cc"
    break;

  case 262:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10917 "Parser/parser.cc"
    break;

  case 263:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 10923 "Parser/parser.cc"
    break;

  case 264:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10932 "Parser/parser.cc"
    break;

  case 265:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 266:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 267:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 268:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 10959 "Parser/parser.cc"
    break;

  case 269:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 10965 "Parser/parser.cc"
    break;

  case 270:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 10971 "Parser/parser.cc"
    break;

  case 272:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].forctrl)->init->set_last( (yyvsp[0].forctrl)->init );
			if ( (yyvsp[-2].forctrl)->condition ) {
				if ( (yyvsp[0].forctrl)->condition ) {
					(yyvsp[-2].forctrl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].forctrl)->condition->expr.release(), (yyvsp[0].forctrl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].forctrl)->condition = (yyvsp[0].forctrl)->condition;
			if ( (yyvsp[-2].forctrl)->change ) {
				if ( (yyvsp[0].forctrl)->change ) {
					(yyvsp[-2].forctrl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].forctrl)->change->expr.release(), (yyvsp[0].forctrl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].forctrl)->change = (yyvsp[0].forctrl)->change;
			(yyval.forctrl) = (yyvsp[-2].forctrl);
		}
#line 10990 "Parser/parser.cc"
    break;

  case 273:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 10996 "Parser/parser.cc"
    break;

  case 274:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11004 "Parser/parser.cc"
    break;

  case 275:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 276:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11016 "Parser/parser.cc"
    break;

  case 277:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 278:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11028 "Parser/parser.cc"
    break;

  case 279:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11034 "Parser/parser.cc"
    break;

  case 280:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11040 "Parser/parser.cc"
    break;

  case 281:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11049 "Parser/parser.cc"
    break;

  case 282:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11058 "Parser/parser.cc"
    break;

  case 283:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 284:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11073 "Parser/parser.cc"
    break;

  case 285:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11082 "Parser/parser.cc"
    break;

  case 286:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11088 "Parser/parser.cc"
    break;

  case 287:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11094 "Parser/parser.cc"
    break;

  case 288:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11100 "Parser/parser.cc"
    break;

  case 289:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11106 "Parser/parser.cc"
    break;

  case 290:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11112 "Parser/parser.cc"
    break;

  case 291:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11118 "Parser/parser.cc"
    break;

  case 292:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11124 "Parser/parser.cc"
    break;

  case 293:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11130 "Parser/parser.cc"
    break;

  case 294:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11139 "Parser/parser.cc"
    break;

  case 295:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11149 "Parser/parser.cc"
    break;

  case 296:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11155 "Parser/parser.cc"
    break;

  case 297:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11161 "Parser/parser.cc"
    break;

  case 298:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 299:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11176 "Parser/parser.cc"
    break;

  case 300:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11186 "Parser/parser.cc"
    break;

  case 301:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11192 "Parser/parser.cc"
    break;

  case 302:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11201 "Parser/parser.cc"
    break;

  case 303:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11211 "Parser/parser.cc"
    break;

  case 304:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11217 "Parser/parser.cc"
    break;

  case 305:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11223 "Parser/parser.cc"
    break;

  case 306:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11229 "Parser/parser.cc"
    break;

  case 307:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11235 "Parser/parser.cc"
    break;

  case 308:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11244 "Parser/parser.cc"
    break;

  case 309:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11254 "Parser/parser.cc"
    break;

  case 310:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11260 "Parser/parser.cc"
    break;

  case 311:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 312:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11275 "Parser/parser.cc"
    break;

  case 313:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11285 "Parser/parser.cc"
    break;

  case 314:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11291 "Parser/parser.cc"
    break;

  case 315:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11300 "Parser/parser.cc"
    break;

  case 316:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11310 "Parser/parser.cc"
    break;

  case 317:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11316 "Parser/parser.cc"
    break;

  case 318:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11324 "Parser/parser.cc"
    break;

  case 319:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11336 "Parser/parser.cc"
    break;

  case 320:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11345 "Parser/parser.cc"
    break;

  case 321:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11354 "Parser/parser.cc"
    break;

  case 322:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11363 "Parser/parser.cc"
    break;

  case 323:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11369 "Parser/parser.cc"
    break;

  case 324:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11375 "Parser/parser.cc"
    break;

  case 325:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11381 "Parser/parser.cc"
    break;

  case 326:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11387 "Parser/parser.cc"
    break;

  case 327:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11393 "Parser/parser.cc"
    break;

  case 329:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 11399 "Parser/parser.cc"
    break;

  case 330:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 11405 "Parser/parser.cc"
    break;

  case 331:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 11411 "Parser/parser.cc"
    break;

  case 332:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 11417 "Parser/parser.cc"
    break;

  case 333:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11423 "Parser/parser.cc"
    break;

  case 334:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 11429 "Parser/parser.cc"
    break;

  case 335:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 11435 "Parser/parser.cc"
    break;

  case 336:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 11441 "Parser/parser.cc"
    break;

  case 337:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 11447 "Parser/parser.cc"
    break;

  case 338:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 11453 "Parser/parser.cc"
    break;

  case 339:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 11459 "Parser/parser.cc"
    break;

  case 340:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 11465 "Parser/parser.cc"
    break;

  case 341:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 11471 "Parser/parser.cc"
    break;

  case 342:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 11477 "Parser/parser.cc"
    break;

  case 343:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11483 "Parser/parser.cc"
    break;

  case 344:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 11489 "Parser/parser.cc"
    break;

  case 345:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 11495 "Parser/parser.cc"
    break;

  case 346:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 11501 "Parser/parser.cc"
    break;

  case 347:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 11507 "Parser/parser.cc"
    break;

  case 348:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 11513 "Parser/parser.cc"
    break;

  case 349:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 11519 "Parser/parser.cc"
    break;

  case 350:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 11525 "Parser/parser.cc"
    break;

  case 351:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 11531 "Parser/parser.cc"
    break;

  case 352:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 11537 "Parser/parser.cc"
    break;

  case 353:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11543 "Parser/parser.cc"
    break;

  case 354:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 11552 "Parser/parser.cc"
    break;

  case 355:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11558 "Parser/parser.cc"
    break;

  case 356:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11564 "Parser/parser.cc"
    break;

  case 359:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11570 "Parser/parser.cc"
    break;

  case 360:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11576 "Parser/parser.cc"
    break;

  case 363:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11582 "Parser/parser.cc"
    break;

  case 364:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 365:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 366:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 367:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 368:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 369:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 11618 "Parser/parser.cc"
    break;

  case 370:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11624 "Parser/parser.cc"
    break;

  case 371:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 374:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11636 "Parser/parser.cc"
    break;

  case 375:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11642 "Parser/parser.cc"
    break;

  case 376:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 11648 "Parser/parser.cc"
    break;

  case 377:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11654 "Parser/parser.cc"
    break;

  case 378:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 379:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 11666 "Parser/parser.cc"
    break;

  case 380:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 11672 "Parser/parser.cc"
    break;

  case 381:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11678 "Parser/parser.cc"
    break;

  case 382:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 11684 "Parser/parser.cc"
    break;

  case 383:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 11690 "Parser/parser.cc"
    break;

  case 384:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11696 "Parser/parser.cc"
    break;

  case 385:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 386:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 11708 "Parser/parser.cc"
    break;

  case 387:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 388:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 11720 "Parser/parser.cc"
    break;

  case 389:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 11726 "Parser/parser.cc"
    break;

  case 390:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11732 "Parser/parser.cc"
    break;

  case 391:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11738 "Parser/parser.cc"
    break;

  case 392:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11744 "Parser/parser.cc"
    break;

  case 393:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 11750 "Parser/parser.cc"
    break;

  case 394:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 11756 "Parser/parser.cc"
    break;

  case 395:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 11762 "Parser/parser.cc"
    break;

  case 396:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 11768 "Parser/parser.cc"
    break;

  case 398:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11774 "Parser/parser.cc"
    break;

  case 399:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11780 "Parser/parser.cc"
    break;

  case 400:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11786 "Parser/parser.cc"
    break;

  case 405:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 11792 "Parser/parser.cc"
    break;

  case 406:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11798 "Parser/parser.cc"
    break;

  case 407:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11804 "Parser/parser.cc"
    break;

  case 408:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 11810 "Parser/parser.cc"
    break;

  case 409:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 11816 "Parser/parser.cc"
    break;

  case 410:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 11822 "Parser/parser.cc"
    break;

  case 411:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 11828 "Parser/parser.cc"
    break;

  case 412:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11834 "Parser/parser.cc"
    break;

  case 415:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 416:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 11846 "Parser/parser.cc"
    break;

  case 417:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 11855 "Parser/parser.cc"
    break;

  case 418:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11861 "Parser/parser.cc"
    break;

  case 419:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11867 "Parser/parser.cc"
    break;

  case 420:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 421:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 11879 "Parser/parser.cc"
    break;

  case 422:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 11885 "Parser/parser.cc"
    break;

  case 423:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11891 "Parser/parser.cc"
    break;

  case 425:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 426:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 427:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11909 "Parser/parser.cc"
    break;

  case 429:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11915 "Parser/parser.cc"
    break;

  case 430:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 440:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 441:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 445:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11939 "Parser/parser.cc"
    break;

  case 447:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 448:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 449:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 450:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11963 "Parser/parser.cc"
    break;

  case 451:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 452:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 453:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 454:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11987 "Parser/parser.cc"
    break;

  case 455:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11993 "Parser/parser.cc"
    break;

  case 457:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 458:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 459:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 460:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12022 "Parser/parser.cc"
    break;

  case 461:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12028 "Parser/parser.cc"
    break;

  case 462:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12034 "Parser/parser.cc"
    break;

  case 463:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12040 "Parser/parser.cc"
    break;

  case 464:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12046 "Parser/parser.cc"
    break;

  case 465:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12052 "Parser/parser.cc"
    break;

  case 466:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12058 "Parser/parser.cc"
    break;

  case 467:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12067 "Parser/parser.cc"
    break;

  case 468:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12076 "Parser/parser.cc"
    break;

  case 469:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneType( (yyvsp[0].tok) )->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12085 "Parser/parser.cc"
    break;

  case 470:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef()->addQualifiers( (yyvsp[-2].decl) ); // watchout frees $3 and $4
		}
#line 12096 "Parser/parser.cc"
    break;

  case 471:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef()->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12105 "Parser/parser.cc"
    break;

  case 472:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12111 "Parser/parser.cc"
    break;

  case 473:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12117 "Parser/parser.cc"
    break;

  case 474:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12123 "Parser/parser.cc"
    break;

  case 475:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12129 "Parser/parser.cc"
    break;

  case 476:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12135 "Parser/parser.cc"
    break;

  case 477:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distTypeSpec( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12141 "Parser/parser.cc"
    break;

  case 480:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "illegal syntax, useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "illegal syntax, useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 12156 "Parser/parser.cc"
    break;

  case 481:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12162 "Parser/parser.cc"
    break;

  case 482:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12168 "Parser/parser.cc"
    break;

  case 483:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12174 "Parser/parser.cc"
    break;

  case 484:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12180 "Parser/parser.cc"
    break;

  case 485:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12186 "Parser/parser.cc"
    break;

  case 491:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12196 "Parser/parser.cc"
    break;

  case 504:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12202 "Parser/parser.cc"
    break;

  case 506:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12208 "Parser/parser.cc"
    break;

  case 507:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12214 "Parser/parser.cc"
    break;

  case 508:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 509:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12226 "Parser/parser.cc"
    break;

  case 510:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12232 "Parser/parser.cc"
    break;

  case 511:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12238 "Parser/parser.cc"
    break;

  case 512:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12244 "Parser/parser.cc"
    break;

  case 513:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12250 "Parser/parser.cc"
    break;

  case 514:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12256 "Parser/parser.cc"
    break;

  case 516:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12262 "Parser/parser.cc"
    break;

  case 517:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 518:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12274 "Parser/parser.cc"
    break;

  case 519:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12280 "Parser/parser.cc"
    break;

  case 520:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12286 "Parser/parser.cc"
    break;

  case 521:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12292 "Parser/parser.cc"
    break;

  case 522:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12298 "Parser/parser.cc"
    break;

  case 523:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12304 "Parser/parser.cc"
    break;

  case 524:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12310 "Parser/parser.cc"
    break;

  case 525:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12316 "Parser/parser.cc"
    break;

  case 526:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12322 "Parser/parser.cc"
    break;

  case 527:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12328 "Parser/parser.cc"
    break;

  case 528:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12334 "Parser/parser.cc"
    break;

  case 529:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12340 "Parser/parser.cc"
    break;

  case 530:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12346 "Parser/parser.cc"
    break;

  case 531:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12352 "Parser/parser.cc"
    break;

  case 532:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12358 "Parser/parser.cc"
    break;

  case 533:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12364 "Parser/parser.cc"
    break;

  case 534:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12370 "Parser/parser.cc"
    break;

  case 535:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 536:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12382 "Parser/parser.cc"
    break;

  case 537:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12388 "Parser/parser.cc"
    break;

  case 538:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12394 "Parser/parser.cc"
    break;

  case 539:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12400 "Parser/parser.cc"
    break;

  case 540:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12406 "Parser/parser.cc"
    break;

  case 541:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12412 "Parser/parser.cc"
    break;

  case 542:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12418 "Parser/parser.cc"
    break;

  case 543:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12424 "Parser/parser.cc"
    break;

  case 544:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 12430 "Parser/parser.cc"
    break;

  case 545:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 12436 "Parser/parser.cc"
    break;

  case 546:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 12442 "Parser/parser.cc"
    break;

  case 547:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 12448 "Parser/parser.cc"
    break;

  case 548:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 12454 "Parser/parser.cc"
    break;

  case 549:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 12460 "Parser/parser.cc"
    break;

  case 550:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 12466 "Parser/parser.cc"
    break;

  case 551:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 12472 "Parser/parser.cc"
    break;

  case 552:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12478 "Parser/parser.cc"
    break;

  case 553:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12484 "Parser/parser.cc"
    break;

  case 554:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12490 "Parser/parser.cc"
    break;

  case 555:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 12496 "Parser/parser.cc"
    break;

  case 556:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 12502 "Parser/parser.cc"
    break;

  case 557:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 12508 "Parser/parser.cc"
    break;

  case 558:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 12514 "Parser/parser.cc"
    break;

  case 559:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 12520 "Parser/parser.cc"
    break;

  case 560:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 12526 "Parser/parser.cc"
    break;

  case 561:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 12532 "Parser/parser.cc"
    break;

  case 562:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 12538 "Parser/parser.cc"
    break;

  case 564:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12544 "Parser/parser.cc"
    break;

  case 566:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 567:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 12556 "Parser/parser.cc"
    break;

  case 568:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 12562 "Parser/parser.cc"
    break;

  case 570:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12568 "Parser/parser.cc"
    break;

  case 571:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 572:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12580 "Parser/parser.cc"
    break;

  case 573:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 12586 "Parser/parser.cc"
    break;

  case 574:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12592 "Parser/parser.cc"
    break;

  case 575:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12598 "Parser/parser.cc"
    break;

  case 576:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12604 "Parser/parser.cc"
    break;

  case 578:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12610 "Parser/parser.cc"
    break;

  case 579:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12616 "Parser/parser.cc"
    break;

  case 580:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 12622 "Parser/parser.cc"
    break;

  case 581:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12628 "Parser/parser.cc"
    break;

  case 582:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 12634 "Parser/parser.cc"
    break;

  case 583:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 12640 "Parser/parser.cc"
    break;

  case 584:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 12646 "Parser/parser.cc"
    break;

  case 585:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 12652 "Parser/parser.cc"
    break;

  case 586:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 12658 "Parser/parser.cc"
    break;

  case 588:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12664 "Parser/parser.cc"
    break;

  case 589:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12670 "Parser/parser.cc"
    break;

  case 590:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12676 "Parser/parser.cc"
    break;

  case 592:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 12682 "Parser/parser.cc"
    break;

  case 593:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 594:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 12697 "Parser/parser.cc"
    break;

  case 596:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 597:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 598:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 600:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 601:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 602:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 603:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 604:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 605:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 606:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 607:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 608:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 609:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 610:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 611:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 613:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 614:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 615:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 12805 "Parser/parser.cc"
    break;

  case 616:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 12811 "Parser/parser.cc"
    break;

  case 617:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 622:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 12823 "Parser/parser.cc"
    break;

  case 623:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), nullptr, (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 624:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 12838 "Parser/parser.cc"
    break;

  case 625:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 12846 "Parser/parser.cc"
    break;

  case 626:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 12855 "Parser/parser.cc"
    break;

  case 627:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-7].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 12864 "Parser/parser.cc"
    break;

  case 628:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 12873 "Parser/parser.cc"
    break;

  case 629:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-7].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 12882 "Parser/parser.cc"
    break;

  case 631:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12888 "Parser/parser.cc"
    break;

  case 632:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12894 "Parser/parser.cc"
    break;

  case 633:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12904 "Parser/parser.cc"
    break;

  case 634:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12923 "Parser/parser.cc"
    break;

  case 637:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 12929 "Parser/parser.cc"
    break;

  case 638:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 12935 "Parser/parser.cc"
    break;

  case 639:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 12941 "Parser/parser.cc"
    break;

  case 640:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12947 "Parser/parser.cc"
    break;

  case 641:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 12953 "Parser/parser.cc"
    break;

  case 642:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 12959 "Parser/parser.cc"
    break;

  case 643:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12968 "Parser/parser.cc"
    break;

  case 644:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 12974 "Parser/parser.cc"
    break;

  case 645:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12983 "Parser/parser.cc"
    break;

  case 646:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 12989 "Parser/parser.cc"
    break;

  case 647:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 12998 "Parser/parser.cc"
    break;

  case 648:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13004 "Parser/parser.cc"
    break;

  case 649:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.decl) = (yyvsp[-2].decl) ? (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13010 "Parser/parser.cc"
    break;

  case 650:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 651:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13025 "Parser/parser.cc"
    break;

  case 652:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13031 "Parser/parser.cc"
    break;

  case 653:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13037 "Parser/parser.cc"
    break;

  case 654:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distTypeSpec( (yyvsp[-2].decl), (yyvsp[-1].decl) );				// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13050 "Parser/parser.cc"
    break;

  case 655:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13056 "Parser/parser.cc"
    break;

  case 658:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13062 "Parser/parser.cc"
    break;

  case 659:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13068 "Parser/parser.cc"
    break;

  case 662:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13074 "Parser/parser.cc"
    break;

  case 665:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13080 "Parser/parser.cc"
    break;

  case 666:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13086 "Parser/parser.cc"
    break;

  case 667:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13092 "Parser/parser.cc"
    break;

  case 668:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13098 "Parser/parser.cc"
    break;

  case 669:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13104 "Parser/parser.cc"
    break;

  case 670:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13110 "Parser/parser.cc"
    break;

  case 672:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13116 "Parser/parser.cc"
    break;

  case 674:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13122 "Parser/parser.cc"
    break;

  case 675:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13128 "Parser/parser.cc"
    break;

  case 677:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13134 "Parser/parser.cc"
    break;

  case 678:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13140 "Parser/parser.cc"
    break;

  case 680:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13146 "Parser/parser.cc"
    break;

  case 681:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, false )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13157 "Parser/parser.cc"
    break;

  case 682:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-7].decl) && ((yyvsp[-7].decl)->storageClasses.val != 0 || (yyvsp[-7].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, true, (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13171 "Parser/parser.cc"
    break;

  case 683:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 1" ); }
#line 13177 "Parser/parser.cc"
    break;

  case 684:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) ->addQualifiers( (yyvsp[-7].decl) ))->addQualifiers( (yyvsp[0].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 685:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 686:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13200 "Parser/parser.cc"
    break;

  case 687:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, true, (yyvsp[-10].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13206 "Parser/parser.cc"
    break;

  case 688:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, true, (yyvsp[-9].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13212 "Parser/parser.cc"
    break;

  case 690:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13218 "Parser/parser.cc"
    break;

  case 691:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13224 "Parser/parser.cc"
    break;

  case 692:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13230 "Parser/parser.cc"
    break;

  case 693:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13236 "Parser/parser.cc"
    break;

  case 694:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13245 "Parser/parser.cc"
    break;

  case 695:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13254 "Parser/parser.cc"
    break;

  case 696:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13260 "Parser/parser.cc"
    break;

  case 697:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13266 "Parser/parser.cc"
    break;

  case 698:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13276 "Parser/parser.cc"
    break;

  case 699:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13282 "Parser/parser.cc"
    break;

  case 700:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13288 "Parser/parser.cc"
    break;

  case 702:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13294 "Parser/parser.cc"
    break;

  case 703:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13300 "Parser/parser.cc"
    break;

  case 704:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13306 "Parser/parser.cc"
    break;

  case 705:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13312 "Parser/parser.cc"
    break;

  case 706:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13318 "Parser/parser.cc"
    break;

  case 707:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13324 "Parser/parser.cc"
    break;

  case 709:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13330 "Parser/parser.cc"
    break;

  case 711:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13336 "Parser/parser.cc"
    break;

  case 713:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13342 "Parser/parser.cc"
    break;

  case 714:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 13348 "Parser/parser.cc"
    break;

  case 715:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 13354 "Parser/parser.cc"
    break;

  case 716:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13360 "Parser/parser.cc"
    break;

  case 717:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13366 "Parser/parser.cc"
    break;

  case 720:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13372 "Parser/parser.cc"
    break;

  case 721:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13378 "Parser/parser.cc"
    break;

  case 722:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13384 "Parser/parser.cc"
    break;

  case 724:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13390 "Parser/parser.cc"
    break;

  case 725:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13396 "Parser/parser.cc"
    break;

  case 726:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13402 "Parser/parser.cc"
    break;

  case 728:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13408 "Parser/parser.cc"
    break;

  case 729:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13414 "Parser/parser.cc"
    break;

  case 730:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13420 "Parser/parser.cc"
    break;

  case 731:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13426 "Parser/parser.cc"
    break;

  case 732:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13432 "Parser/parser.cc"
    break;

  case 734:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13438 "Parser/parser.cc"
    break;

  case 735:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 13444 "Parser/parser.cc"
    break;

  case 736:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 13450 "Parser/parser.cc"
    break;

  case 741:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13456 "Parser/parser.cc"
    break;

  case 743:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 13462 "Parser/parser.cc"
    break;

  case 744:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 13468 "Parser/parser.cc"
    break;

  case 746:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13474 "Parser/parser.cc"
    break;

  case 749:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13480 "Parser/parser.cc"
    break;

  case 751:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13486 "Parser/parser.cc"
    break;

  case 752:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13492 "Parser/parser.cc"
    break;

  case 753:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 13498 "Parser/parser.cc"
    break;

  case 754:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 13504 "Parser/parser.cc"
    break;

  case 755:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13510 "Parser/parser.cc"
    break;

  case 756:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13516 "Parser/parser.cc"
    break;

  case 757:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13522 "Parser/parser.cc"
    break;

  case 758:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13528 "Parser/parser.cc"
    break;

  case 760:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 13534 "Parser/parser.cc"
    break;

  case 761:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 13540 "Parser/parser.cc"
    break;

  case 762:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 13546 "Parser/parser.cc"
    break;

  case 764:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 13552 "Parser/parser.cc"
    break;

  case 766:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 13558 "Parser/parser.cc"
    break;

  case 767:
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 13564 "Parser/parser.cc"
    break;

  case 768:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13570 "Parser/parser.cc"
    break;

  case 769:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13576 "Parser/parser.cc"
    break;

  case 770:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 13582 "Parser/parser.cc"
    break;

  case 771:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13588 "Parser/parser.cc"
    break;

  case 773:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13594 "Parser/parser.cc"
    break;

  case 774:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13600 "Parser/parser.cc"
    break;

  case 775:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13606 "Parser/parser.cc"
    break;

  case 776:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" ); }
#line 13612 "Parser/parser.cc"
    break;

  case 777:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13618 "Parser/parser.cc"
    break;

  case 778:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 13624 "Parser/parser.cc"
    break;

  case 779:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 13630 "Parser/parser.cc"
    break;

  case 780:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 13639 "Parser/parser.cc"
    break;

  case 781:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( "" ) )->addAssertions( (yyvsp[0].decl) ); }
#line 13645 "Parser/parser.cc"
    break;

  case 782:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 13655 "Parser/parser.cc"
    break;

  case 783:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 13661 "Parser/parser.cc"
    break;

  case 784:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 13667 "Parser/parser.cc"
    break;

  case 785:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 13673 "Parser/parser.cc"
    break;

  case 786:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 13679 "Parser/parser.cc"
    break;

  case 787:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
#line 13685 "Parser/parser.cc"
    break;

  case 788:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
#line 13691 "Parser/parser.cc"
    break;

  case 789:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 13697 "Parser/parser.cc"
    break;

  case 790:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
#line 13703 "Parser/parser.cc"
    break;

  case 791:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13709 "Parser/parser.cc"
    break;

  case 794:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13715 "Parser/parser.cc"
    break;

  case 795:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13721 "Parser/parser.cc"
    break;

  case 796:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13727 "Parser/parser.cc"
    break;

  case 797:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13733 "Parser/parser.cc"
    break;

  case 799:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 13739 "Parser/parser.cc"
    break;

  case 800:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 13745 "Parser/parser.cc"
    break;

  case 801:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 13751 "Parser/parser.cc"
    break;

  case 802:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13757 "Parser/parser.cc"
    break;

  case 803:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 13763 "Parser/parser.cc"
    break;

  case 804:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 13769 "Parser/parser.cc"
    break;

  case 805:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 13775 "Parser/parser.cc"
    break;

  case 806:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 13784 "Parser/parser.cc"
    break;

  case 807:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 13793 "Parser/parser.cc"
    break;

  case 808:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 13802 "Parser/parser.cc"
    break;

  case 809:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 13808 "Parser/parser.cc"
    break;

  case 810:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 13817 "Parser/parser.cc"
    break;

  case 811:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 13823 "Parser/parser.cc"
    break;

  case 813:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 13829 "Parser/parser.cc"
    break;

  case 818:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13835 "Parser/parser.cc"
    break;

  case 819:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 13841 "Parser/parser.cc"
    break;

  case 820:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 13847 "Parser/parser.cc"
    break;

  case 821:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 13853 "Parser/parser.cc"
    break;

  case 823:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13859 "Parser/parser.cc"
    break;

  case 824:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13865 "Parser/parser.cc"
    break;

  case 826:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13871 "Parser/parser.cc"
    break;

  case 827:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-4].decl) ? (yyvsp[-4].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl)->addQualifiers( (yyvsp[-3].decl) ); }
#line 13877 "Parser/parser.cc"
    break;

  case 828:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 13883 "Parser/parser.cc"
    break;

  case 829:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 13889 "Parser/parser.cc"
    break;

  case 830:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 13895 "Parser/parser.cc"
    break;

  case 831:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13911 "Parser/parser.cc"
    break;

  case 832:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 13917 "Parser/parser.cc"
    break;

  case 833:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 13923 "Parser/parser.cc"
    break;

  case 834:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 13929 "Parser/parser.cc"
    break;

  case 835:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13935 "Parser/parser.cc"
    break;

  case 836:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13941 "Parser/parser.cc"
    break;

  case 837:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 13947 "Parser/parser.cc"
    break;

  case 839:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 13956 "Parser/parser.cc"
    break;

  case 840:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 13962 "Parser/parser.cc"
    break;

  case 841:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13971 "Parser/parser.cc"
    break;

  case 842:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 13981 "Parser/parser.cc"
    break;

  case 843:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 13990 "Parser/parser.cc"
    break;

  case 844:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14000 "Parser/parser.cc"
    break;

  case 845:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14011 "Parser/parser.cc"
    break;

  case 846:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14021 "Parser/parser.cc"
    break;

  case 847:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14032 "Parser/parser.cc"
    break;

  case 848:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14042 "Parser/parser.cc"
    break;

  case 849:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14053 "Parser/parser.cc"
    break;

  case 850:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14063 "Parser/parser.cc"
    break;

  case 851:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14069 "Parser/parser.cc"
    break;

  case 853:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14075 "Parser/parser.cc"
    break;

  case 854:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14081 "Parser/parser.cc"
    break;

  case 855:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14087 "Parser/parser.cc"
    break;

  case 856:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14099 "Parser/parser.cc"
    break;

  case 857:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14110 "Parser/parser.cc"
    break;

  case 858:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14119 "Parser/parser.cc"
    break;

  case 859:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14128 "Parser/parser.cc"
    break;

  case 860:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14134 "Parser/parser.cc"
    break;

  case 861:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14140 "Parser/parser.cc"
    break;

  case 862:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14146 "Parser/parser.cc"
    break;

  case 863:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14155 "Parser/parser.cc"
    break;

  case 864:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14161 "Parser/parser.cc"
    break;

  case 865:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14167 "Parser/parser.cc"
    break;

  case 866:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14173 "Parser/parser.cc"
    break;

  case 871:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14179 "Parser/parser.cc"
    break;

  case 872:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14185 "Parser/parser.cc"
    break;

  case 873:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14195 "Parser/parser.cc"
    break;

  case 874:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14201 "Parser/parser.cc"
    break;

  case 877:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14207 "Parser/parser.cc"
    break;

  case 878:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14213 "Parser/parser.cc"
    break;

  case 879:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14219 "Parser/parser.cc"
    break;

  case 880:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14225 "Parser/parser.cc"
    break;

  case 881:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14231 "Parser/parser.cc"
    break;

  case 883:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14237 "Parser/parser.cc"
    break;

  case 884:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14243 "Parser/parser.cc"
    break;

  case 885:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14249 "Parser/parser.cc"
    break;

  case 886:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14255 "Parser/parser.cc"
    break;

  case 888:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14261 "Parser/parser.cc"
    break;

  case 889:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14267 "Parser/parser.cc"
    break;

  case 890:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14273 "Parser/parser.cc"
    break;

  case 891:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14279 "Parser/parser.cc"
    break;

  case 892:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14285 "Parser/parser.cc"
    break;

  case 893:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14291 "Parser/parser.cc"
    break;

  case 895:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14297 "Parser/parser.cc"
    break;

  case 896:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14303 "Parser/parser.cc"
    break;

  case 897:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14309 "Parser/parser.cc"
    break;

  case 898:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14315 "Parser/parser.cc"
    break;

  case 899:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14321 "Parser/parser.cc"
    break;

  case 900:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14327 "Parser/parser.cc"
    break;

  case 901:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14333 "Parser/parser.cc"
    break;

  case 902:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14339 "Parser/parser.cc"
    break;

  case 903:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14345 "Parser/parser.cc"
    break;

  case 904:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14351 "Parser/parser.cc"
    break;

  case 905:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14357 "Parser/parser.cc"
    break;

  case 906:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14363 "Parser/parser.cc"
    break;

  case 907:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14369 "Parser/parser.cc"
    break;

  case 908:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14375 "Parser/parser.cc"
    break;

  case 909:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14381 "Parser/parser.cc"
    break;

  case 910:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14387 "Parser/parser.cc"
    break;

  case 911:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14393 "Parser/parser.cc"
    break;

  case 912:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14399 "Parser/parser.cc"
    break;

  case 913:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14405 "Parser/parser.cc"
    break;

  case 915:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14411 "Parser/parser.cc"
    break;

  case 916:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14417 "Parser/parser.cc"
    break;

  case 917:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14423 "Parser/parser.cc"
    break;

  case 918:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14429 "Parser/parser.cc"
    break;

  case 919:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14435 "Parser/parser.cc"
    break;

  case 920:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14441 "Parser/parser.cc"
    break;

  case 921:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14447 "Parser/parser.cc"
    break;

  case 922:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14453 "Parser/parser.cc"
    break;

  case 923:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14459 "Parser/parser.cc"
    break;

  case 924:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14465 "Parser/parser.cc"
    break;

  case 925:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14471 "Parser/parser.cc"
    break;

  case 926:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14477 "Parser/parser.cc"
    break;

  case 927:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14483 "Parser/parser.cc"
    break;

  case 928:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14489 "Parser/parser.cc"
    break;

  case 929:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14495 "Parser/parser.cc"
    break;

  case 930:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14501 "Parser/parser.cc"
    break;

  case 931:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14507 "Parser/parser.cc"
    break;

  case 935:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 14513 "Parser/parser.cc"
    break;

  case 936:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14519 "Parser/parser.cc"
    break;

  case 937:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14525 "Parser/parser.cc"
    break;

  case 938:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14531 "Parser/parser.cc"
    break;

  case 939:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14537 "Parser/parser.cc"
    break;

  case 940:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14543 "Parser/parser.cc"
    break;

  case 941:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14549 "Parser/parser.cc"
    break;

  case 942:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14555 "Parser/parser.cc"
    break;

  case 943:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14561 "Parser/parser.cc"
    break;

  case 944:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14567 "Parser/parser.cc"
    break;

  case 945:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14573 "Parser/parser.cc"
    break;

  case 946:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14579 "Parser/parser.cc"
    break;

  case 947:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14585 "Parser/parser.cc"
    break;

  case 948:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14591 "Parser/parser.cc"
    break;

  case 949:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14597 "Parser/parser.cc"
    break;

  case 950:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14603 "Parser/parser.cc"
    break;

  case 951:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 14612 "Parser/parser.cc"
    break;

  case 952:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14618 "Parser/parser.cc"
    break;

  case 953:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14624 "Parser/parser.cc"
    break;

  case 955:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14630 "Parser/parser.cc"
    break;

  case 956:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14636 "Parser/parser.cc"
    break;

  case 957:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14642 "Parser/parser.cc"
    break;

  case 958:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14648 "Parser/parser.cc"
    break;

  case 959:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14654 "Parser/parser.cc"
    break;

  case 960:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14660 "Parser/parser.cc"
    break;

  case 961:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14666 "Parser/parser.cc"
    break;

  case 962:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14672 "Parser/parser.cc"
    break;

  case 963:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14678 "Parser/parser.cc"
    break;

  case 964:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14684 "Parser/parser.cc"
    break;

  case 965:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14690 "Parser/parser.cc"
    break;

  case 966:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14696 "Parser/parser.cc"
    break;

  case 967:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14702 "Parser/parser.cc"
    break;

  case 968:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14708 "Parser/parser.cc"
    break;

  case 969:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14714 "Parser/parser.cc"
    break;

  case 970:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14720 "Parser/parser.cc"
    break;

  case 971:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14726 "Parser/parser.cc"
    break;

  case 972:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14732 "Parser/parser.cc"
    break;

  case 973:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14738 "Parser/parser.cc"
    break;

  case 975:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14744 "Parser/parser.cc"
    break;

  case 976:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14750 "Parser/parser.cc"
    break;

  case 977:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14756 "Parser/parser.cc"
    break;

  case 978:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14762 "Parser/parser.cc"
    break;

  case 979:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14768 "Parser/parser.cc"
    break;

  case 980:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14774 "Parser/parser.cc"
    break;

  case 981:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14780 "Parser/parser.cc"
    break;

  case 982:
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14786 "Parser/parser.cc"
    break;

  case 983:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14792 "Parser/parser.cc"
    break;

  case 984:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14798 "Parser/parser.cc"
    break;

  case 985:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14804 "Parser/parser.cc"
    break;

  case 986:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14810 "Parser/parser.cc"
    break;

  case 987:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14816 "Parser/parser.cc"
    break;

  case 988:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14822 "Parser/parser.cc"
    break;

  case 989:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14828 "Parser/parser.cc"
    break;

  case 990:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14834 "Parser/parser.cc"
    break;

  case 991:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14840 "Parser/parser.cc"
    break;

  case 992:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14846 "Parser/parser.cc"
    break;

  case 993:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14853 "Parser/parser.cc"
    break;

  case 995:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14859 "Parser/parser.cc"
    break;

  case 996:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14865 "Parser/parser.cc"
    break;

  case 997:
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14871 "Parser/parser.cc"
    break;

  case 998:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14877 "Parser/parser.cc"
    break;

  case 999:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14883 "Parser/parser.cc"
    break;

  case 1000:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14889 "Parser/parser.cc"
    break;

  case 1001:
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14895 "Parser/parser.cc"
    break;

  case 1002:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14901 "Parser/parser.cc"
    break;

  case 1003:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14907 "Parser/parser.cc"
    break;

  case 1004:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14913 "Parser/parser.cc"
    break;

  case 1005:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14919 "Parser/parser.cc"
    break;

  case 1006:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14925 "Parser/parser.cc"
    break;

  case 1007:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14931 "Parser/parser.cc"
    break;

  case 1008:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14937 "Parser/parser.cc"
    break;

  case 1009:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14944 "Parser/parser.cc"
    break;

  case 1011:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14950 "Parser/parser.cc"
    break;

  case 1012:
#line 3941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14956 "Parser/parser.cc"
    break;

  case 1013:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14962 "Parser/parser.cc"
    break;

  case 1014:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14968 "Parser/parser.cc"
    break;

  case 1015:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14974 "Parser/parser.cc"
    break;

  case 1016:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14980 "Parser/parser.cc"
    break;

  case 1017:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14986 "Parser/parser.cc"
    break;

  case 1018:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14992 "Parser/parser.cc"
    break;

  case 1019:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14998 "Parser/parser.cc"
    break;

  case 1020:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15004 "Parser/parser.cc"
    break;

  case 1021:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15010 "Parser/parser.cc"
    break;

  case 1022:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15016 "Parser/parser.cc"
    break;

  case 1024:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15022 "Parser/parser.cc"
    break;

  case 1025:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15028 "Parser/parser.cc"
    break;

  case 1026:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15034 "Parser/parser.cc"
    break;

  case 1027:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15040 "Parser/parser.cc"
    break;

  case 1028:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15046 "Parser/parser.cc"
    break;

  case 1029:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) )->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 15052 "Parser/parser.cc"
    break;

  case 1030:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15058 "Parser/parser.cc"
    break;

  case 1031:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15064 "Parser/parser.cc"
    break;

  case 1033:
#line 4014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15070 "Parser/parser.cc"
    break;

  case 1034:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15076 "Parser/parser.cc"
    break;

  case 1035:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15082 "Parser/parser.cc"
    break;

  case 1036:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15088 "Parser/parser.cc"
    break;

  case 1037:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15094 "Parser/parser.cc"
    break;

  case 1038:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15100 "Parser/parser.cc"
    break;

  case 1039:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15106 "Parser/parser.cc"
    break;

  case 1040:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15112 "Parser/parser.cc"
    break;

  case 1041:
#line 4038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-3].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15118 "Parser/parser.cc"
    break;

  case 1042:
#line 4045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15124 "Parser/parser.cc"
    break;

  case 1044:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15130 "Parser/parser.cc"
    break;

  case 1045:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15136 "Parser/parser.cc"
    break;

  case 1047:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15142 "Parser/parser.cc"
    break;

  case 1048:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15148 "Parser/parser.cc"
    break;

  case 1050:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15154 "Parser/parser.cc"
    break;

  case 1051:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15160 "Parser/parser.cc"
    break;

  case 1052:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15166 "Parser/parser.cc"
    break;

  case 1053:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15172 "Parser/parser.cc"
    break;

  case 1054:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15178 "Parser/parser.cc"
    break;

  case 1055:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15184 "Parser/parser.cc"
    break;

  case 1056:
#line 4116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15190 "Parser/parser.cc"
    break;

  case 1059:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15197 "Parser/parser.cc"
    break;

  case 1060:
#line 4126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15203 "Parser/parser.cc"
    break;

  case 1061:
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15209 "Parser/parser.cc"
    break;

  case 1062:
#line 4133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15215 "Parser/parser.cc"
    break;

  case 1063:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15221 "Parser/parser.cc"
    break;

  case 1064:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15227 "Parser/parser.cc"
    break;

  case 1065:
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15233 "Parser/parser.cc"
    break;

  case 1066:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15239 "Parser/parser.cc"
    break;

  case 1068:
#line 4147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15245 "Parser/parser.cc"
    break;

  case 1069:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15251 "Parser/parser.cc"
    break;

  case 1070:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15257 "Parser/parser.cc"
    break;

  case 1071:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15263 "Parser/parser.cc"
    break;

  case 1072:
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15269 "Parser/parser.cc"
    break;

  case 1073:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15275 "Parser/parser.cc"
    break;

  case 1075:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15281 "Parser/parser.cc"
    break;

  case 1077:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15287 "Parser/parser.cc"
    break;

  case 1078:
#line 4181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15293 "Parser/parser.cc"
    break;

  case 1079:
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15299 "Parser/parser.cc"
    break;

  case 1080:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15305 "Parser/parser.cc"
    break;

  case 1081:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15311 "Parser/parser.cc"
    break;

  case 1082:
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15317 "Parser/parser.cc"
    break;

  case 1084:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15323 "Parser/parser.cc"
    break;

  case 1085:
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15329 "Parser/parser.cc"
    break;

  case 1086:
#line 4212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15335 "Parser/parser.cc"
    break;

  case 1087:
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15341 "Parser/parser.cc"
    break;

  case 1088:
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15347 "Parser/parser.cc"
    break;

  case 1089:
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15353 "Parser/parser.cc"
    break;

  case 1090:
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15359 "Parser/parser.cc"
    break;

  case 1092:
#line 4226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15365 "Parser/parser.cc"
    break;

  case 1093:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15371 "Parser/parser.cc"
    break;

  case 1094:
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15377 "Parser/parser.cc"
    break;

  case 1095:
#line 4235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15383 "Parser/parser.cc"
    break;

  case 1096:
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15389 "Parser/parser.cc"
    break;

  case 1099:
#line 4247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15395 "Parser/parser.cc"
    break;

  case 1102:
#line 4254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15401 "Parser/parser.cc"
    break;

  case 1103:
#line 4260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15407 "Parser/parser.cc"
    break;

  case 1104:
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15413 "Parser/parser.cc"
    break;

  case 1105:
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15419 "Parser/parser.cc"
    break;

  case 1106:
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15425 "Parser/parser.cc"
    break;

  case 1107:
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15431 "Parser/parser.cc"
    break;

  case 1108:
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15437 "Parser/parser.cc"
    break;

  case 1109:
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15443 "Parser/parser.cc"
    break;

  case 1110:
#line 4279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15449 "Parser/parser.cc"
    break;

  case 1111:
#line 4281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15455 "Parser/parser.cc"
    break;

  case 1112:
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15461 "Parser/parser.cc"
    break;

  case 1113:
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15467 "Parser/parser.cc"
    break;

  case 1114:
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15473 "Parser/parser.cc"
    break;

  case 1115:
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15479 "Parser/parser.cc"
    break;

  case 1116:
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15485 "Parser/parser.cc"
    break;

  case 1117:
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15491 "Parser/parser.cc"
    break;

  case 1118:
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15497 "Parser/parser.cc"
    break;

  case 1119:
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15503 "Parser/parser.cc"
    break;

  case 1120:
#line 4300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15509 "Parser/parser.cc"
    break;

  case 1121:
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15515 "Parser/parser.cc"
    break;

  case 1122:
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15521 "Parser/parser.cc"
    break;

  case 1123:
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 15527 "Parser/parser.cc"
    break;

  case 1124:
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15533 "Parser/parser.cc"
    break;

  case 1125:
#line 4313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 15539 "Parser/parser.cc"
    break;

  case 1126:
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 15545 "Parser/parser.cc"
    break;

  case 1127:
#line 4320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 15551 "Parser/parser.cc"
    break;

  case 1128:
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 15557 "Parser/parser.cc"
    break;

  case 1130:
#line 4349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15563 "Parser/parser.cc"
    break;

  case 1134:
#line 4360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15569 "Parser/parser.cc"
    break;

  case 1135:
#line 4362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15575 "Parser/parser.cc"
    break;

  case 1136:
#line 4364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15581 "Parser/parser.cc"
    break;

  case 1137:
#line 4366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15587 "Parser/parser.cc"
    break;

  case 1138:
#line 4368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15593 "Parser/parser.cc"
    break;

  case 1139:
#line 4370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15599 "Parser/parser.cc"
    break;

  case 1140:
#line 4372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15605 "Parser/parser.cc"
    break;

  case 1141:
#line 4379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15611 "Parser/parser.cc"
    break;

  case 1142:
#line 4381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15617 "Parser/parser.cc"
    break;

  case 1143:
#line 4383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15623 "Parser/parser.cc"
    break;

  case 1144:
#line 4385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15629 "Parser/parser.cc"
    break;

  case 1145:
#line 4387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 15635 "Parser/parser.cc"
    break;

  case 1146:
#line 4389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 15641 "Parser/parser.cc"
    break;

  case 1147:
#line 4394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 15647 "Parser/parser.cc"
    break;

  case 1148:
#line 4396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15653 "Parser/parser.cc"
    break;

  case 1149:
#line 4398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15659 "Parser/parser.cc"
    break;

  case 1150:
#line 4403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 15665 "Parser/parser.cc"
    break;

  case 1151:
#line 4405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15671 "Parser/parser.cc"
    break;

  case 1152:
#line 4407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 15677 "Parser/parser.cc"
    break;

  case 1155:
#line 4431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 15683 "Parser/parser.cc"
    break;

  case 1156:
#line 4433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 15689 "Parser/parser.cc"
    break;


#line 15693 "Parser/parser.cc"

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
#line 4436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
