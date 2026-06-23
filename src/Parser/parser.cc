/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

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
#include "Common/Iterate.hpp"							// for reverseIterate
#include "AST/Attribute.hpp"							// for Attribute
#include "AST/Print.hpp"								// for print

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
	for ( DeclarationNode * decl = declaration ; decl != nullptr ; decl = decl->next ) {
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

#line 361 "Parser/parser.cc"

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

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    TYPEDEF = 258,                 /* TYPEDEF  */
    EXTERN = 259,                  /* EXTERN  */
    STATIC = 260,                  /* STATIC  */
    AUTO = 261,                    /* AUTO  */
    REGISTER = 262,                /* REGISTER  */
    THREADLOCALGCC = 263,          /* THREADLOCALGCC  */
    THREADLOCALC11 = 264,          /* THREADLOCALC11  */
    INLINE = 265,                  /* INLINE  */
    FORTRAN = 266,                 /* FORTRAN  */
    NORETURN = 267,                /* NORETURN  */
    CONST = 268,                   /* CONST  */
    VOLATILE = 269,                /* VOLATILE  */
    RESTRICT = 270,                /* RESTRICT  */
    ATOMIC = 271,                  /* ATOMIC  */
    FORALL = 272,                  /* FORALL  */
    MUTEX = 273,                   /* MUTEX  */
    VIRTUAL = 274,                 /* VIRTUAL  */
    VTABLE = 275,                  /* VTABLE  */
    COERCE = 276,                  /* COERCE  */
    VOID = 277,                    /* VOID  */
    CHAR = 278,                    /* CHAR  */
    SHORT = 279,                   /* SHORT  */
    INT = 280,                     /* INT  */
    LONG = 281,                    /* LONG  */
    FLOAT = 282,                   /* FLOAT  */
    DOUBLE = 283,                  /* DOUBLE  */
    SIGNED = 284,                  /* SIGNED  */
    UNSIGNED = 285,                /* UNSIGNED  */
    BOOL = 286,                    /* BOOL  */
    COMPLEX = 287,                 /* COMPLEX  */
    IMAGINARY = 288,               /* IMAGINARY  */
    INT128 = 289,                  /* INT128  */
    UINT128 = 290,                 /* UINT128  */
    FLOAT80 = 291,                 /* FLOAT80  */
    uuFLOAT128 = 292,              /* uuFLOAT128  */
    FLOAT16 = 293,                 /* FLOAT16  */
    FLOAT32 = 294,                 /* FLOAT32  */
    FLOAT32X = 295,                /* FLOAT32X  */
    FLOAT64 = 296,                 /* FLOAT64  */
    FLOAT64X = 297,                /* FLOAT64X  */
    FLOAT128 = 298,                /* FLOAT128  */
    FLOAT128X = 299,               /* FLOAT128X  */
    FLOAT32X4 = 300,               /* FLOAT32X4  */
    FLOAT64X2 = 301,               /* FLOAT64X2  */
    SVFLOAT32 = 302,               /* SVFLOAT32  */
    SVFLOAT64 = 303,               /* SVFLOAT64  */
    SVBOOL = 304,                  /* SVBOOL  */
    DECIMAL32 = 305,               /* DECIMAL32  */
    DECIMAL64 = 306,               /* DECIMAL64  */
    DECIMAL128 = 307,              /* DECIMAL128  */
    ZERO_T = 308,                  /* ZERO_T  */
    ONE_T = 309,                   /* ONE_T  */
    SIZEOF = 310,                  /* SIZEOF  */
    TYPEOF = 311,                  /* TYPEOF  */
    VA_LIST = 312,                 /* VA_LIST  */
    VA_ARG = 313,                  /* VA_ARG  */
    AUTO_TYPE = 314,               /* AUTO_TYPE  */
    COUNTOF = 315,                 /* COUNTOF  */
    OFFSETOF = 316,                /* OFFSETOF  */
    BASETYPEOF = 317,              /* BASETYPEOF  */
    TYPEID = 318,                  /* TYPEID  */
    ENUM = 319,                    /* ENUM  */
    STRUCT = 320,                  /* STRUCT  */
    UNION = 321,                   /* UNION  */
    EXCEPTION = 322,               /* EXCEPTION  */
    GENERATOR = 323,               /* GENERATOR  */
    COROUTINE = 324,               /* COROUTINE  */
    MONITOR = 325,                 /* MONITOR  */
    THREAD = 326,                  /* THREAD  */
    OTYPE = 327,                   /* OTYPE  */
    FTYPE = 328,                   /* FTYPE  */
    DTYPE = 329,                   /* DTYPE  */
    TTYPE = 330,                   /* TTYPE  */
    TRAIT = 331,                   /* TRAIT  */
    LABEL = 332,                   /* LABEL  */
    SUSPEND = 333,                 /* SUSPEND  */
    ATTRIBUTE = 334,               /* ATTRIBUTE  */
    EXTENSION = 335,               /* EXTENSION  */
    IF = 336,                      /* IF  */
    ELSE = 337,                    /* ELSE  */
    SWITCH = 338,                  /* SWITCH  */
    CASE = 339,                    /* CASE  */
    DEFAULT = 340,                 /* DEFAULT  */
    DO = 341,                      /* DO  */
    WHILE = 342,                   /* WHILE  */
    FOR = 343,                     /* FOR  */
    BREAK = 344,                   /* BREAK  */
    CONTINUE = 345,                /* CONTINUE  */
    GOTO = 346,                    /* GOTO  */
    RETURN = 347,                  /* RETURN  */
    CHOOSE = 348,                  /* CHOOSE  */
    FALLTHROUGH = 349,             /* FALLTHROUGH  */
    WITH = 350,                    /* WITH  */
    WHEN = 351,                    /* WHEN  */
    WAITFOR = 352,                 /* WAITFOR  */
    WAITUNTIL = 353,               /* WAITUNTIL  */
    CORUN = 354,                   /* CORUN  */
    COFOR = 355,                   /* COFOR  */
    DISABLE = 356,                 /* DISABLE  */
    ENABLE = 357,                  /* ENABLE  */
    TRY = 358,                     /* TRY  */
    THROW = 359,                   /* THROW  */
    THROWRESUME = 360,             /* THROWRESUME  */
    AT = 361,                      /* AT  */
    ASM = 362,                     /* ASM  */
    ALIGNAS = 363,                 /* ALIGNAS  */
    ALIGNOF = 364,                 /* ALIGNOF  */
    __ALIGNOF = 365,               /* __ALIGNOF  */
    GENERIC = 366,                 /* GENERIC  */
    STATICASSERT = 367,            /* STATICASSERT  */
    IDENTIFIER = 368,              /* IDENTIFIER  */
    TYPEDIMname = 369,             /* TYPEDIMname  */
    TYPEDEFname = 370,             /* TYPEDEFname  */
    TYPEGENname = 371,             /* TYPEGENname  */
    TIMEOUT = 372,                 /* TIMEOUT  */
    WAND = 373,                    /* WAND  */
    WOR = 374,                     /* WOR  */
    CATCH = 375,                   /* CATCH  */
    RECOVER = 376,                 /* RECOVER  */
    CATCHRESUME = 377,             /* CATCHRESUME  */
    FIXUP = 378,                   /* FIXUP  */
    FINALLY = 379,                 /* FINALLY  */
    INTEGERconstant = 380,         /* INTEGERconstant  */
    CHARACTERconstant = 381,       /* CHARACTERconstant  */
    STRINGliteral = 382,           /* STRINGliteral  */
    DIRECTIVE = 383,               /* DIRECTIVE  */
    C23_ATTRIBUTE = 384,           /* C23_ATTRIBUTE  */
    FLOATING_DECIMALconstant = 385, /* FLOATING_DECIMALconstant  */
    FLOATING_FRACTIONconstant = 386, /* FLOATING_FRACTIONconstant  */
    FLOATINGconstant = 387,        /* FLOATINGconstant  */
    ARROW = 388,                   /* ARROW  */
    ICR = 389,                     /* ICR  */
    DECR = 390,                    /* DECR  */
    LS = 391,                      /* LS  */
    RS = 392,                      /* RS  */
    LE = 393,                      /* LE  */
    GE = 394,                      /* GE  */
    EQ = 395,                      /* EQ  */
    NE = 396,                      /* NE  */
    ANDAND = 397,                  /* ANDAND  */
    OROR = 398,                    /* OROR  */
    ATTR = 399,                    /* ATTR  */
    ELLIPSIS = 400,                /* ELLIPSIS  */
    EXPassign = 401,               /* EXPassign  */
    MULTassign = 402,              /* MULTassign  */
    DIVassign = 403,               /* DIVassign  */
    MODassign = 404,               /* MODassign  */
    PLUSassign = 405,              /* PLUSassign  */
    MINUSassign = 406,             /* MINUSassign  */
    LSassign = 407,                /* LSassign  */
    RSassign = 408,                /* RSassign  */
    ANDassign = 409,               /* ANDassign  */
    ERassign = 410,                /* ERassign  */
    ORassign = 411,                /* ORassign  */
    ErangeUpLt = 412,              /* ErangeUpLt  */
    ErangeUpLe = 413,              /* ErangeUpLe  */
    ErangeEq = 414,                /* ErangeEq  */
    ErangeNe = 415,                /* ErangeNe  */
    ErangeDownGt = 416,            /* ErangeDownGt  */
    ErangeDownGe = 417,            /* ErangeDownGe  */
    ErangeDownEq = 418,            /* ErangeDownEq  */
    ErangeDownNe = 419,            /* ErangeDownNe  */
    ATassign = 420,                /* ATassign  */
    THEN = 421                     /* THEN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
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
#define __ALIGNOF 365
#define GENERIC 366
#define STATICASSERT 367
#define IDENTIFIER 368
#define TYPEDIMname 369
#define TYPEDEFname 370
#define TYPEGENname 371
#define TIMEOUT 372
#define WAND 373
#define WOR 374
#define CATCH 375
#define RECOVER 376
#define CATCHRESUME 377
#define FIXUP 378
#define FINALLY 379
#define INTEGERconstant 380
#define CHARACTERconstant 381
#define STRINGliteral 382
#define DIRECTIVE 383
#define C23_ATTRIBUTE 384
#define FLOATING_DECIMALconstant 385
#define FLOATING_FRACTIONconstant 386
#define FLOATINGconstant 387
#define ARROW 388
#define ICR 389
#define DECR 390
#define LS 391
#define RS 392
#define LE 393
#define GE 394
#define EQ 395
#define NE 396
#define ANDAND 397
#define OROR 398
#define ATTR 399
#define ELLIPSIS 400
#define EXPassign 401
#define MULTassign 402
#define DIVassign 403
#define MODassign 404
#define PLUSassign 405
#define MINUSassign 406
#define LSassign 407
#define RSassign 408
#define ANDassign 409
#define ERassign 410
#define ORassign 411
#define ErangeUpLt 412
#define ErangeUpLe 413
#define ErangeEq 414
#define ErangeNe 415
#define ErangeDownGt 416
#define ErangeDownGe 417
#define ErangeDownEq 418
#define ErangeDownNe 419
#define ATassign 420
#define THEN 421

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

#line 779 "Parser/parser.cc"

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
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TYPEDEF = 3,                    /* TYPEDEF  */
  YYSYMBOL_EXTERN = 4,                     /* EXTERN  */
  YYSYMBOL_STATIC = 5,                     /* STATIC  */
  YYSYMBOL_AUTO = 6,                       /* AUTO  */
  YYSYMBOL_REGISTER = 7,                   /* REGISTER  */
  YYSYMBOL_THREADLOCALGCC = 8,             /* THREADLOCALGCC  */
  YYSYMBOL_THREADLOCALC11 = 9,             /* THREADLOCALC11  */
  YYSYMBOL_INLINE = 10,                    /* INLINE  */
  YYSYMBOL_FORTRAN = 11,                   /* FORTRAN  */
  YYSYMBOL_NORETURN = 12,                  /* NORETURN  */
  YYSYMBOL_CONST = 13,                     /* CONST  */
  YYSYMBOL_VOLATILE = 14,                  /* VOLATILE  */
  YYSYMBOL_RESTRICT = 15,                  /* RESTRICT  */
  YYSYMBOL_ATOMIC = 16,                    /* ATOMIC  */
  YYSYMBOL_FORALL = 17,                    /* FORALL  */
  YYSYMBOL_MUTEX = 18,                     /* MUTEX  */
  YYSYMBOL_VIRTUAL = 19,                   /* VIRTUAL  */
  YYSYMBOL_VTABLE = 20,                    /* VTABLE  */
  YYSYMBOL_COERCE = 21,                    /* COERCE  */
  YYSYMBOL_VOID = 22,                      /* VOID  */
  YYSYMBOL_CHAR = 23,                      /* CHAR  */
  YYSYMBOL_SHORT = 24,                     /* SHORT  */
  YYSYMBOL_INT = 25,                       /* INT  */
  YYSYMBOL_LONG = 26,                      /* LONG  */
  YYSYMBOL_FLOAT = 27,                     /* FLOAT  */
  YYSYMBOL_DOUBLE = 28,                    /* DOUBLE  */
  YYSYMBOL_SIGNED = 29,                    /* SIGNED  */
  YYSYMBOL_UNSIGNED = 30,                  /* UNSIGNED  */
  YYSYMBOL_BOOL = 31,                      /* BOOL  */
  YYSYMBOL_COMPLEX = 32,                   /* COMPLEX  */
  YYSYMBOL_IMAGINARY = 33,                 /* IMAGINARY  */
  YYSYMBOL_INT128 = 34,                    /* INT128  */
  YYSYMBOL_UINT128 = 35,                   /* UINT128  */
  YYSYMBOL_FLOAT80 = 36,                   /* FLOAT80  */
  YYSYMBOL_uuFLOAT128 = 37,                /* uuFLOAT128  */
  YYSYMBOL_FLOAT16 = 38,                   /* FLOAT16  */
  YYSYMBOL_FLOAT32 = 39,                   /* FLOAT32  */
  YYSYMBOL_FLOAT32X = 40,                  /* FLOAT32X  */
  YYSYMBOL_FLOAT64 = 41,                   /* FLOAT64  */
  YYSYMBOL_FLOAT64X = 42,                  /* FLOAT64X  */
  YYSYMBOL_FLOAT128 = 43,                  /* FLOAT128  */
  YYSYMBOL_FLOAT128X = 44,                 /* FLOAT128X  */
  YYSYMBOL_FLOAT32X4 = 45,                 /* FLOAT32X4  */
  YYSYMBOL_FLOAT64X2 = 46,                 /* FLOAT64X2  */
  YYSYMBOL_SVFLOAT32 = 47,                 /* SVFLOAT32  */
  YYSYMBOL_SVFLOAT64 = 48,                 /* SVFLOAT64  */
  YYSYMBOL_SVBOOL = 49,                    /* SVBOOL  */
  YYSYMBOL_DECIMAL32 = 50,                 /* DECIMAL32  */
  YYSYMBOL_DECIMAL64 = 51,                 /* DECIMAL64  */
  YYSYMBOL_DECIMAL128 = 52,                /* DECIMAL128  */
  YYSYMBOL_ZERO_T = 53,                    /* ZERO_T  */
  YYSYMBOL_ONE_T = 54,                     /* ONE_T  */
  YYSYMBOL_SIZEOF = 55,                    /* SIZEOF  */
  YYSYMBOL_TYPEOF = 56,                    /* TYPEOF  */
  YYSYMBOL_VA_LIST = 57,                   /* VA_LIST  */
  YYSYMBOL_VA_ARG = 58,                    /* VA_ARG  */
  YYSYMBOL_AUTO_TYPE = 59,                 /* AUTO_TYPE  */
  YYSYMBOL_COUNTOF = 60,                   /* COUNTOF  */
  YYSYMBOL_OFFSETOF = 61,                  /* OFFSETOF  */
  YYSYMBOL_BASETYPEOF = 62,                /* BASETYPEOF  */
  YYSYMBOL_TYPEID = 63,                    /* TYPEID  */
  YYSYMBOL_ENUM = 64,                      /* ENUM  */
  YYSYMBOL_STRUCT = 65,                    /* STRUCT  */
  YYSYMBOL_UNION = 66,                     /* UNION  */
  YYSYMBOL_EXCEPTION = 67,                 /* EXCEPTION  */
  YYSYMBOL_GENERATOR = 68,                 /* GENERATOR  */
  YYSYMBOL_COROUTINE = 69,                 /* COROUTINE  */
  YYSYMBOL_MONITOR = 70,                   /* MONITOR  */
  YYSYMBOL_THREAD = 71,                    /* THREAD  */
  YYSYMBOL_OTYPE = 72,                     /* OTYPE  */
  YYSYMBOL_FTYPE = 73,                     /* FTYPE  */
  YYSYMBOL_DTYPE = 74,                     /* DTYPE  */
  YYSYMBOL_TTYPE = 75,                     /* TTYPE  */
  YYSYMBOL_TRAIT = 76,                     /* TRAIT  */
  YYSYMBOL_LABEL = 77,                     /* LABEL  */
  YYSYMBOL_SUSPEND = 78,                   /* SUSPEND  */
  YYSYMBOL_ATTRIBUTE = 79,                 /* ATTRIBUTE  */
  YYSYMBOL_EXTENSION = 80,                 /* EXTENSION  */
  YYSYMBOL_IF = 81,                        /* IF  */
  YYSYMBOL_ELSE = 82,                      /* ELSE  */
  YYSYMBOL_SWITCH = 83,                    /* SWITCH  */
  YYSYMBOL_CASE = 84,                      /* CASE  */
  YYSYMBOL_DEFAULT = 85,                   /* DEFAULT  */
  YYSYMBOL_DO = 86,                        /* DO  */
  YYSYMBOL_WHILE = 87,                     /* WHILE  */
  YYSYMBOL_FOR = 88,                       /* FOR  */
  YYSYMBOL_BREAK = 89,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 90,                  /* CONTINUE  */
  YYSYMBOL_GOTO = 91,                      /* GOTO  */
  YYSYMBOL_RETURN = 92,                    /* RETURN  */
  YYSYMBOL_CHOOSE = 93,                    /* CHOOSE  */
  YYSYMBOL_FALLTHROUGH = 94,               /* FALLTHROUGH  */
  YYSYMBOL_WITH = 95,                      /* WITH  */
  YYSYMBOL_WHEN = 96,                      /* WHEN  */
  YYSYMBOL_WAITFOR = 97,                   /* WAITFOR  */
  YYSYMBOL_WAITUNTIL = 98,                 /* WAITUNTIL  */
  YYSYMBOL_CORUN = 99,                     /* CORUN  */
  YYSYMBOL_COFOR = 100,                    /* COFOR  */
  YYSYMBOL_DISABLE = 101,                  /* DISABLE  */
  YYSYMBOL_ENABLE = 102,                   /* ENABLE  */
  YYSYMBOL_TRY = 103,                      /* TRY  */
  YYSYMBOL_THROW = 104,                    /* THROW  */
  YYSYMBOL_THROWRESUME = 105,              /* THROWRESUME  */
  YYSYMBOL_AT = 106,                       /* AT  */
  YYSYMBOL_ASM = 107,                      /* ASM  */
  YYSYMBOL_ALIGNAS = 108,                  /* ALIGNAS  */
  YYSYMBOL_ALIGNOF = 109,                  /* ALIGNOF  */
  YYSYMBOL___ALIGNOF = 110,                /* __ALIGNOF  */
  YYSYMBOL_GENERIC = 111,                  /* GENERIC  */
  YYSYMBOL_STATICASSERT = 112,             /* STATICASSERT  */
  YYSYMBOL_IDENTIFIER = 113,               /* IDENTIFIER  */
  YYSYMBOL_TYPEDIMname = 114,              /* TYPEDIMname  */
  YYSYMBOL_TYPEDEFname = 115,              /* TYPEDEFname  */
  YYSYMBOL_TYPEGENname = 116,              /* TYPEGENname  */
  YYSYMBOL_TIMEOUT = 117,                  /* TIMEOUT  */
  YYSYMBOL_WAND = 118,                     /* WAND  */
  YYSYMBOL_WOR = 119,                      /* WOR  */
  YYSYMBOL_CATCH = 120,                    /* CATCH  */
  YYSYMBOL_RECOVER = 121,                  /* RECOVER  */
  YYSYMBOL_CATCHRESUME = 122,              /* CATCHRESUME  */
  YYSYMBOL_FIXUP = 123,                    /* FIXUP  */
  YYSYMBOL_FINALLY = 124,                  /* FINALLY  */
  YYSYMBOL_INTEGERconstant = 125,          /* INTEGERconstant  */
  YYSYMBOL_CHARACTERconstant = 126,        /* CHARACTERconstant  */
  YYSYMBOL_STRINGliteral = 127,            /* STRINGliteral  */
  YYSYMBOL_DIRECTIVE = 128,                /* DIRECTIVE  */
  YYSYMBOL_C23_ATTRIBUTE = 129,            /* C23_ATTRIBUTE  */
  YYSYMBOL_FLOATING_DECIMALconstant = 130, /* FLOATING_DECIMALconstant  */
  YYSYMBOL_FLOATING_FRACTIONconstant = 131, /* FLOATING_FRACTIONconstant  */
  YYSYMBOL_FLOATINGconstant = 132,         /* FLOATINGconstant  */
  YYSYMBOL_ARROW = 133,                    /* ARROW  */
  YYSYMBOL_ICR = 134,                      /* ICR  */
  YYSYMBOL_DECR = 135,                     /* DECR  */
  YYSYMBOL_LS = 136,                       /* LS  */
  YYSYMBOL_RS = 137,                       /* RS  */
  YYSYMBOL_LE = 138,                       /* LE  */
  YYSYMBOL_GE = 139,                       /* GE  */
  YYSYMBOL_EQ = 140,                       /* EQ  */
  YYSYMBOL_NE = 141,                       /* NE  */
  YYSYMBOL_ANDAND = 142,                   /* ANDAND  */
  YYSYMBOL_OROR = 143,                     /* OROR  */
  YYSYMBOL_ATTR = 144,                     /* ATTR  */
  YYSYMBOL_ELLIPSIS = 145,                 /* ELLIPSIS  */
  YYSYMBOL_EXPassign = 146,                /* EXPassign  */
  YYSYMBOL_MULTassign = 147,               /* MULTassign  */
  YYSYMBOL_DIVassign = 148,                /* DIVassign  */
  YYSYMBOL_MODassign = 149,                /* MODassign  */
  YYSYMBOL_PLUSassign = 150,               /* PLUSassign  */
  YYSYMBOL_MINUSassign = 151,              /* MINUSassign  */
  YYSYMBOL_LSassign = 152,                 /* LSassign  */
  YYSYMBOL_RSassign = 153,                 /* RSassign  */
  YYSYMBOL_ANDassign = 154,                /* ANDassign  */
  YYSYMBOL_ERassign = 155,                 /* ERassign  */
  YYSYMBOL_ORassign = 156,                 /* ORassign  */
  YYSYMBOL_ErangeUpLt = 157,               /* ErangeUpLt  */
  YYSYMBOL_ErangeUpLe = 158,               /* ErangeUpLe  */
  YYSYMBOL_ErangeEq = 159,                 /* ErangeEq  */
  YYSYMBOL_ErangeNe = 160,                 /* ErangeNe  */
  YYSYMBOL_ErangeDownGt = 161,             /* ErangeDownGt  */
  YYSYMBOL_ErangeDownGe = 162,             /* ErangeDownGe  */
  YYSYMBOL_ErangeDownEq = 163,             /* ErangeDownEq  */
  YYSYMBOL_ErangeDownNe = 164,             /* ErangeDownNe  */
  YYSYMBOL_ATassign = 165,                 /* ATassign  */
  YYSYMBOL_THEN = 166,                     /* THEN  */
  YYSYMBOL_167_ = 167,                     /* '}'  */
  YYSYMBOL_168_ = 168,                     /* '('  */
  YYSYMBOL_169_ = 169,                     /* '@'  */
  YYSYMBOL_170_ = 170,                     /* ')'  */
  YYSYMBOL_171_ = 171,                     /* '.'  */
  YYSYMBOL_172_ = 172,                     /* '['  */
  YYSYMBOL_173_ = 173,                     /* ']'  */
  YYSYMBOL_174_ = 174,                     /* ','  */
  YYSYMBOL_175_ = 175,                     /* ':'  */
  YYSYMBOL_176_ = 176,                     /* '{'  */
  YYSYMBOL_177_ = 177,                     /* '`'  */
  YYSYMBOL_178_ = 178,                     /* '^'  */
  YYSYMBOL_179_ = 179,                     /* '*'  */
  YYSYMBOL_180_ = 180,                     /* '&'  */
  YYSYMBOL_181_ = 181,                     /* '+'  */
  YYSYMBOL_182_ = 182,                     /* '-'  */
  YYSYMBOL_183_ = 183,                     /* '!'  */
  YYSYMBOL_184_ = 184,                     /* '~'  */
  YYSYMBOL_185_ = 185,                     /* '\\'  */
  YYSYMBOL_186_ = 186,                     /* '/'  */
  YYSYMBOL_187_ = 187,                     /* '%'  */
  YYSYMBOL_188_ = 188,                     /* '<'  */
  YYSYMBOL_189_ = 189,                     /* '>'  */
  YYSYMBOL_190_ = 190,                     /* '|'  */
  YYSYMBOL_191_ = 191,                     /* '?'  */
  YYSYMBOL_192_ = 192,                     /* '='  */
  YYSYMBOL_193_ = 193,                     /* ';'  */
  YYSYMBOL_YYACCEPT = 194,                 /* $accept  */
  YYSYMBOL_push = 195,                     /* push  */
  YYSYMBOL_pop = 196,                      /* pop  */
  YYSYMBOL_constant = 197,                 /* constant  */
  YYSYMBOL_quasi_keyword = 198,            /* quasi_keyword  */
  YYSYMBOL_identifier = 199,               /* identifier  */
  YYSYMBOL_identifier_at = 200,            /* identifier_at  */
  YYSYMBOL_identifier_or_type_name = 201,  /* identifier_or_type_name  */
  YYSYMBOL_string_literal = 202,           /* string_literal  */
  YYSYMBOL_string_literal_list = 203,      /* string_literal_list  */
  YYSYMBOL_primary_expression = 204,       /* primary_expression  */
  YYSYMBOL_generic_assoc_list = 205,       /* generic_assoc_list  */
  YYSYMBOL_generic_association = 206,      /* generic_association  */
  YYSYMBOL_postfix_expression = 207,       /* postfix_expression  */
  YYSYMBOL_field_name_list = 208,          /* field_name_list  */
  YYSYMBOL_field = 209,                    /* field  */
  YYSYMBOL_field_name = 210,               /* field_name  */
  YYSYMBOL_fraction_constants_opt = 211,   /* fraction_constants_opt  */
  YYSYMBOL_unary_expression = 212,         /* unary_expression  */
  YYSYMBOL_alignof_operator = 213,         /* alignof_operator  */
  YYSYMBOL_ptrref_operator = 214,          /* ptrref_operator  */
  YYSYMBOL_unary_operator = 215,           /* unary_operator  */
  YYSYMBOL_cast_expression = 216,          /* cast_expression  */
  YYSYMBOL_qualifier_cast_list = 217,      /* qualifier_cast_list  */
  YYSYMBOL_cast_modifier = 218,            /* cast_modifier  */
  YYSYMBOL_exponential_expression = 219,   /* exponential_expression  */
  YYSYMBOL_multiplicative_expression = 220, /* multiplicative_expression  */
  YYSYMBOL_additive_expression = 221,      /* additive_expression  */
  YYSYMBOL_shift_expression = 222,         /* shift_expression  */
  YYSYMBOL_relational_expression = 223,    /* relational_expression  */
  YYSYMBOL_equality_expression = 224,      /* equality_expression  */
  YYSYMBOL_AND_expression = 225,           /* AND_expression  */
  YYSYMBOL_exclusive_OR_expression = 226,  /* exclusive_OR_expression  */
  YYSYMBOL_inclusive_OR_expression = 227,  /* inclusive_OR_expression  */
  YYSYMBOL_logical_AND_expression = 228,   /* logical_AND_expression  */
  YYSYMBOL_logical_OR_expression = 229,    /* logical_OR_expression  */
  YYSYMBOL_conditional_expression = 230,   /* conditional_expression  */
  YYSYMBOL_constant_expression = 231,      /* constant_expression  */
  YYSYMBOL_argument_expression_list_opt = 232, /* argument_expression_list_opt  */
  YYSYMBOL_argument_expression_list = 233, /* argument_expression_list  */
  YYSYMBOL_argument_expression = 234,      /* argument_expression  */
  YYSYMBOL_assignment_expression = 235,    /* assignment_expression  */
  YYSYMBOL_assignment_expression_opt = 236, /* assignment_expression_opt  */
  YYSYMBOL_assignment_operator = 237,      /* assignment_operator  */
  YYSYMBOL_simple_assignment_operator = 238, /* simple_assignment_operator  */
  YYSYMBOL_compound_assignment_operator = 239, /* compound_assignment_operator  */
  YYSYMBOL_tuple = 240,                    /* tuple  */
  YYSYMBOL_tuple_expression_list = 241,    /* tuple_expression_list  */
  YYSYMBOL_comma_expression = 242,         /* comma_expression  */
  YYSYMBOL_comma_expression_opt = 243,     /* comma_expression_opt  */
  YYSYMBOL_statement = 244,                /* statement  */
  YYSYMBOL_labelled_statement = 245,       /* labelled_statement  */
  YYSYMBOL_compound_statement = 246,       /* compound_statement  */
  YYSYMBOL_statement_decl_list = 247,      /* statement_decl_list  */
  YYSYMBOL_statement_decl = 248,           /* statement_decl  */
  YYSYMBOL_statement_list_nodecl = 249,    /* statement_list_nodecl  */
  YYSYMBOL_expression_statement = 250,     /* expression_statement  */
  YYSYMBOL_selection_statement = 251,      /* selection_statement  */
  YYSYMBOL_conditional_declaration = 252,  /* conditional_declaration  */
  YYSYMBOL_case_value = 253,               /* case_value  */
  YYSYMBOL_case_value_list = 254,          /* case_value_list  */
  YYSYMBOL_case_label = 255,               /* case_label  */
  YYSYMBOL_case_label_list = 256,          /* case_label_list  */
  YYSYMBOL_case_clause = 257,              /* case_clause  */
  YYSYMBOL_switch_clause_list_opt = 258,   /* switch_clause_list_opt  */
  YYSYMBOL_switch_clause_list = 259,       /* switch_clause_list  */
  YYSYMBOL_iteration_statement = 260,      /* iteration_statement  */
  YYSYMBOL_for_control_expression_list = 261, /* for_control_expression_list  */
  YYSYMBOL_for_control_expression = 262,   /* for_control_expression  */
  YYSYMBOL_enum_key = 263,                 /* enum_key  */
  YYSYMBOL_updown = 264,                   /* updown  */
  YYSYMBOL_updownS = 265,                  /* updownS  */
  YYSYMBOL_updownEq = 266,                 /* updownEq  */
  YYSYMBOL_jump_statement = 267,           /* jump_statement  */
  YYSYMBOL_with_statement = 268,           /* with_statement  */
  YYSYMBOL_mutex_statement = 269,          /* mutex_statement  */
  YYSYMBOL_when_clause = 270,              /* when_clause  */
  YYSYMBOL_when_clause_opt = 271,          /* when_clause_opt  */
  YYSYMBOL_cast_expression_list = 272,     /* cast_expression_list  */
  YYSYMBOL_timeout = 273,                  /* timeout  */
  YYSYMBOL_wor = 274,                      /* wor  */
  YYSYMBOL_waitfor = 275,                  /* waitfor  */
  YYSYMBOL_wor_waitfor_clause = 276,       /* wor_waitfor_clause  */
  YYSYMBOL_waitfor_statement = 277,        /* waitfor_statement  */
  YYSYMBOL_wand = 278,                     /* wand  */
  YYSYMBOL_waituntil = 279,                /* waituntil  */
  YYSYMBOL_waituntil_clause = 280,         /* waituntil_clause  */
  YYSYMBOL_wand_waituntil_clause = 281,    /* wand_waituntil_clause  */
  YYSYMBOL_wor_waituntil_clause = 282,     /* wor_waituntil_clause  */
  YYSYMBOL_waituntil_statement = 283,      /* waituntil_statement  */
  YYSYMBOL_corun_statement = 284,          /* corun_statement  */
  YYSYMBOL_cofor_statement = 285,          /* cofor_statement  */
  YYSYMBOL_exception_statement = 286,      /* exception_statement  */
  YYSYMBOL_handler_clause = 287,           /* handler_clause  */
  YYSYMBOL_handler_predicate_opt = 288,    /* handler_predicate_opt  */
  YYSYMBOL_handler_key = 289,              /* handler_key  */
  YYSYMBOL_finally_clause = 290,           /* finally_clause  */
  YYSYMBOL_exception_declaration = 291,    /* exception_declaration  */
  YYSYMBOL_enable_disable_statement = 292, /* enable_disable_statement  */
  YYSYMBOL_enable_disable_key = 293,       /* enable_disable_key  */
  YYSYMBOL_asm_statement = 294,            /* asm_statement  */
  YYSYMBOL_asm_volatile_opt = 295,         /* asm_volatile_opt  */
  YYSYMBOL_asm_operands_opt = 296,         /* asm_operands_opt  */
  YYSYMBOL_asm_operands_list = 297,        /* asm_operands_list  */
  YYSYMBOL_asm_operand = 298,              /* asm_operand  */
  YYSYMBOL_asm_clobbers_list_opt = 299,    /* asm_clobbers_list_opt  */
  YYSYMBOL_asm_label_list = 300,           /* asm_label_list  */
  YYSYMBOL_declaration_list_opt = 301,     /* declaration_list_opt  */
  YYSYMBOL_declaration_list = 302,         /* declaration_list  */
  YYSYMBOL_KR_parameter_list_opt = 303,    /* KR_parameter_list_opt  */
  YYSYMBOL_KR_parameter_list = 304,        /* KR_parameter_list  */
  YYSYMBOL_local_label_declaration_opt = 305, /* local_label_declaration_opt  */
  YYSYMBOL_local_label_declaration_list = 306, /* local_label_declaration_list  */
  YYSYMBOL_local_label_list = 307,         /* local_label_list  */
  YYSYMBOL_declaration = 308,              /* declaration  */
  YYSYMBOL_static_assert = 309,            /* static_assert  */
  YYSYMBOL_cfa_declaration = 310,          /* cfa_declaration  */
  YYSYMBOL_cfa_variable_declaration = 311, /* cfa_variable_declaration  */
  YYSYMBOL_cfa_variable_specifier = 312,   /* cfa_variable_specifier  */
  YYSYMBOL_cfa_function_declaration = 313, /* cfa_function_declaration  */
  YYSYMBOL_cfa_function_specifier = 314,   /* cfa_function_specifier  */
  YYSYMBOL_cfa_function_return = 315,      /* cfa_function_return  */
  YYSYMBOL_cfa_typedef_declaration = 316,  /* cfa_typedef_declaration  */
  YYSYMBOL_typedef_declaration = 317,      /* typedef_declaration  */
  YYSYMBOL_typedef_expression = 318,       /* typedef_expression  */
  YYSYMBOL_c_declaration = 319,            /* c_declaration  */
  YYSYMBOL_declaring_list = 320,           /* declaring_list  */
  YYSYMBOL_general_function_declarator = 321, /* general_function_declarator  */
  YYSYMBOL_declaration_specifier = 322,    /* declaration_specifier  */
  YYSYMBOL_invalid_types = 323,            /* invalid_types  */
  YYSYMBOL_declaration_specifier_nobody = 324, /* declaration_specifier_nobody  */
  YYSYMBOL_type_specifier = 325,           /* type_specifier  */
  YYSYMBOL_type_specifier_nobody = 326,    /* type_specifier_nobody  */
  YYSYMBOL_type_qualifier_list_opt = 327,  /* type_qualifier_list_opt  */
  YYSYMBOL_type_qualifier_list = 328,      /* type_qualifier_list  */
  YYSYMBOL_type_qualifier = 329,           /* type_qualifier  */
  YYSYMBOL_type_qualifier_name = 330,      /* type_qualifier_name  */
  YYSYMBOL_forall = 331,                   /* forall  */
  YYSYMBOL_declaration_qualifier_list = 332, /* declaration_qualifier_list  */
  YYSYMBOL_storage_class_list = 333,       /* storage_class_list  */
  YYSYMBOL_storage_class = 334,            /* storage_class  */
  YYSYMBOL_basic_type_name = 335,          /* basic_type_name  */
  YYSYMBOL_basic_type_name_type = 336,     /* basic_type_name_type  */
  YYSYMBOL_vtable_opt = 337,               /* vtable_opt  */
  YYSYMBOL_vtable = 338,                   /* vtable  */
  YYSYMBOL_default_opt = 339,              /* default_opt  */
  YYSYMBOL_basic_declaration_specifier = 340, /* basic_declaration_specifier  */
  YYSYMBOL_basic_type_specifier = 341,     /* basic_type_specifier  */
  YYSYMBOL_direct_type = 342,              /* direct_type  */
  YYSYMBOL_indirect_type = 343,            /* indirect_type  */
  YYSYMBOL_sue_declaration_specifier = 344, /* sue_declaration_specifier  */
  YYSYMBOL_sue_type_specifier = 345,       /* sue_type_specifier  */
  YYSYMBOL_346_1 = 346,                    /* $@1  */
  YYSYMBOL_sue_declaration_specifier_nobody = 347, /* sue_declaration_specifier_nobody  */
  YYSYMBOL_sue_type_specifier_nobody = 348, /* sue_type_specifier_nobody  */
  YYSYMBOL_type_declaration_specifier = 349, /* type_declaration_specifier  */
  YYSYMBOL_type_type_specifier = 350,      /* type_type_specifier  */
  YYSYMBOL_type_name = 351,                /* type_name  */
  YYSYMBOL_typegen_name = 352,             /* typegen_name  */
  YYSYMBOL_elaborated_type = 353,          /* elaborated_type  */
  YYSYMBOL_elaborated_type_nobody = 354,   /* elaborated_type_nobody  */
  YYSYMBOL_aggregate_type = 355,           /* aggregate_type  */
  YYSYMBOL_356_2 = 356,                    /* $@2  */
  YYSYMBOL_357_3 = 357,                    /* $@3  */
  YYSYMBOL_358_4 = 358,                    /* $@4  */
  YYSYMBOL_359_5 = 359,                    /* $@5  */
  YYSYMBOL_type_parameters_opt = 360,      /* type_parameters_opt  */
  YYSYMBOL_aggregate_type_nobody = 361,    /* aggregate_type_nobody  */
  YYSYMBOL_aggregate_key = 362,            /* aggregate_key  */
  YYSYMBOL_aggregate_data = 363,           /* aggregate_data  */
  YYSYMBOL_aggregate_control = 364,        /* aggregate_control  */
  YYSYMBOL_field_declaration_list_opt = 365, /* field_declaration_list_opt  */
  YYSYMBOL_field_declaration = 366,        /* field_declaration  */
  YYSYMBOL_field_declaring_list_opt = 367, /* field_declaring_list_opt  */
  YYSYMBOL_field_declaring_list = 368,     /* field_declaring_list  */
  YYSYMBOL_field_declarator = 369,         /* field_declarator  */
  YYSYMBOL_field_abstract_list_opt = 370,  /* field_abstract_list_opt  */
  YYSYMBOL_field_abstract = 371,           /* field_abstract  */
  YYSYMBOL_cfa_field_declaring_list = 372, /* cfa_field_declaring_list  */
  YYSYMBOL_cfa_field_abstract_list = 373,  /* cfa_field_abstract_list  */
  YYSYMBOL_bit_subrange_size_opt = 374,    /* bit_subrange_size_opt  */
  YYSYMBOL_bit_subrange_size = 375,        /* bit_subrange_size  */
  YYSYMBOL_enum_type = 376,                /* enum_type  */
  YYSYMBOL_377_6 = 377,                    /* $@6  */
  YYSYMBOL_378_7 = 378,                    /* $@7  */
  YYSYMBOL_enumerator_type = 379,          /* enumerator_type  */
  YYSYMBOL_hide_opt = 380,                 /* hide_opt  */
  YYSYMBOL_enum_type_nobody = 381,         /* enum_type_nobody  */
  YYSYMBOL_enumerator_list = 382,          /* enumerator_list  */
  YYSYMBOL_visible_hide_opt = 383,         /* visible_hide_opt  */
  YYSYMBOL_enumerator_value_opt = 384,     /* enumerator_value_opt  */
  YYSYMBOL_parameter_list_ellipsis_opt = 385, /* parameter_list_ellipsis_opt  */
  YYSYMBOL_parameter_list = 386,           /* parameter_list  */
  YYSYMBOL_cfa_parameter_list_ellipsis_opt = 387, /* cfa_parameter_list_ellipsis_opt  */
  YYSYMBOL_cfa_parameter_list = 388,       /* cfa_parameter_list  */
  YYSYMBOL_cfa_abstract_parameter_list = 389, /* cfa_abstract_parameter_list  */
  YYSYMBOL_parameter_declaration = 390,    /* parameter_declaration  */
  YYSYMBOL_abstract_parameter_declaration = 391, /* abstract_parameter_declaration  */
  YYSYMBOL_cfa_parameter_declaration = 392, /* cfa_parameter_declaration  */
  YYSYMBOL_cfa_abstract_parameter_declaration = 393, /* cfa_abstract_parameter_declaration  */
  YYSYMBOL_identifier_list = 394,          /* identifier_list  */
  YYSYMBOL_type_no_function = 395,         /* type_no_function  */
  YYSYMBOL_type = 396,                     /* type  */
  YYSYMBOL_initializer_opt = 397,          /* initializer_opt  */
  YYSYMBOL_initializer = 398,              /* initializer  */
  YYSYMBOL_initializer_list_opt = 399,     /* initializer_list_opt  */
  YYSYMBOL_designation = 400,              /* designation  */
  YYSYMBOL_designator_list = 401,          /* designator_list  */
  YYSYMBOL_designator = 402,               /* designator  */
  YYSYMBOL_type_parameter_list = 403,      /* type_parameter_list  */
  YYSYMBOL_type_initializer_opt = 404,     /* type_initializer_opt  */
  YYSYMBOL_type_parameter = 405,           /* type_parameter  */
  YYSYMBOL_406_8 = 406,                    /* $@8  */
  YYSYMBOL_407_9 = 407,                    /* $@9  */
  YYSYMBOL_new_type_class = 408,           /* new_type_class  */
  YYSYMBOL_type_class = 409,               /* type_class  */
  YYSYMBOL_assertion_list_opt = 410,       /* assertion_list_opt  */
  YYSYMBOL_assertion_list = 411,           /* assertion_list  */
  YYSYMBOL_assertion = 412,                /* assertion  */
  YYSYMBOL_type_list = 413,                /* type_list  */
  YYSYMBOL_type_declaring_list = 414,      /* type_declaring_list  */
  YYSYMBOL_type_declarator = 415,          /* type_declarator  */
  YYSYMBOL_type_declarator_name = 416,     /* type_declarator_name  */
  YYSYMBOL_trait_specifier = 417,          /* trait_specifier  */
  YYSYMBOL_trait_declaration_list = 418,   /* trait_declaration_list  */
  YYSYMBOL_trait_declaration = 419,        /* trait_declaration  */
  YYSYMBOL_cfa_trait_declaring_list = 420, /* cfa_trait_declaring_list  */
  YYSYMBOL_trait_declaring_list = 421,     /* trait_declaring_list  */
  YYSYMBOL_translation_unit = 422,         /* translation_unit  */
  YYSYMBOL_external_definition_list_opt = 423, /* external_definition_list_opt  */
  YYSYMBOL_external_definition_list = 424, /* external_definition_list  */
  YYSYMBOL_up = 425,                       /* up  */
  YYSYMBOL_down = 426,                     /* down  */
  YYSYMBOL_external_definition = 427,      /* external_definition  */
  YYSYMBOL_428_10 = 428,                   /* $@10  */
  YYSYMBOL_429_11 = 429,                   /* $@11  */
  YYSYMBOL_430_12 = 430,                   /* $@12  */
  YYSYMBOL_431_13 = 431,                   /* $@13  */
  YYSYMBOL_432_14 = 432,                   /* $@14  */
  YYSYMBOL_external_function_definition = 433, /* external_function_definition  */
  YYSYMBOL_with_clause_opt = 434,          /* with_clause_opt  */
  YYSYMBOL_function_definition = 435,      /* function_definition  */
  YYSYMBOL_declarator = 436,               /* declarator  */
  YYSYMBOL_subrange = 437,                 /* subrange  */
  YYSYMBOL_asm_name_opt = 438,             /* asm_name_opt  */
  YYSYMBOL_attribute_list_opt = 439,       /* attribute_list_opt  */
  YYSYMBOL_attribute_list = 440,           /* attribute_list  */
  YYSYMBOL_attribute = 441,                /* attribute  */
  YYSYMBOL_attribute_name_list = 442,      /* attribute_name_list  */
  YYSYMBOL_attribute_name = 443,           /* attribute_name  */
  YYSYMBOL_attr_name = 444,                /* attr_name  */
  YYSYMBOL_paren_identifier = 445,         /* paren_identifier  */
  YYSYMBOL_variable_declarator = 446,      /* variable_declarator  */
  YYSYMBOL_variable_ptr = 447,             /* variable_ptr  */
  YYSYMBOL_variable_array = 448,           /* variable_array  */
  YYSYMBOL_variable_function = 449,        /* variable_function  */
  YYSYMBOL_function_declarator = 450,      /* function_declarator  */
  YYSYMBOL_function_no_ptr = 451,          /* function_no_ptr  */
  YYSYMBOL_function_ptr = 452,             /* function_ptr  */
  YYSYMBOL_function_array = 453,           /* function_array  */
  YYSYMBOL_KR_function_declarator = 454,   /* KR_function_declarator  */
  YYSYMBOL_KR_function_no_ptr = 455,       /* KR_function_no_ptr  */
  YYSYMBOL_KR_function_ptr = 456,          /* KR_function_ptr  */
  YYSYMBOL_KR_function_array = 457,        /* KR_function_array  */
  YYSYMBOL_paren_type = 458,               /* paren_type  */
  YYSYMBOL_variable_type_redeclarator = 459, /* variable_type_redeclarator  */
  YYSYMBOL_variable_type_ptr = 460,        /* variable_type_ptr  */
  YYSYMBOL_variable_type_array = 461,      /* variable_type_array  */
  YYSYMBOL_variable_type_function = 462,   /* variable_type_function  */
  YYSYMBOL_function_type_redeclarator = 463, /* function_type_redeclarator  */
  YYSYMBOL_function_type_no_ptr = 464,     /* function_type_no_ptr  */
  YYSYMBOL_function_type_ptr = 465,        /* function_type_ptr  */
  YYSYMBOL_function_type_array = 466,      /* function_type_array  */
  YYSYMBOL_identifier_parameter_declarator = 467, /* identifier_parameter_declarator  */
  YYSYMBOL_identifier_parameter_ptr = 468, /* identifier_parameter_ptr  */
  YYSYMBOL_identifier_parameter_array = 469, /* identifier_parameter_array  */
  YYSYMBOL_identifier_parameter_function = 470, /* identifier_parameter_function  */
  YYSYMBOL_type_parameter_redeclarator = 471, /* type_parameter_redeclarator  */
  YYSYMBOL_typedef_name = 472,             /* typedef_name  */
  YYSYMBOL_type_parameter_ptr = 473,       /* type_parameter_ptr  */
  YYSYMBOL_type_parameter_array = 474,     /* type_parameter_array  */
  YYSYMBOL_type_parameter_function = 475,  /* type_parameter_function  */
  YYSYMBOL_abstract_declarator = 476,      /* abstract_declarator  */
  YYSYMBOL_abstract_ptr = 477,             /* abstract_ptr  */
  YYSYMBOL_abstract_array = 478,           /* abstract_array  */
  YYSYMBOL_abstract_function = 479,        /* abstract_function  */
  YYSYMBOL_array_dimension = 480,          /* array_dimension  */
  YYSYMBOL_array_type_list = 481,          /* array_type_list  */
  YYSYMBOL_upupeq = 482,                   /* upupeq  */
  YYSYMBOL_multi_array_dimension = 483,    /* multi_array_dimension  */
  YYSYMBOL_abstract_parameter_declarator_opt = 484, /* abstract_parameter_declarator_opt  */
  YYSYMBOL_abstract_parameter_declarator = 485, /* abstract_parameter_declarator  */
  YYSYMBOL_abstract_parameter_ptr = 486,   /* abstract_parameter_ptr  */
  YYSYMBOL_abstract_parameter_array = 487, /* abstract_parameter_array  */
  YYSYMBOL_abstract_parameter_function = 488, /* abstract_parameter_function  */
  YYSYMBOL_array_parameter_dimension = 489, /* array_parameter_dimension  */
  YYSYMBOL_array_parameter_1st_dimension = 490, /* array_parameter_1st_dimension  */
  YYSYMBOL_variable_abstract_declarator = 491, /* variable_abstract_declarator  */
  YYSYMBOL_variable_abstract_ptr = 492,    /* variable_abstract_ptr  */
  YYSYMBOL_variable_abstract_array = 493,  /* variable_abstract_array  */
  YYSYMBOL_variable_abstract_function = 494, /* variable_abstract_function  */
  YYSYMBOL_cfa_identifier_parameter_declarator_tuple = 495, /* cfa_identifier_parameter_declarator_tuple  */
  YYSYMBOL_cfa_identifier_parameter_declarator_no_tuple = 496, /* cfa_identifier_parameter_declarator_no_tuple  */
  YYSYMBOL_cfa_identifier_parameter_ptr = 497, /* cfa_identifier_parameter_ptr  */
  YYSYMBOL_cfa_identifier_parameter_array = 498, /* cfa_identifier_parameter_array  */
  YYSYMBOL_cfa_array_parameter_1st_dimension = 499, /* cfa_array_parameter_1st_dimension  */
  YYSYMBOL_cfa_abstract_declarator_tuple = 500, /* cfa_abstract_declarator_tuple  */
  YYSYMBOL_cfa_abstract_declarator_no_tuple = 501, /* cfa_abstract_declarator_no_tuple  */
  YYSYMBOL_cfa_abstract_ptr = 502,         /* cfa_abstract_ptr  */
  YYSYMBOL_cfa_abstract_array = 503,       /* cfa_abstract_array  */
  YYSYMBOL_cfa_abstract_tuple = 504,       /* cfa_abstract_tuple  */
  YYSYMBOL_cfa_abstract_function = 505,    /* cfa_abstract_function  */
  YYSYMBOL_comma_opt = 506,                /* comma_opt  */
  YYSYMBOL_default_initializer_opt = 507   /* default_initializer_opt  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




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

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
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
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
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

#if 1

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
#endif /* 1 */

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
#define YYLAST   32561

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  194
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  314
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1158
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2271

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   421


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   183,     2,     2,     2,   187,   180,     2,
     168,   170,   179,   181,   174,   182,   171,   186,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   175,   193,
     188,   192,   189,   191,   169,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   172,   185,   173,   178,     2,   177,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   176,   190,   167,   184,     2,     2,     2,
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
     165,   166
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   653,   653,   657,   664,   665,   666,   667,   668,   672,
     673,   674,   675,   676,   677,   678,   679,   683,   684,   688,
     689,   694,   695,   696,   700,   704,   705,   716,   718,   720,
     722,   723,   725,   727,   729,   731,   741,   743,   745,   747,
     749,   751,   756,   757,   768,   773,   778,   779,   784,   786,
     788,   794,   796,   798,   800,   802,   822,   825,   827,   829,
     831,   833,   835,   837,   839,   841,   843,   845,   847,   856,
     857,   861,   862,   864,   866,   868,   870,   872,   877,   879,
     881,   887,   888,   896,   899,   900,   902,   907,   923,   925,
     927,   929,   931,   933,   935,   938,   944,   946,   949,   951,
     956,   958,   963,   964,   968,   969,   971,   975,   976,   977,
     978,   982,   983,   985,   987,   989,   991,   993,   995,   997,
    1004,  1005,  1006,  1007,  1011,  1012,  1016,  1017,  1022,  1023,
    1025,  1027,  1032,  1033,  1035,  1040,  1041,  1043,  1048,  1049,
    1051,  1053,  1055,  1060,  1061,  1063,  1068,  1069,  1074,  1075,
    1080,  1081,  1086,  1087,  1092,  1093,  1098,  1099,  1101,  1106,
    1111,  1112,  1116,  1118,  1123,  1126,  1129,  1134,  1135,  1143,
    1149,  1150,  1154,  1155,  1159,  1160,  1164,  1165,  1166,  1167,
    1168,  1169,  1170,  1171,  1172,  1173,  1174,  1180,  1183,  1185,
    1187,  1189,  1194,  1195,  1197,  1199,  1204,  1205,  1211,  1212,
    1218,  1219,  1220,  1221,  1222,  1223,  1224,  1225,  1226,  1227,
    1228,  1229,  1230,  1231,  1233,  1234,  1240,  1242,  1252,  1254,
    1262,  1263,  1268,  1270,  1272,  1274,  1276,  1281,  1283,  1285,
    1291,  1320,  1323,  1325,  1327,  1337,  1339,  1341,  1346,  1351,
    1353,  1355,  1357,  1365,  1366,  1368,  1372,  1374,  1378,  1380,
    1381,  1383,  1385,  1390,  1391,  1395,  1400,  1401,  1405,  1407,
    1412,  1414,  1419,  1421,  1423,  1425,  1430,  1432,  1434,  1436,
    1441,  1443,  1448,  1449,  1471,  1473,  1477,  1480,  1482,  1485,
    1487,  1490,  1492,  1497,  1503,  1505,  1510,  1515,  1517,  1519,
    1521,  1523,  1528,  1530,  1533,  1535,  1540,  1546,  1549,  1552,
    1554,  1559,  1565,  1567,  1572,  1578,  1581,  1583,  1586,  1588,
    1593,  1600,  1603,  1605,  1610,  1616,  1618,  1623,  1629,  1632,
    1636,  1647,  1652,  1657,  1668,  1670,  1672,  1674,  1679,  1681,
    1685,  1687,  1689,  1691,  1696,  1698,  1703,  1705,  1707,  1709,
    1712,  1716,  1719,  1723,  1725,  1727,  1729,  1731,  1733,  1735,
    1737,  1739,  1741,  1743,  1748,  1754,  1762,  1767,  1768,  1772,
    1773,  1778,  1782,  1783,  1786,  1788,  1793,  1796,  1798,  1800,
    1803,  1805,  1810,  1815,  1816,  1820,  1825,  1827,  1832,  1834,
    1839,  1841,  1843,  1848,  1853,  1858,  1863,  1865,  1867,  1872,
    1874,  1880,  1881,  1885,  1886,  1887,  1888,  1892,  1897,  1898,
    1900,  1902,  1904,  1908,  1912,  1913,  1917,  1919,  1921,  1923,
    1925,  1931,  1932,  1938,  1939,  1943,  1944,  1949,  1951,  1960,
    1961,  1963,  1968,  1970,  1978,  1979,  1983,  1985,  1991,  1992,
    1996,  1998,  2002,  2004,  2008,  2009,  2013,  2014,  2018,  2019,
    2020,  2024,  2026,  2041,  2042,  2043,  2044,  2046,  2050,  2052,
    2056,  2063,  2065,  2067,  2069,  2077,  2079,  2084,  2085,  2087,
    2089,  2091,  2101,  2103,  2115,  2118,  2123,  2125,  2131,  2136,
    2141,  2152,  2159,  2164,  2166,  2168,  2174,  2176,  2181,  2183,
    2184,  2185,  2201,  2203,  2206,  2208,  2211,  2216,  2217,  2221,
    2222,  2223,  2224,  2233,  2234,  2235,  2244,  2245,  2246,  2250,
    2251,  2252,  2261,  2262,  2263,  2268,  2269,  2278,  2280,  2285,
    2290,  2292,  2294,  2296,  2303,  2308,  2313,  2314,  2316,  2326,
    2328,  2333,  2335,  2337,  2339,  2341,  2343,  2346,  2348,  2350,
    2355,  2361,  2363,  2365,  2367,  2369,  2371,  2373,  2375,  2377,
    2379,  2381,  2383,  2385,  2387,  2389,  2391,  2394,  2396,  2398,
    2400,  2402,  2404,  2406,  2408,  2410,  2412,  2414,  2416,  2418,
    2420,  2422,  2424,  2426,  2428,  2433,  2434,  2438,  2444,  2445,
    2451,  2452,  2454,  2456,  2458,  2463,  2466,  2468,  2473,  2474,
    2476,  2478,  2483,  2485,  2487,  2489,  2491,  2493,  2498,  2499,
    2501,  2503,  2508,  2510,  2509,  2513,  2521,  2522,  2524,  2526,
    2531,  2532,  2534,  2539,  2541,  2543,  2545,  2550,  2552,  2554,
    2559,  2561,  2563,  2565,  2566,  2568,  2573,  2575,  2577,  2582,
    2583,  2587,  2588,  2595,  2594,  2599,  2598,  2608,  2607,  2618,
    2617,  2627,  2632,  2633,  2638,  2644,  2662,  2663,  2667,  2669,
    2671,  2676,  2678,  2680,  2682,  2687,  2689,  2694,  2696,  2705,
    2706,  2711,  2713,  2718,  2720,  2722,  2731,  2733,  2734,  2735,
    2737,  2739,  2740,  2745,  2746,  2750,  2751,  2756,  2758,  2761,
    2764,  2771,  2772,  2773,  2778,  2783,  2785,  2791,  2792,  2798,
    2799,  2803,  2811,  2818,  2831,  2830,  2834,  2837,  2836,  2845,
    2849,  2853,  2855,  2861,  2862,  2867,  2872,  2881,  2882,  2884,
    2890,  2892,  2897,  2898,  2904,  2905,  2906,  2915,  2916,  2918,
    2919,  2924,  2925,  2927,  2928,  2930,  2932,  2938,  2939,  2941,
    2942,  2943,  2945,  2947,  2954,  2955,  2957,  2959,  2964,  2965,
    2974,  2976,  2981,  2983,  2988,  2989,  2991,  2994,  2996,  3000,
    3001,  3002,  3004,  3006,  3014,  3016,  3021,  3022,  3024,  3028,
    3029,  3031,  3032,  3038,  3039,  3040,  3041,  3045,  3046,  3051,
    3052,  3053,  3054,  3055,  3069,  3070,  3075,  3076,  3081,  3083,
    3085,  3087,  3089,  3112,  3113,  3119,  3120,  3126,  3125,  3130,
    3129,  3133,  3139,  3142,  3152,  3153,  3155,  3159,  3164,  3166,
    3168,  3170,  3176,  3177,  3181,  3182,  3187,  3189,  3196,  3198,
    3199,  3201,  3206,  3208,  3210,  3215,  3217,  3222,  3227,  3235,
    3240,  3242,  3247,  3252,  3253,  3258,  3259,  3263,  3264,  3265,
    3271,  3273,  3275,  3281,  3283,  3289,  3290,  3294,  3296,  3301,
    3305,  3309,  3311,  3323,  3325,  3327,  3329,  3331,  3333,  3335,
    3336,  3341,  3344,  3343,  3355,  3354,  3367,  3366,  3380,  3379,
    3393,  3392,  3405,  3410,  3416,  3418,  3424,  3425,  3436,  3443,
    3448,  3454,  3457,  3460,  3464,  3470,  3473,  3476,  3481,  3482,
    3483,  3484,  3488,  3496,  3497,  3509,  3510,  3514,  3515,  3520,
    3522,  3524,  3526,  3531,  3532,  3538,  3539,  3541,  3546,  3547,
    3549,  3584,  3586,  3589,  3594,  3596,  3597,  3599,  3604,  3606,
    3608,  3610,  3612,  3617,  3619,  3621,  3623,  3625,  3627,  3629,
    3634,  3636,  3638,  3640,  3649,  3651,  3652,  3657,  3659,  3661,
    3663,  3665,  3670,  3672,  3674,  3676,  3678,  3683,  3685,  3687,
    3689,  3691,  3693,  3705,  3706,  3707,  3711,  3713,  3715,  3717,
    3719,  3724,  3726,  3728,  3730,  3732,  3737,  3739,  3741,  3743,
    3745,  3747,  3759,  3764,  3769,  3771,  3772,  3774,  3779,  3781,
    3783,  3785,  3787,  3792,  3794,  3796,  3798,  3800,  3802,  3804,
    3809,  3811,  3813,  3815,  3824,  3826,  3827,  3832,  3834,  3836,
    3838,  3840,  3845,  3847,  3849,  3851,  3853,  3858,  3860,  3862,
    3864,  3866,  3868,  3878,  3880,  3883,  3884,  3886,  3891,  3893,
    3895,  3897,  3902,  3904,  3906,  3908,  3913,  3915,  3917,  3931,
    3933,  3936,  3937,  3939,  3944,  3946,  3951,  3953,  3955,  3957,
    3962,  3964,  3969,  3971,  3988,  3989,  3991,  3996,  3998,  4000,
    4002,  4004,  4006,  4011,  4012,  4014,  4016,  4021,  4023,  4025,
    4031,  4033,  4036,  4039,  4046,  4048,  4057,  4059,  4061,  4062,
    4064,  4066,  4070,  4072,  4077,  4079,  4081,  4083,  4118,  4119,
    4123,  4124,  4127,  4129,  4134,  4136,  4138,  4140,  4142,  4147,
    4148,  4150,  4152,  4157,  4159,  4161,  4167,  4168,  4170,  4179,
    4182,  4184,  4187,  4189,  4191,  4205,  4206,  4208,  4213,  4215,
    4217,  4219,  4221,  4226,  4227,  4229,  4231,  4236,  4238,  4246,
    4247,  4248,  4253,  4254,  4255,  4261,  4263,  4265,  4267,  4269,
    4271,  4273,  4280,  4282,  4284,  4286,  4288,  4290,  4292,  4294,
    4296,  4298,  4301,  4303,  4305,  4307,  4309,  4314,  4316,  4318,
    4323,  4349,  4350,  4352,  4356,  4357,  4361,  4363,  4365,  4367,
    4369,  4371,  4373,  4380,  4382,  4384,  4386,  4388,  4390,  4395,
    4397,  4399,  4404,  4406,  4408,  4426,  4428,  4433,  4434
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TYPEDEF", "EXTERN",
  "STATIC", "AUTO", "REGISTER", "THREADLOCALGCC", "THREADLOCALC11",
  "INLINE", "FORTRAN", "NORETURN", "CONST", "VOLATILE", "RESTRICT",
  "ATOMIC", "FORALL", "MUTEX", "VIRTUAL", "VTABLE", "COERCE", "VOID",
  "CHAR", "SHORT", "INT", "LONG", "FLOAT", "DOUBLE", "SIGNED", "UNSIGNED",
  "BOOL", "COMPLEX", "IMAGINARY", "INT128", "UINT128", "FLOAT80",
  "uuFLOAT128", "FLOAT16", "FLOAT32", "FLOAT32X", "FLOAT64", "FLOAT64X",
  "FLOAT128", "FLOAT128X", "FLOAT32X4", "FLOAT64X2", "SVFLOAT32",
  "SVFLOAT64", "SVBOOL", "DECIMAL32", "DECIMAL64", "DECIMAL128", "ZERO_T",
  "ONE_T", "SIZEOF", "TYPEOF", "VA_LIST", "VA_ARG", "AUTO_TYPE", "COUNTOF",
  "OFFSETOF", "BASETYPEOF", "TYPEID", "ENUM", "STRUCT", "UNION",
  "EXCEPTION", "GENERATOR", "COROUTINE", "MONITOR", "THREAD", "OTYPE",
  "FTYPE", "DTYPE", "TTYPE", "TRAIT", "LABEL", "SUSPEND", "ATTRIBUTE",
  "EXTENSION", "IF", "ELSE", "SWITCH", "CASE", "DEFAULT", "DO", "WHILE",
  "FOR", "BREAK", "CONTINUE", "GOTO", "RETURN", "CHOOSE", "FALLTHROUGH",
  "WITH", "WHEN", "WAITFOR", "WAITUNTIL", "CORUN", "COFOR", "DISABLE",
  "ENABLE", "TRY", "THROW", "THROWRESUME", "AT", "ASM", "ALIGNAS",
  "ALIGNOF", "__ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER",
  "TYPEDIMname", "TYPEDEFname", "TYPEGENname", "TIMEOUT", "WAND", "WOR",
  "CATCH", "RECOVER", "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
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
  "fraction_constants_opt", "unary_expression", "alignof_operator",
  "ptrref_operator", "unary_operator", "cast_expression",
  "qualifier_cast_list", "cast_modifier", "exponential_expression",
  "multiplicative_expression", "additive_expression", "shift_expression",
  "relational_expression", "equality_expression", "AND_expression",
  "exclusive_OR_expression", "inclusive_OR_expression",
  "logical_AND_expression", "logical_OR_expression",
  "conditional_expression", "constant_expression",
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

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1985)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1157)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     100,   -59, -1985,  1941,   115,   166, -1985,    94, -1985,  1330,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985, -1985, -1985,   742, -1985,   -22, -1985,
   -1985, 12713, -1985,  1941,   717, -1985,  1941,  7986, 12713,  2973,
      64, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985,    63,  1391,   168, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,    40,
     104, -1985, -1985, -1985, -1985, -1985, -1985,  6468,  6468, 12713,
     186,   258, 32385, -1985,   287, -1985, -1985,  3894, -1985,   885,
   15994, -1985, -1985,  3907, -1985, -1985, -1985, 18995, -1985,   268,
     305,   109,   428,    46, -1985,  6711,   357,   366,   410,   375,
    7132,   265,   918, 13603,    94, -1985,   520, 19335,  2284,    94,
   -1985, -1985, -1985,  3379,   604,  9311, 13524,  1715,  3379,  1284,
     459, -1985, -1985, -1985, -1985,    94, -1985, -1985, -1985, -1985,
     471, -1985, -1985, -1985, -1985,   516,   517,    94, -1985,    94,
   23045, -1985, -1985, -1985, 26919,  6468, -1985, -1985,  6468,   748,
   -1985, -1985, 31736,   576, 31814,   600,   671, 31892, -1985, -1985,
     684, 32445, -1985, -1985, -1985, -1985, -1985, -1985, -1985, 31970,
   31970, 23381, 11849,  5703, -1985, -1985, -1985, -1985,  3907,    58,
   -1985,   185,   743, -1985,  2139,  6346, 32048, 31892, 31892, -1985,
     598,   552,   840,   891,   302,   979,   701,   728,   706,   783,
     -44, -1985,   760,   758, -1985, -1985, -1985,   770, -1985,   765,
   26979,   784,  3718, -1985, -1985, -1985, -1985,   233, 21151,    94,
    3937, -1985, -1985,   800, -1985,   792,   809, -1985,   858, 31892,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985, 23553,  4533,
    3894,   157,   817,   820,   828,   843,   860,   863, -1985, -1985,
      94, 14908, 25572,   874, 23897,   883, -1985,  5735,  5400,   928,
   21859, 18215,  3379,  3379,   933,  3379,  1219,  3379,  1256,   770,
   -1985, -1985,    94, -1985,   975,   999, -1985, -1985, -1985, -1985,
   27147,  6468, -1985, -1985, 27207,  6115, -1985, -1985, 16175,   912,
   -1985, 19675, -1985,  1893,  1284, 19165, -1985, -1985, 27375, -1985,
   -1985,   917, -1985, -1985, -1985,   914, -1985, 11482,  1092, 29625,
   -1985,   929,  6468,   517,   949,   958, -1985,    94,    94,  3907,
   -1985, -1985, -1985,  3818,  4136,   960,  1050,   450,  1050, -1985,
      94,    94,   -17, 22815,   597,  1050, -1985,    94,    94,   -17,
      94, -1985,    94, -1985,  5830, -1985, -1985,   996,  1013,  1284,
   18383, 21328, 18995, -1985,  6711,    94,  3379, -1985,  1912,   459,
    1008,  1094, 22815,  6468, -1985,  6468,   428, -1985, 14168, -1985,
    1893,  1284,  1019,  1094, 22815,  6468,    94, -1985,  8357, -1985,
   -1985, -1985, -1985,  1893, -1985, -1985, -1985, -1985,  1284, -1985,
    1205,  1126,  5195,  6468, -1985, 25077,  1047, -1985, -1985, -1985,
    2973,   517, 22930,  1023,  7231, 25017, 18383, 17080, 25959, -1985,
   28570, -1985,  6468,  1050,    30,  1049, 23725, -1985,  5703, 24241,
   -1985, 27435, 25959, -1985, -1985, 31892, -1985, -1985, -1985, -1985,
   -1985, -1985, 24241, -1985, -1985, 26463, 27435, 27435, 15089,  1666,
    2126, 24069,   655,  2233, -1985,   893,  1052,   394, 28570,  1132,
    1057, -1985, -1985,  1102,  1105,  1107, 29703,  1116,  1125, 31892,
    3907, 31892,  3907, -1985, -1985,  3153, -1985, -1985,  7986,  2834,
   29781,  7986,  3907, -1985, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985,  1143, 31892, -1985, -1985, 24413, -1985,
   -1985, -1985, 31892, 31892, 31892, 31892, 31892, 31892, 31892, 31892,
   31892, 31892, 31892, 31892, 31892, 31892, 31892, 31892, 31892, 31892,
   31892, 29859, -1985,  7986,  5105, -1985, 31892, -1985, -1985,  7231,
   28620, -1985,  1173,  1176, -1985, -1985, -1985, -1985,  6468,  4917,
     373,   896, -1985,  6468,   792, -1985,  1090, -1985, 16356, 26127,
    1148, 21859, -1985,  1893,  1284,  1181, -1985,   917,  3429,   542,
     466, -1985,   719,   459,  1190,    94,  3718,  1187,   792,  3718,
    1211, -1985,   919, -1985, 15270, -1985, -1985, -1985,   926, 25959,
   -1985,  4321,  3894,  1214,  1221,  1235,  1248,  1250,  1261, -1985,
   -1985,   834,  1266, -1985,   971,  1266,  5612, 25236,  1168, 17261,
   24585,  1287, 32126,  1290, -1985, 28781, 27375, -1985, -1985, 15451,
   -1985, 27603, -1985,  1893,  1893, 29161, -1985, -1985,   917, -1985,
   -1985, -1985, 13980, 29937,  1446, 31892,  4770,   847,  1279, -1985,
      94,    94,  1279,   880, -1985,    94,    94,  1304,  1279, -1985,
      94,    94, -1985,  1266, -1985, 30015, 16537, 27663, -1985, -1985,
    6468, 25899,  1893,  1893, -1985, -1985,  5612, -1985, 22036, -1985,
   22036, -1985, 28409, -1985, -1985,  1279, 17442, -1985, 27147, -1985,
   -1985, -1985,   137, 26523, 22213, -1985, -1985, -1985, -1985, -1985,
   29461, -1985, -1985, 32204, -1985, 22451,  5361, 11849, 11482,  1319,
    1322, -1985, -1985,  1331, 29625,   536, -1985, -1985, -1985, 24069,
    1348, -1985,   858, -1985,  3907,  7231,  1326,  3818,   718,  1356,
    1359,  1371,   768,  1398,  1404,  1408,  1416,  1418,  1434,  6532,
    3818, -1985, -1985, -1985,    94,  1362, 28824, -1985, -1985,  1304,
     428, -1985, -1985,   517,  1094, 25413, -1985, -1985,   428, -1985,
   -1985,   517, -1985, -1985,  4440,  6209,  5830, -1985,   907, -1985,
   -1985, -1985, -1985, 24069, 24069, -1985,  1893,    94,  7231, 16718,
    2401, -1985, -1985, -1985, -1985, -1985, -1985, -1985,   517,  1094,
    1450,  1438, -1985, -1985,  3379,  1444,  1094, 22815, -1985, -1985,
     517,  1094, -1985, -1985, 13842, -1985, -1985,  1893,  1893, -1985,
   -1985, -1985,   169,   829,   169,   459,  1449, -1985, -1985, -1985,
   25899,  1462,  1460, -1985, -1985,   970, 25749, -1985,  1562, 18383,
   -1985,  1453, -1985, -1985, -1985, 27840,  1470,  1475,  1238, 29311,
   27893,  6468,  1050, -1985, -1985, -1985, -1985,  1489, 26295,  1492,
    1490,  1498, 16899,  1503,  1516,  1515,  1556, 31892,  1563,  1568,
    1573, 27953, 31892, -1985, -1985,  2445, -1985, -1985, -1985, 31892,
   -1985, 20620,  2285, -1985, -1985,    94,    94, -1985,  1582,  1595,
   29469, 29781,  1535, -1985, 29547,  7986, 31892,  1589, -1985,  1594,
   -1985, -1985,  5965, -1985,  1603, -1985,  5965, -1985, -1985, -1985,
   -1985,  1318,  1608, -1985, 11482, -1985,  1614,  1625, -1985,   598,
     598,   598,   552,   552,   840,   840,   891,   891,   891,   891,
     302,   302,   979,   701,   728,   706,   783, 31892,  1158, -1985,
    5965, -1985, -1985, -1985, 22036,  1050, 12713, -1985,  6468,  1626,
    9680,  1640, -1985, -1985, -1985, -1985, -1985,  3718, -1985, -1985,
    1730, 26691, 20797,  1799,  2554, -1985, -1985,    94,  1642,    94,
   -1985,    75,  1636,  1032, 25959,  1033,  1629, -1985,   858, -1985,
   24069, -1985, -1985, -1985,   915,  1266, -1985,  1043,  1266, 25413,
   -1985, -1985,  1304, 25413, -1985,  1304, -1985, 20974, -1985, 27147,
   -1985, -1985, 15632,  1650, 24757,  1651,   770,  1652, 17623, -1985,
   -1985, -1985, -1985, -1985, -1985, 18215, 20974, 13980,  1657,   936,
    1658,  1659,  1663,  1667,  1670,  1671,  1672, -1985,  2063,  5152,
   -1985,  3166, -1985,  4686, -1985, -1985, -1985, 25413, -1985, -1985,
   -1985, -1985, -1985, -1985, 25413, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985,  1304, -1985,  1674, 27207, 17261, -1985, -1985, -1985,
    1279,  1893, -1985,  1372, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985, 20974, -1985,  6468,  5965, -1985,  1662,   193,  1675,  1331,
   -1985, 11482,  1677, -1985,  3469, 31892, -1985, -1985,  1054, -1985,
    1679, 20974, -1985, -1985, 31892,  1081,  1682,  1686,  1693,  1095,
    1697,  1698,  1700,  1701,  1704,  1705,   974,  1266, -1985, -1985,
    1186,  1266, -1985, -1985,  1403,  1266, -1985, -1985, -1985, -1985,
   -1985, -1985,  7231,  1851,  1266,   994, -1985,   770,  1376, -1985,
   -1985,   517,  1708, -1985, -1985,  4440,  1137,  5830,  4440, -1985,
   25413,  1073,  1710,  1117,  1711, -1985,  1287,   488, -1985,   517,
   18155, -1985,   517,  1094,   488, -1985,   517, -1985, -1985, -1985,
   -1985, -1985,   264, -1985,  3907, -1985, -1985,  6468,    94,  1807,
   -1985, -1985, -1985, 22213,  1050, -1985, 20974,   513,  1719, -1985,
   25899,   513,  3907, -1985, 26751,   513, -1985, 31892, 31892, 31892,
   -1985, -1985, -1985, -1985,  1721,  1723,  1729,  1731,  2145, -1985,
    1227, -1985, -1985, -1985, 31892, 31892,  1709, 11482, -1985,  1726,
   -1985, -1985,  1726,  1735, -1985, -1985, -1985, -1985,  4504, -1985,
   -1985,  1384, -1985,     6, -1985,  1387, -1985, 30093, -1985,  1331,
     513, -1985, -1985, 31892,  1392,  1739, -1985,   488,  1752,   792,
   -1985, -1985, -1985,  7231, 28121, 18323, -1985,   -11,   236, 24069,
    1740, -1985,  1740, -1985, -1985,    94,  2363, -1985,    75,  1636,
    1636,   233, -1985, -1985,  1753,  6468,  1747, -1985, -1985,  1757,
   -1985,  1764, -1985, -1985, 25413, -1985, -1985,  1304, 25413, -1985,
    1304,  1766,  1769, -1985,  1772,  1773,  1774, -1985, -1985, -1985,
   -1985, -1985, -1985,  1770, 20974, 20974, -1985,  1779, -1985,  1443,
    1266, -1985,  1497,  1506,  1266, -1985,  1893, 10673,  2659, -1985,
      94,    94, -1985, -1985, -1985,  3596,  1939,  6241, -1985, -1985,
    1788,  1800, -1985, -1985, -1985, 22036, -1985,   428,  1421, 31892,
   -1985, 31892, -1985,  1804, -1985, 29625, -1985,    94, 20974,    94,
   -1985, -1985,  1531,  1266, -1985,  1592,  1266, -1985, -1985,  1615,
    1266, 25413, -1985, -1985,  1304, 25413, -1985, -1985,  1304, 25413,
   -1985, -1985,  1304,  1050, -1985,  1304, -1985, 30171, -1985, 31892,
   -1985, 28989, -1985, -1985,  1166, -1985, -1985, -1985, -1985, -1985,
     727, -1985, -1985, 18491,   488, -1985,   517, -1985, -1985,  1797,
    1808,  1810,   983, -1985, 25899, -1985, -1985,   277,   986, -1985,
   12952,  6468, -1985, -1985, -1985,  1018,  1818,  1815,  1212, -1985,
    1817, -1985, -1985, -1985, -1985,  1617,  1266, -1985, -1985, -1985,
   -1985, -1985, 11482,  1331, 30093,  1820,  1823, -1985,  1860,  5965,
   -1985,  1860,  1860, -1985,  5965,  4849,  5319, -1985, -1985, -1985,
    1830, -1985, -1985, -1985, -1985,  6468, -1985, -1985, -1985, -1985,
    6468, -1985,  7231, -1985,  1220, 25959,   792,   792,  1636,  1753,
    1824,  1826,   459,   119,  1832,  1812,    75, 18659, -1985,  1836,
    1839, -1985, -1985, -1985, 21505, 21682, -1985, -1985, -1985,  1841,
      94, 25413, -1985, -1985,  1304, 25413, -1985, -1985, 25413, -1985,
   -1985,  1304, 31892, 31892,  1843,  1845, -1985,  1842, -1985, -1985,
    3596,  4116,  6626,  4686, -1985, -1985, -1985,  1849, -1985, -1985,
    1847, -1985, -1985, -1985, -1985, -1985, -1985,  1852, 25413, -1985,
   -1985,  1304, 25413, -1985, -1985,  1304, 25413, -1985, -1985,  1304,
    1855,  1856,  1857,   428, -1985,  1424, -1985,   120, -1985,   770,
    1854, -1985, -1985, -1985,  1863, -1985, -1985, -1985,  1864, 20213,
   -1985, -1985,  6468, -1985,  1866, -1985,   582,   388, 14727,  1868,
    1871, 22624,  1873,  1875,  3286,  3797,  4408, 30249,  1880,  3575,
    1881,  1882, 22624,  1884, -1985, -1985,   517, 31892, 31892,  2014,
    1878,   656, -1985, 23209, 15813,  1895,  1898,  1838, -1985, -1985,
   -1985, -1985, -1985, -1985, -1985, -1985, -1985, -1985,  1505,   204,
   -1985,   222, -1985,   204, -1985, -1985, -1985, -1985, -1985,  3907,
   -1985, -1985, 13792, 19505, -1985,   278,  1885,  1914, -1985, -1985,
   31892, -1985, 26751, 31892, 25413, -1985, -1985,  1304,  1331,  1920,
   -1985, -1985, -1985,  1439, -1985,  5965, -1985,  5965, -1985, -1985,
    1921,   373, -1985, -1985, -1985, -1985, -1985, -1985,  1915,  1923,
      75,    75,   233,  6468,    94, 30327, -1985,  1753, -1985, 18827,
   -1985, -1985, -1985,  1919, -1985,  1924,  1928, -1985,  1930,  1932,
    1933, -1985, -1985,  1931, -1985,  1935, -1985, -1985,  1942,    94,
    1948,  1950,  1952, -1985, -1985, -1985, -1985, -1985, 31892, -1985,
    1938, -1985,  1159,  1195,  1273, 24069,    94,    94, 18383,    94,
   27435,  1918,   356,   482,  2801, 20443, -1985,   485,  6468, -1985,
   -1985,  7986,   225,   487, -1985, -1985, -1985, -1985, 14727, 31892,
    1955,  2038, 14545, 13143, -1985,  1934, -1985,  1943, 31892,  1954,
   11482,  1956, 31892,  1958, -1985,  1960, 24069, 31892, -1985, 13334,
    1840, -1985,  1961,    14, -1985,     2,  2028,   400,    94, -1985,
    1967,  1977, 22624, 22624, -1985, -1985,  2032, -1985, -1985,    29,
      29,   293, 14356, -1985,    94, -1985, -1985, -1985, -1985,  1978,
    1989, -1985, -1985,  1457,  1493, -1985,  1740,    75,    94,  1753,
    1753,   459,  1812, -1985, 11482, -1985,  1997, -1985,    94,    94,
   -1985, -1985, -1985,  1992,  1993, -1985, -1985, -1985, -1985, -1985,
   -1985, -1985, -1985, -1985,  1864,  1864,  1864,  1222, -1985,  6732,
   27435,  6732,   524, -1985, -1985, -1985,  5066, 31892,  6453,   365,
   -1985, -1985, -1985,   548,  1994,  1994,  1994,  6468, -1985, -1985,
    2000, -1985, -1985, -1985, -1985,  1898,  2001, 31892,   305,  1998,
     375, 20391, 26979,  1240,  2006, 22624,  2010, -1985, -1985, -1985,
   -1985,  1620, 22624, 31892,  1703,    90, -1985, 31892, 11332, -1985,
   -1985,   530, -1985,  1331, -1985,  1260, -1985, -1985,  1299,  1300,
     675, -1985, -1985, -1985, -1985,   517,  1840,  2013, -1985, -1985,
   31892, -1985,  2015,   858, -1985, 12489, 31892, 31892, -1985, -1985,
     194,    29, -1985,    54, -1985, -1985, -1985, -1985, -1985, -1985,
   -1985,   792,  1753, -1985,  2017,  2019, -1985,  1331,    94, -1985,
   -1985, -1985, -1985,    94,    94,    94, -1985,   540,  1247,  1995,
     571, -1985,   578, -1985,  5066,   406, -1985,  6986,  5066, -1985,
      94, -1985, -1985, -1985, -1985, -1985, -1985, 22624, 22624,  1898,
   22390,    96, 30405,  2111, 22624, -1985, 31892, -1985, 30483,  2113,
    2003, 24835, 30561, 22624, 13334,  1898,   630,  4747,  2005, 31892,
   -1985,  2033,   235, 22624, -1985, 22624, -1985,  2031, -1985, 28181,
    2012,   858,   683, -1985, -1985,  2036,  1527,  1333, 22624,  2035,
   22624, 22624, 22624, -1985,  2040,    94,    94,  2043, -1985, -1985,
   -1985, -1985, -1985,  1247,  2657,   592, -1985, -1985, -1985, -1985,
      94,    94, -1985, -1985, -1985, -1985,  2039,  6732, -1985,  2132,
    5860,    62, 17807, -1985, 22493, -1985,    -5,  1334, 22624,  2133,
     608,  2041,   147, 22624, 31892,   630,  4747,  2023, 30644,   939,
    1893,  2042,   602,  2138, -1985, 30722, -1985, -1985, -1985, -1985,
   30800, 31892, 31892,  1898,  2030, 17987, -1985, -1985, -1985, 28181,
    2037,  6944, 28349,  3907, -1985,  2053,  2044,   -14, -1985, 31892,
    7986, -1985, -1985, 31892,   204, -1985, -1985,    94, -1985, -1985,
   -1985,  2059,  2061,  2065,  2847, -1985, -1985,    94, -1985, -1985,
   -1985, -1985, 22624, -1985,   -28, -1985,    88, -1985, -1985, -1985,
    2071,  1357, -1985, -1985, 22624, -1985,    10, -1985, 22624, 31892,
    2072, 30878, -1985, -1985, 30956, 31034, 31892, 31892,  5612,  1898,
   -1985,   770, 31112, 31190, 22624,  2056,   620,  2068,   623,  1898,
   -1985, -1985,  2087,  1357,  2037, 31892,  2085,  3498,  6036, -1985,
   -1985, -1985,  2083, -1985,  2146,  2092,   689,  2088, -1985, -1985,
    2091,  1340,   442, -1985,  1687,  1266, -1985, -1985,  1247, -1985,
   31892, -1985, 31892, -1985, -1985,  1622, 19864, 20042, -1985, 22624,
   -1985, -1985,  1898, -1985, -1985,  1898,  2080,   626,  2094,   699,
    1898, -1985, -1985,   459, -1985,  1898, -1985,  1898, -1985,  2102,
   31268, 31346, 31424, -1985,  1622,  2105, -1985,   517,  6493,  4440,
     -14,  2103, 31892,  2086,   -14,   -14, -1985, -1985, 22624,  2198,
   25413, -1985, -1985,  1304, -1985, -1985, -1985,   674, -1985,  1622,
   -1985, -1985, -1985,  2112, 31502, 31580, 31658, -1985, -1985,  1898,
   -1985,  1898, -1985,  1898, -1985,   517, -1985,  2108,   858,  2115,
   -1985,   702, -1985, -1985, 22624,  2127, 12200, 22624,  2136,   674,
   -1985, -1985,  1898, -1985,  1898, -1985,  1898,  2137, -1985,   858,
    2140, -1985,  2116,   858, -1985, -1985, -1985, 22624, -1985, -1985,
   12347, -1985, -1985,  1538, 31892, -1985,  1343, -1985,   858,  6468,
    2144,  2129, -1985, -1985,  1353, -1985, -1985,  2134,  6468, -1985,
   -1985
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     875,     0,   882,   885,     0,   875,     2,   876,   877,   885,
     890,   889,    17,    22,    23,     9,    10,    11,    12,    13,
      14,    15,    16,    18,    21,   888,     0,   883,   886,     1,
       2,   505,   878,   885,     0,   881,   885,   160,   505,   875,
     521,   522,   523,   524,   525,   526,   527,   528,   529,   510,
     512,   511,   513,     0,     0,     0,   531,   533,   560,   534,
     561,   537,   538,   558,   559,   532,   556,   557,   535,   536,
     539,   540,   541,   542,   543,   544,   545,   546,   547,   548,
     549,   550,   551,   552,   553,   554,   555,   562,   563,   875,
     565,   639,   640,   643,   645,   641,   647,     0,     0,   505,
       0,     0,    17,   610,   616,   831,   106,     0,    20,     0,
     505,   104,   105,     0,   852,    19,   891,   505,   832,     0,
       0,   443,   753,   445,   457,   873,   444,   479,   480,     0,
       0,     0,     0,   593,   875,   509,   514,   505,   516,   875,
     578,   530,   564,   489,   570,   875,   491,   588,   490,   875,
     607,   613,   592,   619,   631,   875,   636,   637,   620,   690,
     446,   447,     3,   839,   853,     0,     0,   875,   915,   875,
     505,   933,   934,   935,   505,     0,  1134,  1135,     0,     0,
     880,   884,     0,     0,     0,     0,     0,     0,   102,   103,
       0,    27,    29,     4,     8,    25,     5,     6,     7,     0,
       0,   505,     0,     0,   107,   108,   109,   110,   164,    84,
      28,    85,    24,    46,    83,   111,     0,     0,     0,   126,
     128,   132,   135,   138,   143,   146,   148,   150,   152,   154,
     156,   167,     0,   161,   162,   166,    30,     0,     3,     0,
     505,   842,     0,   642,   644,   646,   648,     0,   505,   875,
     693,   638,   566,   807,   802,   792,     0,   840,     0,     0,
     521,   833,   837,   838,   834,   514,   835,   836,   505,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   611,   614,
     875,   505,   505,   104,   505,     0,   738,     0,  1157,     0,
     506,   505,   516,   496,   570,   497,   596,   498,   875,   607,
     600,   621,   875,   622,     0,     0,   734,   739,   724,   728,
     505,   740,  1102,  1103,   505,   741,   743,   892,   505,     0,
    1136,   593,   499,   500,   875,   505,   922,   941,   505,  1141,
    1133,  1131,  1139,   440,   439,     0,   175,   759,   174,     0,
     448,     0,     0,     0,     0,     0,   455,   875,   875,     0,
     438,  1014,  1015,     0,     0,   478,   873,   875,   873,   895,
     875,   875,   488,   505,   875,   873,   955,   875,   875,   487,
     875,   975,   875,   952,     0,   586,   587,     0,     0,   505,
     505,   505,   505,   458,   873,   875,   517,   579,     0,   608,
       0,   856,   505,     0,   507,     0,   753,   459,   593,   571,
     589,   875,     0,   856,   505,     0,   875,   519,   875,   580,
     581,   575,   492,   590,   494,   495,   493,   595,   875,   609,
     603,     0,   623,     0,   827,   505,     2,   854,   914,   916,
     875,     0,   505,     0,     0,   593,   505,   505,   505,  1145,
     593,  1148,     0,   873,   873,     0,   505,    91,     0,   505,
     100,   505,   505,   111,    86,     0,    36,    40,    41,    37,
      38,    39,   505,    89,    90,   505,   505,   505,   505,   107,
     108,   505,     0,     0,   196,     0,     0,   746,   593,   637,
       0,   748,  1131,  1155,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    26,    58,     0,    64,    65,   160,     0,
       0,   160,     0,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   185,   186,   174,     0,   172,   173,   505,    94,
      87,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   887,     0,     0,   828,     0,   468,   469,     0,
     593,   829,     0,     0,   788,   790,   789,   791,     0,     0,
     784,     0,   773,     0,   782,   794,     0,   691,   505,   505,
    1157,   506,   570,   596,   875,     0,   740,   741,   693,   610,
     616,   694,   695,   696,     0,   875,     0,   805,   793,     0,
       0,   159,     0,   617,   505,   799,   749,   798,     0,   505,
     751,     0,     0,     0,     0,     0,     0,     0,     0,   893,
     920,   875,   931,   939,   944,   950,     0,   505,     0,   506,
     505,   610,     0,     0,  1143,   593,   505,  1146,  1055,   505,
    1105,   506,   502,   503,   504,   505,  1110,  1099,  1100,  1108,
    1054,     2,   505,     2,   105,     0,   875,   875,  1157,   995,
     875,   875,  1157,   875,  1011,   875,   875,  1078,  1157,  1060,
     875,   875,  1069,  1076,   732,     0,   505,   505,   601,  1104,
     742,   506,   597,   598,   602,   603,     0,   466,   505,  1149,
     505,  1120,   506,  1126,  1121,  1157,   505,  1114,   505,  1123,
    1115,     2,  1157,   505,   505,   924,   943,  1132,   501,  1137,
     593,   923,   942,     0,     2,    27,     0,     0,   759,    28,
       0,   757,   760,  1155,     0,     0,   766,   755,   754,   505,
       0,   858,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   898,   958,   982,   875,   484,     0,   894,   903,  1045,
     753,   896,   897,     0,   856,   505,   954,   963,   753,   956,
     957,     0,   974,   976,     0,     0,     0,   474,   875,   868,
     870,   869,   871,   505,   505,   577,   506,   576,     0,   505,
       0,  1138,  1142,  1140,   456,   508,   594,   829,     0,   856,
       0,     0,   449,   460,   518,     0,   856,   505,   604,   829,
       0,   856,   803,   520,   573,   574,   572,   591,   606,   605,
     612,   615,   610,   616,   634,   635,     0,   804,   708,   744,
     506,     0,   709,   711,   713,     0,   505,   218,   432,   505,
     855,     0,   430,   488,   487,   593,   104,     0,     0,   505,
     505,     0,   873,   451,     2,   452,   879,     0,   505,     0,
       0,     0,   505,     0,     0,     0,     0,     0,     0,     0,
       0,   505,     0,   125,   124,     0,   121,   120,    31,     0,
      32,   505,   875,   747,  1024,   875,   875,  1033,     0,     0,
       0,  1156,     0,   187,     0,   160,     0,     0,    54,     0,
      55,    62,     0,    61,     0,    57,     0,    56,    60,   193,
     192,     0,     0,    53,   759,   168,     0,     0,   127,   129,
     130,   131,   133,   134,   136,   137,   141,   142,   139,   140,
     144,   145,   147,   149,   151,   153,   155,     0,     0,   163,
       0,    33,   476,   471,   505,   873,   505,   829,     0,     0,
       0,     0,   787,   786,   785,   779,   515,     0,   777,   795,
     568,   505,   505,   105,   875,   742,   692,   875,     0,   875,
     684,   693,   693,     0,   505,     0,     0,   442,     0,   618,
     505,   750,   752,   921,   875,   932,   940,   945,   951,   505,
     925,   927,   929,   505,   946,   948,   695,   505,  1112,   505,
    1122,  1113,   505,   104,   505,     0,   608,     0,   506,     2,
       2,  1144,  1147,  1101,  1106,   506,   505,   505,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1079,     0,   875,
    1158,  1065,  1064,   876,   998,  1016,  1066,   505,   993,  1002,
     730,   996,   997,   731,   505,  1009,  1020,  1012,  1013,   733,
    1062,  1063,  1077,  1150,     0,   505,   506,  1107,  1111,  1109,
    1157,   599,   634,     0,   726,   725,   729,   735,  1118,  1125,
    1119,   505,   736,     0,     0,   768,   159,     0,     0,  1155,
     765,  1156,     0,   761,     0,     0,   764,   767,     0,     2,
       0,   505,   470,   472,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   875,   908,   912,   953,
     875,   968,   972,   980,   875,   991,   900,   960,   984,   899,
     959,   983,     0,     0,  1040,     0,  1046,  1047,     0,   482,
     859,     0,     0,   483,   860,     0,     0,     0,     0,   475,
     505,     0,     0,     0,     0,   473,     0,   875,   861,     0,
       0,   829,     0,   856,   875,   862,     0,   627,   629,   625,
     649,   917,   875,   936,     0,   712,   714,     0,   875,   433,
     431,  1057,  1056,   505,   873,   453,   505,    92,     0,    96,
     505,   101,     0,    99,   505,     0,   115,     0,     0,     0,
     119,   123,   122,   197,     0,     0,     0,     0,  1028,  1027,
     876,  1029,  1025,  1026,     0,     0,     0,   759,   112,  1155,
     189,   188,  1155,     0,   165,    48,    49,    81,     0,    81,
      81,     0,    69,    71,    51,     0,    47,     0,    50,  1155,
      95,    97,   158,     0,     0,     0,   830,   875,     0,   792,
     822,   817,   818,     0,   506,     0,   813,     0,     0,   505,
     775,   774,   775,   569,   567,   875,  1065,   687,   693,   693,
     693,     0,   703,   702,  1155,     0,     0,   808,   806,     0,
     841,     0,   801,   800,   505,   926,   928,   930,   505,   947,
     949,     0,     0,   718,     0,   719,   720,  1116,  1124,  1117,
    1127,  1128,  1129,     0,   505,   505,     3,     0,  1073,   875,
    1005,  1008,   875,   875,  1072,  1075,   505,     3,     0,  1061,
     875,   875,  1000,  1018,  1067,     0,   105,     0,   999,  1017,
       0,     0,  1151,   737,   467,   505,     3,   753,     0,     0,
     769,     0,   770,     0,   762,     0,   756,   875,   505,   875,
       3,   477,   875,   909,   913,   875,   969,   973,   981,   875,
     992,   505,   901,   904,   906,   505,   961,   964,   966,   505,
     985,   987,   989,   873,   485,  1041,  1053,     0,  1052,     0,
    1044,     0,   864,   977,     0,   583,   582,   585,   584,   830,
     875,   865,   810,     0,   875,   863,     0,   830,   866,     0,
       0,     0,   875,   710,   505,   745,   436,     0,   875,   220,
     505,     0,   454,     3,    93,  1058,     0,     0,     0,    42,
       0,   116,   118,   117,  1037,   875,  1036,  1039,  1031,  1030,
     114,   113,   759,  1155,  1156,     0,     0,    68,    78,     0,
      72,    79,    80,    63,     0,     0,     0,    59,   195,   194,
       0,   157,    34,   843,   830,     0,   781,   820,   797,   814,
       0,   815,     0,   816,     0,   505,   792,   792,   693,  1155,
       0,     0,   699,   693,     0,   704,   693,     0,   441,     0,
       0,   918,   937,  1152,   505,   505,  1130,     3,     3,     0,
     875,   505,  1001,  1003,  1004,   505,  1019,  1021,   505,  1068,
    1070,  1071,     0,     0,   104,     0,     3,     0,   994,  1010,
       0,     0,     0,     0,  1006,  1022,   727,     0,   450,   772,
       0,   872,   758,   763,   857,     3,   874,     0,   505,   902,
     905,   907,   505,   962,   965,   967,   505,   986,   988,   990,
       0,     0,     0,   753,  1042,     0,  1048,     0,  1049,  1050,
       0,   812,   830,   867,     0,   649,   649,   649,   632,   505,
     715,   716,     0,   434,     0,   221,     0,     0,   505,     0,
       0,   357,     0,     0,     0,     0,     0,   198,     0,     0,
       0,     0,   357,     0,   405,   404,     0,   170,   170,   411,
     610,   616,   215,   505,   505,     0,   199,     0,   226,   200,
     201,   202,   203,   204,   205,   206,   207,   358,     0,   372,
     208,   378,   380,   383,   209,   210,   211,   212,   213,     0,
     214,   222,   593,   505,   224,     0,     0,     0,  1059,    98,
       0,    35,   505,     0,   505,  1032,  1034,  1035,  1155,     0,
     191,   190,    82,     0,    70,     0,    76,     0,    74,   169,
       0,   784,   819,   821,   796,   776,   780,   778,     0,     0,
     693,   693,     0,     0,   875,     0,   698,  1155,   809,     0,
     919,   938,   722,   721,   723,     0,     0,  1154,     0,     0,
       0,     3,     3,     0,  1081,     0,  1153,   771,     0,   875,
       0,     0,     0,   910,   970,   978,   486,  1043,     0,   847,
       0,   849,   875,   875,   875,   505,   875,   875,   505,   875,
     505,     0,     0,     0,   663,   593,   650,     0,     0,   437,
     219,   160,     0,     0,   345,   346,   223,   225,   505,     0,
       0,     0,   505,   505,   341,     0,   339,     0,     0,     0,
     759,     0,     0,     0,   336,     0,   505,     0,   384,   505,
       0,   171,     0,     0,   412,     0,     0,     0,   875,   230,
       0,     0,   357,   357,   363,   362,   357,   374,   373,   357,
     357,     0,   593,   435,   875,    52,    45,    43,    44,     0,
       0,    66,    73,     0,     0,   845,   775,   693,   875,  1155,
    1155,   701,   704,   682,   759,   705,     0,   811,   875,   875,
    1007,  1023,  1074,     0,     0,  1080,  1082,   461,   465,   911,
     971,   979,  1051,   851,   632,   632,   632,     0,   624,   663,
     505,   663,     0,   662,   661,   657,     0,     0,     0,     0,
     664,   665,   667,   875,   679,   679,   679,     0,   658,   675,
       0,   349,   350,   347,   348,   239,     0,     0,   241,   445,
     240,   593,   505,     0,     0,   357,     0,   324,   326,   325,
     327,     0,   357,   198,   279,     0,   272,     0,   198,   342,
     340,     0,   334,  1155,   343,     0,   338,   337,     0,     0,
       0,   393,   394,   395,   396,     0,   386,     0,   387,   351,
       0,   352,     0,     0,   377,     0,     0,     0,   366,   376,
       0,   357,   379,     0,   381,   403,   464,  1038,    67,    77,
      75,   792,  1155,   683,     0,     0,   700,  1155,   875,   463,
     462,  1083,  1084,   875,   875,   875,   633,     0,   671,   637,
       0,   677,     0,   659,     0,     0,   681,     0,     0,   652,
     875,   651,   668,   680,   669,   670,   676,   357,   357,   242,
     593,     0,     0,   260,   357,   328,     0,   329,     0,   268,
       0,   198,     0,   357,   505,   280,     0,   306,     0,     0,
     335,     0,     0,   357,   356,   357,   397,     0,   388,   505,
       0,     0,     0,   217,   216,   359,     0,     0,   357,     0,
     357,   357,   357,   783,     0,   875,   875,     0,   686,   628,
     630,   626,   654,     0,   875,     0,   672,  1093,   674,  1085,
     875,   875,   656,   678,   660,   653,     0,     0,   355,   231,
       0,     0,     0,   253,   357,   233,     0,     0,   357,   262,
     277,   288,   282,   357,   198,     0,   292,     0,     0,     0,
     319,   283,   281,   270,   273,     0,   330,   331,   332,   333,
       0,     0,   198,   307,     0,     0,   236,   354,   385,   505,
     391,   398,   506,   402,   353,     0,     0,   413,   364,     0,
     160,   375,   368,     0,   369,   367,   382,   875,   689,   685,
     706,     0,     0,     0,  1089,  1088,  1090,   875,   655,  1086,
    1087,   666,   357,   248,   243,   246,     0,   245,   252,   251,
       0,   875,   255,   254,   357,   264,     0,   261,   357,     0,
       0,     0,   269,   274,     0,     0,     0,   198,     0,   293,
     320,   321,     0,     0,   357,     0,   309,   310,   308,   311,
     276,   344,     0,   875,   391,     0,     0,     0,   875,   399,
     400,   401,     0,   406,     0,     0,     0,   414,   415,   360,
       0,     0,     0,   688,   875,  1096,  1098,  1091,     0,   232,
       0,   250,     0,   249,   235,   256,   505,   505,   265,   357,
     266,   263,   278,   291,   289,   285,   297,   295,   296,   294,
     298,   275,   322,   323,   290,   286,   287,   284,   271,     0,
       0,     0,     0,   238,   256,     0,   392,     0,  1089,   876,
     413,     0,     0,     0,   413,     0,   365,   361,   357,     0,
     505,  1092,  1094,  1095,   673,   244,   247,   875,     3,   257,
     427,   426,   267,     0,     0,     0,     0,   318,   316,   313,
     317,   314,   315,   312,     3,     0,   389,     0,     0,     0,
     407,     0,   416,   370,   357,     0,     0,   357,     0,   875,
     305,   303,   300,   304,   301,   302,   299,     0,   390,   419,
       0,   417,     0,   419,   371,  1097,   229,   357,   227,   234,
       0,   237,   420,     0,     0,   408,     0,   228,     0,     0,
       0,     0,   421,   422,     0,   418,   409,     0,     0,   410,
     423
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1985,    11,  -131, -1985,    -3,   787,  2451,   102,    24, -1985,
    -155, -1985,   707, -1985,  -832, -1137, -1985,   532,  9884, -1985,
    6766, -1985,   615, -1985,  1848,   424,  1196,  1241,  1122,  1213,
    1790,  1791,  1792,  1793,  1795, -1985,   369,  -237,  -461, -1985,
    1794, 10438,   771, -1985,  2118, -1985, -1985,  -680,  5856,  -909,
    3692, -1985,    17, -1985,   948,   111, -1985, -1985,   632,   203,
   -1985, -1922, -1984,   395,   174, -1985, -1985,   621,   407, -1985,
   -1338, -1632,   338, -1985, -1985, -1985,   223, -1272, -1985, -1985,
   -1560,   477, -1985, -1985, -1985, -1985, -1985,    52, -1518, -1985,
   -1985, -1985, -1985, -1985,   244,   493,   497,   326, -1985, -1985,
   -1985, -1985, -1474, -1985,   181,   125, -1985,   259, -1985,  -247,
   -1985, -1985, -1985,   982, -1196,   835,  -174, -1985,  -128,   -13,
     243,  1539,   836,   842, -1985,  -168, -1985, -1985,   -12, -1985,
     155,   876,  2078,  -368,  4074,  9825,  -458,   -15,    43,    45,
    1075,  2495, -1985, -1985,  2295, -1985,   122,  4732, -1985,  2241,
   -1985,   709, -1985, -1985,  2370,   295,  5497,  3132,   -21,  2009,
    -189, -1985, -1985, -1985, -1985, -1985,  -435,  9050,  8674, -1985,
    -193,   -52, -1985,  -923, -1985,   396, -1985,   256,   708, -1985,
      -9,  -234, -1985, -1985, -1985, -1985,  -211,  9500, -1195,   950,
     629,  1103, -1985,  -424,  -152,  1816,  3354,  1861,  -621,  -221,
     807,   -93,  -324,  -353,  -336,  -663,  1336, -1985,  1696,    80,
   -1238,  1461, -1985, -1985,   781, -1985, -1191,  -190,    27,  -718,
   -1985,  -302, -1985, -1985, -1089, -1177, -1985, -1985, -1985, -1078,
    2413,  -726, -1215,   -31, -1985, -1985, -1985, -1985, -1985, -1985,
    -177, -1323,  -532, -1940,   146,  1749,  4024,     3,   538,  2380,
   -1985,  6165,   127,  -341,  -306,  -291,    34,  -102,   -94,   -83,
     866,   -53,   -37,   -16,  -263,   435,  -251,  -227,  -222,   583,
    -205,  -179,  -164,  -267,  -622,  -593,  -573,  -242,  -208,  -545,
   -1985, -1985,  -858,  1547,  1548,  1549,  3014, -1985,   898,  7858,
   -1985,  -567,  -519,  -499,  -492,  -597, -1985, -1909, -1877, -1865,
   -1818,  -633,   627,   -20,   -62, -1985,   -73,   489,  -142, -1985,
   11101,  2266,  -372,  -442
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    31,   335,   209,   210,   115,   116,  1575,   211,   212,
     213,  1398,  1399,   214,  1211,  1212,  1213,  1418,   215,   216,
     217,   218,   219,   472,   473,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,  1067,   232,   233,
     234,   474,  1732,   515,   339,   517,   236,   901,  1576,  1577,
    1578,  1579,  1580,  1388,  1389,  2226,  1581,  1582,  1826,  2075,
    2076,  2003,  2004,  2005,  2198,  2199,  1583,  1845,  1846,  2100,
    1937,  1938,  2031,  1584,  1585,  1586,  1587,  1588,  1966,  1970,
    1750,  1742,  1589,  1590,  1749,  1743,  1591,  1592,  1593,  1594,
    1595,  1596,  1597,  1866,  2116,  1867,  1868,  2040,  1598,  1599,
    1600,  1735,  2126,  2127,  2128,  2253,  2264,  2145,  2146,   431,
     432,  1158,  1159,  1387,   118,   119,   120,   121,   122,  1829,
     286,   319,   126,   127,   128,   129,   355,   356,   434,   412,
     288,   477,   289,   132,   478,   134,   135,   265,   291,   292,
     139,   140,   141,   251,   142,  1244,   293,   322,   145,   379,
     146,   323,   388,   295,   573,   297,   324,   237,   151,   152,
     300,   153,   816,  1381,  1379,  1380,  1686,   301,   302,   156,
     157,  1382,  1696,  1809,  1810,  1811,  1985,  1986,  1697,  1910,
    1922,  1812,   158,  1250,  1448,   249,  1253,   303,  1254,  1255,
    1646,  1008,   822,  1274,   304,   305,   823,   307,   308,   309,
     825,   596,   597,   340,   712,   713,   714,   715,   716,   561,
    1446,   562,  1242,  1240,   945,   563,   587,   588,   565,   598,
     160,   254,   255,   161,  1235,  1236,  1237,  1238,     4,  1369,
    1370,   936,  1433,   162,   551,   552,   390,   402,   795,   163,
     343,   164,   767,  1068,   784,     6,     7,     8,    26,    27,
      28,   165,   769,   359,   360,   361,   770,   167,   168,   169,
     170,   171,   172,   173,   364,   771,   366,   367,   368,   772,
     370,   371,   372,  1024,   649,   650,   651,  1025,   373,   654,
     655,   656,   873,   874,   875,   876,   748,  1118,  1359,   310,
    1607,   658,   659,   660,   661,   662,   663,  1988,  1989,  1990,
    1991,   636,   311,   312,   313,   314,   481,   330,   176,   177,
     178,   316,   882,   664
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      23,  1078,   433,   718,  1447,   272,    23,   238,   479,   396,
      32,   775,   729,   273,  1191,   867,   136,   933,   123,   130,
    1010,   648,   592,   136,   274,   123,   130,   575,    23,  1746,
      23,   424,   441,    23,  1048,    23,    23,   894,  1436,   584,
     902,    38,   585,   792,   329,  1069,   652,   730,   487,  1011,
    1029,  1373,   564,  1449,   275,  1737,  1036,  1054,  1439,  1055,
    2077,  1137,   731,  2078,  1215,   166,  1377,  1604,   257,  1012,
     276,  1420,   166,  1144,   137,  2066,   138,  2084,   341,  1026,
     653,   137,  2083,   138,   136,  1251,   123,   130,   279,  2141,
     732,   277,  2149,  1872,    23,    23,    23,  1013,  1224,   540,
    -823,   668,   733,   802,    23,    25,  2061,   545,   480,     9,
      23,    25,   547,   195,    23,    29,   754,  2140,  2062,     1,
    1870,   817,    23,  1014,    55,  1561,   734,    23,   854,  1642,
      23,   735,  2120,   166,    23,    25,  1972,   344,    25,  1425,
     627,   341,   137,  1015,   138,   789,    37,   541,   736,  1434,
    1016,   326,  1741,   143,  1530,  2137,  1321,   801,  2124,  -856,
     143,  2197,  1534,  1440,   362,  2063,  -824,   391,   603,     2,
    1873,   403,    23,     1,   737,    23,   604,  1426,   386,     1,
    2000,  2001,  1441,   427,     3,   753,   441,   605,  2085,   738,
    2197,   241,   761,  1083,  1601,   433,  1439,  1881,   844,   253,
     256,  1199,  2077,  2150,  1202,    23,  1030,  1871,   248,  2066,
    1033,  1227,  1942,   436,   788,  2229,  1039,   606,   476,  1630,
     342,   143,  -856,     2,   433,  1707,   800,   345,   669,     2,
     489,   242,  1318,   607,  1129,   490,   433,  2079,     3,    23,
    2061,  1219,  -697,  1057,     3,     1,  1135,    23,     1,  -697,
    1062,   272,  2062,  1252,   608,  1636,  1637,   358,   581,   273,
    1943,  1647,  2142,  2143,   831,  1944,    23,    23,   374,  2137,
     274,   346,  2002,    32,   124,  2083,  1968,   443,  1356,    23,
     444,   124,   590,    -3,    23,    23, -1156,  1624,  1626,  1628,
     683,  1740,   143,   850,   689,     2,  1532,  1252,     2,  2063,
     275,  1736,   581,   326,  1358,   844,   898,  2083,    23,   782,
       3,  1969,    23,     3,  2025,  2030,   276,  1680,    23,  2000,
    2001,   869,    23,  1744,   620,   425,   148,   609,    32,   645,
    1408,  2091,  1409,   148,   709,   386,   247,   277,  1319,    23,
    1747,  1072,   124,     1,   560,  -875,    23,  1745,   103,   104,
      23,    23,  1706,   847,   258,   695,   851,   491,   853,   701,
     721,  1048,   492,  1737,  1748,   329,  1320,   958,  1649,   856,
     959,    23,   858,   859,   860,  1847,   383,  1321,  1073,    23,
     397,    23,   668,  2094,  2095,  1010,  1086,  1026,   326,   345,
      23,  1847,    23,     2,   148,    23,   564,  1119,   329,   564,
     811,   426,    23,   570,   109,  1123,   436,  1182,     3,  1383,
    1442,  2035,  1029,   685,  1011,  1374,   326,   692,  1821,    23,
      23,  1087,    23,   729,  1203,   906,   259,    23,  1044,  1443,
     668,    23,   796,  1014,  1012,   436,  1088,   828,   653,    23,
     530,   531,   668,   794,   720,  1769,  1770,   436,   830,  1132,
    1134,  1542,  1542,  1015,  1304,   268,  1702,  1703,   730,  1056,
    1016,   333,  1287,   476,  1089,   148,   476,  1154,   833,   426,
    1543,  1753,  1439,   731,  1880,   436,  1090,  1736,  1883,   476,
     386,   741,   668,   548,  1002,   143,   345,    23,  1014,    23,
     532,   533,    23,   668,  2132,   790,    23,   791,   334,    23,
    1091,   732,   745,   476,   750,  1092,   971,   253,  1015,   669,
    1949,   758,  1827,   733,   143,  1016,  1827,  1848,   942,  1744,
     175,  1444,  1093,   811,  2188,   253,   143,   175,  1891,     1,
     347,   347,  1919,  1848,  1413,   476,   106,   734,  1561,  1920,
     348,    23,   735,  1745,   842, -1015,    23,    34,  1094,  1804,
    1143,   627,   943,   944,   143,    23,    23,   669,  1921,   736,
      23,   358,   871,  1095,   426,   365,   746,     1,   350,   669,
    1874,   179,  1892,   111,   112,    23,   609,  1121,   746,     2,
    1353,  1704,  1286,    23,   349,   737,    23,  1623,   175,   843,
     845,   949,   570,   336,     3, -1015,   395,   990,    23,    23,
     738,   897,    32,  2018,   337,    32,  1847,  -499,  1313,  1736,
   -1015,   620,  1139,    23,    23,   949,   386,     2,   425,  1142,
     338, -1014,   746,   570,  1146,   603,   175,     1,   591,   433,
     421,   668,     3,   604,   268,   695,   701,  1316,    32,    23,
    1258,   793, -1015,    23,   605,   423,  1263,   243,  1721, -1015,
     244,   245,  1006,   246,  1018,  -825,   348,  1330,   148,  1817,
     939,   941,   272,   426,   386,   948,   963,    23,  1059,   965,
     273, -1014,   620,    23,   606,  1805,     1,     2,  1818,  1304,
    1823,   274,  1196,  1010,   425,   279, -1014,   148,   560,  1197,
     607,   560,     3,   426,  1496,  1477,  1480,  1323,  1817,   148,
    1973,  1437,  1061,    23,   869,   709,  2217,  1074,  1075,   620,
    2221,   608,  1011,   369,  1920,  1063,   794,  1913, -1014,   627,
     746,    23,    23,  1950,    23, -1014,     2,   148,  1076,   175,
      32,   523,  1012,  1982,  1081,  1324,    23,    23,   524,   525,
     147,     3,  1393,    32,   448,  1993,  1080,   147,  1848,  1618,
    1701,  1256,  1920,     1,  1302,  1168,  1308,   564,  2000,  2001,
    1013,    23,    23,    23,  1994,   755,  2067,   782,   451,   746,
    1120,  1995,  1050,   695,   701,    23,   869,    23,  1124,  1303,
      32,  1309,   869,   522,  1086,  2068,  2103,  1837,  1838,   742,
      24,  1839,  1840,  1763,   869,  1764,    24,   869,     1,   326,
     869,  2089,   454,     2,  2170,  1138,     1,  2172,   147,   668,
    2204,  1301,  1231,   653,  1935,   653,   668,  1145,     3,  1087,
      24,   570,   620,    24,   268,   862,   239,  1415,  1608,    32,
    1416,   -23,   520,   521,  1088,  1275,   863,   864,    23,   452,
     436,   570,    32,  1054,  1055,  1955,   400,  1430,     2,   570,
    1944,    32,   455,  2046,  1275,  1166,     2,   668,  2047,  2183,
    1467,  1468,  1089,     3,  2184,   386,  1106,  1109,  1010,   365,
     493,     3,  2242,   869,  1090,   576,  1907,  2243,  1912,   147,
     794,   536,  1454,  2206,    24,    24,   425,   180,   609,    23,
     746,    36,   741,    23,  -826,  -875,   538,  1011,  1091,   520,
     317,   709,  -875,  1092,  1505,  1226,   537,   131,     1,  1275,
    1633,   637,    24,     1,   131,    35,    36,  1012,   445,   143,
    1093,   136,    36,   123,   130,   539,     1,    23,  1482,  1275,
     542,   990,   543,    23,  1940,    23,   755,   743,  1099,  1948,
     746,   544,  1056,  1164,    23,  1287,  1094,   909,   910,   911,
    2200,  2201,    49,    50,    51,    52,    53,   546,     2,     1,
    -844,  1095,    24,     2,  1498,    24,  1376,  1797,   586,  1278,
     166,   375,   376,     3,   377,   131,     2,   589,     3,   137,
     378,   138,   559,   327,   669,   195,     1,   610,  1165,  1503,
     611,     3,  1261,   320,     1,   488,   363,   268,   612,   392,
     278,   104,   979,   404,    23,  -875,   746,   570,  1858,     2,
    1284,  1285,  1231,   613,  1275,  1027,    23,   834,    23,   643,
      23,   526,   527,   603,     3,   990,    32,   528,   529,    24,
     614,   604,  2017,   615,   591,   620,     2,   582,  1450,  1451,
    1228,  1619,   605,   794,     2,   668,   131,   628,  1034,   560,
     439,     3,   643,     1,   103,   104,   640,  1853,   143,     3,
      23,    23,     1,   868,   576,     1,   946,   869,   709,   623,
     947,    23,   147,   665,    24,  1130,  1066,  1639,  -502,   746,
     641,  1400,  1500,  1264,  1501,   691,   520,   746,   704,   967,
    1328,   386,   148,   968,  1056,   576,   969,   719,    24,   653,
     970,   147,    24,     2,  1027,  2093,   609,  1231,   643,    23,
     109,  1897,     2,   147,   717,     2,   549,   722,     3,   534,
     535,  1635,    23,  2110,    23,    23,   723,     3,    32,    24,
       3,    32,  1275,  1275,   744,   327,   726,   908,  1362,   983,
    1153,   147,  1341,   746,  1154,   400,   746,   570,   677,   678,
    1538,    23,  1356,    -3,    23,  1469,  1371,   344,   624,  1375,
     106,   695,   701,  1378,   773,  1317,  1485,   640,  1357,    23,
    1676,    24,   679,   680,  1107,  1110,  1275,   266,  1358,   124,
      24,   774,    24,  1232,   787,  1497,   952,   696,  2161,   341,
     643,   702,    24,    32,   709,   799,   637,   111,   953,  1507,
     742,   699,  1257,  1259,   439,    23,   947,   947,  -501,   814,
      24,  1268,   819,   406,   827,   746,   832,   239,   408,   846,
     327,   413,   870,   418,  1327,  1302,  1308,   880,   970,    24,
      23,   148,    49,    50,    51,    52,    53,  1638,     1,   131,
    1820,   810,   104,  1365,  1056,  1231,  1760,   869,   327,   425,
    1303,  1309,    23,   746,  1106,  1109,   778,  1544,   781,  1386,
     950,   421,  1606,   755,   797,     1,   460,   746,   131,    49,
      50,    51,    52,    53,     1,  1776,   881,   888,   883,   890,
     131,   884,   893,  1301,   653,   653,    24,  1367,     2,   903,
     106,   869,   885,   576,   637,    23,  1222,    49,    50,    51,
      52,    53,    23,     3,    23,  1130,     1,   609,   131,   746,
    1392,   878,   879,   576,   320,     2,   952,   886,   570,   904,
     643,   576,  1108,  1111,     2,  1395,  1794,   111,   953,  1231,
       3,   931,   869,  1223,  1130,     1,  1655,  1656,   746,     3,
     645,   640,   884,    10,   938,    24,    24,   320,   743,   937,
      24,   956,     1,  1663,  1345,  1665,     2,  1455,   746,  1903,
    1904,  1905,  1795,     1,  -503,   957,   961,   406,   408,   106,
     673,     3,   418,    24,  1668,   136,    24,   123,   130,   964,
     479,   966,  1611,  1232,   973,     2,  1612,   709,    23,   106,
    1634,   974,  1906,  1533,   970,   871,   970,  1894,  1895,   746,
       3,  -504,     2,   986,   623,   975,   111,   112,  1775,   709,
    1931,   679,  1163,     2,   869,  1983,    23,     3,   976,   746,
     977,    23,    23,    23,    11,   175,   111,   112,     3,   175,
    1952,   978,    23,  1603,   869,   138,     1,    23,   622,    23,
    1796,  -424,  -424,    12,   591,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,   999,   243,    24,  1000,   244,
     245,   406,   246,  1052,  1019,   729,  1698,   696,   702,  1953,
    1954,   645,  1176,   970,   869,   396,   703,  1180,  1232,   576,
     480,  1951,     1,  1682,  1683,  1684,     2,    23,    23,    23,
      23,  1216,  1217,  1386,   -18,  1198,    32,  1070,    33,  1523,
     730,     3,  1001,  2051,  2086,  1071,   147,   869,   869,  2119,
    2187,  1082,   143,  2261,   869,   731,  1079,  2258,  1084,  1400,
    1974,  1231,     1,  2267,  -424,  1977,  1096,  2268,   821,  1097,
    1783,  1784,     2,   136,  1828,   123,   130,  1631,  1828,    23,
    1830,  1098,  1632,   732,  1830,  1314,  1315,     3,   709,  1360,
    1361,    23,    23,    23,  1113,   733,    23,  1423,  1424,   709,
    1427,  1424,  1107,  1110,  1705,  1432,  1424,   623,  1100,   624,
     125,  1349,     2,  1086,  1101,   746,     1,   125,  1102,   734,
    1923,  1923,  1923,  1730,   735,     1,  1103,     3,  1104,  2130,
     476,  1603,  1431,   138,  1499,  1424,    23,  1677,   869,    23,
      23,   736,  1740,  1741,  1105,   696,   702,  1909,  1087,   520,
       1,  1471,  1762,  1424,  1140,   746,  1232,  1698,   844,   576,
    1141,   637,    23,  1088,    23,  1150,     2,   737,    24,   175,
    1889,  1424,  1151,   124,  1152,     2,   391,   403,   125,  1157,
      23,     3,   738,  1161,  1699,   147,  1160,   386,  1162,   287,
       3,  1089,   916,   917,   918,   919,  1715,  1717,  1719,  1167,
       2,  1725,  1169,  1090,  1170,  1475,  1890,  1424,  1171,   643,
     143,     1,   384,   131,  1478,     3,   125,  1172,   643,  -125,
    -125,  -125,  -125,  -125,  -125,   148,  1173,  1091,   591,  1174,
     591,    23,  1092,   136,     1,    23,     1,   136,   136,  1508,
    1232,  2049,  2050,   746,   396,   549,  2000,  2001,  1200,  1093,
    1108,  1111,  2258,  2259,   136,   699,   781,   709,  -500,  1822,
    1824,     2,   912,   913,   175,    24,  1175,  1911,    49,    50,
      51,    52,    53,  1177,    24,  1094,     3,   624,  1178,   709,
     709,  1421,  1422,  1179,     2,  1772,     2,   920,   921,    23,
    1095,  1832,  1194,   138,    30,  1832,  1832,   138,   138,     3,
    1512,     3,  1205,  2074,   746,  1195,     1,  1206,  1885,   914,
     915,   709,  1832,  1214,   138,  1218,   729,  1837,  1838,   125,
     460,  1839,  1840,  1516,  1220,  1614,   796,   746,   240,   746,
     576,   124,  1401,  1402,  1403,  1221,    23,   794,    23,  1229,
    1819,  1882,  1884,    23,  1935,    23,  1924,  1925,  1239,  1410,
    1411,   730,   131,  1936,    23,  1243,     2,  1245,  1248,   581,
     287,  1814,  1260,  1280,  1281,  1282,   731,  1288,  1289,  1290,
     143,     3,   709,  1291,   143,   143,  -167,  1292,   250,   709,
    1293,  1294,  1295,   148,  1326,   383,   397,  1312,  1322,  1329,
      24,   143,  1332,   668,   732,  2190,  1333,   287,  1122,   746,
    1837,  1838,   175,  1334,  1839,  1840,   733,  1335,  1336,   406,
    1337,  1338,   709,  1354,  1339,  1340,   386,   869,  1363,   175,
    1366,  1368,  1956,   394,  1391,  1412,  2043,  1935,   407,  1394,
     734,  1404,  1232,  1405,   411,   735,  1941,  1962,   420,  1406,
    1414,  1407,  1417,  2195,   422,  2074,    49,    50,    51,    52,
      53,    23,   736,   680,    23,    23,   428,    32,   429,  1926,
     287,    32,  1435,  1456,   709,   709,  1814,  1453,  1814,   136,
      54,   709,  1445,  1457,  1458,   741,  1461,   384,   737,  1462,
     709,  1385,  1463,  1466,    24,  2219,   175,  1464,  1465,  1470,
     709,   124,   709,   738,    10,   124,   124,  1491,  1494,  1396,
    1861,  1862,  1863,  1864,  1865,   709,  2043,   709,   709,   709,
    1495,  1502,   124,  1535,  1184,   794,    89,    90,    91,    92,
      93,    94,    95,    96,  1536,  2045,  1537,  1832,  1609,   138,
    1610,  1622,  1613,  1620,    23,   793,  1621,  1629,   578,  1644,
    1640,   709,  1641,   148,  1645,   709,  1650,   148,   148,  1651,
     709,  1657,   609,  2081,   591,   272,    -3,  2260,  1664,  1666,
    1667,  1679,  1669,   273,   148,  1673,  1674,  1675,  1734,   616,
    1681,  1739,  1685,  1700,   274,    11,  1708,   175,    23,  1709,
      23,  1712,    24,  1713,  1106,  1109,  2113,   675,  1722,  1726,
    1727,   676,  1729,   -22,    12,  1754,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,   143,  2228,  1296,   709,
    1738,  2125,   869,   698,   383,   397,    49,    50,    51,    52,
      53,   709,  1271,  2237,  1755,   709,  1272,  1761,  1765,   935,
    1768,  1767,   175,  1315,  1778,    23,   724,   725,  1779,   147,
    1780,   709,  1781,  1782,  1785,  1793,   747,   287,  1786,   751,
     752,  1803,  1787,   756,    23,    23,   759,   760,  1789,   762,
    1790,   763,  1791,  1701,  1814,  1834,  1741,  1849,  1561,  1815,
    1310,   136,   136,   287,   785,  1876,  1850,  1311,   175,  -124,
    -124,  -124,  -124,  -124,  -124,  1877,   709,  1852,  1887,  1854,
     798,  1856,   326,  1857,  1869,   803,  1888,   806,    49,    50,
      51,    52,    53,   454,  1898,  1901,  1902,   809,   287,  1807,
    1927,  1928,   342,   793,  1932,    23,    23,    24,    24,   829,
    1934,  1959,    32,  1961,  1975,   709,  1976,   124,  1992,  1832,
    1832,   138,   138,  2008,  2216,  2013,  2014,   175,  2032,  2039,
    2034,   175,   175,  2053,  2125,  2044,  2048,  2057,  2125,  2125,
    2060,   746,   695,   701,  2072,  2088,  2097,   287,   175,   287,
    2104,   709,    24,  2111,   709,  2090,  2102,    24,  2122,  2134,
    2115,  2135,  2238,   821,  1815,  2136,  1815,  2123,  2144,   148,
    2169,  2153,  2240,   742,   709,   741,    49,    50,    51,    52,
      53,   866,  2171,   838,  2173,  2177,    23,   147,  2180,  2181,
    2182,  2186,  2185,  2252,  2203,    23,   131,  2252,   143,   143,
     494,  2207,   495,   496,   497,  2215,  2218,  1816,  2205,  2220,
    2224,  2230,  2262,  2239,   838,  2241,   824,   106,   260,    41,
      42,    43,    44,    45,    46,    47,    48,  2245,    49,    50,
      51,    52,    53,  2249,  2251,  1106,  1109,   498,  2254,  2255,
     499,   500,   400,   871,  2265,   501,   502,   746,   287,  1757,
     865,   175,  2266,   675,   111,   112,   922,  2269,   923,    24,
     924,   960,   925,   516,   962,   926,  1545,   929,    24,  1733,
    2250,    24,    24,    24,  1836,  2196,    24,  2036,  2214,    24,
    1860,  2024,  1107,  1110,  2096,  2189,   405,  1971,  2175,  1957,
     980,  2263,   630,  1958,     1,  2114,  2222,  1459,  2256,   591,
    2270,  1460,  2174,  1605,  1691,  1692,    49,    50,    51,    52,
      53,  1693,  1816,   332,  1816,   252,   819,   415,   681,   124,
     124,   743,   687,  2071,  2194,  1022,  1028,   786,  1802,  1031,
    1032,  1896,  1035,  1643,  1037,  1038,  1751,  1325,  1241,  1040,
    1041,  1077,  1766,     5,     2,  1694,   181,   147,  1185,  1186,
    1187,   147,   147,     0,   131,  1678,     0,   106,     0,     3,
      24,     0,  1815,   175,     0,     0,     0,     0,   147,     0,
       0,   148,   148,     0,  1520,     0,     0,     0,  1521,     0,
       0,     0,  1522,   871,     0,     0,     0,   746,    49,    50,
      51,    52,    53,  1181,   111,   112,     0,     0,   392,   404,
       0,     0,     0,   287,     0,   125,     0,     0,     0,   125,
     296,     0,   838,     0,  2176,    24,     0,     0,     0,     0,
       0,  1965,     0,  1112,  1053,     0,     0,     0,     0,     0,
    1108,  1111,   838,   824,     0,   106,     0,     0,     0,   591,
     838,   591,     0,     0,    12,     0,  1136,   747,    15,    16,
      17,    18,    19,    20,    21,    22,   287,     0,     0,    24,
      24,   952,     0,     0,   600,   643,     0,     0,     0,     0,
       0,   400,   111,   953,     0,   287,     0,     0,     0,     0,
     639,   591,     0,   742,     0,     0,     0,     0,     0,     0,
       0,  1147,  1148,  1149,  1799,     0,  1801,    49,    50,    51,
      52,    53,     0,     0,  1658,     0,     0,     0,  1659,     0,
    1816,  1660,     0,     0,   131,     0,     0,     0,   131,   131,
       0,     0,     0,     0,     0,     0,     0,   267,     0,     0,
     287,     0,     0,     0,    24,   131,     0,     0,     0,     0,
       0,  1670,     0,  1107,  1110,  1671,   824,     0,   797,  1672,
     287,  1189,    24,   591,  1192,  1193,     0,     0,   387,    24,
       0,     0,     0,     1,     0,   175,   175,     0,     0,     0,
     410,   414,     0,     0,     0,     0,     0,   630,   783,     0,
       0,   296,     0,   147,   633,     0,     0,     0,     0,     0,
       0,   672,    24,     0,  2129,     0,     0,     0,   838,     0,
      49,    50,    51,    52,    53,     0,  1908,     0,     0,   125,
     633,     0,     0,     2,   633,     0,   461,  1156,   296,     0,
       0,     0,     0,     0,     0,   988,   106,     0,     3,     0,
       0,   743,     0,  1022,   332,   287,  1247,     0,  1249,     0,
       0,     0,   849,  1004,    24,    24,     0,  1759,   600,     0,
       0,    24,   952,  1265,     0,     0,   643,     0,     0,     0,
      24,     0,   824,   111,   953,     0,     1,   332,     0,     0,
      24,     0,    24,     0,     0,  1047,     0,     0,     0,     0,
    1225,   296,     0,     0,     0,    24,     0,    24,    24,    24,
       0,  1108,  1111,     0,     0,     0,  1058,     0,  1299,     0,
       0,     0,    12,   935,   125,     0,    15,    16,    17,    18,
      19,    20,    21,    22,   907,   387,     2,     0,   710,     0,
       0,    24,     0,     0,     0,    24,     0,     0,     0,   106,
      24,     3,     0,  1276,   374,     0,     0,   296,   838,     0,
       0,     0,     0,   824,     0,     0,   387,     0,     0,     0,
     131,     0,  1276,   287,   287,  1983,     0,  1298,   108,   746,
    2121,     0,     0,     0,     0,   639,   111,   112,   296,     0,
     824,     0,     0,     0,   824,  1342,     0,     0,     0,  1346,
     113,     0,    54,  1350,   287,   147,   147,     0,     0,    24,
      49,    50,    51,    52,    53,   972,     0,   287,   824,     0,
       0,    24,     0,     0,     0,    24,     0,  1276,     0,     0,
       0,     0,     0,     0,     0,  2162,     0,     0,   824,     0,
       0,    24,     0,   387,     0,   824,     0,  1276,     0,     0,
       0,  1384,    93,    94,    95,    96,     0,  1390,     0,     0,
       0,     0,   125,     0,    12,     0,   351,   352,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,   125,
     387,     0,     0,  1049,     0,   387,    24,     0,   296,   633,
       0,     0,     0,   106,     0,     0,     0,    12,     0,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,   895,
       0,     0,     0,     0,   296,     0,     0,     0,     0,  1806,
     108,     0,     0,   387,     0,    24,  1807,     0,     0,     0,
     111,   112,  1276,     0,     0,   600,     0,   633,     0,   106,
     672,   824,   113,     0,  1299,     0,   125,     0,     0,   296,
       0,     0,     0,   287,   287,   633,   896,     0,     0,     0,
       0,    24,     0,     0,    24,  1983,     0,     0,  1233,   746,
       0,     0,   131,   131,     0,     0,   111,   112,     0,   988,
       0,     0,     0,     0,    24,     0,   296,   633,  1472,   600,
     600,  1476,  1479,     0,     0,   387,    24,     0,     0,  1488,
    1489,     0,     1,     0,     0,    24,   296,     0,   633,     0,
       0,     0,     0,     0,   296,     0,   387,  1277,     0,     0,
       0,     0,  1047,     0,     0,     0,  1504,     0,  1506,     0,
       0,  1509,     0,     0,  1513,     0,    12,   125,  1517,     0,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
    1276,  1276,     2,     0,     0,     0,   783,     0,     0,     0,
       0,     0,     0,   287,   387,     0,     0,     3,     0,    30,
     387,     0,     0,   988,     0,   824,   387,     0,     0,   824,
       0,  1539,     0,     0,     0,     0,     0,  1390,     0,     0,
       0,   384,   125,     0,  1276,     0,     0,     0,     0,   296,
       0,     0,     0,     0,  1615,     0,     0,  1065,     0,   710,
       0,     0,     0,   150,     0,     0,   387,     0,     0,     0,
     150,     0,     0,     0,     0,     0,     0,   387,     0,    49,
      50,    51,    52,    53,     0,     0,     0,     0,   125,     0,
       0,     0,     0,     0,     0,   387,     0,     0,     0,     0,
     461,     0,   824,     0,     0,     0,   824,     0,     0,     0,
     824,     0,     0,     0,     0,     0,     0,     0,  1233,  1476,
       0,     0,   296,     0,     0,     0,     0,     0,     0,     0,
     600,   150,     0,     0,     0,     0,   600,     0,     0,     0,
       0,  1116,   299,     0,     0,  1541,     0,   125,     0,   150,
       0,   125,   125,     0,     0,     0,     0,     0,     0,     0,
    1049,     0,     0,     0,     0,   389,    12,     0,   125,   150,
      15,    16,    17,    18,    19,    20,    21,    22,   891,    12,
    1653,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,   384,     0,  2225,     0,     0,     0,     0,     0,   387,
       0,     0,   150,     0,     0,     0,   150,     0,   106,     0,
     633,     0,     0,  1233,     0,   387,     0,     0,     0,     0,
       0,   633,     0,     0,     0,   892,     0,     0,     0,     0,
     387,     0,   824,   299,   642,   108,   824,     0,   643,   824,
       0,     0,     0,  1210,     0,   111,   644,  1210,     0,     0,
       0,     0,     0,     0,     0,   710,     0,   113,     0,   633,
       0,     0,   296,     0,   633,     0,     0,     0,     0,   824,
     384,   125,   150,   824,     0,     0,     0,   824,   757,   566,
     150,  1210,   583,   260,    41,    42,    43,    44,    45,    46,
      47,    48,     0,  1773,     0,     0,     0,     0,     0,    12,
     299,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   299,   150,   633,   299,     0,  1788,     0,
       0,     0,   389,   150,     0,     0,     0,     0,     0,     0,
       0,  1539,  1539,  1539,     0,  1798,   240,     0,  1800,     0,
       0,     0,   150,     0,     0,     0,   150,     0,     0,     0,
     299,  1233,     0,   389,     0,     0,     0,   150,     0,     0,
     150,     0,     0,     0,   306,     0,     0,     0,     0,   384,
       0,     0,     0,     0,     0,   824,     0,     0,     0,  1714,
       0,     0,     0,   125,     0,     0,     0,  1875,     0,     0,
       0,   877,     0,   387,     0,   150,     0,     0,     0,     0,
     387,     0,     0,  1886,     0,   600,     0,     0,     0,     0,
     633,     0,   150,   150,   150,  1210,     0,  1893,     0,     0,
       0,     0,   710,     0,   150,  1065,     0,  1899,  1900,     0,
     389,     0,     0,     0,     0,  1233,   150,     0,     0,     0,
       0,   387,    12,     0,   351,   352,    15,    16,    17,    18,
      19,    20,    21,    22,   815,     0,     0,   150,     0,     0,
       0,     0,   747,     0,   150,     0,     0,   389,   150,   299,
     150,     0,   389,     0,     0,     0,     0,     1,   299,     0,
       0,   299,    12,   150,   150,     0,    15,    16,    17,    18,
      19,    20,    21,    22,   299,     0,     0,   150,   150,   150,
     299,     0,     0,   299,     0,   633,     0,     0,     0,     0,
     389,    12,   581,   351,   352,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,   981,     0,     2,   984,     0,
       0,     0,     0,     0,     0,   306,     0,     0,   108,     0,
     106,  1064,     3,     0,     0,     0,     0,  1978,   710,     0,
     299,     0,  1979,  1980,  1981,     0,     0,     0,     0,  1210,
    1723,     0,     0,     0,     0,     0,  2117,   108,     0,  1997,
     746,     0,   306,     0,     0,     0,     0,   111,   112,     0,
       0,     0,   389,     0,     0,   125,   125,     0,    12,   113,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     299,   150,     0,   389,     0,     0,     0,     0,     0,    12,
       0,   600,     0,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,  2058,  2059,   299,  1233,     0,   387,
       0,   150,     0,  2065,     0,   306,     0,     0,   106,  2069,
    2070,     0,     0,   633,     0,     0,   757,     0,   583,   150,
       0,   996,   299,     0,     0,     0,     0,   389,   150,     0,
       0,   299,     0,   389,  1490,   108,     0,   150,  1724,     0,
       0,     0,     0,     0,   150,   111,   112,     0,     0,     0,
       0,     0,   553,     0,     0,     0,     0,   113,     0,     0,
     554,   555,   556,   557,     0,     0,     0,     0,   299,   150,
       0,     0,     0,   389,     0,     0,  2133,     0,   815,     0,
     150,     0,   150,     0,   389,     0,  2138,     0,   299,     0,
     150,     0,     0,     0,     0,   150,   150,   633,     0,     0,
    2147,    12,   389,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     0,     0,     0,
       0,   299,     0,     0,     0,     0,  1528,     0,     0,     0,
       0,     0,  2147,   710,     0,     0,     0,  2065,     0,     0,
    1210,     0,     0,     0,     0,  1210,  1210,  1210,  1117,     0,
       0,     0,     0,  2191,     0,   877,   877,   150,     0,     0,
     558,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     0,   299,   299,     0,   559,     0,
      12,   299,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,   306,     0,     0,     0,     0,     0,     0,   150,
       0,    12,     0,   351,   352,    15,    16,    17,    18,    19,
      20,    21,    22,     0,   296,     0,  2227,     2,   306,     0,
       0,   600,   389,     0,     0,     0,     0,     0,   150,     0,
     106,   150,     3,     0,     0,     0,     0,   389,     0,     0,
       0,   150,   150,     1,     0,  2247,     0,     0,  2227,     0,
     150,     0,     0,   306,   299,     0,   353,   108,  1266,     0,
    1716,  1269,   600,   150,     0,     0,     0,   111,   112,  2247,
       0,     0,     0,   150,     0,     0,     0,    12,     0,   113,
       0,    15,    16,    17,    18,    19,    20,    21,    22,   633,
      12,     0,     0,     2,    15,    16,    17,    18,    19,    20,
      21,    22,   306,     0,   306,     0,   106,  2041,     3,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      12,   824,   579,   580,    15,    16,    17,    18,    19,    20,
      21,    22,   107,   108,     0,     0,   150,     0,   150,     0,
       0,     0,   150,   111,   112,     0,  1210,     0,  1210,     0,
       0,     0,     0,   150,   150,   113,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   150,   387,     0,     0,
       0,     0,   299,   757,     0,   133,     0,     0,   109,     0,
    1343,   150,   133,     0,  1347,   150,     0,  2041,  1351,   150,
     581,   150,     0,     0,   299,     0,   299,     0,     0,     0,
     996,   270,     0,   306,     0,     0,     0,   389,   150,   150,
       0,   325,     0,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,     0,     0,     0,     0,   150,
       0,     0,     0,     0,     0,     0,   150,     0,     0,     0,
       0,   710,     0,   133,     0,     0,     0,   150,   996,     0,
    1155,     0,     0,     0,   290,     0,     0,     0,     0,     0,
     387,   321,     0,   150,     0,     0,     0,     0,     0,     0,
       0,     0,   877,     0,   877,     0,     0,     0,     0,     0,
       0,   398,     0,   150,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   710,     0,     0,     0,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,   435,     0,     0,   387,   440,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,   150,     0,     0,     2,     0,     0,     0,     0,
       0,     0,   150,     0,     0,     0,     0,     0,   106,     0,
       3,     0,     0,     0,  1298,   108,     0,     0,   306,     0,
       0,     0,   599,   602,     0,   150,     0,     0,   150,     0,
       0,     0,   150,  1473,   353,   108,   150,   113,   635,     0,
       0,     0,     0,     0,   550,   111,   112,     0,     0,     0,
       0,     0,   571,     0,     0,     0,   387,   113,     0,   633,
       0,     0,     0,     0,    49,    50,    51,    52,    53,     0,
       0,   306,     0,   601,     0,     0,  1510,     0,     0,  1514,
       0,     0,     0,  1518,     0,   619,   625,     0,   631,     0,
     306,     0,     0,     0,     0,   671,   389,   150,     0,     0,
       0,   299,     0,     0,     0,     0,     0,   727,   740,     0,
       0,     0,     0,  1452,   682,     0,     0,     0,   682,     0,
       0,     0,   290,     0,     0,     0,   150,     0,     0,   700,
     150,     0,   625,   777,     0,     0,   602,     0,     0,   633,
       0,     0,     0,     0,     0,   306,   150,   150,     0,  1616,
       0,     0,     0,     0,     0,   387,     0,     0,   739,     0,
       0,     0,     0,     0,    12,   306,     0,   435,    15,    16,
      17,    18,    19,    20,    21,    22,     0,   150,   766,   826,
       0,     0,     0,   776,   700,   290,   321,     0,     0,     0,
     150,     0,   839,   106,     0,     0,   435,     0,     0,     0,
     848,     0,     0,   150,     0,     0,   599,   150,   435,     0,
       0,   150,   804,     0,     0,     0,     0,   807,     0,   107,
     108,     0,   808,  1529,     0,   839,     0,     0,     0,   820,
     111,   112,     0,     0,     0,   150,   435,     0,     0,     0,
     835,   571,   113,     0,     0,     0,   150,     0,     0,     1,
     306,    12,   299,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,   387,     0,     0,
       0,     0,   571,     0,     0,     0,    49,    50,    51,    52,
      53,     0,     0,    12,     0,   351,   352,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,   150,     0,     0,
       0,     0,   106,     0,     3,     0,     0,  1718,     0,   150,
       0,     0,     0,   635,     0,     0,   150,   150,     0,     0,
       0,     0,     0,   150,     0,     0,     0,   150,   764,   108,
     150,     0,     1,     0,     0,     0,     0,    12,     0,   111,
     112,    15,    16,    17,    18,    19,    20,    21,    22,  1207,
       0,   113,     0,     0,  1208,     0,  1209,     0,   306,   306,
     150,     0,   619,   631,   150,     0,    12,     0,   150,     0,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       0,     0,     2,     0,     0,     0,   826,     0,   290,   306,
    1023,   150,     0,   108,     0,   106,  1419,     3,     0,     0,
     299,     0,   306,     0,     0,     0,     0,     0,     0,     0,
       0,   682,     0,     0,   998,     0,     0,     0,     0,     0,
     625,   107,   108,   619,     0,   299,   299,     0,     0,  1005,
       0,     0,   111,   112,     0,     0,   820,     0,     0,     0,
    1021,     0,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   389,   150,     0,     0,  1540,     0,
    1046,   631,     0,   599,   150,     0,   150,  1051,     0,     0,
       0,     0,   290,     0,   290,     0,     0,     0,     0,     0,
     571,     0,   682,   144,     0,     1,     0,   625,   571,     0,
     144,     0,     0,     0,  1771,     0,     0,     0,     0,   826,
       0,   150,     0,    49,    50,    51,    52,    53,  1125,  1128,
       0,     0,     0,     0,     0,     0,     0,   599,   599,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     2,     0,   299,   306,   306,
     150,     0,   150,     0,     0,     0,     0,   389,   106,   820,
       3,   144,     0,     0,     0,     0,     0,     0,     0,  1127,
     299,     0,   294,     0,   299,   299,     0,     0,     0,     1,
       0,     0,     0,   619,  1305,   108,     0,     0,   299,     0,
       0,   299,     0,     0,     0,   111,  1306,     0,     0,   399,
       0,   435,     0,     0,     0,     0,     0,   113,     0,     0,
       0,     0,     0,    12,   389,   351,   352,    15,    16,    17,
      18,    19,    20,    21,    22,   826,  1190,     0,     0,     2,
     820,     0,   144,   700,  1837,  1838,  2026,  2027,  1839,  1840,
    2028,  2029,   106,   700,     3,     0,     0,     0,     0,     0,
       0,   869,  1987,     0,     0,     0,   571,     0,   306,     0,
       0,  1935,   150,     0,     0,   625,     0,     0,   642,   108,
    -199,     0,   643,     0,     0,   820,  1188,     0,     0,   111,
     644,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,    12,   389,   150,     0,    15,    16,    17,    18,
      19,    20,    21,    22,  1207,     0,   826,     0,     0,  1208,
     572,  1209,     0,     0,     0,     0,     0,     0,   599,     0,
       0,     0,     0,     0,   599,     0,     0,  1987,  1987,     0,
       0,     0,     0,   826,     0,     0,     0,   826,   290,     0,
     133,     0,     0,   294,  1234,     0,   632,     0,   108,     0,
       0,  1625,     0,   399,     0,   682,   820,     0,  1246,     0,
      12,   826,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,   632,     0,     0,     0,   632,     0,     0,     0,
     294,   826,     0,   820,     0,  1987,     0,   820,   826,     0,
       0,   290,   389,   682,     0,     0,  1046,     0,   631,     0,
       0,     0,     0,   299,     0,     0,   299,     0,  1987,     0,
     290,   820,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,  1297,   940,     0,   144,     0,     0,     0,     0,
       0,   820,     0,     0,     0,     0,     0,     0,   820,     0,
       0,     0,     0,   294,     0,     0,     0,     0,     0,   682,
       0,     0,     0,     0,   144,     0,     0,     0,     0,     0,
       0,  1987,  1987,     0,     0,   290,   144,     0,     0,     0,
     805,     0,     0,     0,     0,     1,     0,     0,  2192,     0,
    2101,   389,  1987,     0,   826,   290,     0,   572,     0,     0,
       0,     0,     0,     0,   144,     0,     0,     0,   399,   294,
       0,   150,     0,     0,   389,     0,     0,     0,     0,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,  1987,     0,     0,     2,     0,     0,     0,     0,
     294,     0,     0,     0,   820,     0,     0,     0,   106,     0,
       3,     0,     0,     0,  1234,     0,     0,     0,    12,     0,
     810,   104,    15,    16,    17,    18,    19,    20,    21,    22,
    2163,     1,     0,     0,  1806,   108,     0,   571,     0,     0,
     290,     0,     0,  1711,   820,   111,   112,     0,     0,     0,
       0,     0,     0,     0,  1728,     0,     0,   113,     0,     0,
       0,     0,     0,   599,     0,    12,     0,   351,   352,    15,
      16,    17,    18,    19,    20,    21,    22,   930,   150,   150,
       0,     2,     0,     0,     0,     0,     0,     0,   826,     0,
       0,     0,   826,     0,     0,     0,     3,     0,     0,     0,
     294,   632,     0,     0,     0,     0,     0,     0,    12,  1234,
     812,   813,    15,    16,    17,    18,    19,    20,    21,    22,
    1298,   108,   150,     0,     0,     0,   294,     0,     0,     0,
       0,  1493,     0,     0,     0,     0,     0,     0,   820,     0,
       0,     0,   820,   113,     0,     0,     0,     0,     0,   632,
       0,     0,   399,     0,     0,     0,     0,     0,   290,   290,
       0,   294,     0,     0,     0,   826,   109,   632,     0,   826,
     776,     0,     0,   826,   572,     0,     0,     0,     0,     0,
       0,  1492,     0,     0,     0,     0,     0,     0,     0,   290,
       0,     0,     0,     0,     0,     0,     0,     0,   294,   632,
       0,     0,   290,     0,     0,     0,     0,     0,     0,     0,
     572,     0,   572,     0,     0,   820,     0,     0,   294,   820,
     632,     0,     0,   820,     0,     0,   294,     0,     0,     0,
       0,     0,    12,     0,  1878,  1879,    15,    16,    17,    18,
      19,    20,    21,    22,  1207,     0,     0,  1234,     0,  1208,
       0,  1209,     0,     0,     0,     0,     0,     0,   820,     0,
       0,     0,     0,     0,  1602,     0,     0,     0,     0,   599,
       0,     0,     0,     0,    12,     0,   278,   104,    15,    16,
      17,    18,    19,    20,    21,    22,     0,   572,   108,     0,
       0,  1627,     0,     0,     0,   826,     0,     0,     0,   826,
       0,     0,   826,     0,     0,     0,     0,     0,     0,     0,
       0,   294,     0,    12,     0,   351,   352,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,  1933,   149,   144,
     108,  1234,   826,  1064,  1939,   149,   826,     0,   290,   290,
     826,     0,   106,     0,     0,   820,     0,     0,     0,   820,
       0,     0,   820,     0,     0,     0,     0,     0,   572,     0,
       0,     0,     0,     0,     0,     0,     0,  1964,   642,   108,
       0,     0,   643,     0,     0,     0,     0,     0,     0,   111,
     644,     0,   820,     0,   294,     0,   820,     0,     0,     0,
     820,   113,   645,     0,     0,     0,   149,     0,     0,     0,
       0,     0,     0,   572,     0,     0,     0,   298,     0,     0,
       0,     0,     0,  1695,     0,     0,     0,     0,     0,  1998,
    1999,     0,  1602,     0,     0,     0,  2009,     0,     0,     0,
       0,     0,     0,     0,   401,  2023,     0,     0,   826,     0,
       0,     0,     0,     0,     0,  2037,     0,  2038,   290,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2052,     0,  2054,  2055,  2056,     0,   572,   149,   144,     0,
       0,     0,   632,     0,     0,     0,     0,  1752,     0,     0,
       0,     0,     0,   632,   572,     0,     0,     0,   820,     0,
       0,     0,     0,     0,     0,     0,  2082,     0,     0,     0,
    2087,     0,     0,     0,     0,  2092,     0,     0,     0,   599,
       0,   572,     0,     0,     0,   572,     0,     0,     0,   572,
       0,   632,     0,  1234,   294,    12,   632,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,   572,   572,
       0,     0,     0,     0,     0,   574,     0,     0,     0,     0,
     599,     0,     0,     0,     0,     0,     0,     0,     0,   572,
       0,     0,   700,     0,  2139,     0,   572,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2148,   632,   298,     0,
    2151,   634,  1831,   109,     0,     0,  1831,  1831,   401,     0,
       0,     0,     0,   572,     0,     0,  2168,     0,     0,     0,
       0,     0,     0,  1831,     0,     0,     0,   634,     0,     0,
       0,   634,     0,   572,   190,   298,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
    1914,     0,  1918,     0,     0,     0,     0,     0,     0,     0,
       0,  2202,     0,    49,    50,    51,    52,    53,    12,     0,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     149,  2073,   572,     0,     0,     0,     0,     0,     0,     0,
       0,   486,   632,     0,   109,   202,     0,     0,   298,     0,
    2223,     0,  1917,     0,     0,     0,     0,     0,     0,   149,
       0,     0,     0,     0,     0,   572,     0,     0,   572,     0,
       0,   149,   572,   641,     0,     0,  1930,     0,     0,     0,
       0,     0,     0,     0,     0,   182,  2244,     0,   183,  2248,
     184,   185,   574,   186,     0,     0,     0,     0,     0,   149,
       0,     0,     0,   401,   298,     0,     0,     0,     0,  2257,
     187,     0,     0,    12,     0,   351,   352,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   298,     0,   632,     0,   188,
     189,   190,   106,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,     0,   199,   200,   572,     0,   764,   108,
     572,     0,   106,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,     0,     0,  2019,   572,   572,  1831,     0,
       0,   113,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   109,   202,  2042,     0,     0,     0,     0,   203,   111,
     112,   204,   205,   206,   207,     0,     0,   572,     0,    49,
      50,    51,    52,    53,     0,     0,     0,   475,  2064,     0,
     572,     0,     0,     0,     0,   298,   634,     0,     0,     0,
       0,     0,     0,   572,     0,     0,     0,   572,    12,     0,
       0,   572,    15,    16,    17,    18,    19,    20,    21,    22,
    1207,   298,     0,     0,     0,  1208,     0,  1209,     0,     0,
       0,     0,     0,     0,     0,   632,     0,     0,     0,     0,
       0,     0,     0,  2042,   634,     1,   572,   401,     0,     0,
       0,     0,   144,     0,     0,     0,   298,     0,     0,     0,
       0,     0,   634,     0,   108,     0,     0,     0,     0,   574,
       0,  1125,  2179,     0,     0,     0,     0,     0,     0,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,   298,   634,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   574,     0,   574,   106,     0,
       3,     0,     0,   298,     0,   634,     0,     0,     0,   632,
       0,   298,  2178,     0,     0,     0,   572,   572,     0,     0,
       0,     0,     0,   572,  2117,   108,     0,   572,   746,     0,
     572,     0,     0,     0,   826,   111,   112,     0,     0,     0,
    1831,  1831,    49,    50,    51,    52,    53,   113,    12,     0,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
     572,     0,     0,     0,   572,     0,     0,     0,   572,     0,
       0,     0,   574,     0,    49,    50,    51,    52,    53,     0,
       0,     0,     0,     0,   820,     0,     0,     0,     0,     0,
       0,     0,   271,     0,     0,     0,   298,     0,     0,     0,
     144,     0,     0,   691,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,   149,   357,     0,     0,     0,     0,
       0,     0,   475,     0,     0,   475,   294,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   475,     0,
       1,     0,    12,   574,   351,   352,    15,    16,    17,    18,
      19,    20,    21,    22,     0,   399,     0,     0,     2,     0,
       0,     0,   475,     0,     0,     0,   572,     0,     0,   298,
       0,   106,     0,     3,    12,     0,   351,   352,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,   574,     0,
       2,     0,     0,     0,   475,     0,     0,   764,   108,     0,
       0,   632,     0,   106,     0,     3,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,   928,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,  1305,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,  1306,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   574,   113,   149,     0,     0,     0,   634,     0,     0,
     144,     0,     0,     0,   144,   144,     0,     0,   634,   574,
       0,     0,     0,   647,     0,     0,     0,     0,     0,     0,
       0,   144,     0,     0,     0,     0,    49,    50,    51,    52,
      53,     0,     0,     0,     0,     0,   574,     0,     0,     0,
     574,     0,     0,     0,   574,     0,   634,     0,     0,   298,
       0,   634,   503,   504,   505,   506,   507,   508,   509,   510,
     511,   512,   513,   574,   574,     0,    49,    50,    51,    52,
      53,   336,     0,     0,     0,     0,     0,     0,   728,   357,
       0,     0,     0,     0,   574,     0,     0,     0,     0,     0,
       0,   574,     1,     0,     0,     0,     0,     0,   514,   768,
       0,     0,   634,     0,     0,    49,    50,    51,    52,    53,
       0,     0,     0,     0,     0,     0,     0,     0,   574,     0,
       0,     0,     0,     0,   399,     0,    12,     0,   351,   352,
      15,    16,    17,    18,    19,    20,    21,    22,   574,     0,
       0,    12,     2,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,   106,     0,     3,     0,   768,
       0,     0,     0,     0,     0,     0,    12,     0,   351,   352,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       0,  1806,   108,     0,     0,     0,     0,   574,     0,  1131,
    1133,     0,   111,   112,     0,   106,     0,   634,     0,    49,
      50,    51,    52,    53,   113,    12,     0,   351,   352,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
     574,  2117,   108,   574,     0,   746,     0,   574,     0,     0,
       0,     0,   111,   112,   106,     0,   144,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,     0,     0,     0,
       0,   632,     0,     0,     0,     0,     0,     0,     0,     0,
     353,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,   112,     0,   768,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,   634,     0,     0,     0,     0,     0,     0,    12,
       0,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   574,     0,     0,     0,   574,     0,     0,   106,     0,
       0,   632,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   574,   574,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1305,   108,     0,   117,     0,     0,
       0,     0,     0,     0,   117,   111,  1306,  1009,     0,     0,
       0,   647,   574,     0,     0,     0,     0,   113,   344,     0,
       0,     0,     0,     0,    12,   574,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,   574,     0,
       0,     0,   574,     0,     0,    12,   574,   351,   352,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,   117,     0,     0,     0,     0,
     634,     0,     0,   269,   106,     0,   284,     0,   144,   144,
       0,   574,     0,   117,     0,     0,     0,   149,     0,     0,
     768,     0,  1085,     0,     0,     0,   354,     0,     0,   382,
    1806,   108,     0,   117,   357,   357,     0,  1807,     0,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   572,   113,     0,     0,     0,     0,     0,  1126,
     768,   768,     0,     0,     0,     0,     0,     0,     0,     0,
     438,     0,     0,   768,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   634,     0,     0,     0,     0,     0,
       0,   574,   574,     0,     0,     0,     0,   471,   574,     0,
       0,     0,   574,     0,     0,   574,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,   574,   438,     0,     0,   574,
       0,     0,     0,   574,   569,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   471,   269,   269,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,   284,   438,     0,
     284,     0,     0,     0,   646,     0,   667,    12,     0,   351,
     352,    15,    16,    17,    18,    19,    20,    21,    22,     0,
       0,   298,     0,     0,     0,     0,   569,     0,     0,     0,
     569,     0,     0,     0,   284,     0,   106,   382,     0,     0,
       0,   269,     0,     0,   438,     0,     0,     0,     0,    12,
     401,   351,   352,    15,    16,    17,    18,    19,    20,    21,
      22,   574,  2117,   108,     0,     0,   746,     0,     0,   354,
     354,     0,     0,   111,   112,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   113,     0,     0,     0,     0,
     765,     0,     0,     0,     0,     0,   634,   569,   117,     0,
       0,     0,     0,     0,  1806,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   382,   111,   112,     0,     0,     0,
       0,     0,  1009,     0,     0,     0,     0,   113,     0,     0,
       0,     0,     0,     0,  1300,     0,   647,     0,   647,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     765,     0,     0,   284,   438,   149,   840,     0,     0,   149,
     149,     0,   471,  1525,     0,   471,     0,   438,   438,     0,
       0,     0,     0,     0,     0,     0,   149,     0,   471,     0,
       0,   438,   438,   438,   284,     0,     0,   471,     0,     0,
       0,     0,     0,   872,   840,    12,     0,   351,   352,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,   768,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
    1364,     0,   768,   768,     0,     0,     0,     0,     0,     0,
     353,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,   112,     0,     0,   765,   840,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,     0,     0,   401,
       0,     0,     0,     0,   284,   569,   954,   667,     0,     0,
       0,     0,     0,     0,    12,     0,   351,   352,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     0,
     284,     0,     0,     0,     0,   438,     0,   269,   269,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   569,     0,   994,     0,     0,     0,     0,
       0,   840,   438,     0,     0,   284,     0,   667,   768,   764,
     108,     0,     0,     0,     0,     0,     0,     0,   646,     0,
     111,   112,   646,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   475,
       0,     0,   284,   569,     0,     0,     0,     0,  2020,     0,
       0,   149,     0,     0,   569,     0,   569,     0,   667,     0,
       0,     0,   284,     0,   569,     0,   634,     0,     0,   438,
     569,     0,     0,  1487,     0,     0,     0,     0,     0,     0,
    1009,     0,   647,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   471,     0,     0,     0,     0,
       0,   765,     0,   354,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   354,   354,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     765,   765,   765,     0,     0,     0,   634,     0,     0,   471,
     471,     0,     0,     0,   765,   284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1825,  1833,     0,     0,  1825,  1844,
       0,     0,     0,     0,  1851,     0,     0,     0,  1855,     0,
       0,     0,     0,  1859,     0,  1844,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   438,   768,     0,     0,
       0,     0,     0,     0,   438,     0,     0,     0,   284,     0,
       0,     0,     0,     0,     0,     0,     0,   438,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   872,   872,     0,
       0,     0,     0,   149,   149,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1009,  1300,   647,   647,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1929,     0,     0,     0,   574,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     569,     0,   117,  1945,  1947,     0,   438,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   569,   954,     0,
     954,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     438,     0,     0,  1967,     0,     0,   471,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   569,     0,   569,     0,     0,   284,     0,
     284,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   569,   646,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   646,  2007,  1307,
       0,     0,  2010,     0,  2012,     0,     0,  2016,  2022,     0,
    1844,     0,     0,     0,     0,  2033,     0,     0,     0,     0,
       0,   569,   994,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   569,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   569,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1813,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2099,     0,     0,     0,   765,     0,
       0,  2106,     0,     0,     0,     0,  2108,  2109,     0,   174,
       0,   765,     0,   765,   765,     0,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   438,     0,     0,  2131,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   569,
       0,     0,   569,     0,     0,     0,     0,     0,     0,     0,
     438,     0,     0,     0,     0,  2152,     0,  2155,     0,     0,
    2157,  2159,  2160,     0,   872,     0,   872,   174,  2165,  2167,
       0,     0,     0,     0,  1813,     0,  1813,     0,     0,     0,
       0,  1915,     0,  1813,     0,   328,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,     0,     0,     0,   765,
     840,   438,     0,     0,     0,   471,     0,     0,     0,     0,
       0,     0,   954,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2209,  2211,  2213,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   182,     0,     0,   183,     0,   184,   185,     0,   186,
     569,   569,     0,     0,     0,     0,     0,     0,     0,   328,
    2232,  2234,  2236,     0,     0,     0,   187,     0,     0,     0,
       0,  1307,     0,  1307,     0,     0,     0,     0,     0,  1996,
       0,   569,  1813,  1813,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   569,   188,   189,   190,   174,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,     0,
     199,   200,     0,     0,     0,     0,   328,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   438,
     626,     0,     0,     0,     0,     0,   657,     0,     0,     0,
       0,     0,     0,     0,   201,     0,   471,   109,   202,     0,
       0,   954,  1813,     0,   203,   111,   112,   204,   205,   206,
     207,     0,   688,     0,     0,     0,     0,   208,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   768,     0,   765,     0,
       0,   438,     0,     0,     0,   749,     0,     0,     0,     0,
       0,     0,   749,   438,     0,     0,     0,     0,     0,     0,
     569,   569,     0,     0,     0,     0,     0,     0,     0,     0,
     328,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1307,     0,  1307,  1307,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1126,   768,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,     0,     0,     0,
       0,     0,     0,     0,   328,   438,     0,   328,     0,   328,
     328,     0,     0,     0,   471,     0,     0,     0,     0,     0,
     328,     0,     0,   328,   328,   328,     0,     0,     0,   328,
       0,     0,     0,     0,     0,   749,     0,     0,     0,   471,
     284,     0,     0,   768,   768,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   382,   117,
      49,    50,    51,    52,    53,     0,   328,    55,   438,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    -505,  -505,     0,  -505,    87,   438,    88,     0,     0,  -505,
       0,     0,     0,     0,     0,     0,     0,     0,   657,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   471,     0,     0,     0,     0,   438,   328,     0,     0,
    1808,   840,     0,     0,     0,     0,     0,     0,     0,   749,
     982,     0,   749,   985,   471,   989,     0,     0,   471,   471,
       0,     0,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,   471,     0,     0,   471,     0,     0,     0,     0,
     657,     3,     0,     0,   657,   657,     0,     0,     0,     0,
       0,   657,     0,     0,     0,     0,     0,     0,   382,     0,
       0,  1042,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   626,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1808,   438,  1808,     0,     0,
       0,     0,  1808,     0,  1808,     0,     0,   328,     0,     0,
       0,     0,     0,     0,     0,     0,   749,     0,     0,     0,
     749,     0,     0,     0,     0,     0,     0,   840,   438,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   749,     0,     0,     0,
       0,   328,   328,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1984,     0,     0,     0,     0,     0,
    1808,     0,     0,  1808,  1808,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   840,     0,   328,     0,
       0,     0,     0,     0,     0,   155,   328,     0,     0,     0,
     471,     0,   155,     0,     0,     0,     0,     0,     0,   626,
       0,     0,     0,     0,     0,   438,     0,     0,     0,   749,
     749,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1984,
    1984,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1808,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,     0,   174,     0,     0,     0,   174,     0,
       0,     0,     0,     0,     0,   438,     0,  2118,   840,   989,
     657,   155,   657,     0,     0,     0,     0,     0,     0,     0,
     416,     0,   328,     0,     0,     0,     0,     0,   328,     0,
    1984,     0,   749,  1267,     0,   749,  1270,     0,     0,     0,
       0,     0,     0,     0,   155,     0,     0,     0,   155,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   657,     0,   657,     0,     0,
       0,     0,     0,     0,     0,   155,     0,     0,     0,   657,
       0,     0,     0,  2118,  2118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   989,  1984,     0,     0,     0,     0,     0,
       0,     0,   438,   438,   155,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   155,   749,  2118,   765,     0,   749,     0,     0,
       0,     0,     0,     0,   749,  1344,   155,     0,   749,  1348,
       0,     0,   749,  1352,     0,     0,     0,     0,     0,     0,
       0,     0,  1355,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   749,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   155,
       0,     0,   155,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   328,     0,     0,     0,     0,   155,     0,     0,
       0,     0,     0,     0,     0,     0,   749,     0,   749,     0,
       0,     0,     0,     0,   155,     0,   155,     0,     0,     0,
       0,     0,   155,     0,     0,     0,   155,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   155,     0,
       0,   154,     0,     0,     0,     0,     0,     0,   154,     0,
       0,     0,     0,   174,     0,     0,     0,   328,     0,     0,
       0,     0,     0,     0,   657,     0,   155,     0,     0,     0,
     155,     0,   155,     0,     0,     0,     0,     0,     0,     0,
     155,     0,     0,   155,     0,   155,   155,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   155,     0,     0,   155,
     155,   155,     0,     0,     0,   155,     0,   749,  1474,   154,
     657,   657,  1481,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   154,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   154,     0,     0,
     749,  1511,   155,   749,  1515,     0,     0,   749,  1519,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,     0,   749,     0,   154,     0,     0,     0,     0,     0,
       0,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,     0,
       0,   154,     0,   657,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   749,  1617,     0,     0,     0,     0,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,     0,     0,   328,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,     0,     0,   154,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,     0,
       0,    55,   154,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,   155,    87,     0,
      88,     0,     0,     0,     0,   154,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,   155,     0,     0,     0,   328,     0,     0,
       0,     0,     0,     0,     0,     0,   174,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,   328,   154,     0,     0,     0,     0,     0,   154,     0,
       2,     0,   154,     0,     0,     0,     0,   155,   155,     0,
       0,     0,     0,     0,   154,     3,     0,     0,     0,     0,
       0,   174,     0,     0,     0,     0,     0,     0,     0,     0,
     328,   155,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,   154,     0,   154,     0,
       0,     0,     0,     0,     0,     0,   154,     0,     0,   154,
       0,   154,   154,   155,     0,     0,     0,   174,     0,     0,
       0,     0,   154,   155,   155,   154,   154,   154,     0,     0,
       0,   154,   155,     0,     0,     0,     0,     0,     0,     0,
       0,   159,     0,     0,     0,   155,     0,     0,   159,     0,
       0,     0,     0,   328,     0,     0,     0,     0,   328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,     0,   154,     0,
     174,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   328,     0,     0,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,     0,     0,     0,     0,     0,     0,   159,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,   155,     0,
       0,     0,     0,     0,   155,     0,     0,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,   749,     0,     0,   159,     0,   154,     0,     0,     0,
       0,  1230,     0,     0,     0,     0,     0,     0,     0,     0,
     174,     0,     0,    49,    50,    51,    52,    53,    54,     0,
      55,   159,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -505,  -505,     0,  -505,    87,     0,    88,
     159,     0,  -505,   154,   280,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   749,     0,   159,   154,
       0,     0,     0,   749,     0,     0,     0,     0,     0,     0,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,     0,   174,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,     0,     0,
       0,     0,   106,   154,   154,   159,     0,     0,   159,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   749,   749,     0,     0,     0,     0,   154,   155,     0,
       0,   109,   110,     0,   749,     0,     0,     0,     0,   111,
     112,     0,     0,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   154,
     159,     0,   159,     0,     0,     0,     0,     0,   159,   154,
     154,     0,   159,     0,     0,     0,     0,   328,   154,   749,
       0,     0,     0,     0,   159,     0,     0,     0,     0,     0,
       0,   154,     0,   155,     0,     0,     0,     0,     0,     0,
       0,     0,   749,     0,     0,     0,     0,   264,     0,     0,
       0,     0,   159,     0,     0,     0,   159,     0,   159,     0,
       0,     0,     0,     0,     0,     0,   159,     0,     0,   159,
       0,   159,   159,     0,     0,     0,     0,     0,   385,     0,
       0,     0,   159,     0,     0,   159,   159,   159,     0,     0,
     409,   159,   417,     0,   419,   749,   749,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   154,     0,     0,     0,
       0,     0,   749,  2193,     0,     0,   749,     0,     0,     0,
       0,     0,     0,     0,   174,   174,     0,     0,     0,     0,
       0,     0,     0,     0,   154,     0,   459,     0,   159,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   749,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   155,     0,   447,     0,   450,     0,
       0,   453,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   463,   464,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
     519,   453,   453,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,   155,
       0,   674,     0,   419,     0,     0,   159,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,     0,   385,     0,   417,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   453,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,   159,
       0,     0,   155,   385,   154,   417,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   155,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,   385,     0,     0,     0,     0,
       0,     0,     0,   159,   159,     0,     0,   155,     0,     0,
       0,     0,     0,     0,     0,     0,   155,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,   159,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
     159,     0,     0,     0,     0,     0,     0,     0,   159,     0,
       0,     0,     0,     0,     0,   453,     0,     0,     0,   155,
       0,   159,   155,     0,   155,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
       0,     0,   155,     0,     0,     0,   155,   155,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,   674,   419,
     155,     0,     0,   155,     0,     0,   453,   453,   453,   453,
     453,   453,   453,   453,   453,   453,   453,   453,   453,   453,
     453,   453,   453,   453,   453,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,     0,     0,     0,
     154,     0,     0,     0,   385,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,     0,   385,     0,   674,   419,
       0,     0,     0,     0,   159,     0,     0,     0,     0,     0,
     159,     0,     0,     0,   155,   235,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   154,   385,   674,     0,     0,
       0,     0,     0,     0,     0,     0,   155,   385,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     459,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   285,   453,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   154,
       0,   385,     0,     0,     0,     0,     0,     0,   154,     0,
       0,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   155,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,   385,
       0,     0,   385,   385,     0,     0,     0,     0,     0,     0,
     485,     0,     0,     0,     0,   385,     0,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,     0,
     385,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,     0,     0,  1483,     0,
       0,     0,     0,     0,     0,     0,    49,    50,    51,    52,
      53,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   595,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   618,
       0,     0,     0,     0,     0,     0,     0,     0,   182,     0,
       0,   183,     0,   184,   185,   154,   186,     0,   154,   159,
     154,   453,     0,     0,     0,     0,   453,     0,     0,     0,
       0,     0,     0,   187,     0,     0,   285,     0,   154,     0,
       0,     0,   154,   154,   453,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   711,   154,   711,     0,   154,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,     0,   199,   200,     0,
       0,   453,     0,     0,     0,   106,     0,     0,     0,     0,
     155,   155,     0,   385,     0,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,     0,     0,   109,   202,   385,     0,     0,     0,
     154,   203,  1484,   112,   204,   205,   206,   207,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,   837,   385,     0,   453,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
     159,     0,     0,   855,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   618,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   887,     0,   889,
       0,     0,     0,     0,     0,     0,   235,     0,   900,   235,
       0,     0,     0,     0,     0,   159,     0,     0,     0,     0,
       0,     0,   385,   905,     0,     0,     0,     0,     0,   453,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   235,     0,     0,   932,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   285,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   618,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,     0,     0,     0,     0,     0,   159,     0,
       0,     0,     0,     0,     0,     0,     0,   995,   997,   385,
     285,   453,   453,   453,     0,     0,     0,   618,     0,     0,
       0,   385,     0,   159,     0,     0,     0,     0,   453,   453,
       0,   285,     0,  1020,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,   285,     0,     0,   453,     0,     0,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   385,     0,   285,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   837,     0,     0,     0,   485,   711,     0,     0,     0,
       0,     0,   711,     0,     0,     0,     0,   595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1115,   159,     0,     0,   159,     0,
     159,     0,     0,     0,     0,     0,   154,   154,     0,     0,
       0,     0,     0,   453,     0,   453,     0,     0,   159,     0,
       0,   315,   159,   159,     0,     0,     0,   285,   331,     0,
       0,     0,     0,     0,     0,     0,   159,     0,     0,   159,
       0,     0,     0,     0,   393,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   442,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     285,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,   482,     0,     0,     0,     0,  1183,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,   900,
       0,     0,   900,   235,  1204,     0,     0,     0,     0,     0,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   711,     0,     0,     0,     0,     0,     0,   577,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,   638,     0,   182,     0,     0,
     183,   670,   184,   185,     0,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1262,     0,
       0,   684,   187,     0,     0,   690,     0,     0,     0,   315,
       0,     0,   697,     0,     0,     0,     0,   385,     0,     0,
     618,     0,   453,     0,     0,     0,  1283,     0,     0,     0,
       0,   188,   189,   190,   159,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,     0,   199,   200,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,   315,   331,   995,     0,     0,     0,     0,  1837,
    1838,     0,     0,  1839,  1840,     0,     0,     0,     0,   393,
     201,  1946,     0,   109,   202,     0,     0,     0,     0,   711,
     203,   111,   112,   204,   205,   206,   207,     0,     0,     0,
     385,     0,  1331,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,     0,     0,     0,     0,   182,   577,   331,
     183,   841,   184,   185,     0,   186,     0,   331,     0,     0,
     482,     0,   482,   331,     0,     0,     0,     0,     0,     0,
       0,     0,   187,   482,     0,     0,   482,   482,   482,   577,
       0,     0,   331,     0,     0,     0,     0,   385,     0,   697,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,   705,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,     0,   199,   200,     0,   331,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   711,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   159,   159,     0,     0,
     201,   108,     0,   706,   707,  1429,   385,     0,   708,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,   315,
     638,     0,   955,     0,     0,     0,     0,   595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   991,     0,
     670,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,  1003,     0,     0,  1486,     0,     0,     0,     0,
       0,     0,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     453,     0,     0,   711,     0,     0,     0,   577,   638,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   315,
       0,   315,     0,     0,     0,     0,     0,   577,     0,  1060,
       0,     0,     0,     0,     0,   577,     0,  1526,     0,  1527,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,   419,     0,     0,     0,     0,
     711,     0,  1429,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,     0,     0,   331,   331,     0,     0,     0,     0,
     315,     0,     0,     0,   453,     0,     0,     0,     0,   385,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,     0,     0,   183,     0,   184,
     185,     0,   186,     0,     0,     0,     0,     0,     0,     0,
    1661,  1662,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,   453,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,     0,     0,     0,     0,     0,   482,
       0,     0,     0,   577,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,     0,   385,     0,  1731,  1731,     0,     0,     0,
       0,     0,   618,     0,     0,     0,     0,   201,   483,     0,
     109,   202,     0,   484,   453,     0,   453,   203,   111,   112,
     204,   205,   206,   207,     0,   315,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1756,     0,
       0,  1758,   991,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,   453,     0,     0,     0,
       0,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   315,     0,
    1279,     0,     0,   577,     0,   638,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,     0,     0,     0,     0,  1792,     0,     0,     0,
       0,     0,     0,   595,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   453,   235,
       0,     0,     0,     0,     0,     0,   991,   955,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   711,     0,
       0,     0,   315,     0,   595,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2246,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   711,     0,     0,     0,     0,     0,  -875,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1916,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -875,     0,     0,  -875,     0,
    -875,  -875,     0,  -875,   577,     0,     0,   315,     0,     0,
       0,     0,     0,     0,     0,   482,     0,     0,  -875,     1,
    -875,  -875,     0,  -875,  -258,  -258,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,  -875,     0,  -875,  1960,  -875,
    -875,  -875,     0,  -875,  -875,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,     2,
    -875,  -875,  -875,     0,  -875,  -875,     0,     0,     0,     0,
     331,     0,  -875,     0,     3,     0,     0,     0,  2246,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -875,     0,  -258,  -875,     0,
       0,  -875,  -875,     0,     0,     0,  -875,     0,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,   315,   315,     0,     0,     0,
       0,     0,     0,  -875,     0,     0,     0,     0,     0,     0,
       0,     0,  -875,     0,     0,  -875,     0,  -875,  -875,     0,
    -875,     0,     0,     0,     0,     0,   315,     0,     0,     0,
       0,     0,     0,     0,     0,  -875,     1,  -875,  -875,   315,
    -875,  -259,  -259,  -875,  -875,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,     0,  -875,     0,  -875,  -875,  -875,     0,
    -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,  -875,  -875,  -875,  -875,     2,  -875,  -875,  -875,
       0,  -875,  -875,     0,     0,     0,     0,     0,   235,  -875,
    1963,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1710,     0,     0,
       0,     0,     0,     0,  -259,  -875,     0,     0,  -875,  -875,
       0,     0,     0,  -875,     0,  -875,  -875,  -875,  -875,  -875,
    -875,  -875,     0,     0,     0,     0,     0,     0,     0,     0,
    -875,     0,     0,     0,   182,     0,   331,   183,     0,   184,
     185,     0,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,   315,  1547,     0,   187,
    1549,     0,  1550,     0,     0,  1551,  1552,  1553,  1554,  1555,
    1556,  1557,  1558,  1559,  1560,  1561,  -357,  -357,  1562,  1563,
    1564,  1565,  1566,  1567,  1568,     0,  1569,     0,   188,   189,
     190,     0,   705,   192,  1570,  1571,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,  1572,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1573,     0,     0,
     109,   202,     0,     0,     0,   426,     0,   203,   111,   112,
     204,   205,   206,   207,   482,   315,     0,     0,     0,     0,
       0,     0,  -198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   393,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   482,     0,     0,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,    97,   331,     0,     0,    98,
       0,   482,     0,    99,     0,     0,   697,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     100,     0,     0,     0,     0,   101,   102,   331,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   393,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,   108,     0,   109,   110,     0,     0,     0,     0,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,   482,     0,     0,   113,     0,   114,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   393,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    1546,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,    89,    90,    91,    92,
      93,    94,    95,    96,    97,     0,     0,     0,    98,     0,
    1547,   393,  1548,  1549,     0,  1550,     0,     0,  1551,  1552,
    1553,  1554,  1555,  1556,  1557,  1558,  1559,  1560,  1561,  -357,
    -357,  1562,  1563,  1564,  1565,  1566,  1567,  1568,     0,  1569,
     482,   188,   189,   190,   101,   705,   192,  1570,  1571,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
    1572,     0,   196,   197,   198,     0,   199,   200,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1573,     0,     0,   109,  1574,     0,     0,     0,   426,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,     0,
     482,     0,     0,   697,     0,  -198,    39,   260,    41,    42,
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
       0,     0,   188,   189,   190,   101,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,     0,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1837,  1838,     0,     0,  1839,  1840,     0,     0,     0,     0,
       0,   201,  1841,  1842,   109,  1574,     0,     0,     0,     0,
       0,   203,   111,   112,   204,   205,   206,   207,     0,     0,
       0,     0,     0,     0,     0,     0,  1843,    39,   260,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,   182,
       0,    87,   183,    88,   184,   185,     0,   186,    89,    90,
      91,    92,    93,    94,    95,    96,    97,     0,     0,     0,
      98,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,   101,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,     0,   199,   200,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1837,  1838,     0,     0,  1839,  1840,     0,     0,     0,
       0,     0,   201,  1841,     0,   109,  1574,     0,     0,     0,
       0,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,     0,     0,     0,     0,     0,     0,  1843,   260,    41,
      42,    43,    44,    45,    46,    47,    48,     0,     0,     0,
       0,     0,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,   375,   376,     0,
     377,    87,     0,    88,     0,     0,   378,     0,     0,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   380,   260,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -506,  -506,     0,  -506,
      87,     0,    88,     0,     0,  -506,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -481,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    12,  -481,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,   108,     0,   109,   381,     0,     0,     0,  -846,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,   380,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,  -506,  -506,     0,  -506,    87,
       0,    88,     0,     0,  -506,    49,    50,    51,    52,    53,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,  -506,  -506,     0,  -506,    87,
       0,    88,     0,     0,  -506,    12,     0,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,   108,     0,   109,   381,     0,     0,     0,     0,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   280,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    12,     0,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     3,   818,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1007,   108,
    -707,   109,   643,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,   260,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,  -506,  -506,     0,  -506,    87,     0,    88,     0,     0,
    -506,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    12,     0,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,   108,     0,   109,
     381,     0,     0,     0,  -850,     0,     0,   111,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
     260,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,  -506,
    -506,     0,  -506,    87,     0,    88,     0,     0,  -506,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
       0,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,   108,     0,   109,   381,     0,
       0,     0,     0,     0,     0,   111,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   113,    39,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     182,     0,    87,   183,    88,   184,   185,     0,   186,    89,
      90,    91,    92,    93,    94,    95,    96,    97,     0,     0,
       0,    98,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,   101,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,     0,   199,
     200,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,  1835,   109,  1574,     0,     0,
       0,     0,     0,   203,   111,   112,   204,   205,   206,   207,
      39,   260,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,    89,    90,    91,    92,    93,    94,    95,    96,    97,
       0,     0,     0,    98,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,   101,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
       0,   199,   200,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,  1574,
       0,     0,     0,     0,     0,   203,   111,   112,   204,   205,
     206,   207,   260,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,   280,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,     0,   199,   200,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,   483,     0,   109,
     281,   617,   484,     0,     0,     0,   203,   283,   112,   204,
     205,   206,   207,   260,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,   483,     0,
     109,   629,   861,   484,     0,     0,     0,   203,   283,   112,
     204,   205,   206,   207,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   182,     0,    87,   183,    88,
     184,   185,     0,   186,   280,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,     0,   199,   200,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,   483,
       0,   109,   281,   693,   484,     0,     0,     0,   203,   283,
     112,   204,   205,   206,   207,   260,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,     0,   199,   200,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
     483,     0,   109,   281,   951,   484,     0,     0,     0,   203,
     283,   112,   204,   205,   206,   207,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,   280,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,     0,   199,   200,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,   483,     0,   109,   629,  1045,   484,     0,     0,     0,
     203,   283,   112,   204,   205,   206,   207,   260,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,   280,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,     0,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,   483,     0,   109,   281,   282,   484,     0,     0,
       0,   203,   283,   112,   204,   205,   206,   207,   260,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,   182,
       0,    87,   183,    88,   184,   185,     0,   186,   280,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,     0,   199,   200,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   109,   281,   282,     0,     0,
       0,     0,   203,   283,   112,   204,   205,   206,   207,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     182,     0,    87,   183,    88,   184,   185,     0,   186,   280,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,     0,   199,
     200,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   109,   281,   693,     0,
       0,     0,     0,   203,   283,   112,   204,   205,   206,   207,
     260,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,   182,     0,    87,   183,    88,   184,   185,     0,   186,
     280,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,     0,
     199,   200,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   109,   281,   951,
       0,     0,     0,     0,   203,   283,   112,   204,   205,   206,
     207,   260,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
       0,   199,   200,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,   629,
    1045,     0,     0,     0,     0,   203,   283,   112,   204,   205,
     206,   207,   260,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,   280,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,     0,   199,   200,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,     0,     0,   109,
     281,   617,     0,     0,     0,     0,   203,   283,   112,   204,
     205,   206,   207,   260,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,     0,     0,
     109,   629,   861,     0,     0,     0,     0,   203,   283,   112,
     204,   205,   206,   207,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   182,     0,    87,   183,    88,
     184,   185,     0,   186,   280,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,     0,   199,   200,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,   109,   629,     0,     0,     0,     0,     0,   203,   836,
     112,   204,   205,   206,   207,   260,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,     0,   199,   200,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
       0,     0,   109,   992,     0,     0,     0,     0,     0,   203,
     993,   112,   204,   205,   206,   207,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,   280,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,     0,   199,   200,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,   109,   629,     0,     0,     0,     0,     0,
     203,   283,   112,   204,   205,   206,   207,   260,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,   280,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,     0,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,     0,     0,   109,   202,     0,     0,     0,     0,
       0,   203,   111,   112,   204,   205,   206,   207,  2080,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,    -2,    -2,  2112,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,     0,    -2,     0,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,  1230,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,    -2,    -2,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -505,  -505,
       0,  -505,    87,     0,    88,     0,     0,  -505,     0,   280,
      90,    91,    92,    93,    94,    95,    96,     0,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     103,   104,    87,     0,    88,     0,     0,     0,     0,   280,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1372,     0,  1230,     0,   109,   110,     0,     0,
     103,   104,     0,     0,   111,   112,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -505,  -505,     0,  -505,
      87,     0,    88,     0,     0,  -505,   109,   280,    90,    91,
      92,    93,    94,    95,    96,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1438,     0,  1230,     0,   109,   110,     0,     0,   103,   104,
       0,     0,   111,   112,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -505,  -505,     0,  -505,    87,     0,
      88,     0,     0,  -505,   109,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1531,     0,
    1230,     0,   109,   110,     0,     0,     0,     0,     0,     0,
     111,   112,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -505,  -505,     0,  -505,    87,     0,    88,     0,
       0,  -505,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1648,     0,  1230,     0,
     109,   110,     0,     0,     0,     0,     0,     0,   111,   112,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    -505,  -505,     0,  -505,    87,     0,    88,     0,     0,  -505,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1777,     0,     0,     0,   109,   110,
       0,     0,     0,     0,     0,     0,   111,   112,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,     0,   109,   318,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,    49,    50,
      51,    52,    53,    54,     0,    55,   113,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,     0,   109,     0,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,    49,    50,
      51,    52,    53,    54,     0,    55,   113,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,     0,   109,   110,     0,     0,
       0,  -848,     0,     0,   111,   112,     0,     0,    49,    50,
      51,    52,    53,    54,     0,    55,   113,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,     0,   109,   110,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,    49,    50,
      51,    52,    53,     0,     0,    55,   113,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -506,  -506,
       0,  -506,    87,     0,    88,     0,     0,  -506,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,     0,   109,   694,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,    39,   260,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,     0,     0,  -425,  -425,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -425,     0,     0,     0,   109,   110,     0,     0,     0,
       0,     0,     0,   111,   112,    39,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,    97,     0,     0,     0,    98,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   110,     0,  1687,     0,  1688,     0,
       0,   111,   112,  1689,     0,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,  1690,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   852,     0,     0,     0,     0,
       0,     0,   111,   112,   380,   260,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -506,  -506,   380,  -506,    87,     0,
      88,     0,     0,  -506,     0,     0,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -506,  -506,     0,  -506,
      87,     0,    88,     0,     0,  -506,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,   109,   381,     0,     0,     0,     0,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   694,     0,     0,     0,     0,
       0,     0,   111,   112,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,   280,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     3,   818,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   871,     0,
    -707,   109,   746,     0,     0,     0,     0,     0,     0,   111,
     112,   260,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     3,   818,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   952,     0,  -707,   109,   643,
       0,     0,     0,     0,     0,     0,   111,   112,   260,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,   280,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,  1273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -717,   109,   779,     0,     0,     0,
       0,     0,     0,   111,   112,   260,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   567,   109,   568,     0,     0,     0,     0,     0,     0,
     111,   112,   260,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,   280,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     779,   780,     0,     0,     0,     0,     0,   111,   112,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,   280,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
    1652,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   779,     0,     0,
       0,     0,     0,     0,   111,   112,   260,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,   280,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,  1654,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   779,     0,     0,     0,     0,     0,
       0,   111,   112,   260,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   666,     0,     0,     0,     0,     0,     0,   111,   112,
     260,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     280,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   779,     0,
       0,     0,     0,     0,     0,   111,   112,   260,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   280,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   568,     0,     0,     0,     0,
       0,     0,   111,   112,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -506,  -506,     0,  -506,    87,     0,    88,
       0,     0,  -506,     0,     0,   260,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,   103,   104,     0,    87,     0,
      88,  1710,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,     0,
       0,   183,     0,   184,   185,     0,   186,     0,     0,     0,
       0,   109,   381,     0,   456,     0,   457,   458,     0,   111,
     112,  1547,     0,   187,  1549,     0,  1550,  2000,  2001,  1551,
    1552,  1553,  1554,  1555,  1556,  1557,  1558,  1559,  1560,  1561,
       0,     0,  1562,  1563,  1564,  1565,  1566,  1567,  1568,     0,
    1569,     0,   188,   189,   190,     0,   705,   192,  1570,  1571,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,  1572,     0,   196,   197,   198,   -17,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,  1710,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1573,     0,     0,   109,   202,     0,     0,     0,   426,
       0,   203,   111,   112,   204,   205,   206,   207,     0,   182,
       0,     0,   183,     0,   184,   185,  -198,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1547,     0,   187,  1549,     0,  1550,     0,     0,
    1551,  1552,  1553,  1554,  1555,  1556,  1557,  1558,  1559,  1560,
    1561,     0,     0,  1562,  1563,  1564,  1565,  1566,  1567,  1568,
       0,  1569,     0,   188,   189,   190,     0,   705,   192,  1570,
    1571,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,  1572,     0,   196,   197,   198,     0,   199,   200,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1573,     0,     0,   109,   202,     0,     0,     0,
     426,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,     0,     0,     0,     0,     0,     0,  -198,   430,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -428,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,   430,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,   109,    87,     0,    88,
       0,  -428,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -429,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,   430,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,   109,    87,     0,    88,     0,  -429,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,  -428,    49,    50,    51,    52,    53,    54,   465,    55,
     466,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,     0,     0,     0,  1561,     0,  -357,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1573,     0,     0,
     109,   468,     0,     0,     0,   426,     0,   203,   111,   112,
     469,   470,   206,   207,    49,    50,    51,    52,    53,    54,
     465,    55,   466,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   467,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,     0,   199,   200,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
       0,     0,   109,   468,     0,     0,     0,   426,     0,   203,
     111,   112,   469,   470,   206,   207,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     2,   196,   197,   198,     0,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     3,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,     0,   593,   109,   594,     0,     0,     0,     0,
       0,   203,   111,   112,   204,   205,   206,   207,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     182,     0,    87,   183,    88,   184,   185,     0,   186,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     1,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     2,   196,   197,   198,     0,   199,
     200,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   109,   594,     0,     0,
       0,   426,     0,   203,   111,   112,   204,   205,   206,   207,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     2,   196,   197,   198,
       0,   199,   200,     0,     0,     0,     0,     0,     0,   106,
       0,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,   629,
       0,     0,     0,     0,     0,   203,   111,   112,   204,   205,
     206,   207,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     2,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,     0,     0,
     109,   594,     0,     0,     0,     0,     0,   203,   111,   112,
     204,   205,   206,   207,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,     0,   199,   200,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
       0,     0,   109,   468,     0,     0,     0,   426,     0,   203,
     111,   112,   204,   205,   206,   207,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,     0,   199,   200,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,     0,     0,   109,   594,     0,     0,     0,   426,
       0,   203,   111,   112,   204,   205,   206,   207,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     182,     0,    87,   183,    88,   184,   185,     0,   186,   280,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,     0,   199,
     200,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   109,   202,     0,     0,
       0,     0,     0,   203,   111,   112,   204,   205,   206,   207,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
      51,    52,    53,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
     182,   199,   200,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,   629,
       0,     0,     0,     0,     0,   203,   111,   112,   204,   205,
     206,   207,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,     0,   199,
     200,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1837,  1838,     0,     0,  1839,  1840,     0,     0,
       0,     0,     0,   201,  2015,     0,   109,   202,     0,     0,
       0,     0,     0,   203,   111,   112,   204,   205,   206,   207,
     380,   260,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    -506,  -506,     0,  -506,    87,     0,    88,     0,     0,  -506,
       0,   260,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   103,   104,    87,     0,    88,     0,     0,     0,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
      12,     0,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     3,   818,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -707,   109,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
     280,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
       0,   621,   104,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   987,     0,     0,   109,   686,     0,
       0,     0,     0,     0,     0,   111,   112,   260,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   280,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     3,   818,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -707,   109,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    12,     0,   621,   104,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   622,     0,     0,     0,     0,     0,
       0,   111,   112,   260,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   260,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
     109,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   318,     0,     0,     0,     0,     0,     0,   111,   112,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   568,
       0,     0,     0,     0,     0,     0,   111,   112,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   852,     0,     0,
       0,     0,     0,     0,   111,   112,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,     0,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   857,   109,   852,     0,     0,   103,   104,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   987,     0,     0,   109,   622,     0,     0,     0,     0,
       0,     0,   111,   112,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   280,    90,    91,    92,    93,
      94,    95,    96,     0,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   103,   104,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,  1397,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   987,
       0,     0,   109,   686,     0,     0,   103,   104,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   852,     0,     0,     0,     0,     0,     0,
     111,   112,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   437,     0,     0,   103,   104,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   110,     0,     0,     0,     0,     0,     0,   111,   112,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   103,   104,    87,     0,    88,     0,     0,     0,
       0,   280,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   437,
       0,     0,   103,   104,     0,     0,   111,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   686,
       0,     0,     0,     0,     0,     0,   111,   112,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     103,   104,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   703,     0,     0,
     103,   104,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   852,     0,     0,
       0,     0,     0,     0,   111,   112,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   280,    90,    91,
      92,    93,    94,    95,    96,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,     0,    88,     0,     0,     0,     0,   280,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   666,     0,     0,   103,   104,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   568,     0,     0,     0,     0,
       0,     0,   111,   112,   260,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -506,  -506,     0,  -506,    87,     0,    88,
       0,     0,  -506,     0,     0,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,   103,   104,    89,    90,    91,
      92,    93,    94,    95,    96,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   103,   104,
      87,   109,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   318,     0,     0,   103,   104,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   622,     0,     0,     0,     0,
       0,     0,   111,   112,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   280,    90,    91,    92,    93,
      94,    95,    96,     0,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   103,   104,    87,     0,
      88,     0,     0,     0,     0,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   934,     0,     0,   103,   104,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   852,     0,     0,     0,     0,     0,     0,
     111,   112,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,   280,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   694,     0,     0,   103,   104,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,    49,    50,    51,    52,    53,   111,   112,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -506,  -506,     0,  -506,    87,     0,    88,
       0,     0,  -506,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -506,  -506,     0,  -506,    87,     0,    88,
       0,     0,  -506,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,   109,   694,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   934,     0,    49,    50,    51,    52,    53,   111,
     112,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -506,  -506,     0,  -506,    87,     0,
      88,     0,     0,  -506,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,   182,
       0,    87,   183,    88,   184,   185,     0,   186,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,   109,     0,   196,   197,   198,     0,   199,   200,
     111,   112,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   109,   202,  1114,     0,     0,
       0,     0,   203,   283,   112,   204,   205,   206,   207,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,     0,   199,   200,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,     0,     0,
     109,   202,     0,     0,     0,     0,     0,   203,   111,   112,
     204,   205,   206,   207,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   280,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,    54,
       0,    55,   109,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,     0,
       0,    55,   109,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -506,  -506,     0,  -506,    87,     0,
      88,     0,     0,  -506,   182,     0,     0,   183,     0,   184,
     185,     0,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,   182,   199,   200,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,   109,     0,     0,     0,     0,   201,  1196,     0,
     109,   202,     0,     0,     0,  1197,     0,   203,   111,   112,
     204,   205,   206,   207,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
     182,   199,   200,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,   899,     0,   109,   202,
    1201,     0,     0,     0,     0,   203,   111,   112,   204,   205,
     206,   207,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,   182,   199,
     200,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   109,   202,     0,     0,
       0,   708,     0,   203,   111,   112,   204,   205,   206,   207,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,   182,   199,   200,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,     0,     0,   109,   202,     0,     0,     0,   426,
       0,   203,   111,   112,   204,   205,   206,   207,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,   182,   199,   200,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
     899,     0,   109,   202,     0,     0,     0,     0,     0,   203,
     111,   112,   204,   205,   206,   207,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,   182,   199,   200,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,     0,     0,
     109,   202,     0,     0,   927,     0,     0,   203,   111,   112,
     204,   205,   206,   207,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
     182,   199,   200,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,   202,
    1017,     0,     0,     0,     0,   203,   283,   112,   204,   205,
     206,   207,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,   182,   199,
     200,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,   109,   202,  1043,     0,
       0,     0,     0,   203,   111,   112,   204,   205,   206,   207,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,   182,   199,   200,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,  1428,     0,   109,   202,     0,     0,     0,     0,
       0,   203,   111,   112,   204,   205,   206,   207,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,   182,   199,   200,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
       0,     0,   109,   202,  1524,     0,     0,     0,     0,   203,
     111,   112,   204,   205,   206,   207,     0,     0,   188,   189,
     190,     0,   191,   192,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   193,   194,   195,     0,     0,   196,
     197,   198,   182,   199,   200,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,     0,     0,
     109,   202,     0,     0,     0,  1720,     0,   203,   111,   112,
     204,   205,   206,   207,     0,     0,   188,   189,   190,     0,
     191,   192,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   193,   194,   195,     0,     0,   196,   197,   198,
     182,   199,   200,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   201,     0,     0,   109,   202,
       0,     0,     0,  1774,     0,   203,   111,   112,   204,   205,
     206,   207,     0,     0,   188,   189,   190,     0,   191,   192,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     193,   194,   195,     0,     0,   196,   197,   198,   182,   199,
     200,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,  2006,   109,   202,     0,     0,
       0,     0,     0,   203,   111,   112,   204,   205,   206,   207,
       0,     0,   188,   189,   190,     0,   191,   192,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   193,   194,
     195,     0,     0,   196,   197,   198,   182,   199,   200,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   201,  2011,     0,   109,   202,     0,     0,     0,     0,
       0,   203,   111,   112,   204,   205,   206,   207,     0,     0,
     188,   189,   190,     0,   191,   192,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   193,   194,   195,     0,
       0,   196,   197,   198,     0,   199,   200,     0,     0,   182,
       0,     0,   183,   106,   184,   185,     0,   186,  2098,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,   201,
    2021,     0,   109,   202,     0,     0,     0,     0,     0,   203,
     111,   112,   204,   205,   206,   207,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,   182,   199,   200,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   109,   202,     0,     0,     0,
       0,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,   182,   199,   200,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,  2105,     0,   109,   202,     0,     0,     0,     0,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,   182,   199,   200,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,  2107,
       0,   109,   202,     0,     0,     0,     0,     0,   203,   111,
     112,   204,   205,   206,   207,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,   182,   199,   200,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,  2154,     0,   109,
     202,     0,     0,     0,     0,     0,   203,   111,   112,   204,
     205,   206,   207,     0,     0,   188,   189,   190,     0,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,   182,
     199,   200,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,  2156,     0,   109,   202,     0,
       0,     0,     0,     0,   203,   111,   112,   204,   205,   206,
     207,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,   182,   199,   200,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,  2158,     0,   109,   202,     0,     0,     0,
       0,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,   182,   199,   200,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,  2164,     0,   109,   202,     0,     0,     0,     0,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,   182,   199,   200,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,  2166,
       0,   109,   202,     0,     0,     0,     0,     0,   203,   111,
     112,   204,   205,   206,   207,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,   182,   199,   200,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,  2208,     0,   109,
     202,     0,     0,     0,     0,     0,   203,   111,   112,   204,
     205,   206,   207,     0,     0,   188,   189,   190,     0,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,   182,
     199,   200,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,  2210,     0,   109,   202,     0,
       0,     0,     0,     0,   203,   111,   112,   204,   205,   206,
     207,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,   182,   199,   200,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,  2212,     0,   109,   202,     0,     0,     0,
       0,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,   182,   199,   200,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,  2231,     0,   109,   202,     0,     0,     0,     0,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,   182,   199,   200,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,  2233,
       0,   109,   202,     0,     0,     0,     0,     0,   203,   111,
     112,   204,   205,   206,   207,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,   182,   199,   200,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,  2235,     0,   109,
     202,     0,     0,     0,     0,     0,   203,   111,   112,   204,
     205,   206,   207,     0,     0,   188,   189,   190,     0,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,   182,
     199,   200,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   446,     0,     0,   109,   202,     0,
       0,     0,     0,     0,   203,   111,   112,   204,   205,   206,
     207,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,   182,   199,   200,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   449,     0,     0,   109,   202,     0,     0,     0,
       0,     0,   203,   111,   112,   204,   205,   206,   207,     0,
       0,   188,   189,   190,     0,   191,   192,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   193,   194,   195,
       0,     0,   196,   197,   198,   182,   199,   200,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,   109,   202,     0,     0,     0,     0,     0,
     203,   111,   112,   204,   205,   206,   207,     0,     0,   188,
     189,   190,     0,   191,   192,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   193,   194,   195,     0,     0,
     196,   197,   198,   182,   199,   200,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   462,     0,
       0,   109,   202,     0,     0,     0,     0,     0,   203,   111,
     112,   204,   205,   206,   207,     0,     0,   188,   189,   190,
       0,   191,   192,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   193,   194,   195,     0,     0,   196,   197,
     198,   182,   199,   200,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   518,     0,     0,   109,
     202,     0,     0,     0,     0,     0,   203,   111,   112,   204,
     205,   206,   207,     0,     0,   188,   189,   190,     0,   191,
     192,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   193,   194,   195,     0,     0,   196,   197,   198,   182,
     199,   200,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   109,   202,     0,
       0,     0,     0,     0,   203,   283,   112,   204,   205,   206,
     207,     0,     0,   188,   189,   190,     0,   191,   192,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   193,
     194,   195,     0,     0,   196,   197,   198,     0,   199,   200,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   201,     0,     0,   109,   202,     0,     0,     0,
       0,     0,   203,   836,   112,   204,   205,   206,   207,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,   260,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,   261,     0,
     262,   263,    87,     0,    88,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   456,     0,
     457,   458
};

static const yytype_int16 yycheck[] =
{
       3,   719,   170,   339,  1242,   107,     9,    38,   201,   137,
       7,   379,   353,   107,   872,   473,    31,   549,    31,    31,
     642,   288,   259,    38,   107,    38,    38,   248,    31,  1589,
      33,   162,   174,    36,   667,    38,    39,   498,  1229,   250,
     501,    30,   250,   396,   117,   708,   288,   353,   203,   642,
     647,  1140,   242,  1248,   107,  1573,   653,   678,  1235,   680,
    2000,   787,   353,     1,   896,    31,  1144,  1390,    99,   642,
     107,  1208,    38,   799,    31,  1984,    31,    82,    95,   646,
     288,    38,  2004,    38,    99,    10,    99,    99,   109,     1,
     353,   107,    82,    91,    97,    98,    99,   642,   930,   143,
       0,   290,   353,   405,   107,     3,  1983,   238,   201,   168,
     113,     9,   240,   127,   117,     0,   363,   145,  1983,    79,
     106,   423,   125,   642,    20,    96,   353,   130,   452,    10,
     133,   353,  2041,    99,   137,    33,    82,   107,    36,   133,
     282,    95,    99,   642,    99,   392,   168,   191,   353,  1227,
     642,   117,    98,    31,  1369,  2064,   184,   404,   172,   176,
      38,  2145,  1377,   174,   130,  1983,     0,   133,   270,   129,
     168,   137,   175,    79,   353,   178,   270,   171,   133,    79,
      84,    85,   193,   166,   144,   362,   328,   270,   193,   353,
    2174,   127,   369,   725,  1390,   363,  1373,   168,   168,    97,
      98,   881,  2142,   193,   884,   208,   648,   193,   168,  2118,
     652,   937,  1844,   170,   391,  2199,   658,   270,   201,  1434,
     174,    99,   176,   129,   392,  1548,   403,   125,   290,   129,
     172,   168,  1064,   270,   766,   177,   404,   175,   144,   242,
    2117,   904,   167,   685,   144,    79,   778,   250,    79,   174,
     692,   353,  2117,   178,   270,  1446,  1447,   130,   183,   353,
     170,  1456,   174,   175,   432,   175,   269,   270,     3,  2178,
     353,   125,   176,   270,    31,  2197,    82,   175,   158,   282,
     178,    38,   258,   174,   287,   288,   167,  1424,  1425,  1426,
     310,    97,   170,   448,   314,   129,  1374,   178,   129,  2117,
     353,  1573,   183,   269,   184,   168,   499,  2229,   311,   382,
     144,   117,   315,   144,  1946,  1947,   353,  1532,   321,    84,
      85,   174,   325,   119,   281,   168,    31,   170,   325,   192,
    1188,   184,  1190,    38,   337,   290,   168,   353,   145,   342,
     118,   713,    99,    79,   242,   176,   349,   143,   115,   116,
     353,   354,  1548,   446,   168,   321,   449,   172,   451,   325,
     343,   994,   177,  1881,   142,   438,   173,   578,  1457,   462,
     578,   374,   465,   466,   467,  1713,   133,   184,   714,   382,
     137,   384,   571,  2015,  2016,  1007,   727,   954,   354,   287,
     393,  1729,   395,   129,    99,   398,   586,   750,   471,   589,
     421,   176,   405,   248,   171,   758,   363,   865,   144,   145,
     174,   176,  1009,   311,  1007,  1141,   382,   315,   193,   422,
     423,   727,   425,   764,   885,   518,   168,   430,   665,   193,
     619,   434,   398,   952,  1007,   392,   727,   426,   646,   442,
     138,   139,   631,   398,   342,  1640,  1641,   404,   431,   773,
     774,   174,   174,   952,  1021,   168,    68,    69,   764,   680,
     952,   193,  1007,   446,   727,   170,   449,   174,   434,   176,
     193,   193,  1649,   764,  1746,   432,   727,  1749,  1750,   462,
     435,   354,   671,   240,   626,   363,   384,   490,  1007,   492,
     188,   189,   495,   682,  2054,   393,   499,   395,   193,   502,
     727,   764,   356,   486,   358,   727,   599,   405,  1007,   571,
    1848,   365,  1708,   764,   392,  1007,  1712,  1713,   145,   119,
      31,  1239,   727,   544,    82,   423,   404,    38,  1766,    79,
     174,   174,   167,  1729,  1197,   518,   142,   764,    96,   174,
     174,   544,   764,   143,   442,    79,   549,     9,   727,   193,
     797,   693,   179,   180,   432,   558,   559,   619,   193,   764,
     563,   434,   168,   727,   176,   130,   172,    79,   193,   631,
     170,    33,  1767,   179,   180,   578,   170,   754,   172,   129,
    1112,   193,  1006,   586,   174,   764,   589,  1419,    99,   443,
     444,   564,   437,   165,   144,   129,    76,   617,   601,   602,
     764,   499,   599,  1941,   176,   602,  1944,     3,  1050,  1881,
     144,   568,   789,   616,   617,   588,   571,   129,   168,   796,
     192,    79,   172,   468,   801,   727,   137,    79,   259,   797,
     171,   820,   144,   727,   168,   601,   602,  1061,   635,   642,
     964,   398,   176,   646,   727,   174,   970,    65,  1557,   183,
      68,    69,   641,    71,   643,   167,   174,  1081,   363,   174,
     558,   559,   764,   176,   619,   563,   586,   670,   688,   589,
     764,   129,   629,   676,   727,   193,    79,   129,   193,  1246,
     193,   764,   169,  1305,   168,   706,   144,   392,   586,   176,
     727,   589,   144,   176,  1315,  1292,  1293,  1069,   174,   404,
    1891,  1233,   691,   706,   174,   708,  2180,   171,   172,   666,
    2184,   727,  1305,   130,   174,   704,   671,   193,   176,   861,
     172,   724,   725,   193,   727,   183,   129,   432,   192,   240,
     727,   179,  1305,   193,   723,  1071,   739,   740,   186,   187,
      31,   144,  1166,   740,   168,   174,   722,    38,  1944,  1412,
     168,   962,   174,    79,  1021,   848,  1023,   947,    84,    85,
    1305,   764,   765,   766,   193,   168,   174,   840,   168,   172,
     753,   193,   670,   739,   740,   778,   174,   780,   761,  1021,
     777,  1023,   174,   185,  1125,   193,   184,   157,   158,   354,
       3,   161,   162,  1625,   174,  1627,     9,   174,    79,   765,
     174,   193,   187,   129,   184,   788,    79,   184,    99,   998,
     184,  1019,   940,  1021,   184,  1023,  1005,   800,   144,  1125,
      33,   666,   779,    36,   168,   170,    39,  1199,  1395,   826,
    1202,   175,   217,   218,  1125,   987,   181,   182,   841,   168,
     797,   686,   839,  1464,  1465,   170,   137,  1219,   129,   694,
     175,   848,   168,   170,  1006,   844,   129,  1046,   175,   170,
    1284,  1285,  1125,   144,   175,   820,   739,   740,  1490,   434,
     127,   144,   170,   174,  1125,   248,  1799,   175,  1801,   170,
     835,   180,  1254,   184,    97,    98,   168,   170,   170,   892,
     172,   174,   765,   896,   167,   176,   190,  1490,  1125,   284,
     113,   904,   183,  1125,  1328,   936,   178,    31,    79,  1061,
    1442,   284,   125,    79,    38,   173,   174,  1490,   170,   797,
    1125,   936,   174,   936,   936,   142,    79,   930,  1296,  1081,
     170,   951,   174,   936,  1843,   938,   168,   354,   170,  1848,
     172,   171,  1163,   841,   947,  1490,  1125,   523,   524,   525,
    2146,  2147,    13,    14,    15,    16,    17,   192,   129,    79,
     176,  1125,   175,   129,  1317,   178,  1143,  1685,   168,   989,
     936,    53,    54,   144,    56,    99,   129,   168,   144,   936,
      62,   936,   190,   117,  1046,   127,    79,   170,   842,  1325,
     170,   144,   968,   117,    79,   208,   130,   168,   170,   133,
     115,   116,   168,   137,  1007,   176,   172,   852,  1726,   129,
     999,  1000,  1140,   170,  1166,   168,  1019,   434,  1021,   172,
    1023,   181,   182,  1125,   144,  1045,  1023,   136,   137,   242,
     170,  1125,  1941,   170,   665,   992,   129,   250,  1249,  1250,
     938,  1413,  1125,   998,   129,  1234,   170,   173,   168,   947,
     174,   144,   172,    79,   115,   116,   173,  1720,   936,   144,
    1063,  1064,    79,   170,   437,    79,   170,   174,  1071,   282,
     174,  1074,   363,   145,   287,   168,   707,  1449,   145,   172,
     168,  1174,  1319,   168,  1321,   168,   471,   172,   174,   170,
    1079,  1046,   797,   174,  1315,   468,   170,   168,   311,  1307,
     174,   392,   315,   129,   168,  2014,   170,  1235,   172,  1112,
     171,  1774,   129,   404,    22,   129,   240,   168,   144,   140,
     141,  1445,  1125,  2032,  1127,  1128,   168,   144,  1125,   342,
     144,  1128,  1284,  1285,   174,   269,   349,   522,  1121,   168,
     170,   432,   168,   172,   174,   436,   172,   992,   173,   174,
     167,  1154,   158,   167,  1157,  1286,  1139,   107,   282,  1142,
     142,  1127,  1128,  1146,   168,  1063,  1297,   173,   174,  1172,
    1523,   384,   173,   174,   739,   740,  1328,   102,   184,   936,
     393,   168,   395,   940,   176,  1316,   168,   321,  2097,    95,
     172,   325,   405,  1190,  1197,   176,   569,   179,   180,  1330,
     765,   325,   170,   170,   328,  1208,   174,   174,     3,   422,
     423,   168,   425,   138,   167,   172,   193,   430,   143,   170,
     354,   146,   170,   148,   170,  1492,  1493,   170,   174,   442,
    1233,   936,    13,    14,    15,    16,    17,  1448,    79,   363,
    1701,   115,   116,   170,  1465,  1373,  1618,   174,   382,   168,
    1492,  1493,  1255,   172,  1127,  1128,   380,  1388,   382,  1157,
     170,   171,  1393,   168,   398,    79,   191,   172,   392,    13,
      14,    15,    16,    17,    79,  1647,   174,   490,   173,   492,
     404,   174,   495,  1491,  1492,  1493,   499,   170,   129,   502,
     142,   174,   176,   666,   667,  1298,   927,    13,    14,    15,
      16,    17,  1305,   144,  1307,   168,    79,   170,   432,   172,
    1164,   179,   180,   686,   438,   129,   168,   192,  1163,   176,
     172,   694,   739,   740,   129,  1170,   167,   179,   180,  1457,
     144,   544,   174,   175,   168,    79,  1467,  1468,   172,   144,
     192,   173,   174,    13,   168,   558,   559,   471,   765,   176,
     563,   170,    79,  1484,   168,  1486,   129,  1255,   172,  1794,
    1795,  1796,   167,    79,   145,   578,   176,   292,   293,   142,
     295,   144,   297,   586,  1505,  1390,   589,  1390,  1390,   192,
    1573,   170,   170,  1140,   170,   129,   174,  1390,  1391,   142,
     170,   170,   170,  1376,   174,   168,   174,  1769,  1770,   172,
     144,   145,   129,   616,   617,   170,   179,   180,  1645,  1412,
     170,   173,   174,   129,   174,   168,  1419,   144,   170,   172,
     170,  1424,  1425,  1426,    94,   936,   179,   180,   144,   940,
     170,   170,  1435,  1390,   174,  1390,    79,  1440,   172,  1442,
     167,    84,    85,   113,  1075,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   168,    65,   670,   168,    68,
      69,   386,    71,   676,    18,  1806,  1539,   601,   602,   170,
     170,   192,   857,   174,   174,  1603,   172,   862,  1235,   852,
    1573,  1853,    79,  1535,  1536,  1537,   129,  1490,  1491,  1492,
    1493,   173,   174,  1391,   175,   880,  1493,   175,   168,  1353,
    1806,   144,   626,   170,   170,   174,   797,   174,   174,  2041,
     170,   724,  1390,   170,   174,  1806,   168,   174,   192,  1612,
    1892,  1649,    79,   170,   167,  1897,   170,   174,   425,   170,
    1661,  1662,   129,  1548,  1708,  1548,  1548,  1435,  1712,  1542,
    1708,   170,  1440,  1806,  1712,   173,   174,   144,  1551,   173,
     174,  1554,  1555,  1556,   192,  1806,  1559,   173,   174,  1562,
     173,   174,  1127,  1128,  1547,   173,   174,   780,   170,   693,
      31,   168,   129,  1914,   170,   172,    79,    38,   170,  1806,
    1814,  1815,  1816,  1566,  1806,    79,   170,   144,   170,  2050,
    1573,  1548,  1223,  1548,   173,   174,  1599,   173,   174,  1602,
    1603,  1806,    97,    98,   170,   739,   740,  1800,  1914,   994,
      79,   168,   173,   174,   176,   172,  1373,  1690,   168,   992,
     176,   994,  1625,  1914,  1627,   176,   129,  1806,   841,  1140,
     173,   174,   170,  1390,   174,   129,  1602,  1603,    99,    77,
    1643,   144,  1806,   173,  1542,   936,   193,  1602,   173,   110,
     144,  1914,   530,   531,   532,   533,  1554,  1555,  1556,   170,
     129,  1559,   170,  1914,   174,   168,   173,   174,   170,   172,
    1548,    79,   133,   797,   168,   144,   137,   174,   172,    13,
      14,    15,    16,    17,    18,  1390,   170,  1914,  1319,   174,
    1321,  1694,  1914,  1708,    79,  1698,    79,  1712,  1713,   168,
    1457,   174,   175,   172,  1832,   829,    84,    85,   173,  1914,
    1127,  1128,   174,   175,  1729,   839,   840,  1720,     3,  1702,
    1703,   129,   526,   527,  1235,   938,   170,  1800,    13,    14,
      15,    16,    17,   170,   947,  1914,   144,   861,   170,  1742,
    1743,  1209,  1210,   170,   129,  1643,   129,   534,   535,  1752,
    1914,  1708,   170,  1708,     5,  1712,  1713,  1712,  1713,   144,
     168,   144,   173,  2000,   172,   170,    79,   173,  1751,   528,
     529,  1774,  1729,   170,  1729,   167,  2117,   157,   158,   240,
     705,   161,   162,   168,   170,   168,  1752,   172,    39,   172,
    1163,  1548,  1177,  1178,  1179,   170,  1799,  1752,  1801,   173,
    1698,  1749,  1750,  1806,   184,  1808,  1815,  1816,   168,  1194,
    1195,  2117,   936,   193,  1817,    85,   129,    18,   176,   183,
     281,  1694,   193,   173,   173,   173,  2117,   170,   170,   170,
    1708,   144,  1835,   170,  1712,  1713,   174,   170,    89,  1842,
     170,   170,   170,  1548,   167,  1602,  1603,   173,   173,   170,
    1063,  1729,   170,  2042,  2117,   168,   170,   318,   755,   172,
     157,   158,  1373,   170,   161,   162,  2117,   170,   170,   794,
     170,   170,  1875,    22,   170,   170,  1831,   174,   170,  1390,
     170,   170,  1865,   134,    77,   176,  1959,   184,   139,   170,
    2117,   170,  1649,   170,   145,  2117,   193,  1873,   149,   170,
     174,   170,   167,  2140,   155,  2142,    13,    14,    15,    16,
      17,  1914,  2117,   174,  1917,  1918,   167,  1914,   169,  1817,
     381,  1918,   170,   176,  1927,  1928,  1799,   174,  1801,  1944,
      18,  1934,   192,   176,   170,  1808,   170,   398,  2117,   170,
    1943,  1154,   170,   173,  1157,  2182,  1457,   174,   174,   170,
    1953,  1708,  1955,  2117,    13,  1712,  1713,    18,   170,  1172,
     120,   121,   122,   123,   124,  1968,  2039,  1970,  1971,  1972,
     170,   167,  1729,   176,   871,  1930,    64,    65,    66,    67,
      68,    69,    70,    71,   176,  1961,   176,  1944,   170,  1944,
     175,   131,   175,   173,  1997,  1752,   173,   167,   249,   167,
     176,  2004,   176,  1708,   192,  2008,   170,  1712,  1713,   170,
    2013,   170,   170,  2002,  1645,  2117,   173,  2254,   173,   170,
     173,   167,   170,  2117,  1729,   170,   170,   170,    14,   280,
     167,   193,   168,   167,  2117,    94,   168,  1548,  2041,   168,
    2043,   168,  1255,   168,  1917,  1918,  2035,   298,   168,   168,
     168,   302,   168,   175,   113,   170,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,  1944,  2198,     5,  2072,
     175,  2047,   174,   324,  1831,  1832,    13,    14,    15,    16,
      17,  2084,   979,  2214,   170,  2088,   983,   167,   167,   550,
     167,   176,  1603,   174,   170,  2098,   347,   348,   170,  1390,
     170,  2104,   170,   170,   173,   167,   357,   568,   173,   360,
     361,   193,   170,   364,  2117,  2118,   367,   368,   170,   370,
     170,   372,   170,   168,  1997,    87,    98,   193,    96,  1694,
    1027,  2146,  2147,   594,   385,   168,   193,  1034,  1649,    13,
      14,    15,    16,    17,    18,   168,  2149,   193,   170,   193,
     401,   193,  2118,   193,   193,   406,   167,   408,    13,    14,
      15,    16,    17,  1548,   167,   173,   173,   418,   629,   175,
     170,   170,   174,  1930,   168,  2178,  2179,  1390,  1391,   430,
     170,   168,  2179,   168,   167,  2188,   167,  1944,   193,  2146,
    2147,  2146,  2147,    82,  2177,    82,   193,  1708,   193,   168,
     167,  1712,  1713,   168,  2180,   193,   170,   167,  2184,  2185,
     167,   172,  2178,  2179,    82,    82,   193,   678,  1729,   680,
      82,  2224,  1435,   193,  2227,   184,   184,  1440,   175,   170,
     193,   170,  2215,  1130,  1799,   170,  1801,   193,   167,  1944,
     184,   169,  2218,  1808,  2247,  2118,    13,    14,    15,    16,
      17,    18,   184,   437,   167,   170,  2259,  1548,   175,   113,
     168,   170,   174,  2239,   184,  2268,  1390,  2243,  2146,  2147,
     131,   169,   133,   134,   135,   170,   173,  1694,   184,   193,
      82,   169,  2258,   175,   468,   170,   425,   142,     4,     5,
       6,     7,     8,     9,    10,    11,    12,   170,    13,    14,
      15,    16,    17,   167,   167,  2178,  2179,   168,   168,   193,
     171,   172,  1603,   168,   170,   176,   177,   172,   779,  1612,
     472,  1832,   193,   574,   179,   180,   536,   193,   537,  1542,
     538,   582,   539,   215,   585,   540,  1388,   543,  1551,  1568,
    2229,  1554,  1555,  1556,  1712,  2142,  1559,  1952,  2174,  1562,
    1729,  1944,  1917,  1918,  2016,  2132,    72,  1880,  2114,  1866,
     611,  2259,   284,  1866,    79,  2039,  2185,  1264,  2243,  2000,
    2268,  1268,  2113,  1391,  1539,  1539,    13,    14,    15,    16,
      17,  1539,  1799,   117,  1801,    90,  1599,   146,   310,  2146,
    2147,  1808,   314,  1997,  2138,   646,   647,   388,  1690,   650,
     651,  1772,   653,  1453,   655,   656,  1599,  1071,   947,   660,
     661,   715,  1631,     0,   129,  1539,    36,  1708,   871,   871,
     871,  1712,  1713,    -1,  1548,  1527,    -1,   142,    -1,   144,
    1643,    -1,  1997,  1944,    -1,    -1,    -1,    -1,  1729,    -1,
      -1,  2146,  2147,    -1,  1341,    -1,    -1,    -1,  1345,    -1,
      -1,    -1,  1349,   168,    -1,    -1,    -1,   172,    13,    14,
      15,    16,    17,    18,   179,   180,    -1,    -1,  1602,  1603,
      -1,    -1,    -1,   934,    -1,   936,    -1,    -1,    -1,   940,
     110,    -1,   666,    -1,  2115,  1698,    -1,    -1,    -1,    -1,
      -1,  1876,    -1,   744,   678,    -1,    -1,    -1,    -1,    -1,
    1917,  1918,   686,   642,    -1,   142,    -1,    -1,    -1,  2140,
     694,  2142,    -1,    -1,   113,    -1,   115,   768,   117,   118,
     119,   120,   121,   122,   123,   124,   987,    -1,    -1,  1742,
    1743,   168,    -1,    -1,   268,   172,    -1,    -1,    -1,    -1,
      -1,  1832,   179,   180,    -1,  1006,    -1,    -1,    -1,    -1,
     284,  2182,    -1,  2118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   812,   813,   814,  1688,    -1,  1690,    13,    14,    15,
      16,    17,    -1,    -1,  1471,    -1,    -1,    -1,  1475,    -1,
    1997,  1478,    -1,    -1,  1708,    -1,    -1,    -1,  1712,  1713,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
    1061,    -1,    -1,    -1,  1817,  1729,    -1,    -1,    -1,    -1,
      -1,  1508,    -1,  2178,  2179,  1512,   755,    -1,  1752,  1516,
    1081,   872,  1835,  2254,   875,   876,    -1,    -1,   133,  1842,
      -1,    -1,    -1,    79,    -1,  2146,  2147,    -1,    -1,    -1,
     145,   146,    -1,    -1,    -1,    -1,    -1,   569,   382,    -1,
      -1,   281,    -1,  1944,   284,    -1,    -1,    -1,    -1,    -1,
      -1,   291,  1875,    -1,  2049,    -1,    -1,    -1,   852,    -1,
      13,    14,    15,    16,    17,    -1,  1800,    -1,    -1,  1140,
     310,    -1,    -1,   129,   314,    -1,   191,   826,   318,    -1,
      -1,    -1,    -1,    -1,    -1,   617,   142,    -1,   144,    -1,
      -1,  2118,    -1,   954,   438,  1166,   957,    -1,   959,    -1,
      -1,    -1,   446,   635,  1927,  1928,    -1,  1614,   452,    -1,
      -1,  1934,   168,   974,    -1,    -1,   172,    -1,    -1,    -1,
    1943,    -1,   871,   179,   180,    -1,    79,   471,    -1,    -1,
    1953,    -1,  1955,    -1,    -1,   667,    -1,    -1,    -1,    -1,
     934,   381,    -1,    -1,    -1,  1968,    -1,  1970,  1971,  1972,
      -1,  2178,  2179,    -1,    -1,    -1,   688,    -1,  1019,    -1,
      -1,    -1,   113,  1234,  1235,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,   518,   290,   129,    -1,   337,    -1,
      -1,  2004,    -1,    -1,    -1,  2008,    -1,    -1,    -1,   142,
    2013,   144,    -1,   987,     3,    -1,    -1,   437,   992,    -1,
      -1,    -1,    -1,   952,    -1,    -1,   321,    -1,    -1,    -1,
    1944,    -1,  1006,  1284,  1285,   168,    -1,   168,   169,   172,
    2043,    -1,    -1,    -1,    -1,   569,   179,   180,   468,    -1,
     979,    -1,    -1,    -1,   983,  1096,    -1,    -1,    -1,  1100,
     191,    -1,    18,  1104,  1315,  2146,  2147,    -1,    -1,  2072,
      13,    14,    15,    16,    17,   599,    -1,  1328,  1007,    -1,
      -1,  2084,    -1,    -1,    -1,  2088,    -1,  1061,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2098,    -1,    -1,  1027,    -1,
      -1,  2104,    -1,   398,    -1,  1034,    -1,  1081,    -1,    -1,
      -1,  1152,    68,    69,    70,    71,    -1,  1158,    -1,    -1,
      -1,    -1,  1373,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,  1390,
     435,    -1,    -1,   667,    -1,   440,  2149,    -1,   568,   569,
      -1,    -1,    -1,   142,    -1,    -1,    -1,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,    -1,    -1,    -1,   594,    -1,    -1,    -1,    -1,   168,
     169,    -1,    -1,   478,    -1,  2188,   175,    -1,    -1,    -1,
     179,   180,  1166,    -1,    -1,   719,    -1,   617,    -1,   142,
     620,  1130,   191,    -1,  1245,    -1,  1457,    -1,    -1,   629,
      -1,    -1,    -1,  1464,  1465,   635,   172,    -1,    -1,    -1,
      -1,  2224,    -1,    -1,  2227,   168,    -1,    -1,   940,   172,
      -1,    -1,  2146,  2147,    -1,    -1,   179,   180,    -1,   951,
      -1,    -1,    -1,    -1,  2247,    -1,   666,   667,  1289,   773,
     774,  1292,  1293,    -1,    -1,   550,  2259,    -1,    -1,  1300,
    1301,    -1,    79,    -1,    -1,  2268,   686,    -1,   688,    -1,
      -1,    -1,    -1,    -1,   694,    -1,   571,   989,    -1,    -1,
      -1,    -1,   994,    -1,    -1,    -1,  1327,    -1,  1329,    -1,
      -1,  1332,    -1,    -1,  1335,    -1,   113,  1548,  1339,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
    1284,  1285,   129,    -1,    -1,    -1,   840,    -1,    -1,    -1,
      -1,    -1,    -1,  1574,   619,    -1,    -1,   144,    -1,  1370,
     625,    -1,    -1,  1045,    -1,  1264,   631,    -1,    -1,  1268,
      -1,  1382,    -1,    -1,    -1,    -1,    -1,  1388,    -1,    -1,
      -1,  1602,  1603,    -1,  1328,    -1,    -1,    -1,    -1,   779,
      -1,    -1,    -1,    -1,  1405,    -1,    -1,   706,    -1,   708,
      -1,    -1,    -1,    31,    -1,    -1,   671,    -1,    -1,    -1,
      38,    -1,    -1,    -1,    -1,    -1,    -1,   682,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,  1649,    -1,
      -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,    -1,
     705,    -1,  1341,    -1,    -1,    -1,  1345,    -1,    -1,    -1,
    1349,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1140,  1470,
      -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     964,    99,    -1,    -1,    -1,    -1,   970,    -1,    -1,    -1,
      -1,   746,   110,    -1,    -1,  1384,    -1,  1708,    -1,   117,
      -1,  1712,  1713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     994,    -1,    -1,    -1,    -1,   133,   113,    -1,  1729,   137,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   113,
    1464,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,  1752,    -1,  2190,    -1,    -1,    -1,    -1,    -1,   804,
      -1,    -1,   170,    -1,    -1,    -1,   174,    -1,   142,    -1,
     940,    -1,    -1,  1235,    -1,   820,    -1,    -1,    -1,    -1,
      -1,   951,    -1,    -1,    -1,   172,    -1,    -1,    -1,    -1,
     835,    -1,  1471,   201,   168,   169,  1475,    -1,   172,  1478,
      -1,    -1,    -1,   892,    -1,   179,   180,   896,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   904,    -1,   191,    -1,   989,
      -1,    -1,   992,    -1,   994,    -1,    -1,    -1,    -1,  1508,
    1831,  1832,   240,  1512,    -1,    -1,    -1,  1516,   364,   247,
     248,   930,   250,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,  1644,    -1,    -1,    -1,    -1,    -1,   113,
     268,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   281,   282,  1045,   284,    -1,  1669,    -1,
      -1,    -1,   290,   291,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1682,  1683,  1684,    -1,  1686,  1687,    -1,  1689,    -1,
      -1,    -1,   310,    -1,    -1,    -1,   314,    -1,    -1,    -1,
     318,  1373,    -1,   321,    -1,    -1,    -1,   325,    -1,    -1,
     328,    -1,    -1,    -1,   110,    -1,    -1,    -1,    -1,  1930,
      -1,    -1,    -1,    -1,    -1,  1614,    -1,    -1,    -1,   193,
      -1,    -1,    -1,  1944,    -1,    -1,    -1,  1738,    -1,    -1,
      -1,   477,    -1,   998,    -1,   363,    -1,    -1,    -1,    -1,
    1005,    -1,    -1,  1754,    -1,  1239,    -1,    -1,    -1,    -1,
    1140,    -1,   380,   381,   382,  1064,    -1,  1768,    -1,    -1,
      -1,    -1,  1071,    -1,   392,  1074,    -1,  1778,  1779,    -1,
     398,    -1,    -1,    -1,    -1,  1457,   404,    -1,    -1,    -1,
      -1,  1046,   113,    -1,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   422,    -1,    -1,   425,    -1,    -1,
      -1,    -1,  1813,    -1,   432,    -1,    -1,   435,   436,   437,
     438,    -1,   440,    -1,    -1,    -1,    -1,    79,   446,    -1,
      -1,   449,   113,   451,   452,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,   462,    -1,    -1,   465,   466,   467,
     468,    -1,    -1,   471,    -1,  1235,    -1,    -1,    -1,    -1,
     478,   113,   183,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   611,    -1,   129,   614,    -1,
      -1,    -1,    -1,    -1,    -1,   281,    -1,    -1,   169,    -1,
     142,   172,   144,    -1,    -1,    -1,    -1,  1898,  1197,    -1,
     518,    -1,  1903,  1904,  1905,    -1,    -1,    -1,    -1,  1208,
      85,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,  1920,
     172,    -1,   318,    -1,    -1,    -1,    -1,   179,   180,    -1,
      -1,    -1,   550,    -1,    -1,  2146,  2147,    -1,   113,   191,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     568,   569,    -1,   571,    -1,    -1,    -1,    -1,    -1,   113,
      -1,  1445,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,  1975,  1976,   594,  1649,    -1,  1234,
      -1,   599,    -1,  1984,    -1,   381,    -1,    -1,   142,  1990,
    1991,    -1,    -1,  1373,    -1,    -1,   732,    -1,   616,   617,
      -1,   619,   620,    -1,    -1,    -1,    -1,   625,   626,    -1,
      -1,   629,    -1,   631,   168,   169,    -1,   635,   193,    -1,
      -1,    -1,    -1,    -1,   642,   179,   180,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      72,    73,    74,    75,    -1,    -1,    -1,    -1,   666,   667,
      -1,    -1,    -1,   671,    -1,    -1,  2057,    -1,   676,    -1,
     678,    -1,   680,    -1,   682,    -1,  2067,    -1,   686,    -1,
     688,    -1,    -1,    -1,    -1,   693,   694,  1457,    -1,    -1,
    2081,   113,   700,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   719,    -1,    -1,    -1,    -1,  1361,    -1,    -1,    -1,
      -1,    -1,  2113,  1412,    -1,    -1,    -1,  2118,    -1,    -1,
    1419,    -1,    -1,    -1,    -1,  1424,  1425,  1426,   746,    -1,
      -1,    -1,    -1,  2134,    -1,   871,   872,   755,    -1,    -1,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   773,   774,    -1,   190,    -1,
     113,   779,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   568,    -1,    -1,    -1,    -1,    -1,    -1,   797,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1574,    -1,  2197,   129,   594,    -1,
      -1,  1685,   820,    -1,    -1,    -1,    -1,    -1,   826,    -1,
     142,   829,   144,    -1,    -1,    -1,    -1,   835,    -1,    -1,
      -1,   839,   840,    79,    -1,  2226,    -1,    -1,  2229,    -1,
     848,    -1,    -1,   629,   852,    -1,   168,   169,   974,    -1,
     193,   977,  1726,   861,    -1,    -1,    -1,   179,   180,  2250,
      -1,    -1,    -1,   871,    -1,    -1,    -1,   113,    -1,   191,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,  1649,
     113,    -1,    -1,   129,   117,   118,   119,   120,   121,   122,
     123,   124,   678,    -1,   680,    -1,   142,  1959,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,  2190,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   168,   169,    -1,    -1,   934,    -1,   936,    -1,
      -1,    -1,   940,   179,   180,    -1,  1625,    -1,  1627,    -1,
      -1,    -1,    -1,   951,   952,   191,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   964,  1602,    -1,    -1,
      -1,    -1,   970,  1089,    -1,    31,    -1,    -1,   171,    -1,
    1096,   979,    38,    -1,  1100,   983,    -1,  2039,  1104,   987,
     183,   989,    -1,    -1,   992,    -1,   994,    -1,    -1,    -1,
     998,   107,    -1,   779,    -1,    -1,    -1,  1005,  1006,  1007,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,  1027,
      -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,    -1,    -1,
      -1,  1720,    -1,    99,    -1,    -1,    -1,  1045,  1046,    -1,
     826,    -1,    -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,
    1695,   117,    -1,  1061,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1188,    -1,  1190,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,  1081,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1774,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,   170,    -1,    -1,  1752,   174,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,  1130,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,  1140,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,   168,   169,    -1,    -1,   934,    -1,
      -1,    -1,   268,   269,    -1,  1163,    -1,    -1,  1166,    -1,
      -1,    -1,  1170,  1289,   168,   169,  1174,   191,   284,    -1,
      -1,    -1,    -1,    -1,   240,   179,   180,    -1,    -1,    -1,
      -1,    -1,   248,    -1,    -1,    -1,  1831,   191,    -1,  1959,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,   987,    -1,   269,    -1,    -1,  1332,    -1,    -1,  1335,
      -1,    -1,    -1,  1339,    -1,   281,   282,    -1,   284,    -1,
    1006,    -1,    -1,    -1,    -1,   291,  1234,  1235,    -1,    -1,
      -1,  1239,    -1,    -1,    -1,    -1,    -1,   353,   354,    -1,
      -1,    -1,    -1,  1251,   310,    -1,    -1,    -1,   314,    -1,
      -1,    -1,   318,    -1,    -1,    -1,  1264,    -1,    -1,   325,
    1268,    -1,   328,   379,    -1,    -1,   382,    -1,    -1,  2039,
      -1,    -1,    -1,    -1,    -1,  1061,  1284,  1285,    -1,  1405,
      -1,    -1,    -1,    -1,    -1,  1930,    -1,    -1,   354,    -1,
      -1,    -1,    -1,    -1,   113,  1081,    -1,   363,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,  1315,   374,   425,
      -1,    -1,    -1,   379,   380,   381,   382,    -1,    -1,    -1,
    1328,    -1,   438,   142,    -1,    -1,   392,    -1,    -1,    -1,
     446,    -1,    -1,  1341,    -1,    -1,   452,  1345,   404,    -1,
      -1,  1349,   408,    -1,    -1,    -1,    -1,   413,    -1,   168,
     169,    -1,   418,  1361,    -1,   471,    -1,    -1,    -1,   425,
     179,   180,    -1,    -1,    -1,  1373,   432,    -1,    -1,    -1,
     436,   437,   191,    -1,    -1,    -1,  1384,    -1,    -1,    79,
    1166,   113,  1390,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,  2042,    -1,    -1,
      -1,    -1,   468,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1445,    -1,    -1,
      -1,    -1,   142,    -1,   144,    -1,    -1,   179,    -1,  1457,
      -1,    -1,    -1,   569,    -1,    -1,  1464,  1465,    -1,    -1,
      -1,    -1,    -1,  1471,    -1,    -1,    -1,  1475,   168,   169,
    1478,    -1,    79,    -1,    -1,    -1,    -1,   113,    -1,   179,
     180,   117,   118,   119,   120,   121,   122,   123,   124,   125,
      -1,   191,    -1,    -1,   130,    -1,   132,    -1,  1284,  1285,
    1508,    -1,   568,   569,  1512,    -1,   113,    -1,  1516,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,   642,    -1,   594,  1315,
     646,  1539,    -1,   169,    -1,   142,   172,   144,    -1,    -1,
    1548,    -1,  1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   617,    -1,    -1,   620,    -1,    -1,    -1,    -1,    -1,
     626,   168,   169,   629,    -1,  1573,  1574,    -1,    -1,   635,
      -1,    -1,   179,   180,    -1,    -1,   642,    -1,    -1,    -1,
     646,    -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1602,  1603,    -1,    -1,  1384,    -1,
     666,   667,    -1,   719,  1612,    -1,  1614,   673,    -1,    -1,
      -1,    -1,   678,    -1,   680,    -1,    -1,    -1,    -1,    -1,
     686,    -1,   688,    31,    -1,    79,    -1,   693,   694,    -1,
      38,    -1,    -1,    -1,  1642,    -1,    -1,    -1,    -1,   755,
      -1,  1649,    -1,    13,    14,    15,    16,    17,   764,   765,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   773,   774,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,   129,    -1,  1685,  1464,  1465,
    1688,    -1,  1690,    -1,    -1,    -1,    -1,  1695,   142,   755,
     144,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   765,
    1708,    -1,   110,    -1,  1712,  1713,    -1,    -1,    -1,    79,
      -1,    -1,    -1,   779,   168,   169,    -1,    -1,  1726,    -1,
      -1,  1729,    -1,    -1,    -1,   179,   180,    -1,    -1,   137,
      -1,   797,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,   113,  1752,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   871,   872,    -1,    -1,   129,
     826,    -1,   170,   829,   157,   158,   159,   160,   161,   162,
     163,   164,   142,   839,   144,    -1,    -1,    -1,    -1,    -1,
      -1,   174,  1908,    -1,    -1,    -1,   852,    -1,  1574,    -1,
      -1,   184,  1800,    -1,    -1,   861,    -1,    -1,   168,   169,
     193,    -1,   172,    -1,    -1,   871,   872,    -1,    -1,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   191,   113,  1831,  1832,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,   952,    -1,    -1,   130,
     248,   132,    -1,    -1,    -1,    -1,    -1,    -1,   964,    -1,
      -1,    -1,    -1,    -1,   970,    -1,    -1,  1983,  1984,    -1,
      -1,    -1,    -1,   979,    -1,    -1,    -1,   983,   934,    -1,
     936,    -1,    -1,   281,   940,    -1,   284,    -1,   169,    -1,
      -1,   172,    -1,   291,    -1,   951,   952,    -1,   954,    -1,
     113,  1007,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   310,    -1,    -1,    -1,   314,    -1,    -1,    -1,
     318,  1027,    -1,   979,    -1,  2041,    -1,   983,  1034,    -1,
      -1,   987,  1930,   989,    -1,    -1,   992,    -1,   994,    -1,
      -1,    -1,    -1,  1941,    -1,    -1,  1944,    -1,  2064,    -1,
    1006,  1007,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1959,  1018,   176,    -1,   363,    -1,    -1,    -1,    -1,
      -1,  1027,    -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,
      -1,    -1,    -1,   381,    -1,    -1,    -1,    -1,    -1,  1045,
      -1,    -1,    -1,    -1,   392,    -1,    -1,    -1,    -1,    -1,
      -1,  2117,  2118,    -1,    -1,  1061,   404,    -1,    -1,    -1,
     408,    -1,    -1,    -1,    -1,    79,    -1,    -1,  2134,    -1,
    2018,  2019,  2138,    -1,  1130,  1081,    -1,   425,    -1,    -1,
      -1,    -1,    -1,    -1,   432,    -1,    -1,    -1,   436,   437,
      -1,  2039,    -1,    -1,  2042,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,  2178,    -1,    -1,   129,    -1,    -1,    -1,    -1,
     468,    -1,    -1,    -1,  1130,    -1,    -1,    -1,   142,    -1,
     144,    -1,    -1,    -1,  1140,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
    2098,    79,    -1,    -1,   168,   169,    -1,  1163,    -1,    -1,
    1166,    -1,    -1,  1551,  1170,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1562,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,  1239,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   172,  2146,  2147,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,  1264,    -1,
      -1,    -1,  1268,    -1,    -1,    -1,   144,    -1,    -1,    -1,
     568,   569,    -1,    -1,    -1,    -1,    -1,    -1,   113,  1235,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     168,   169,  2190,    -1,    -1,    -1,   594,    -1,    -1,    -1,
      -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,  1264,    -1,
      -1,    -1,  1268,   191,    -1,    -1,    -1,    -1,    -1,   617,
      -1,    -1,   620,    -1,    -1,    -1,    -1,    -1,  1284,  1285,
      -1,   629,    -1,    -1,    -1,  1341,   171,   635,    -1,  1345,
    1296,    -1,    -1,  1349,   642,    -1,    -1,    -1,    -1,    -1,
      -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1315,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   666,   667,
      -1,    -1,  1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     678,    -1,   680,    -1,    -1,  1341,    -1,    -1,   686,  1345,
     688,    -1,    -1,  1349,    -1,    -1,   694,    -1,    -1,    -1,
      -1,    -1,   113,    -1,  1742,  1743,   117,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,  1373,    -1,   130,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,  1384,    -1,
      -1,    -1,    -1,    -1,  1390,    -1,    -1,    -1,    -1,  1445,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   755,   169,    -1,
      -1,   172,    -1,    -1,    -1,  1471,    -1,    -1,    -1,  1475,
      -1,    -1,  1478,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   779,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,  1835,    31,   797,
     169,  1457,  1508,   172,  1842,    38,  1512,    -1,  1464,  1465,
    1516,    -1,   142,    -1,    -1,  1471,    -1,    -1,    -1,  1475,
      -1,    -1,  1478,    -1,    -1,    -1,    -1,    -1,   826,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1875,   168,   169,
      -1,    -1,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     180,    -1,  1508,    -1,   852,    -1,  1512,    -1,    -1,    -1,
    1516,   191,   192,    -1,    -1,    -1,    99,    -1,    -1,    -1,
      -1,    -1,    -1,   871,    -1,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,  1539,    -1,    -1,    -1,    -1,    -1,  1927,
    1928,    -1,  1548,    -1,    -1,    -1,  1934,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,  1943,    -1,    -1,  1614,    -1,
      -1,    -1,    -1,    -1,    -1,  1953,    -1,  1955,  1574,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1968,    -1,  1970,  1971,  1972,    -1,   934,   170,   936,    -1,
      -1,    -1,   940,    -1,    -1,    -1,    -1,  1603,    -1,    -1,
      -1,    -1,    -1,   951,   952,    -1,    -1,    -1,  1614,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2004,    -1,    -1,    -1,
    2008,    -1,    -1,    -1,    -1,  2013,    -1,    -1,    -1,  1685,
      -1,   979,    -1,    -1,    -1,   983,    -1,    -1,    -1,   987,
      -1,   989,    -1,  1649,   992,   113,   994,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,  1006,  1007,
      -1,    -1,    -1,    -1,    -1,   248,    -1,    -1,    -1,    -1,
    1726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1027,
      -1,    -1,  1688,    -1,  2072,    -1,  1034,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2084,  1045,   281,    -1,
    2088,   284,  1708,   171,    -1,    -1,  1712,  1713,   291,    -1,
      -1,    -1,    -1,  1061,    -1,    -1,  2104,    -1,    -1,    -1,
      -1,    -1,    -1,  1729,    -1,    -1,    -1,   310,    -1,    -1,
      -1,   314,    -1,  1081,   111,   318,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
    1806,    -1,  1808,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2149,    -1,    13,    14,    15,    16,    17,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     363,     1,  1130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,  1140,    -1,   171,   172,    -1,    -1,   381,    -1,
    2188,    -1,  1808,    -1,    -1,    -1,    -1,    -1,    -1,   392,
      -1,    -1,    -1,    -1,    -1,  1163,    -1,    -1,  1166,    -1,
      -1,   404,  1170,   168,    -1,    -1,  1832,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,  2224,    -1,    58,  2227,
      60,    61,   425,    63,    -1,    -1,    -1,    -1,    -1,   432,
      -1,    -1,    -1,   436,   437,    -1,    -1,    -1,    -1,  2247,
      80,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   468,    -1,  1235,    -1,   109,
     110,   111,   142,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    -1,   134,   135,  1264,    -1,   168,   169,
    1268,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     180,    -1,    -1,    -1,    -1,  1941,  1284,  1285,  1944,    -1,
      -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,   171,   172,  1959,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,    -1,    -1,  1315,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,   201,  1984,    -1,
    1328,    -1,    -1,    -1,    -1,   568,   569,    -1,    -1,    -1,
      -1,    -1,    -1,  1341,    -1,    -1,    -1,  1345,   113,    -1,
      -1,  1349,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   594,    -1,    -1,    -1,   130,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1373,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2039,   617,    79,  1384,   620,    -1,    -1,
      -1,    -1,  1390,    -1,    -1,    -1,   629,    -1,    -1,    -1,
      -1,    -1,   635,    -1,   169,    -1,    -1,    -1,    -1,   642,
      -1,  2117,  2118,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   666,   667,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   678,    -1,   680,   142,    -1,
     144,    -1,    -1,   686,    -1,   688,    -1,    -1,    -1,  1457,
      -1,   694,  2118,    -1,    -1,    -1,  1464,  1465,    -1,    -1,
      -1,    -1,    -1,  1471,   168,   169,    -1,  1475,   172,    -1,
    1478,    -1,    -1,    -1,  2190,   179,   180,    -1,    -1,    -1,
    2146,  2147,    13,    14,    15,    16,    17,   191,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
    1508,    -1,    -1,    -1,  1512,    -1,    -1,    -1,  1516,    -1,
      -1,    -1,   755,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,  2190,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,   779,    -1,    -1,    -1,
    1548,    -1,    -1,   168,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,   797,   130,    -1,    -1,    -1,    -1,
      -1,    -1,   446,    -1,    -1,   449,  1574,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,
      79,    -1,   113,   826,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,  1603,    -1,    -1,   129,    -1,
      -1,    -1,   486,    -1,    -1,    -1,  1614,    -1,    -1,   852,
      -1,   142,    -1,   144,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,   871,    -1,
     129,    -1,    -1,    -1,   518,    -1,    -1,   168,   169,    -1,
      -1,  1649,    -1,   142,    -1,   144,    -1,    -1,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   541,    -1,    -1,
     191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   934,   191,   936,    -1,    -1,    -1,   940,    -1,    -1,
    1708,    -1,    -1,    -1,  1712,  1713,    -1,    -1,   951,   952,
      -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1729,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,   979,    -1,    -1,    -1,
     983,    -1,    -1,    -1,   987,    -1,   989,    -1,    -1,   992,
      -1,   994,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   156,  1006,  1007,    -1,    13,    14,    15,    16,
      17,   165,    -1,    -1,    -1,    -1,    -1,    -1,   353,   354,
      -1,    -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,  1034,    79,    -1,    -1,    -1,    -1,    -1,   192,   374,
      -1,    -1,  1045,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1061,    -1,
      -1,    -1,    -1,    -1,  1832,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,  1081,    -1,
      -1,   113,   129,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   142,    -1,   144,    -1,   434,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,   168,   169,    -1,    -1,    -1,    -1,  1130,    -1,   773,
     774,    -1,   179,   180,    -1,   142,    -1,  1140,    -1,    13,
      14,    15,    16,    17,   191,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
    1163,   168,   169,  1166,    -1,   172,    -1,  1170,    -1,    -1,
      -1,    -1,   179,   180,   142,    -1,  1944,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,
      -1,  1959,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,   180,    -1,   549,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1235,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1264,    -1,    -1,    -1,  1268,    -1,    -1,   142,    -1,
      -1,  2039,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1284,  1285,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    38,   179,   180,   642,    -1,    -1,
      -1,   646,  1315,    -1,    -1,    -1,    -1,   191,   107,    -1,
      -1,    -1,    -1,    -1,   113,  1328,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,  1341,    -1,
      -1,    -1,  1345,    -1,    -1,   113,  1349,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
    1373,    -1,    -1,   107,   142,    -1,   110,    -1,  2146,  2147,
      -1,  1384,    -1,   117,    -1,    -1,    -1,  1390,    -1,    -1,
     725,    -1,   727,    -1,    -1,    -1,   130,    -1,    -1,   133,
     168,   169,    -1,   137,   739,   740,    -1,   175,    -1,    -1,
      -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2190,   191,    -1,    -1,    -1,    -1,    -1,   764,
     765,   766,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,    -1,    -1,   778,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1457,    -1,    -1,    -1,    -1,    -1,
      -1,  1464,  1465,    -1,    -1,    -1,    -1,   201,  1471,    -1,
      -1,    -1,  1475,    -1,    -1,  1478,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,  1508,   240,    -1,    -1,  1512,
      -1,    -1,    -1,  1516,   248,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   268,   269,   270,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1548,    -1,   281,   282,    -1,
     284,    -1,    -1,    -1,   288,    -1,   290,   113,    -1,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,  1574,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,
     314,    -1,    -1,    -1,   318,    -1,   142,   321,    -1,    -1,
      -1,   325,    -1,    -1,   328,    -1,    -1,    -1,    -1,   113,
    1603,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,  1614,   168,   169,    -1,    -1,   172,    -1,    -1,   353,
     354,    -1,    -1,   179,   180,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,
     374,    -1,    -1,    -1,    -1,    -1,  1649,   381,   382,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   398,   179,   180,    -1,    -1,    -1,
      -1,    -1,  1007,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,    -1,  1019,    -1,  1021,    -1,  1023,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     434,    -1,    -1,   437,   438,  1708,   440,    -1,    -1,  1712,
    1713,    -1,   446,  1357,    -1,   449,    -1,   451,   452,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1729,    -1,   462,    -1,
      -1,   465,   466,   467,   468,    -1,    -1,   471,    -1,    -1,
      -1,    -1,    -1,   477,   478,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,  1112,    -1,    -1,
      -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,    -1,
    1125,    -1,  1127,  1128,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,   180,    -1,    -1,   549,   550,    -1,    -1,    -1,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,  1832,
      -1,    -1,    -1,    -1,   568,   569,   570,   571,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,
     594,    -1,    -1,    -1,    -1,   599,    -1,   601,   602,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   617,    -1,   619,    -1,    -1,    -1,    -1,
      -1,   625,   626,    -1,    -1,   629,    -1,   631,  1233,   168,
     169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,    -1,
     179,   180,   646,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,  1573,
      -1,    -1,   666,   667,    -1,    -1,    -1,    -1,  1941,    -1,
      -1,  1944,    -1,    -1,   678,    -1,   680,    -1,   682,    -1,
      -1,    -1,   686,    -1,   688,    -1,  1959,    -1,    -1,   693,
     694,    -1,    -1,  1298,    -1,    -1,    -1,    -1,    -1,    -1,
    1305,    -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   719,    -1,    -1,    -1,    -1,
      -1,   725,    -1,   727,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   739,   740,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     764,   765,   766,    -1,    -1,    -1,  2039,    -1,    -1,   773,
     774,    -1,    -1,    -1,   778,   779,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1708,  1709,    -1,    -1,  1712,  1713,
      -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,  1722,    -1,
      -1,    -1,    -1,  1727,    -1,  1729,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   840,  1442,    -1,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,   852,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   871,   872,    -1,
      -1,    -1,    -1,  2146,  2147,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1490,  1491,  1492,  1493,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1827,    -1,    -1,    -1,  2190,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     934,    -1,   936,  1847,  1848,    -1,   940,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   951,   952,    -1,
     954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     964,    -1,    -1,  1877,    -1,    -1,   970,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   987,    -1,   989,    -1,    -1,   992,    -1,
     994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1006,  1007,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1021,  1932,  1023,
      -1,    -1,  1936,    -1,  1938,    -1,    -1,  1941,  1942,    -1,
    1944,    -1,    -1,    -1,    -1,  1949,    -1,    -1,    -1,    -1,
      -1,  1045,  1046,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1061,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1081,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1694,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2018,    -1,    -1,    -1,  1112,    -1,
      -1,  2025,    -1,    -1,    -1,    -1,  2030,  2031,    -1,    31,
      -1,  1125,    -1,  1127,  1128,    -1,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1140,    -1,    -1,  2053,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1163,
      -1,    -1,  1166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1174,    -1,    -1,    -1,    -1,  2089,    -1,  2091,    -1,    -1,
    2094,  2095,  2096,    -1,  1188,    -1,  1190,    99,  2102,  2103,
      -1,    -1,    -1,    -1,  1799,    -1,  1801,    -1,    -1,    -1,
      -1,  1806,    -1,  1808,    -1,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,  1233,
    1234,  1235,    -1,    -1,    -1,  1239,    -1,    -1,    -1,    -1,
      -1,    -1,  1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2170,  2171,  2172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,
    1284,  1285,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   201,
    2204,  2205,  2206,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,  1305,    -1,  1307,    -1,    -1,    -1,    -1,    -1,  1914,
      -1,  1315,  1917,  1918,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1328,   109,   110,   111,   240,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,   268,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1373,
     282,    -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,  1390,   171,   172,    -1,
      -1,  1395,  1997,    -1,   178,   179,   180,   181,   182,   183,
     184,    -1,   314,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2041,    -1,  1442,    -1,
      -1,  1445,    -1,    -1,    -1,   357,    -1,    -1,    -1,    -1,
      -1,    -1,   364,  1457,    -1,    -1,    -1,    -1,    -1,    -1,
    1464,  1465,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     382,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1490,    -1,  1492,  1493,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2117,  2118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   446,  1539,    -1,   449,    -1,   451,
     452,    -1,    -1,    -1,  1548,    -1,    -1,    -1,    -1,    -1,
     462,    -1,    -1,   465,   466,   467,    -1,    -1,    -1,   471,
      -1,    -1,    -1,    -1,    -1,   477,    -1,    -1,    -1,  1573,
    1574,    -1,    -1,  2178,  2179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1602,  1603,
      13,    14,    15,    16,    17,    -1,   518,    20,  1612,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,  1649,    59,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1685,    -1,    -1,    -1,    -1,  1690,   599,    -1,    -1,
    1694,  1695,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   611,
     612,    -1,   614,   615,  1708,   617,    -1,    -1,  1712,  1713,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,  1726,    -1,    -1,  1729,    -1,    -1,    -1,    -1,
     642,   144,    -1,    -1,   646,   647,    -1,    -1,    -1,    -1,
      -1,   653,    -1,    -1,    -1,    -1,    -1,    -1,  1752,    -1,
      -1,   663,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   693,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1799,  1800,  1801,    -1,    -1,
      -1,    -1,  1806,    -1,  1808,    -1,    -1,   719,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   728,    -1,    -1,    -1,
     732,    -1,    -1,    -1,    -1,    -1,    -1,  1831,  1832,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,    -1,
      -1,   773,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1908,    -1,    -1,    -1,    -1,    -1,
    1914,    -1,    -1,  1917,  1918,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1930,    -1,   840,    -1,
      -1,    -1,    -1,    -1,    -1,    31,   848,    -1,    -1,    -1,
    1944,    -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,   861,
      -1,    -1,    -1,    -1,    -1,  1959,    -1,    -1,    -1,   871,
     872,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1983,
    1984,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1997,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,   936,    -1,    -1,    -1,   940,    -1,
      -1,    -1,    -1,    -1,    -1,  2039,    -1,  2041,  2042,   951,
     952,   137,   954,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,    -1,   964,    -1,    -1,    -1,    -1,    -1,   970,    -1,
    2064,    -1,   974,   975,    -1,   977,   978,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1007,    -1,  1009,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,    -1,  1021,
      -1,    -1,    -1,  2117,  2118,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1045,  2138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2146,  2147,   240,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   268,  1085,  2178,  2179,    -1,  1089,    -1,    -1,
      -1,    -1,    -1,    -1,  1096,  1097,   282,    -1,  1100,  1101,
      -1,    -1,  1104,  1105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1140,   325,
      -1,    -1,   328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1174,    -1,    -1,    -1,    -1,   363,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1188,    -1,  1190,    -1,
      -1,    -1,    -1,    -1,   380,    -1,   382,    -1,    -1,    -1,
      -1,    -1,   388,    -1,    -1,    -1,   392,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   404,    -1,
      -1,    31,    -1,    -1,    -1,    -1,    -1,    -1,    38,    -1,
      -1,    -1,    -1,  1235,    -1,    -1,    -1,  1239,    -1,    -1,
      -1,    -1,    -1,    -1,  1246,    -1,   432,    -1,    -1,    -1,
     436,    -1,   438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     446,    -1,    -1,   449,    -1,   451,   452,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,   465,
     466,   467,    -1,    -1,    -1,   471,    -1,  1289,  1290,    99,
    1292,  1293,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,    -1,
    1332,  1333,   518,  1335,  1336,    -1,    -1,  1339,  1340,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,    -1,  1364,    -1,   174,    -1,    -1,    -1,    -1,    -1,
      -1,  1373,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1390,    -1,
      -1,   201,    -1,  1395,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1405,  1406,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   599,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     626,    -1,    -1,  1445,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1457,    -1,    -1,   268,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   282,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,   693,    57,    -1,
      59,    -1,    -1,    -1,    -1,   325,    -1,    -1,   328,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,   719,    -1,    -1,    -1,  1539,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,    -1,    -1,
      -1,    -1,    -1,   363,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     380,  1573,   382,    -1,    -1,    -1,    -1,    -1,   388,    -1,
     129,    -1,   392,    -1,    -1,    -1,    -1,   773,   774,    -1,
      -1,    -1,    -1,    -1,   404,   144,    -1,    -1,    -1,    -1,
      -1,  1603,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1612,   797,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   432,    -1,    -1,    -1,   436,    -1,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   446,    -1,    -1,   449,
      -1,   451,   452,   829,    -1,    -1,    -1,  1649,    -1,    -1,
      -1,    -1,   462,   839,   840,   465,   466,   467,    -1,    -1,
      -1,   471,   848,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    -1,    -1,    -1,   861,    -1,    -1,    38,    -1,
      -1,    -1,    -1,  1685,    -1,    -1,    -1,    -1,  1690,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1708,    -1,   518,    -1,
    1712,  1713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1726,    -1,    -1,  1729,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   964,    -1,
      -1,    -1,    -1,    -1,   970,    -1,    -1,    -1,    -1,   599,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1800,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,  1813,    -1,    -1,   174,    -1,   626,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1832,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   201,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
     240,    -1,    62,   693,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1908,    -1,   268,   719,
      -1,    -1,    -1,  1915,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   282,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,  1944,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1959,    -1,    -1,
      -1,    -1,   142,   773,   774,   325,    -1,    -1,   328,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1983,  1984,    -1,    -1,    -1,    -1,   797,  1174,    -1,
      -1,   171,   172,    -1,  1996,    -1,    -1,    -1,    -1,   179,
     180,    -1,    -1,   363,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   829,
     380,    -1,   382,    -1,    -1,    -1,    -1,    -1,   388,   839,
     840,    -1,   392,    -1,    -1,    -1,    -1,  2039,   848,  2041,
      -1,    -1,    -1,    -1,   404,    -1,    -1,    -1,    -1,    -1,
      -1,   861,    -1,  1239,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2064,    -1,    -1,    -1,    -1,   102,    -1,    -1,
      -1,    -1,   432,    -1,    -1,    -1,   436,    -1,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   446,    -1,    -1,   449,
      -1,   451,   452,    -1,    -1,    -1,    -1,    -1,   133,    -1,
      -1,    -1,   462,    -1,    -1,   465,   466,   467,    -1,    -1,
     145,   471,   147,    -1,   149,  2117,  2118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,  2134,  2135,    -1,    -1,  2138,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2146,  2147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   964,    -1,   191,    -1,   518,    -1,
     970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2178,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1390,    -1,   182,    -1,   184,    -1,
      -1,   187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   199,   200,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,
     216,   217,   218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   290,    -1,    -1,    -1,  1445,
      -1,   296,    -1,   298,    -1,    -1,   626,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   259,    -1,    -1,   321,    -1,   323,   324,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   693,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1539,    -1,    -1,    -1,    -1,    -1,   719,
      -1,    -1,  1548,   398,  1174,   400,   401,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     435,    -1,    -1,    -1,    -1,   440,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   773,   774,    -1,    -1,  1603,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1612,    -1,    -1,  1239,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   797,    -1,    -1,
      -1,    -1,    -1,   478,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   829,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   839,
     840,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   848,    -1,
      -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,    -1,  1685,
      -1,   861,  1688,    -1,  1690,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,
      -1,    -1,  1708,    -1,    -1,    -1,  1712,  1713,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,   573,   574,
    1726,    -1,    -1,  1729,    -1,    -1,   522,   523,   524,   525,
     526,   527,   528,   529,   530,   531,   532,   533,   534,   535,
     536,   537,   538,   539,   540,    -1,   601,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
    1390,    -1,    -1,    -1,   619,    -1,    -1,    -1,    -1,    -1,
     625,    -1,    -1,    -1,    -1,    -1,   631,    -1,   633,   634,
      -1,    -1,    -1,    -1,   964,    -1,    -1,    -1,    -1,    -1,
     970,    -1,    -1,    -1,  1800,    37,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1445,   671,   672,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1832,   682,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,    -1,
     705,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,   665,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   739,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1539,
      -1,   766,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,
      -1,   776,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1944,    -1,
      -1,    -1,    -1,  1573,    -1,    -1,    -1,    -1,    -1,   804,
      -1,    -1,   807,   808,    -1,    -1,    -1,    -1,    -1,    -1,
     202,    -1,    -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1603,    -1,    -1,    -1,    -1,    -1,    -1,
     835,    -1,  1612,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   281,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    -1,    60,    61,  1685,    63,    -1,  1688,  1239,
    1690,   857,    -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,   318,    -1,  1708,    -1,
      -1,    -1,  1712,  1713,   880,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   337,  1726,   339,    -1,  1729,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,   927,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
    2146,  2147,    -1,   998,    -1,    -1,    -1,    -1,    -1,    -1,
    1005,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,  1021,    -1,    -1,    -1,
    1800,   178,   179,   180,   181,   182,   183,   184,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1046,    -1,    -1,    -1,   437,  1051,    -1,   994,    -1,
      -1,    -1,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1390,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   468,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   489,    -1,   491,
      -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,   500,   501,
      -1,    -1,    -1,    -1,    -1,  1445,    -1,    -1,    -1,    -1,
      -1,    -1,  1127,   515,    -1,    -1,    -1,    -1,    -1,  1075,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   543,    -1,    -1,   546,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1944,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,
      -1,    -1,    -1,  1188,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   594,    -1,    -1,    -1,    -1,    -1,    -1,  1539,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   619,   620,  1234,
     622,  1177,  1178,  1179,    -1,    -1,    -1,   629,    -1,    -1,
      -1,  1246,    -1,  1573,    -1,    -1,    -1,    -1,  1194,  1195,
      -1,   643,    -1,   645,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1603,   666,    -1,    -1,  1223,    -1,    -1,
      -1,    -1,  1612,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1297,    -1,   686,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   703,    -1,    -1,    -1,   707,   708,    -1,    -1,    -1,
      -1,    -1,   714,    -1,    -1,    -1,    -1,   719,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   746,  1685,    -1,    -1,  1688,    -1,
    1690,    -1,    -1,    -1,    -1,    -1,  2146,  2147,    -1,    -1,
      -1,    -1,    -1,  1319,    -1,  1321,    -1,    -1,  1708,    -1,
      -1,   110,  1712,  1713,    -1,    -1,    -1,   779,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1726,    -1,    -1,  1729,
      -1,    -1,    -1,    -1,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     852,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1800,    -1,   201,    -1,    -1,    -1,    -1,   869,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1492,    -1,   881,
      -1,    -1,   884,   885,   886,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   904,    -1,    -1,    -1,    -1,    -1,    -1,   248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   268,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   281,    -1,    -1,   284,    -1,    55,    -1,    -1,
      58,   290,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   970,    -1,
      -1,   310,    80,    -1,    -1,   314,    -1,    -1,    -1,   318,
      -1,    -1,   321,    -1,    -1,    -1,    -1,  1602,    -1,    -1,
     992,    -1,  1548,    -1,    -1,    -1,   998,    -1,    -1,    -1,
      -1,   109,   110,   111,  1944,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   381,   382,  1046,    -1,    -1,    -1,    -1,   157,
     158,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,   398,
     168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,  1071,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,    -1,
    1695,    -1,  1084,    -1,    -1,    -1,    -1,    -1,    -1,  1645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,   437,   438,
      58,   440,    60,    61,    -1,    63,    -1,   446,    -1,    -1,
     449,    -1,   451,   452,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,   462,    -1,    -1,   465,   466,   467,   468,
      -1,    -1,   471,    -1,    -1,    -1,    -1,  1752,    -1,   478,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,   518,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1197,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2146,  2147,    -1,    -1,
     168,   169,    -1,   171,   172,  1217,  1831,    -1,   176,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,   568,
     569,    -1,   571,    -1,    -1,    -1,    -1,  1239,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   594,    -1,    -1,    -1,    -1,
     599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   617,    -1,
     619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     629,    -1,   631,    -1,    -1,  1297,    -1,    -1,    -1,    -1,
      -1,    -1,  1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1930,    -1,    -1,    -1,    -1,
    1876,    -1,    -1,  1325,    -1,    -1,    -1,   666,   667,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   678,
      -1,   680,    -1,    -1,    -1,    -1,    -1,   686,    -1,   688,
      -1,    -1,    -1,    -1,    -1,   694,    -1,  1359,    -1,  1361,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     719,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2019,  2020,    -1,    -1,    -1,    -1,
    1412,    -1,  1414,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2042,    -1,    -1,
      -1,    -1,    -1,    -1,   773,   774,    -1,    -1,    -1,    -1,
     779,    -1,    -1,    -1,  2000,    -1,    -1,    -1,    -1,  2064,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1482,  1483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,  2049,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   848,
      -1,    -1,    -1,   852,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2115,
      -1,    -1,    -1,  2178,    -1,  1567,  1568,    -1,    -1,    -1,
      -1,    -1,  1574,    -1,    -1,    -1,    -1,   168,   169,    -1,
     171,   172,    -1,   174,  2140,    -1,  2142,   178,   179,   180,
     181,   182,   183,   184,    -1,   934,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1610,    -1,
      -1,  1613,   951,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   964,  2182,    -1,    -1,    -1,
      -1,   970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   987,    -1,
     989,    -1,    -1,   992,    -1,   994,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1006,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1678,    -1,    -1,    -1,
      -1,    -1,    -1,  1685,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2254,  1701,
      -1,    -1,    -1,    -1,    -1,    -1,  1045,  1046,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1720,    -1,
      -1,    -1,  1061,    -1,  1726,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1774,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1807,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,
      60,    61,    -1,    63,  1163,    -1,    -1,  1166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    78,    79,
      80,    81,    -1,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,    -1,   107,  1870,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
    1239,    -1,   142,    -1,   144,    -1,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,   167,   168,    -1,
      -1,   171,   172,    -1,    -1,    -1,   176,    -1,   178,   179,
     180,   181,   182,   183,   184,  1284,  1285,    -1,    -1,    -1,
      -1,    -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,
      63,    -1,    -1,    -1,    -1,    -1,  1315,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    79,    80,    81,  1328,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,    -1,   107,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,  2050,   142,
       1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,   171,   172,
      -1,    -1,    -1,   176,    -1,   178,   179,   180,   181,   182,
     183,   184,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     193,    -1,    -1,    -1,    55,    -1,  1445,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1464,  1465,    78,    -1,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,    -1,   107,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1539,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,    -1,   176,    -1,   178,   179,   180,
     181,   182,   183,   184,  1573,  1574,    -1,    -1,    -1,    -1,
      -1,    -1,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1602,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1612,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    72,  1685,    -1,    -1,    76,
      -1,  1690,    -1,    80,    -1,    -1,  1695,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,   112,   113,  1726,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1800,    -1,    -1,   191,    -1,   193,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      78,  1930,    80,    81,    -1,    83,    -1,    -1,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,    -1,   107,
    1959,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,   171,   172,    -1,    -1,    -1,   176,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,    -1,
    2039,    -1,    -1,  2042,    -1,   193,     3,     4,     5,     6,
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
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,
      -1,   168,   169,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   193,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,   161,   162,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   193,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,   193,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,   176,
      -1,    -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   191,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   191,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,   113,    -1,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   191,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,
     172,    -1,    -1,    -1,   176,    -1,    -1,   179,   180,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,
     172,   173,   174,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,
     171,   172,   173,   174,    -1,    -1,    -1,   178,   179,   180,
     181,   182,   183,   184,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,   171,   172,   173,   174,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,     4,     5,     6,     7,     8,
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
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,   171,   172,   173,   174,    -1,    -1,    -1,   178,
     179,   180,   181,   182,   183,   184,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,   171,   172,   173,   174,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,   172,   173,   174,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,   173,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,   171,   172,   173,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    -1,
     134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,   173,
      -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,   183,
     184,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
     173,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,
     172,   173,    -1,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,   173,    -1,    -1,    -1,    -1,   178,   179,   180,
     181,   182,   183,   184,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,     4,     5,     6,     7,     8,
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
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,
     179,   180,   181,   182,   183,   184,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    76,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,     1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
     115,   116,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,     1,    -1,   171,   172,    -1,    -1,
     115,   116,    -1,    -1,   179,   180,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,   171,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   115,   116,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,     1,    -1,   171,   172,    -1,    -1,   115,   116,
      -1,    -1,   179,   180,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,   171,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
       1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,     1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,   180,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   191,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   191,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,
      -1,   176,    -1,    -1,   179,   180,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   191,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,   191,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   191,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    -1,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,   179,   180,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   171,   172,    -1,     3,    -1,     5,    -1,
      -1,   179,   180,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     3,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,   144,   145,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
     170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     180,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,   170,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,   145,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,   171,   172,    -1,    -1,    -1,
      -1,    -1,    -1,   179,   180,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,
     172,   173,    -1,    -1,    -1,    -1,    -1,   179,   180,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
     145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,   145,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,   180,
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
      -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,   115,   116,    -1,    57,    -1,
      59,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,
      -1,   171,   172,    -1,   113,    -1,   115,   116,    -1,   179,
     180,    78,    -1,    80,    81,    -1,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      -1,    -1,    99,   100,   101,   102,   103,   104,   105,    -1,
     107,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,    -1,   130,   131,   132,   175,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,   176,
      -1,   178,   179,   180,   181,   182,   183,   184,    -1,    55,
      -1,    -1,    58,    -1,    60,    61,   193,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    80,    81,    -1,    83,    -1,    -1,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    -1,    -1,    99,   100,   101,   102,   103,   104,   105,
      -1,   107,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,
     176,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   193,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    -1,   171,    57,    -1,    59,
      -1,   176,    -1,    -1,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,   171,    57,    -1,    59,    -1,   176,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,    -1,    -1,    -1,
      -1,   176,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    96,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,    -1,   176,    -1,   178,   179,   180,
     181,   182,   183,   184,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   171,   172,    -1,    -1,    -1,   176,    -1,   178,
     179,   180,   181,   182,   183,   184,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,   129,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,   170,   171,   172,    -1,    -1,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,   129,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,
      -1,   176,    -1,   178,   179,   180,   181,   182,   183,   184,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,   129,   130,   131,   132,
      -1,   134,   135,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,
     181,   182,   183,   184,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   171,   172,    -1,    -1,    -1,   176,    -1,   178,
     179,   180,   181,   182,   183,   184,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    -1,   134,   135,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,   176,
      -1,   178,   179,   180,   181,   182,   183,   184,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    -1,   134,
     135,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,    -1,   161,   162,    -1,    -1,
      -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,   115,   116,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,    -1,
     113,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,   145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   180,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,   145,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
      -1,   179,   180,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
     171,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,   180,
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
      -1,    -1,   115,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   115,   116,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,   172,    -1,    -1,   115,   116,
      -1,    -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   115,   116,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   171,   172,    -1,    -1,   115,   116,    -1,    -1,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,    -1,    -1,   115,   116,    -1,    -1,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,   180,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,   115,   116,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   172,
      -1,    -1,   115,   116,    -1,    -1,   179,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   172,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   180,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
     115,   116,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,   172,    -1,    -1,
     115,   116,    -1,    -1,   179,   180,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   171,   172,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   180,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   115,   116,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,   115,   116,
      -1,    -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,   115,   116,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,   115,   116,
      57,   171,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,   115,   116,
      -1,    -1,   179,   180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,    -1,   179,   180,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,   115,   116,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   171,   172,    -1,    -1,   115,   116,    -1,    -1,
     179,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,
     179,   180,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   115,   116,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,    -1,    -1,   115,   116,    -1,    -1,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,    -1,    -1,    13,    14,    15,    16,    17,   179,   180,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,    -1,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,    -1,   179,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   171,   172,    -1,    13,    14,    15,    16,    17,   179,
     180,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   171,    -1,   130,   131,   132,    -1,   134,   135,
     179,   180,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,   173,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    -1,   134,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,
     181,   182,   183,   184,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,   171,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   171,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,   116,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,   171,    -1,    -1,    -1,    -1,   168,   169,    -1,
     171,   172,    -1,    -1,    -1,   176,    -1,   178,   179,   180,
     181,   182,   183,   184,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,   172,
     173,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,
      -1,   176,    -1,   178,   179,   180,   181,   182,   183,   184,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,   176,
      -1,   178,   179,   180,   181,   182,   183,   184,    -1,    -1,
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
     169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,
     179,   180,   181,   182,   183,   184,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,   175,    -1,    -1,   178,   179,   180,
     181,   182,   183,   184,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
     173,    -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,
     183,   184,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,    -1,   171,   172,   173,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,    -1,    -1,
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    55,   134,   135,    58,    -1,    60,
      61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,   171,   172,   173,    -1,    -1,    -1,    -1,   178,
     179,   180,   181,   182,   183,   184,    -1,    -1,   109,   110,
     111,    -1,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,    -1,   130,
     131,   132,    55,   134,   135,    58,    -1,    60,    61,    -1,
      63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
     171,   172,    -1,    -1,    -1,   176,    -1,   178,   179,   180,
     181,   182,   183,   184,    -1,    -1,   109,   110,   111,    -1,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,    -1,    -1,   130,   131,   132,
      55,   134,   135,    58,    -1,    60,    61,    -1,    63,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,
      -1,    -1,    -1,   176,    -1,   178,   179,   180,   181,   182,
     183,   184,    -1,    -1,   109,   110,   111,    -1,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    -1,    -1,   130,   131,   132,    55,   134,
     135,    58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   168,    -1,   170,   171,   172,    -1,    -1,
      -1,    -1,    -1,   178,   179,   180,   181,   182,   183,   184,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,    -1,    -1,   130,   131,   132,    55,   134,   135,    58,
      -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,
      -1,   178,   179,   180,   181,   182,   183,   184,    -1,    -1,
     109,   110,   111,    -1,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
      -1,   130,   131,   132,    -1,   134,   135,    -1,    -1,    55,
      -1,    -1,    58,   142,    60,    61,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,   168,
     169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,
     179,   180,   181,   182,   183,   184,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,
     172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,    -1,    -1,   109,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,   183,
     184,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,
     172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,    -1,    -1,   109,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,   169,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,   183,
     184,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,   169,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,   169,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,   169,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,   169,    -1,   171,
     172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,    -1,    -1,   109,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,   183,
     184,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    55,   134,   135,
      58,    -1,    60,    61,    -1,    63,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,    -1,
      -1,   109,   110,   111,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
      -1,    -1,   130,   131,   132,    55,   134,   135,    58,    -1,
      60,    61,    -1,    63,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,   171,   172,    -1,    -1,    -1,    -1,    -1,
     178,   179,   180,   181,   182,   183,   184,    -1,    -1,   109,
     110,   111,    -1,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,    -1,    -1,
     130,   131,   132,    55,   134,   135,    58,    -1,    60,    61,
      -1,    63,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,
      -1,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,   179,
     180,   181,   182,   183,   184,    -1,    -1,   109,   110,   111,
      -1,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,    -1,    -1,   130,   131,
     132,    55,   134,   135,    58,    -1,    60,    61,    -1,    63,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,
     172,    -1,    -1,    -1,    -1,    -1,   178,   179,   180,   181,
     182,   183,   184,    -1,    -1,   109,   110,   111,    -1,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,    -1,   130,   131,   132,    55,
     134,   135,    58,    -1,    60,    61,    -1,    63,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   168,    -1,    -1,   171,   172,    -1,
      -1,    -1,    -1,    -1,   178,   179,   180,   181,   182,   183,
     184,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,    -1,    -1,   130,   131,   132,    -1,   134,   135,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,   171,   172,    -1,    -1,    -1,
      -1,    -1,   178,   179,   180,   181,   182,   183,   184,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      -1,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,   113,    -1,
     115,   116,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
     115,   116
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    79,   129,   144,   422,   424,   439,   440,   441,   168,
      13,    94,   113,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   198,   199,   201,   442,   443,   444,     0,
     439,   195,   441,   168,   442,   173,   174,   168,   195,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    20,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    57,    59,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    76,    80,
     107,   112,   113,   115,   116,   128,   142,   168,   169,   171,
     172,   179,   180,   191,   193,   199,   200,   214,   308,   309,
     310,   311,   312,   313,   314,   315,   316,   317,   318,   319,
     322,   325,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   338,   340,   341,   342,   344,   345,   349,   350,
     351,   352,   353,   355,   361,   362,   363,   364,   376,   381,
     414,   417,   427,   433,   435,   445,   450,   451,   452,   453,
     454,   455,   456,   457,   483,   501,   502,   503,   504,   442,
     170,   443,    55,    58,    60,    61,    63,    80,   109,   110,
     111,   113,   114,   125,   126,   127,   130,   131,   132,   134,
     135,   168,   172,   178,   181,   182,   183,   184,   191,   197,
     198,   202,   203,   204,   207,   212,   213,   214,   215,   216,
     219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   232,   233,   234,   235,   240,   351,   427,   199,
     439,   127,   168,    65,    68,    69,    71,   168,   168,   379,
     439,   337,   338,   201,   415,   416,   201,   427,   168,   168,
       4,   113,   115,   116,   329,   331,   334,   335,   168,   214,
     440,   445,   451,   452,   453,   455,   456,   457,   115,   352,
      64,   172,   173,   179,   214,   235,   314,   315,   324,   326,
     328,   332,   333,   340,   341,   347,   348,   349,   350,   351,
     354,   361,   362,   381,   388,   389,   390,   391,   392,   393,
     483,   496,   497,   498,   499,   504,   505,   199,   172,   315,
     325,   328,   341,   345,   350,   440,   450,   454,   483,   500,
     501,   504,   505,   193,   193,   196,   165,   176,   192,   238,
     397,    95,   174,   434,   107,   201,   438,   174,   174,   174,
     193,   115,   116,   168,   214,   320,   321,   445,   446,   447,
     448,   449,   450,   454,   458,   459,   460,   461,   462,   463,
     464,   465,   466,   472,     3,    53,    54,    56,    62,   343,
       3,   172,   214,   314,   315,   329,   333,   335,   346,   351,
     430,   450,   454,   504,   439,    76,   312,   314,   328,   341,
     345,   350,   431,   450,   454,    72,   334,   439,   334,   329,
     335,   439,   323,   334,   335,   343,   362,   329,   334,   329,
     439,   171,   439,   174,   196,   168,   176,   246,   439,   439,
       3,   303,   304,   319,   322,   328,   332,   172,   214,   325,
     328,   502,   504,   201,   201,   170,   168,   212,   168,   168,
     212,   168,   168,   212,   216,   168,   113,   115,   116,   329,
     334,   335,   168,   212,   212,    19,    21,    92,   172,   181,
     182,   214,   217,   218,   235,   242,   246,   325,   328,   364,
     395,   500,   504,   169,   174,   235,   168,   204,   199,   172,
     177,   172,   177,   127,   131,   133,   134,   135,   168,   171,
     172,   176,   177,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,   156,   192,   237,   238,   239,   168,   212,
     216,   216,   185,   179,   186,   187,   181,   182,   136,   137,
     138,   139,   188,   189,   140,   141,   180,   178,   190,   142,
     143,   191,   170,   174,   171,   196,   192,   312,   314,   325,
     328,   428,   429,    64,    72,    73,    74,    75,   172,   190,
     201,   403,   405,   409,   411,   412,   351,   170,   172,   214,
     324,   328,   341,   348,   350,   393,   496,   504,   439,   115,
     116,   183,   199,   351,   380,   472,   168,   410,   411,   168,
     202,   230,   231,   170,   172,   235,   395,   396,   413,   440,
     505,   328,   440,   451,   452,   453,   455,   456,   457,   170,
     170,   170,   170,   170,   170,   170,   439,   173,   235,   328,
     332,   115,   172,   199,   325,   328,   483,   502,   173,   172,
     326,   328,   341,   348,   350,   440,   495,   496,   504,   505,
     173,   168,   168,   172,   180,   192,   214,   445,   467,   468,
     469,   470,   471,   472,   473,   474,   475,   483,   485,   486,
     487,   488,   489,   490,   507,   145,   172,   214,   354,   498,
     504,   328,   348,   334,   329,   439,   439,   173,   174,   173,
     174,   326,   328,   497,   504,   201,   172,   326,   483,   497,
     504,   168,   201,   173,   172,   450,   454,   504,   439,   325,
     328,   450,   454,   172,   174,   113,   171,   172,   176,   198,
     200,   235,   398,   399,   400,   401,   402,    22,   398,   168,
     201,   246,   168,   168,   439,   439,   199,   440,   445,   447,
     448,   449,   458,   460,   461,   462,   464,   465,   466,   328,
     440,   446,   459,   463,   174,   438,   172,   439,   480,   483,
     438,   439,   439,   434,   303,   168,   439,   480,   438,   439,
     439,   434,   439,   439,   168,   214,   328,   436,   445,   446,
     450,   459,   463,   168,   168,   327,   328,   440,   325,   172,
     173,   325,   500,   505,   438,   439,   353,   176,   434,   303,
     201,   201,   397,   314,   333,   432,   450,   454,   439,   176,
     434,   303,   415,   439,   328,   341,   439,   328,   328,   439,
     115,   352,   115,   116,   199,   351,   356,   415,   145,   199,
     328,   385,   386,   390,   391,   394,   440,   167,   195,   439,
     246,   319,   193,   450,   463,   328,   179,   235,   389,   440,
     214,   504,   201,   438,   168,   438,   170,   395,   440,   505,
     204,   395,   172,   395,   396,   235,   395,   170,   395,   395,
     395,   173,   170,   181,   182,   218,    18,   330,   170,   174,
     170,   168,   214,   476,   477,   478,   479,   480,   179,   180,
     170,   174,   506,   173,   174,   176,   192,   235,   199,   235,
     199,   125,   172,   199,   232,   125,   172,   201,   364,   169,
     235,   241,   232,   199,   176,   235,   395,   505,   216,   219,
     219,   219,   220,   220,   221,   221,   222,   222,   222,   222,
     223,   223,   224,   225,   226,   227,   228,   175,   242,   234,
     172,   199,   235,   436,   172,   315,   425,   176,   168,   201,
     176,   201,   145,   179,   180,   408,   170,   174,   201,   412,
     170,   173,   168,   180,   214,   504,   170,   199,   380,   472,
     439,   176,   439,   403,   192,   403,   170,   170,   174,   170,
     174,   395,   505,   170,   170,   170,   170,   170,   170,   168,
     439,   480,   483,   168,   480,   483,   199,   168,   326,   483,
     497,   504,   172,   179,   214,   235,   351,   235,   328,   168,
     168,   325,   502,   504,   326,   328,   195,   168,   385,   445,
     468,   469,   470,   473,   486,   487,   488,   173,   195,    18,
     235,   328,   439,   440,   467,   471,   485,   168,   439,   489,
     507,   439,   439,   507,   168,   439,   489,   439,   439,   507,
     439,   439,   483,   173,   231,   173,   328,   326,   495,   505,
     201,   328,   199,   389,   392,   392,   393,   507,   326,   497,
     504,   195,   507,   195,   172,   200,   230,   231,   437,   399,
     175,   174,   506,   398,   171,   172,   192,   402,   413,   168,
     202,   195,   199,   436,   192,   445,   447,   448,   449,   458,
     460,   461,   462,   464,   465,   466,   170,   170,   170,   170,
     170,   170,   170,   170,   170,   170,   446,   459,   463,   446,
     459,   463,   439,   192,   173,   235,   335,   351,   481,   397,
     246,   434,   385,   397,   246,   440,   445,   328,   440,   436,
     168,   242,   396,   242,   396,   436,   115,   425,   246,   434,
     176,   176,   434,   303,   425,   246,   434,   439,   439,   439,
     176,   170,   174,   170,   174,   390,   391,    77,   305,   306,
     193,   173,   173,   174,   201,   438,   195,   170,   395,   170,
     174,   170,   174,   170,   174,   170,   216,   170,   170,   170,
     216,    18,   330,   235,   385,   477,   478,   479,   328,   439,
     440,   476,   439,   439,   170,   170,   169,   176,   216,   241,
     173,   173,   241,   232,   235,   173,   173,   125,   130,   132,
     200,   208,   209,   210,   170,   208,   173,   174,   167,   399,
     170,   170,   230,   175,   208,   389,   427,   425,   201,   173,
       1,   312,   314,   326,   328,   418,   419,   420,   421,   168,
     407,   405,   406,    85,   339,    18,   328,   439,   176,   439,
     377,    10,   178,   380,   382,   383,   380,   170,   396,   170,
     193,   202,   235,   396,   168,   439,   480,   483,   168,   480,
     483,   385,   385,   145,   387,   388,   389,   326,   497,   504,
     173,   173,   173,   235,   195,   195,   387,   473,   170,   170,
     170,   170,   170,   170,   170,   170,     5,   328,   168,   439,
     445,   472,   467,   471,   485,   168,   180,   214,   467,   471,
     385,   385,   173,   507,   173,   174,   387,   201,   208,   145,
     173,   184,   173,   506,   398,   400,   167,   170,   195,   170,
     387,   235,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   168,   439,   480,   483,   168,   439,   480,   483,   168,
     439,   480,   483,   436,    22,   483,   158,   174,   184,   482,
     173,   174,   246,   170,   445,   170,   170,   170,   170,   423,
     424,   246,   167,   418,   425,   246,   434,   423,   246,   358,
     359,   357,   365,   145,   439,   199,   201,   307,   247,   248,
     439,    77,   438,   387,   170,   324,   199,    85,   205,   206,
     395,   216,   216,   216,   170,   170,   170,   170,   476,   476,
     216,   216,   176,   399,   174,   506,   506,   167,   211,   172,
     209,   211,   211,   173,   174,   133,   171,   173,   169,   235,
     506,   230,   173,   426,   423,   170,   410,   436,   167,   419,
     174,   193,   174,   193,   413,   192,   404,   404,   378,   382,
     380,   380,   351,   174,   506,   201,   176,   176,   170,   385,
     385,   170,   170,   170,   174,   174,   173,   387,   387,   196,
     170,   168,   439,   480,   483,   168,   439,   489,   168,   439,
     489,   483,   327,     5,   179,   196,   235,   445,   439,   439,
     168,    18,   328,   440,   170,   170,   392,   196,   397,   173,
     231,   231,   167,   398,   439,   387,   439,   196,   168,   439,
     480,   483,   168,   439,   480,   483,   168,   439,   480,   483,
     385,   385,   385,   438,   173,   242,   235,   235,   335,   351,
     426,   167,   423,   246,   426,   176,   176,   176,   167,   439,
     390,   391,   174,   193,   196,   248,    18,    78,    80,    81,
      83,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    99,   100,   101,   102,   103,   104,   105,   107,
     115,   116,   128,   168,   172,   201,   242,   243,   244,   245,
     246,   250,   251,   260,   267,   268,   269,   270,   271,   276,
     277,   280,   281,   282,   283,   284,   285,   286,   292,   293,
     294,   308,   328,   332,   435,   307,   196,   484,   485,   170,
     175,   170,   174,   175,   168,   439,   480,   483,   399,   506,
     173,   173,   131,   208,   209,   172,   209,   172,   209,   167,
     426,   201,   201,   436,   170,   396,   410,   410,   380,   506,
     176,   176,    10,   383,   167,   192,   384,   382,   167,   418,
     170,   170,   145,   389,   145,   196,   196,   170,   385,   385,
     385,   235,   235,   196,   173,   196,   170,   173,   196,   170,
     385,   385,   385,   170,   170,   170,   397,   173,   482,   167,
     426,   167,   365,   365,   365,   168,   360,     3,     5,    10,
      80,   309,   316,   317,   325,   328,   366,   372,   500,   201,
     167,   168,    68,    69,   193,   246,   308,   435,   168,   168,
      18,   244,   168,   168,   193,   201,   193,   201,   179,   201,
     176,   243,   168,    85,   193,   201,   168,   168,   244,   168,
     246,   235,   236,   236,    14,   295,   271,   282,   175,   193,
      97,    98,   275,   279,   119,   143,   274,   118,   142,   278,
     274,   394,   328,   193,   170,   170,   235,   206,   235,   385,
     506,   167,   173,   208,   208,   167,   408,   176,   167,   382,
     382,   351,   201,   439,   176,   231,   506,   167,   170,   170,
     170,   170,   170,   196,   196,   173,   173,   170,   439,   170,
     170,   170,   235,   167,   167,   167,   167,   413,   439,   325,
     439,   325,   372,   193,   193,   193,   168,   175,   214,   367,
     368,   369,   375,   445,   446,   459,   463,   174,   193,   201,
     232,   193,   246,   193,   246,   242,   252,   308,   310,   313,
     319,   328,   332,   242,    87,   170,   252,   157,   158,   161,
     162,   169,   170,   193,   242,   261,   262,   264,   308,   193,
     193,   242,   193,   399,   193,   242,   193,   193,   413,   242,
     261,   120,   121,   122,   123,   124,   287,   289,   290,   193,
     106,   193,    91,   168,   170,   439,   168,   168,   244,   244,
     271,   168,   281,   271,   281,   246,   439,   170,   167,   173,
     173,   404,   382,   439,   506,   506,   384,   399,   167,   439,
     439,   173,   173,   360,   360,   360,   170,   367,   325,   364,
     373,   500,   367,   193,   440,   445,   235,   328,   440,   167,
     174,   193,   374,   375,   374,   374,   201,   170,   170,   242,
     328,   170,   168,   244,   170,   184,   193,   264,   265,   244,
     243,   193,   265,   170,   175,   242,   169,   242,   243,   264,
     193,   506,   170,   170,   170,   170,   246,   289,   290,   168,
     235,   168,   202,     1,   244,   216,   272,   242,    82,   117,
     273,   275,    82,   410,   506,   167,   167,   506,   439,   439,
     439,   439,   193,   168,   214,   370,   371,   480,   491,   492,
     493,   494,   193,   174,   193,   193,   445,   439,   244,   244,
      84,    85,   176,   255,   256,   257,   170,   242,    82,   244,
     242,   169,   242,    82,   193,   169,   242,   243,   264,   328,
     350,   169,   242,   244,   262,   265,   159,   160,   163,   164,
     265,   266,   193,   242,   167,   176,   257,   244,   244,   168,
     291,   326,   328,   500,   193,   202,   170,   175,   170,   174,
     175,   170,   244,   168,   244,   244,   244,   167,   439,   439,
     167,   492,   493,   494,   328,   439,   491,   174,   193,   439,
     439,   369,    82,     1,   231,   253,   254,   437,     1,   175,
       1,   195,   244,   255,    82,   193,   170,   244,    82,   193,
     184,   184,   244,   243,   265,   265,   266,   193,    64,   242,
     263,   351,   184,   184,    82,   169,   242,   169,   242,   242,
     243,   193,     1,   195,   291,   193,   288,   168,   214,   436,
     491,   199,   175,   193,   172,   202,   296,   297,   298,   216,
     232,   242,   274,   439,   170,   170,   170,   491,   439,   244,
     145,     1,   174,   175,   167,   301,   302,   439,   244,    82,
     193,   244,   242,   169,   169,   242,   169,   242,   169,   242,
     242,   243,   199,   351,   169,   242,   169,   242,   244,   184,
     184,   184,   184,   167,   301,   288,   230,   170,   328,   440,
     175,   113,   168,   170,   175,   174,   170,   170,    82,   270,
     168,   439,   480,   483,   371,   231,   253,   256,   258,   259,
     308,   308,   244,   184,   184,   184,   184,   169,   169,   242,
     169,   242,   169,   242,   258,   170,   246,   296,   173,   231,
     193,   296,   298,   244,    82,   385,   249,   439,   196,   256,
     169,   169,   242,   169,   242,   169,   242,   196,   246,   175,
     202,   170,   170,   175,   244,   170,     1,   439,   244,   167,
     249,   167,   202,   299,   168,   193,   299,   244,   174,   175,
     231,   170,   202,   201,   300,   170,   193,   170,   174,   193,
     201
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   194,   195,   196,   197,   197,   197,   197,   197,   198,
     198,   198,   198,   198,   198,   198,   198,   199,   199,   200,
     200,   201,   201,   201,   202,   203,   203,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   205,   205,   206,   206,   207,   207,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   207,   208,
     208,   209,   209,   209,   209,   209,   209,   209,   210,   210,
     210,   211,   211,   212,   212,   212,   212,   212,   212,   212,
     212,   212,   212,   212,   212,   212,   212,   212,   212,   212,
     212,   212,   213,   213,   214,   214,   214,   215,   215,   215,
     215,   216,   216,   216,   216,   216,   216,   216,   216,   216,
     217,   217,   217,   217,   218,   218,   219,   219,   220,   220,
     220,   220,   221,   221,   221,   222,   222,   222,   223,   223,
     223,   223,   223,   224,   224,   224,   225,   225,   226,   226,
     227,   227,   228,   228,   229,   229,   230,   230,   230,   231,
     232,   232,   233,   233,   234,   234,   234,   235,   235,   235,
     236,   236,   237,   237,   238,   238,   239,   239,   239,   239,
     239,   239,   239,   239,   239,   239,   239,   240,   240,   240,
     240,   240,   241,   241,   241,   241,   242,   242,   243,   243,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   245,   245,   246,   246,
     247,   247,   248,   248,   248,   248,   248,   249,   249,   249,
     250,   251,   251,   251,   251,   251,   251,   251,   251,   252,
     252,   252,   252,   253,   253,   253,   254,   254,   255,   255,
     255,   255,   255,   256,   256,   257,   258,   258,   259,   259,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   261,   261,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   262,   262,   262,   262,   262,
     262,   263,   263,   263,   264,   264,   264,   264,   265,   265,
     266,   266,   266,   266,   267,   267,   267,   267,   267,   267,
     267,   267,   267,   267,   267,   267,   267,   267,   267,   267,
     267,   267,   267,   267,   268,   269,   270,   271,   271,   272,
     272,   273,   274,   274,   275,   275,   276,   276,   276,   276,
     276,   276,   277,   278,   278,   279,   280,   280,   281,   281,
     282,   282,   282,   283,   284,   285,   286,   286,   286,   287,
     287,   288,   288,   289,   289,   289,   289,   290,   291,   291,
     291,   291,   291,   292,   293,   293,   294,   294,   294,   294,
     294,   295,   295,   296,   296,   297,   297,   298,   298,   299,
     299,   299,   300,   300,   301,   301,   302,   302,   303,   303,
     304,   304,   305,   305,   306,   306,   307,   307,   308,   308,
     308,   309,   309,   310,   310,   310,   310,   310,   311,   311,
     311,   312,   312,   312,   312,   312,   312,   313,   313,   313,
     313,   313,   314,   314,   314,   314,   315,   315,   316,   316,
     316,   317,   317,   317,   317,   317,   318,   318,   319,   319,
     319,   319,   320,   320,   320,   320,   320,   321,   321,   322,
     322,   322,   322,   323,   323,   323,   324,   324,   324,   325,
     325,   325,   326,   326,   326,   327,   327,   328,   328,   329,
     330,   330,   330,   330,   330,   331,   332,   332,   332,   333,
     333,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     335,   336,   336,   336,   336,   336,   336,   336,   336,   336,
     336,   336,   336,   336,   336,   336,   336,   336,   336,   336,
     336,   336,   336,   336,   336,   336,   336,   336,   336,   336,
     336,   336,   336,   336,   336,   337,   337,   338,   339,   339,
     340,   340,   340,   340,   340,   341,   341,   341,   342,   342,
     342,   342,   343,   343,   343,   343,   343,   343,   344,   344,
     344,   344,   345,   346,   345,   345,   347,   347,   347,   347,
     348,   348,   348,   349,   349,   349,   349,   350,   350,   350,
     351,   351,   351,   351,   351,   351,   352,   352,   352,   353,
     353,   354,   354,   356,   355,   357,   355,   358,   355,   359,
     355,   355,   360,   360,   361,   361,   362,   362,   363,   363,
     363,   364,   364,   364,   364,   364,   364,   364,   364,   365,
     365,   366,   366,   366,   366,   366,   366,   366,   366,   366,
     366,   366,   366,   367,   367,   368,   368,   369,   369,   369,
     369,   370,   370,   370,   371,   372,   372,   373,   373,   374,
     374,   375,   376,   376,   377,   376,   376,   378,   376,   376,
     376,   379,   379,   380,   380,   381,   381,   382,   382,   382,
     382,   382,   383,   383,   384,   384,   384,   385,   385,   385,
     385,   386,   386,   386,   386,   386,   386,   387,   387,   387,
     387,   387,   387,   387,   388,   388,   388,   388,   389,   389,
     390,   390,   391,   391,   392,   392,   392,   392,   392,   393,
     393,   393,   393,   393,   394,   394,   395,   395,   395,   396,
     396,   396,   396,   397,   397,   397,   397,   398,   398,   399,
     399,   399,   399,   399,   400,   400,   401,   401,   402,   402,
     402,   402,   402,   403,   403,   404,   404,   406,   405,   407,
     405,   405,   405,   405,   408,   408,   408,   408,   409,   409,
     409,   409,   410,   410,   411,   411,   412,   412,   413,   413,
     413,   413,   414,   414,   414,   415,   415,   416,   416,   417,
     417,   417,   417,   418,   418,   419,   419,   420,   420,   420,
     421,   421,   421,   422,   422,   423,   423,   424,   424,   425,
     426,   427,   427,   427,   427,   427,   427,   427,   427,   427,
     427,   427,   428,   427,   429,   427,   430,   427,   431,   427,
     432,   427,   427,   433,   433,   433,   434,   434,   435,   435,
     435,   435,   435,   435,   435,   435,   435,   435,   436,   436,
     436,   436,   437,   438,   438,   439,   439,   440,   440,   441,
     441,   441,   441,   442,   442,   443,   443,   443,   444,   444,
     444,   445,   445,   445,   446,   446,   446,   446,   447,   447,
     447,   447,   447,   448,   448,   448,   448,   448,   448,   448,
     449,   449,   449,   449,   450,   450,   450,   451,   451,   451,
     451,   451,   452,   452,   452,   452,   452,   453,   453,   453,
     453,   453,   453,   454,   454,   454,   455,   455,   455,   455,
     455,   456,   456,   456,   456,   456,   457,   457,   457,   457,
     457,   457,   458,   458,   459,   459,   459,   459,   460,   460,
     460,   460,   460,   461,   461,   461,   461,   461,   461,   461,
     462,   462,   462,   462,   463,   463,   463,   464,   464,   464,
     464,   464,   465,   465,   465,   465,   465,   466,   466,   466,
     466,   466,   466,   467,   467,   467,   467,   467,   468,   468,
     468,   468,   469,   469,   469,   469,   470,   470,   470,   471,
     471,   471,   471,   471,   472,   472,   473,   473,   473,   473,
     474,   474,   475,   475,   476,   476,   476,   477,   477,   477,
     477,   477,   477,   478,   478,   478,   478,   479,   479,   479,
     480,   480,   480,   480,   480,   480,   481,   481,   481,   481,
     481,   481,   482,   482,   483,   483,   483,   483,   484,   484,
     485,   485,   485,   485,   486,   486,   486,   486,   486,   487,
     487,   487,   487,   488,   488,   488,   489,   489,   489,   490,
     490,   490,   490,   490,   490,   491,   491,   491,   492,   492,
     492,   492,   492,   493,   493,   493,   493,   494,   494,   495,
     495,   495,   496,   496,   496,   497,   497,   497,   497,   497,
     497,   497,   498,   498,   498,   498,   498,   498,   498,   498,
     498,   498,   498,   498,   498,   498,   498,   499,   499,   499,
     499,   500,   500,   500,   501,   501,   502,   502,   502,   502,
     502,   502,   502,   503,   503,   503,   503,   503,   503,   504,
     504,   504,   505,   505,   505,   506,   506,   507,   507
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     3,     3,     3,     5,     6,     2,     2,     2,     2,
       2,     2,     1,     3,     3,     3,     1,     4,     4,     4,
       4,     4,     7,     3,     3,     3,     3,     3,     2,     5,
       3,     3,     3,     5,     2,     2,     7,     8,     5,     1,
       3,     1,     2,     4,     3,     5,     3,     5,     2,     2,
       2,     0,     2,     1,     1,     1,     2,     2,     2,     2,
       2,     2,     4,     5,     2,     4,     4,     4,     6,     4,
       2,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     5,     5,     4,     5,     5,     5,     4,
       2,     2,     3,     3,     1,     1,     1,     3,     1,     3,
       3,     3,     1,     3,     3,     1,     3,     3,     1,     3,
       3,     3,     3,     1,     3,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     5,     4,     1,
       0,     1,     1,     3,     1,     4,     1,     1,     3,     6,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     4,
       6,     6,     1,     1,     3,     3,     1,     3,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     4,     2,     6,
       1,     2,     2,     3,     2,     3,     2,     2,     3,     2,
       2,     5,     7,     5,    10,     7,     5,    10,     7,     1,
       1,     1,     2,     1,     3,     1,     1,     3,     2,     3,
       3,     2,     2,     1,     2,     2,     0,     1,     2,     3,
       4,     6,     5,     7,     6,     7,     7,     8,     4,     6,
       5,     7,     1,     3,     4,     5,     4,     3,     5,     1,
       2,     3,     3,     3,     5,     5,     5,     5,     3,     5,
       5,     5,     3,     4,     5,     5,     5,     5,     5,     7,
       7,     7,     7,     7,     7,     7,     2,     3,     4,     4,
       4,     4,     6,     6,     6,     6,     6,     6,     6,     3,
       4,     1,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     4,     2,     3,     3,     2,
       3,     2,     3,     3,     6,     2,     2,     3,     3,     3,
       3,     3,     3,     5,     5,     5,     4,     0,     1,     1,
       3,     4,     1,     1,     4,     6,     3,     5,     5,     5,
       8,     9,     1,     1,     1,     4,     3,     3,     1,     3,
       1,     3,     5,     1,     2,     5,     3,     3,     4,     6,
       7,     0,     2,     1,     1,     1,     1,     2,     1,     2,
       2,     2,     1,     3,     1,     1,     6,     8,    10,    12,
      14,     0,     1,     0,     1,     1,     3,     4,     7,     0,
       1,     3,     1,     3,     0,     1,     2,     2,     0,     1,
       2,     3,     0,     1,     3,     4,     1,     3,     2,     2,
       2,     6,     4,     1,     1,     1,     1,     1,     2,     3,
       6,     3,     3,     4,     5,     2,     3,     1,     2,     2,
       3,     8,     9,     9,     8,     8,     3,     5,     3,     3,
       4,     4,     4,     4,     3,     4,     4,     5,     2,     1,
       1,     1,     3,     3,     2,     4,     6,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     0,     1,     2,     3,     1,
       1,     1,     1,     1,     1,     4,     1,     2,     3,     2,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     5,     0,     1,
       1,     2,     3,     3,     3,     2,     3,     3,     1,     2,
       2,     2,     4,     4,     4,     4,     1,     1,     1,     2,
       2,     3,     1,     0,     3,     2,     1,     2,     2,     3,
       1,     2,     2,     2,     3,     3,     3,     1,     2,     2,
       1,     2,     3,     1,     2,     3,     1,     3,     4,     1,
       1,     1,     1,     0,     8,     0,    10,     0,    10,     0,
      10,     1,     0,     3,     3,     3,     1,     1,     2,     1,
       1,     1,     2,     1,     2,     1,     2,     1,     2,     0,
       3,     3,     3,     4,     4,     5,     4,     2,     2,     3,
       4,     2,     2,     0,     1,     1,     4,     1,     2,     2,
       2,     0,     1,     4,     1,     2,     3,     1,     2,     0,
       1,     2,     8,     9,     0,    11,    10,     0,    12,    11,
       1,     2,     3,     0,     1,     3,     3,     0,     3,     2,
       5,     4,     1,     1,     0,     2,     5,     0,     1,     1,
       3,     1,     2,     1,     2,     4,     4,     0,     1,     1,
       1,     3,     3,     3,     1,     3,     3,     5,     1,     3,
       3,     3,     2,     3,     1,     3,     3,     4,     1,     1,
       1,     1,     2,     1,     1,     3,     1,     2,     1,     1,
       2,     1,     2,     0,     2,     2,     4,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     3,
       3,     5,     4,     1,     3,     0,     2,     0,     5,     0,
       5,     4,     1,     8,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     5,     4,     1,     1,
       3,     3,     2,     3,     3,     2,     4,     1,     4,     7,
       5,     8,     6,     1,     2,     2,     2,     1,     1,     3,
       2,     3,     1,     0,     1,     0,     1,     4,     5,     0,
       0,     1,     1,     2,     2,     2,     2,     2,     2,     1,
       2,     5,     0,     6,     0,     8,     0,     7,     0,     7,
       0,     8,     1,     1,     2,     3,     0,     5,     3,     4,
       4,     4,     4,     5,     5,     5,     5,     6,     1,     1,
       1,     1,     3,     0,     5,     0,     1,     1,     2,     6,
       4,     3,     1,     1,     3,     0,     1,     4,     1,     1,
       1,     1,     2,     3,     2,     1,     2,     2,     2,     3,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     7,     3,     4,     2,     1,     2,     4,     6,     7,
       3,     4,     2,     3,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     1,     1,     1,     4,     6,     7,     3,
       4,     2,     3,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       6,     7,     3,     4,     2,     1,     2,     4,     6,     7,
       3,     4,     2,     3,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     2,     4,     1,     2,     2,     2,     3,
       3,     4,     2,     4,     4,     3,     4,     6,     3,     2,
       4,     1,     2,     2,     1,     1,     2,     3,     3,     4,
       2,     4,     4,     6,     1,     2,     2,     2,     2,     2,
       3,     3,     4,     1,     4,     4,     3,     3,     6,     3,
       2,     3,     4,     5,     3,     1,     1,     1,     3,     3,
       3,     5,     1,     1,     3,     3,     4,     4,     0,     1,
       1,     3,     2,     2,     2,     2,     2,     3,     4,     1,
       4,     4,     3,     3,     6,     3,     1,     2,     1,     2,
       6,     5,     6,     7,     7,     1,     2,     2,     2,     2,
       2,     3,     4,     1,     4,     4,     3,     6,     3,     1,
       1,     2,     1,     1,     2,     2,     3,     3,     2,     3,
       2,     3,     3,     3,     2,     2,     4,     4,     3,     3,
       2,     2,     3,     2,     4,     3,     2,     4,     4,     4,
       5,     1,     2,     1,     1,     1,     2,     3,     3,     2,
       3,     2,     3,     3,     4,     2,     3,     4,     2,     3,
       4,     5,     5,     6,     6,     0,     1,     0,     2
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

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


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

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

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
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
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
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
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
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
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
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
# endif
#endif

#ifndef yytnamerr
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
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
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
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
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
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
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
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
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


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
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
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

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
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
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
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
  case 2: /* push: %empty  */
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 10404 "Parser/parser.cc"
    break;

  case 3: /* pop: %empty  */
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 10410 "Parser/parser.cc"
    break;

  case 4: /* constant: INTEGERconstant  */
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 5: /* constant: FLOATING_DECIMALconstant  */
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 6: /* constant: FLOATING_FRACTIONconstant  */
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 7: /* constant: FLOATINGconstant  */
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 8: /* constant: CHARACTERconstant  */
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 20: /* identifier_at: '@'  */
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 10446 "Parser/parser.cc"
    break;

  case 24: /* string_literal: string_literal_list  */
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 10452 "Parser/parser.cc"
    break;

  case 25: /* string_literal_list: STRINGliteral  */
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 10458 "Parser/parser.cc"
    break;

  case 26: /* string_literal_list: string_literal_list STRINGliteral  */
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 10468 "Parser/parser.cc"
    break;

  case 27: /* primary_expression: IDENTIFIER  */
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 28: /* primary_expression: quasi_keyword  */
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 29: /* primary_expression: TYPEDIMname  */
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 31: /* primary_expression: '(' comma_expression ')'  */
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10492 "Parser/parser.cc"
    break;

  case 32: /* primary_expression: '(' compound_statement ')'  */
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 33: /* primary_expression: type_name '.' identifier  */
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 34: /* primary_expression: type_name '.' '[' field_name_list ']'  */
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10510 "Parser/parser.cc"
    break;

  case 35: /* primary_expression: GENERIC '(' assignment_expression ',' generic_assoc_list ')'  */
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 10520 "Parser/parser.cc"
    break;

  case 36: /* primary_expression: IDENTIFIER IDENTIFIER  */
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 10526 "Parser/parser.cc"
    break;

  case 37: /* primary_expression: IDENTIFIER type_qualifier  */
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 10532 "Parser/parser.cc"
    break;

  case 38: /* primary_expression: IDENTIFIER storage_class  */
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 10538 "Parser/parser.cc"
    break;

  case 39: /* primary_expression: IDENTIFIER basic_type_name  */
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10544 "Parser/parser.cc"
    break;

  case 40: /* primary_expression: IDENTIFIER TYPEDEFname  */
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10550 "Parser/parser.cc"
    break;

  case 41: /* primary_expression: IDENTIFIER TYPEGENname  */
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10556 "Parser/parser.cc"
    break;

  case 43: /* generic_assoc_list: generic_assoc_list ',' generic_association  */
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 10568 "Parser/parser.cc"
    break;

  case 44: /* generic_association: type_no_function ':' assignment_expression  */
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 10577 "Parser/parser.cc"
    break;

  case 45: /* generic_association: DEFAULT ':' assignment_expression  */
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 10583 "Parser/parser.cc"
    break;

  case 47: /* postfix_expression: postfix_expression '[' tuple_expression_list ']'  */
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-1].expr) ) ) ) ); }
#line 10589 "Parser/parser.cc"
    break;

  case 48: /* postfix_expression: constant '[' assignment_expression ']'  */
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 49: /* postfix_expression: string_literal '[' assignment_expression ']'  */
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10601 "Parser/parser.cc"
    break;

  case 50: /* postfix_expression: postfix_expression '{' argument_expression_list_opt '}'  */
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 10611 "Parser/parser.cc"
    break;

  case 51: /* postfix_expression: postfix_expression '(' argument_expression_list_opt ')'  */
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10617 "Parser/parser.cc"
    break;

  case 52: /* postfix_expression: VA_ARG '(' primary_expression ',' declaration_specifier_nobody abstract_parameter_declarator_opt ')'  */
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 10623 "Parser/parser.cc"
    break;

  case 53: /* postfix_expression: postfix_expression '`' identifier  */
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10629 "Parser/parser.cc"
    break;

  case 54: /* postfix_expression: constant '`' identifier  */
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10635 "Parser/parser.cc"
    break;

  case 55: /* postfix_expression: string_literal '`' identifier  */
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 56: /* postfix_expression: postfix_expression '.' identifier_or_type_name  */
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 57: /* postfix_expression: postfix_expression '.' INTEGERconstant  */
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10653 "Parser/parser.cc"
    break;

  case 58: /* postfix_expression: postfix_expression FLOATING_FRACTIONconstant  */
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 59: /* postfix_expression: postfix_expression '.' '[' field_name_list ']'  */
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10665 "Parser/parser.cc"
    break;

  case 60: /* postfix_expression: postfix_expression '.' aggregate_control  */
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 61: /* postfix_expression: postfix_expression ARROW identifier  */
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10677 "Parser/parser.cc"
    break;

  case 62: /* postfix_expression: postfix_expression ARROW INTEGERconstant  */
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 63: /* postfix_expression: postfix_expression ARROW '[' field_name_list ']'  */
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 64: /* postfix_expression: postfix_expression ICR  */
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 65: /* postfix_expression: postfix_expression DECR  */
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10701 "Parser/parser.cc"
    break;

  case 66: /* postfix_expression: '(' type_no_function ')' '{' initializer_list_opt comma_opt '}'  */
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10707 "Parser/parser.cc"
    break;

  case 67: /* postfix_expression: '(' type_no_function ')' '@' '{' initializer_list_opt comma_opt '}'  */
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10713 "Parser/parser.cc"
    break;

  case 68: /* postfix_expression: '^' primary_expression '{' argument_expression_list_opt '}'  */
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10723 "Parser/parser.cc"
    break;

  case 70: /* field_name_list: field_name_list ',' field  */
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 72: /* field: FLOATING_DECIMALconstant field  */
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 73: /* field: FLOATING_DECIMALconstant '[' field_name_list ']'  */
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 74: /* field: field_name '.' field  */
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 75: /* field: field_name '.' '[' field_name_list ']'  */
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 76: /* field: field_name ARROW field  */
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10759 "Parser/parser.cc"
    break;

  case 77: /* field: field_name ARROW '[' field_name_list ']'  */
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10765 "Parser/parser.cc"
    break;

  case 78: /* field_name: INTEGERconstant fraction_constants_opt  */
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 79: /* field_name: FLOATINGconstant fraction_constants_opt  */
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10777 "Parser/parser.cc"
    break;

  case 80: /* field_name: identifier_at fraction_constants_opt  */
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );	}
#line 10783 "Parser/parser.cc"
    break;

  case 81: /* fraction_constants_opt: %empty  */
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10789 "Parser/parser.cc"
    break;

  case 82: /* fraction_constants_opt: fraction_constants_opt FLOATING_FRACTIONconstant  */
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10798 "Parser/parser.cc"
    break;

  case 85: /* unary_expression: string_literal  */
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10804 "Parser/parser.cc"
    break;

  case 86: /* unary_expression: EXTENSION cast_expression  */
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10810 "Parser/parser.cc"
    break;

  case 87: /* unary_expression: ptrref_operator cast_expression  */
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10830 "Parser/parser.cc"
    break;

  case 88: /* unary_expression: unary_operator cast_expression  */
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 89: /* unary_expression: ICR unary_expression  */
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 90: /* unary_expression: DECR unary_expression  */
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 91: /* unary_expression: SIZEOF unary_expression  */
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 92: /* unary_expression: SIZEOF '(' type_no_function ')'  */
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 93: /* unary_expression: SIZEOF '(' attribute_list type_no_function ')'  */
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ) ) ) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 94: /* unary_expression: alignof_operator unary_expression  */
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ),
					(yyvsp[-1].oper) == OperKinds::AlignOf ? ast::AlignofExpr::Alignof : ast::AlignofExpr::__Alignof ) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 95: /* unary_expression: alignof_operator '(' type_no_function ')'  */
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ),
					(yyvsp[-3].oper) == OperKinds::AlignOf ? ast::AlignofExpr::Alignof : ast::AlignofExpr::__Alignof ) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 96: /* unary_expression: SIZEOF '(' cfa_abstract_function ')'  */
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 97: /* unary_expression: alignof_operator '(' cfa_abstract_function ')'  */
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ),
					(yyvsp[-3].oper) == OperKinds::AlignOf ? ast::AlignofExpr::Alignof : ast::AlignofExpr::__Alignof ) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 98: /* unary_expression: OFFSETOF '(' type_no_function ',' identifier ')'  */
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10899 "Parser/parser.cc"
    break;

  case 99: /* unary_expression: TYPEID '(' type ')'  */
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10908 "Parser/parser.cc"
    break;

  case 100: /* unary_expression: COUNTOF unary_expression  */
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 101: /* unary_expression: COUNTOF '(' type_no_function ')'  */
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 102: /* alignof_operator: ALIGNOF  */
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AlignOf; }
#line 10926 "Parser/parser.cc"
    break;

  case 103: /* alignof_operator: __ALIGNOF  */
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::__AlignOf; }
#line 10932 "Parser/parser.cc"
    break;

  case 104: /* ptrref_operator: '*'  */
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10938 "Parser/parser.cc"
    break;

  case 105: /* ptrref_operator: '&'  */
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10944 "Parser/parser.cc"
    break;

  case 106: /* ptrref_operator: ANDAND  */
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10950 "Parser/parser.cc"
    break;

  case 107: /* unary_operator: '+'  */
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10956 "Parser/parser.cc"
    break;

  case 108: /* unary_operator: '-'  */
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10962 "Parser/parser.cc"
    break;

  case 109: /* unary_operator: '!'  */
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10968 "Parser/parser.cc"
    break;

  case 110: /* unary_operator: '~'  */
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10974 "Parser/parser.cc"
    break;

  case 112: /* cast_expression: '(' type_no_function ')' cast_expression  */
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 113: /* cast_expression: '(' aggregate_control '&' ')' cast_expression  */
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 114: /* cast_expression: '(' aggregate_control '*' ')' cast_expression  */
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 115: /* cast_expression: '(' VIRTUAL ')' cast_expression  */
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 116: /* cast_expression: '(' VIRTUAL type_no_function ')' cast_expression  */
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 117: /* cast_expression: '(' RETURN type_no_function ')' cast_expression  */
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 118: /* cast_expression: '(' COERCE type_no_function ')' cast_expression  */
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11016 "Parser/parser.cc"
    break;

  case 119: /* cast_expression: '(' qualifier_cast_list ')' cast_expression  */
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11022 "Parser/parser.cc"
    break;

  case 127: /* exponential_expression: exponential_expression '\\' cast_expression  */
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 129: /* multiplicative_expression: multiplicative_expression '*' exponential_expression  */
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 130: /* multiplicative_expression: multiplicative_expression '/' exponential_expression  */
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 131: /* multiplicative_expression: multiplicative_expression '%' exponential_expression  */
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 133: /* additive_expression: additive_expression '+' multiplicative_expression  */
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 134: /* additive_expression: additive_expression '-' multiplicative_expression  */
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 136: /* shift_expression: shift_expression LS additive_expression  */
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 137: /* shift_expression: shift_expression RS additive_expression  */
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 139: /* relational_expression: relational_expression '<' shift_expression  */
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 140: /* relational_expression: relational_expression '>' shift_expression  */
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 141: /* relational_expression: relational_expression LE shift_expression  */
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 142: /* relational_expression: relational_expression GE shift_expression  */
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 144: /* equality_expression: equality_expression EQ relational_expression  */
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 145: /* equality_expression: equality_expression NE relational_expression  */
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 147: /* AND_expression: AND_expression '&' equality_expression  */
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11112 "Parser/parser.cc"
    break;

  case 149: /* exclusive_OR_expression: exclusive_OR_expression '^' AND_expression  */
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 151: /* inclusive_OR_expression: inclusive_OR_expression '|' exclusive_OR_expression  */
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 153: /* logical_AND_expression: logical_AND_expression ANDAND inclusive_OR_expression  */
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 155: /* logical_OR_expression: logical_OR_expression OROR logical_AND_expression  */
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 157: /* conditional_expression: logical_OR_expression '?' comma_expression ':' conditional_expression  */
#line 1100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 158: /* conditional_expression: logical_OR_expression '?' ':' conditional_expression  */
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 160: /* argument_expression_list_opt: %empty  */
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11154 "Parser/parser.cc"
    break;

  case 163: /* argument_expression_list: argument_expression_list ',' argument_expression  */
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 164: /* argument_expression: '?'  */
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 165: /* argument_expression: '?' identifier '=' assignment_expression  */
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11172 "Parser/parser.cc"
    break;

  case 168: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 11184 "Parser/parser.cc"
    break;

  case 169: /* assignment_expression: unary_expression '=' '{' initializer_list_opt comma_opt '}'  */
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11190 "Parser/parser.cc"
    break;

  case 170: /* assignment_expression_opt: %empty  */
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11196 "Parser/parser.cc"
    break;

  case 174: /* simple_assignment_operator: '='  */
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 11202 "Parser/parser.cc"
    break;

  case 175: /* simple_assignment_operator: ATassign  */
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 11208 "Parser/parser.cc"
    break;

  case 176: /* compound_assignment_operator: EXPassign  */
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 11214 "Parser/parser.cc"
    break;

  case 177: /* compound_assignment_operator: MULTassign  */
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 11220 "Parser/parser.cc"
    break;

  case 178: /* compound_assignment_operator: DIVassign  */
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 11226 "Parser/parser.cc"
    break;

  case 179: /* compound_assignment_operator: MODassign  */
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 11232 "Parser/parser.cc"
    break;

  case 180: /* compound_assignment_operator: PLUSassign  */
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 11238 "Parser/parser.cc"
    break;

  case 181: /* compound_assignment_operator: MINUSassign  */
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 11244 "Parser/parser.cc"
    break;

  case 182: /* compound_assignment_operator: LSassign  */
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 11250 "Parser/parser.cc"
    break;

  case 183: /* compound_assignment_operator: RSassign  */
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 11256 "Parser/parser.cc"
    break;

  case 184: /* compound_assignment_operator: ANDassign  */
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 11262 "Parser/parser.cc"
    break;

  case 185: /* compound_assignment_operator: ERassign  */
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 11268 "Parser/parser.cc"
    break;

  case 186: /* compound_assignment_operator: ORassign  */
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 11274 "Parser/parser.cc"
    break;

  case 187: /* tuple: '[' ',' ']'  */
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 11280 "Parser/parser.cc"
    break;

  case 188: /* tuple: '[' assignment_expression ',' ']'  */
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 189: /* tuple: '[' '@' comma_opt ']'  */
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11292 "Parser/parser.cc"
    break;

  case 190: /* tuple: '[' assignment_expression ',' tuple_expression_list comma_opt ']'  */
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 191: /* tuple: '[' '@' ',' tuple_expression_list comma_opt ']'  */
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11304 "Parser/parser.cc"
    break;

  case 193: /* tuple_expression_list: '@'  */
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11310 "Parser/parser.cc"
    break;

  case 194: /* tuple_expression_list: tuple_expression_list ',' assignment_expression  */
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 195: /* tuple_expression_list: tuple_expression_list ',' '@'  */
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11322 "Parser/parser.cc"
    break;

  case 197: /* comma_expression: comma_expression ',' assignment_expression  */
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 198: /* comma_expression_opt: %empty  */
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11334 "Parser/parser.cc"
    break;

  case 213: /* statement: enable_disable_statement  */
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11340 "Parser/parser.cc"
    break;

  case 215: /* statement: DIRECTIVE  */
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 216: /* labelled_statement: identifier_or_type_name ':' attribute_list_opt statement  */
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 217: /* labelled_statement: identifier_or_type_name ':' attribute_list_opt error  */
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 11363 "Parser/parser.cc"
    break;

  case 218: /* compound_statement: '{' '}'  */
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 11369 "Parser/parser.cc"
    break;

  case 219: /* compound_statement: '{' push local_label_declaration_opt statement_decl_list pop '}'  */
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 11375 "Parser/parser.cc"
    break;

  case 221: /* statement_decl_list: statement_decl_list statement_decl  */
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 11381 "Parser/parser.cc"
    break;

  case 222: /* statement_decl: attribute_list_opt declaration  */
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11387 "Parser/parser.cc"
    break;

  case 223: /* statement_decl: attribute_list_opt EXTENSION declaration  */
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11393 "Parser/parser.cc"
    break;

  case 224: /* statement_decl: attribute_list_opt function_definition  */
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11399 "Parser/parser.cc"
    break;

  case 225: /* statement_decl: attribute_list_opt EXTENSION function_definition  */
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11405 "Parser/parser.cc"
    break;

  case 226: /* statement_decl: attribute_list_opt statement  */
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11411 "Parser/parser.cc"
    break;

  case 227: /* statement_list_nodecl: attribute_list_opt statement  */
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11417 "Parser/parser.cc"
    break;

  case 228: /* statement_list_nodecl: statement_list_nodecl attribute_list_opt statement  */
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-2].stmt) ); (yyvsp[-2].stmt)->set_last( (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ) ); (yyval.stmt) = (yyvsp[-2].stmt); }
#line 11423 "Parser/parser.cc"
    break;

  case 229: /* statement_list_nodecl: statement_list_nodecl error  */
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 11430 "Parser/parser.cc"
    break;

  case 230: /* expression_statement: comma_expression_opt ';'  */
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 11436 "Parser/parser.cc"
    break;

  case 231: /* selection_statement: IF '(' conditional_declaration ')' statement  */
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 11442 "Parser/parser.cc"
    break;

  case 232: /* selection_statement: IF '(' conditional_declaration ')' statement ELSE statement  */
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11448 "Parser/parser.cc"
    break;

  case 233: /* selection_statement: SWITCH '(' comma_expression ')' case_clause  */
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11454 "Parser/parser.cc"
    break;

  case 234: /* selection_statement: SWITCH '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}'  */
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 11468 "Parser/parser.cc"
    break;

  case 235: /* selection_statement: SWITCH '(' comma_expression ')' '{' error '}'  */
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11474 "Parser/parser.cc"
    break;

  case 236: /* selection_statement: CHOOSE '(' comma_expression ')' case_clause  */
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 237: /* selection_statement: CHOOSE '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}'  */
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 11489 "Parser/parser.cc"
    break;

  case 238: /* selection_statement: CHOOSE '(' comma_expression ')' '{' error '}'  */
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11495 "Parser/parser.cc"
    break;

  case 239: /* conditional_declaration: comma_expression  */
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 11501 "Parser/parser.cc"
    break;

  case 240: /* conditional_declaration: c_declaration  */
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11507 "Parser/parser.cc"
    break;

  case 241: /* conditional_declaration: cfa_declaration  */
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11513 "Parser/parser.cc"
    break;

  case 242: /* conditional_declaration: declaration comma_expression  */
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 11519 "Parser/parser.cc"
    break;

  case 243: /* case_value: constant_expression  */
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11525 "Parser/parser.cc"
    break;

  case 244: /* case_value: constant_expression ELLIPSIS constant_expression  */
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11531 "Parser/parser.cc"
    break;

  case 246: /* case_value_list: case_value  */
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 11537 "Parser/parser.cc"
    break;

  case 247: /* case_value_list: case_value_list ',' case_value  */
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 11543 "Parser/parser.cc"
    break;

  case 248: /* case_label: CASE error  */
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 11549 "Parser/parser.cc"
    break;

  case 249: /* case_label: CASE case_value_list ':'  */
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 11555 "Parser/parser.cc"
    break;

  case 250: /* case_label: CASE case_value_list error  */
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 11561 "Parser/parser.cc"
    break;

  case 251: /* case_label: DEFAULT ':'  */
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 11567 "Parser/parser.cc"
    break;

  case 252: /* case_label: DEFAULT error  */
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 11573 "Parser/parser.cc"
    break;

  case 254: /* case_label_list: case_label_list case_label  */
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 11579 "Parser/parser.cc"
    break;

  case 255: /* case_clause: case_label_list statement  */
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11585 "Parser/parser.cc"
    break;

  case 256: /* switch_clause_list_opt: %empty  */
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 11591 "Parser/parser.cc"
    break;

  case 258: /* switch_clause_list: case_label_list statement_list_nodecl  */
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11597 "Parser/parser.cc"
    break;

  case 259: /* switch_clause_list: switch_clause_list case_label_list statement_list_nodecl  */
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 11603 "Parser/parser.cc"
    break;

  case 260: /* iteration_statement: WHILE '(' ')' statement  */
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11609 "Parser/parser.cc"
    break;

  case 261: /* iteration_statement: WHILE '(' ')' statement ELSE statement  */
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11618 "Parser/parser.cc"
    break;

  case 262: /* iteration_statement: WHILE '(' conditional_declaration ')' statement  */
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11624 "Parser/parser.cc"
    break;

  case 263: /* iteration_statement: WHILE '(' conditional_declaration ')' statement ELSE statement  */
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 264: /* iteration_statement: DO statement WHILE '(' ')' ';'  */
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 265: /* iteration_statement: DO statement WHILE '(' ')' ELSE statement  */
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11645 "Parser/parser.cc"
    break;

  case 266: /* iteration_statement: DO statement WHILE '(' comma_expression ')' ';'  */
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 11651 "Parser/parser.cc"
    break;

  case 267: /* iteration_statement: DO statement WHILE '(' comma_expression ')' ELSE statement  */
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11657 "Parser/parser.cc"
    break;

  case 268: /* iteration_statement: FOR '(' ')' statement  */
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11663 "Parser/parser.cc"
    break;

  case 269: /* iteration_statement: FOR '(' ')' statement ELSE statement  */
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11672 "Parser/parser.cc"
    break;

  case 270: /* iteration_statement: FOR '(' for_control_expression_list ')' statement  */
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11678 "Parser/parser.cc"
    break;

  case 271: /* iteration_statement: FOR '(' for_control_expression_list ')' statement ELSE statement  */
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11684 "Parser/parser.cc"
    break;

  case 273: /* for_control_expression_list: for_control_expression_list ':' for_control_expression  */
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11703 "Parser/parser.cc"
    break;

  case 274: /* for_control_expression: ';' comma_expression_opt ';' comma_expression_opt  */
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 275: /* for_control_expression: comma_expression ';' comma_expression_opt ';' comma_expression_opt  */
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11717 "Parser/parser.cc"
    break;

  case 276: /* for_control_expression: declaration comma_expression_opt ';' comma_expression_opt  */
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 277: /* for_control_expression: '@' ';' comma_expression  */
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11729 "Parser/parser.cc"
    break;

  case 278: /* for_control_expression: '@' ';' comma_expression ';' comma_expression  */
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11735 "Parser/parser.cc"
    break;

  case 279: /* for_control_expression: comma_expression  */
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11741 "Parser/parser.cc"
    break;

  case 280: /* for_control_expression: updown comma_expression  */
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11747 "Parser/parser.cc"
    break;

  case 281: /* for_control_expression: comma_expression updownS comma_expression  */
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11753 "Parser/parser.cc"
    break;

  case 282: /* for_control_expression: '@' updownS comma_expression  */
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11762 "Parser/parser.cc"
    break;

  case 283: /* for_control_expression: comma_expression updownS '@'  */
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11771 "Parser/parser.cc"
    break;

  case 284: /* for_control_expression: comma_expression updownS comma_expression '~' comma_expression  */
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 285: /* for_control_expression: '@' updownS comma_expression '~' comma_expression  */
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11786 "Parser/parser.cc"
    break;

  case 286: /* for_control_expression: comma_expression updownS '@' '~' comma_expression  */
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11795 "Parser/parser.cc"
    break;

  case 287: /* for_control_expression: comma_expression updownS comma_expression '~' '@'  */
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11801 "Parser/parser.cc"
    break;

  case 288: /* for_control_expression: '@' updownS '@'  */
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11807 "Parser/parser.cc"
    break;

  case 289: /* for_control_expression: '@' updownS comma_expression '~' '@'  */
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11813 "Parser/parser.cc"
    break;

  case 290: /* for_control_expression: comma_expression updownS '@' '~' '@'  */
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11819 "Parser/parser.cc"
    break;

  case 291: /* for_control_expression: '@' updownS '@' '~' '@'  */
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11825 "Parser/parser.cc"
    break;

  case 292: /* for_control_expression: comma_expression ';' comma_expression  */
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11831 "Parser/parser.cc"
    break;

  case 293: /* for_control_expression: comma_expression ';' updown comma_expression  */
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11837 "Parser/parser.cc"
    break;

  case 294: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression  */
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11843 "Parser/parser.cc"
    break;

  case 295: /* for_control_expression: comma_expression ';' '@' updownS comma_expression  */
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11852 "Parser/parser.cc"
    break;

  case 296: /* for_control_expression: comma_expression ';' comma_expression updownS '@'  */
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11862 "Parser/parser.cc"
    break;

  case 297: /* for_control_expression: comma_expression ';' '@' updownS '@'  */
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11868 "Parser/parser.cc"
    break;

  case 298: /* for_control_expression: comma_expression ';' comma_expression updownEq comma_expression  */
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11874 "Parser/parser.cc"
    break;

  case 299: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression '~' comma_expression  */
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11880 "Parser/parser.cc"
    break;

  case 300: /* for_control_expression: comma_expression ';' '@' updownS comma_expression '~' comma_expression  */
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11889 "Parser/parser.cc"
    break;

  case 301: /* for_control_expression: comma_expression ';' comma_expression updownS '@' '~' comma_expression  */
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11899 "Parser/parser.cc"
    break;

  case 302: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression '~' '@'  */
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11905 "Parser/parser.cc"
    break;

  case 303: /* for_control_expression: comma_expression ';' '@' updownS comma_expression '~' '@'  */
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11914 "Parser/parser.cc"
    break;

  case 304: /* for_control_expression: comma_expression ';' comma_expression updownS '@' '~' '@'  */
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11924 "Parser/parser.cc"
    break;

  case 305: /* for_control_expression: comma_expression ';' '@' updownS '@' '~' '@'  */
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11930 "Parser/parser.cc"
    break;

  case 306: /* for_control_expression: declaration comma_expression  */
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11936 "Parser/parser.cc"
    break;

  case 307: /* for_control_expression: declaration updown comma_expression  */
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11942 "Parser/parser.cc"
    break;

  case 308: /* for_control_expression: declaration comma_expression updownS comma_expression  */
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11948 "Parser/parser.cc"
    break;

  case 309: /* for_control_expression: declaration '@' updownS comma_expression  */
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11957 "Parser/parser.cc"
    break;

  case 310: /* for_control_expression: declaration comma_expression updownS '@'  */
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11967 "Parser/parser.cc"
    break;

  case 311: /* for_control_expression: declaration comma_expression updownEq comma_expression  */
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11973 "Parser/parser.cc"
    break;

  case 312: /* for_control_expression: declaration comma_expression updownS comma_expression '~' comma_expression  */
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 313: /* for_control_expression: declaration '@' updownS comma_expression '~' comma_expression  */
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11988 "Parser/parser.cc"
    break;

  case 314: /* for_control_expression: declaration comma_expression updownS '@' '~' comma_expression  */
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11998 "Parser/parser.cc"
    break;

  case 315: /* for_control_expression: declaration comma_expression updownS comma_expression '~' '@'  */
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 12004 "Parser/parser.cc"
    break;

  case 316: /* for_control_expression: declaration '@' updownS comma_expression '~' '@'  */
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 12013 "Parser/parser.cc"
    break;

  case 317: /* for_control_expression: declaration comma_expression updownS '@' '~' '@'  */
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 12023 "Parser/parser.cc"
    break;

  case 318: /* for_control_expression: declaration '@' updownS '@' '~' '@'  */
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 12029 "Parser/parser.cc"
    break;

  case 319: /* for_control_expression: comma_expression ';' type_type_specifier  */
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 12037 "Parser/parser.cc"
    break;

  case 320: /* for_control_expression: comma_expression ';' updown enum_key  */
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 12049 "Parser/parser.cc"
    break;

  case 321: /* enum_key: type_name  */
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 12058 "Parser/parser.cc"
    break;

  case 322: /* enum_key: ENUM identifier  */
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 12067 "Parser/parser.cc"
    break;

  case 323: /* enum_key: ENUM type_name  */
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 12076 "Parser/parser.cc"
    break;

  case 324: /* updown: ErangeUpLt  */
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 12082 "Parser/parser.cc"
    break;

  case 325: /* updown: ErangeDownGt  */
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 12088 "Parser/parser.cc"
    break;

  case 326: /* updown: ErangeUpLe  */
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 12094 "Parser/parser.cc"
    break;

  case 327: /* updown: ErangeDownGe  */
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 12100 "Parser/parser.cc"
    break;

  case 328: /* updownS: '~'  */
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 12106 "Parser/parser.cc"
    break;

  case 330: /* updownEq: ErangeEq  */
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 12112 "Parser/parser.cc"
    break;

  case 331: /* updownEq: ErangeNe  */
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 12118 "Parser/parser.cc"
    break;

  case 332: /* updownEq: ErangeDownEq  */
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 12124 "Parser/parser.cc"
    break;

  case 333: /* updownEq: ErangeDownNe  */
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 12130 "Parser/parser.cc"
    break;

  case 334: /* jump_statement: GOTO identifier_or_type_name ';'  */
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 12136 "Parser/parser.cc"
    break;

  case 335: /* jump_statement: GOTO '*' comma_expression ';'  */
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 12142 "Parser/parser.cc"
    break;

  case 336: /* jump_statement: FALLTHROUGH ';'  */
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 12148 "Parser/parser.cc"
    break;

  case 337: /* jump_statement: FALLTHROUGH identifier_or_type_name ';'  */
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 12154 "Parser/parser.cc"
    break;

  case 338: /* jump_statement: FALLTHROUGH DEFAULT ';'  */
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 12160 "Parser/parser.cc"
    break;

  case 339: /* jump_statement: CONTINUE ';'  */
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 12166 "Parser/parser.cc"
    break;

  case 340: /* jump_statement: CONTINUE identifier_or_type_name ';'  */
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 12172 "Parser/parser.cc"
    break;

  case 341: /* jump_statement: BREAK ';'  */
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 12178 "Parser/parser.cc"
    break;

  case 342: /* jump_statement: BREAK identifier_or_type_name ';'  */
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 12184 "Parser/parser.cc"
    break;

  case 343: /* jump_statement: RETURN comma_expression_opt ';'  */
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 12190 "Parser/parser.cc"
    break;

  case 344: /* jump_statement: RETURN '{' initializer_list_opt comma_opt '}' ';'  */
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 12196 "Parser/parser.cc"
    break;

  case 345: /* jump_statement: SUSPEND ';'  */
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 12202 "Parser/parser.cc"
    break;

  case 346: /* jump_statement: SUSPEND compound_statement  */
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 12208 "Parser/parser.cc"
    break;

  case 347: /* jump_statement: SUSPEND COROUTINE ';'  */
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 12214 "Parser/parser.cc"
    break;

  case 348: /* jump_statement: SUSPEND COROUTINE compound_statement  */
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 349: /* jump_statement: SUSPEND GENERATOR ';'  */
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 12226 "Parser/parser.cc"
    break;

  case 350: /* jump_statement: SUSPEND GENERATOR compound_statement  */
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 12232 "Parser/parser.cc"
    break;

  case 351: /* jump_statement: THROW assignment_expression_opt ';'  */
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 12238 "Parser/parser.cc"
    break;

  case 352: /* jump_statement: THROWRESUME assignment_expression_opt ';'  */
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 12244 "Parser/parser.cc"
    break;

  case 353: /* jump_statement: THROWRESUME assignment_expression_opt AT assignment_expression ';'  */
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 12250 "Parser/parser.cc"
    break;

  case 354: /* with_statement: WITH '(' type_list ')' statement  */
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 12256 "Parser/parser.cc"
    break;

  case 355: /* mutex_statement: MUTEX '(' argument_expression_list_opt ')' statement  */
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 12265 "Parser/parser.cc"
    break;

  case 356: /* when_clause: WHEN '(' comma_expression ')'  */
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12271 "Parser/parser.cc"
    break;

  case 357: /* when_clause_opt: %empty  */
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12277 "Parser/parser.cc"
    break;

  case 360: /* cast_expression_list: cast_expression_list ',' cast_expression  */
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 12283 "Parser/parser.cc"
    break;

  case 361: /* timeout: TIMEOUT '(' comma_expression ')'  */
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12289 "Parser/parser.cc"
    break;

  case 364: /* waitfor: WAITFOR '(' cast_expression ')'  */
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12295 "Parser/parser.cc"
    break;

  case 365: /* waitfor: WAITFOR '(' cast_expression_list ':' argument_expression_list_opt ')'  */
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 12301 "Parser/parser.cc"
    break;

  case 366: /* wor_waitfor_clause: when_clause_opt waitfor statement  */
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 367: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt waitfor statement  */
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12313 "Parser/parser.cc"
    break;

  case 368: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt ELSE statement  */
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12319 "Parser/parser.cc"
    break;

  case 369: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement  */
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 370: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement wor ELSE statement  */
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 12331 "Parser/parser.cc"
    break;

  case 371: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement wor when_clause ELSE statement  */
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 372: /* waitfor_statement: wor_waitfor_clause  */
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 12343 "Parser/parser.cc"
    break;

  case 375: /* waituntil: WAITUNTIL '(' comma_expression ')'  */
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12349 "Parser/parser.cc"
    break;

  case 376: /* waituntil_clause: when_clause_opt waituntil statement  */
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12355 "Parser/parser.cc"
    break;

  case 377: /* waituntil_clause: '(' wor_waituntil_clause ')'  */
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 12361 "Parser/parser.cc"
    break;

  case 378: /* wand_waituntil_clause: waituntil_clause  */
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 12367 "Parser/parser.cc"
    break;

  case 379: /* wand_waituntil_clause: waituntil_clause wand wand_waituntil_clause  */
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 380: /* wor_waituntil_clause: wand_waituntil_clause  */
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 12379 "Parser/parser.cc"
    break;

  case 381: /* wor_waituntil_clause: wor_waituntil_clause wor wand_waituntil_clause  */
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 382: /* wor_waituntil_clause: wor_waituntil_clause wor when_clause_opt ELSE statement  */
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 383: /* waituntil_statement: wor_waituntil_clause  */
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 12397 "Parser/parser.cc"
    break;

  case 384: /* corun_statement: CORUN statement  */
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 385: /* cofor_statement: COFOR '(' for_control_expression_list ')' statement  */
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 386: /* exception_statement: TRY compound_statement handler_clause  */
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 387: /* exception_statement: TRY compound_statement finally_clause  */
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 12421 "Parser/parser.cc"
    break;

  case 388: /* exception_statement: TRY compound_statement handler_clause finally_clause  */
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 12427 "Parser/parser.cc"
    break;

  case 389: /* handler_clause: handler_key '(' exception_declaration handler_predicate_opt ')' compound_statement  */
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 12433 "Parser/parser.cc"
    break;

  case 390: /* handler_clause: handler_clause handler_key '(' exception_declaration handler_predicate_opt ')' compound_statement  */
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 12439 "Parser/parser.cc"
    break;

  case 391: /* handler_predicate_opt: %empty  */
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12445 "Parser/parser.cc"
    break;

  case 392: /* handler_predicate_opt: ';' conditional_expression  */
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 12451 "Parser/parser.cc"
    break;

  case 393: /* handler_key: CATCH  */
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12457 "Parser/parser.cc"
    break;

  case 394: /* handler_key: RECOVER  */
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12463 "Parser/parser.cc"
    break;

  case 395: /* handler_key: CATCHRESUME  */
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 12469 "Parser/parser.cc"
    break;

  case 396: /* handler_key: FIXUP  */
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 12475 "Parser/parser.cc"
    break;

  case 397: /* finally_clause: FINALLY compound_statement  */
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 12481 "Parser/parser.cc"
    break;

  case 399: /* exception_declaration: type_specifier_nobody declarator  */
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12487 "Parser/parser.cc"
    break;

  case 400: /* exception_declaration: type_specifier_nobody variable_abstract_declarator  */
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12493 "Parser/parser.cc"
    break;

  case 401: /* exception_declaration: cfa_abstract_declarator_tuple identifier  */
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12499 "Parser/parser.cc"
    break;

  case 406: /* asm_statement: ASM asm_volatile_opt '(' string_literal ')' ';'  */
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 407: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ')' ';'  */
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 408: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ')' ';'  */
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 409: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers_list_opt ')' ';'  */
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12523 "Parser/parser.cc"
    break;

  case 410: /* asm_statement: ASM asm_volatile_opt GOTO '(' string_literal ':' ':' asm_operands_opt ':' asm_clobbers_list_opt ':' asm_label_list ')' ';'  */
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 411: /* asm_volatile_opt: %empty  */
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 12535 "Parser/parser.cc"
    break;

  case 412: /* asm_volatile_opt: VOLATILE  */
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 12541 "Parser/parser.cc"
    break;

  case 413: /* asm_operands_opt: %empty  */
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12547 "Parser/parser.cc"
    break;

  case 416: /* asm_operands_list: asm_operands_list ',' asm_operand  */
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12553 "Parser/parser.cc"
    break;

  case 417: /* asm_operand: string_literal '(' constant_expression ')'  */
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 12559 "Parser/parser.cc"
    break;

  case 418: /* asm_operand: '[' IDENTIFIER ']' string_literal '(' constant_expression ')'  */
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 12568 "Parser/parser.cc"
    break;

  case 419: /* asm_clobbers_list_opt: %empty  */
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12574 "Parser/parser.cc"
    break;

  case 420: /* asm_clobbers_list_opt: string_literal  */
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12580 "Parser/parser.cc"
    break;

  case 421: /* asm_clobbers_list_opt: asm_clobbers_list_opt ',' string_literal  */
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12586 "Parser/parser.cc"
    break;

  case 422: /* asm_label_list: identifier_or_type_name  */
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 12592 "Parser/parser.cc"
    break;

  case 423: /* asm_label_list: asm_label_list ',' identifier_or_type_name  */
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 12598 "Parser/parser.cc"
    break;

  case 424: /* declaration_list_opt: %empty  */
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12604 "Parser/parser.cc"
    break;

  case 426: /* declaration_list: attribute_list_opt declaration  */
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12610 "Parser/parser.cc"
    break;

  case 427: /* declaration_list: declaration_list declaration  */
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12616 "Parser/parser.cc"
    break;

  case 428: /* KR_parameter_list_opt: %empty  */
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12622 "Parser/parser.cc"
    break;

  case 430: /* KR_parameter_list: c_declaration ';'  */
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12628 "Parser/parser.cc"
    break;

  case 431: /* KR_parameter_list: KR_parameter_list c_declaration ';'  */
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 12634 "Parser/parser.cc"
    break;

  case 441: /* static_assert: STATICASSERT '(' constant_expression ',' string_literal ')'  */
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 12640 "Parser/parser.cc"
    break;

  case 442: /* static_assert: STATICASSERT '(' constant_expression ')'  */
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 12646 "Parser/parser.cc"
    break;

  case 446: /* cfa_declaration: type_declaring_list  */
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12652 "Parser/parser.cc"
    break;

  case 448: /* cfa_variable_declaration: cfa_variable_specifier initializer_opt  */
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 12658 "Parser/parser.cc"
    break;

  case 449: /* cfa_variable_declaration: declaration_qualifier_list cfa_variable_specifier initializer_opt  */
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12664 "Parser/parser.cc"
    break;

  case 450: /* cfa_variable_declaration: cfa_variable_declaration pop ',' push identifier_or_type_name initializer_opt  */
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12670 "Parser/parser.cc"
    break;

  case 451: /* cfa_variable_specifier: cfa_abstract_declarator_no_tuple identifier_or_type_name asm_name_opt  */
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12676 "Parser/parser.cc"
    break;

  case 452: /* cfa_variable_specifier: cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12682 "Parser/parser.cc"
    break;

  case 453: /* cfa_variable_specifier: multi_array_dimension cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 454: /* cfa_variable_specifier: multi_array_dimension type_qualifier_list cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12694 "Parser/parser.cc"
    break;

  case 455: /* cfa_variable_specifier: cfa_function_return asm_name_opt  */
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12700 "Parser/parser.cc"
    break;

  case 456: /* cfa_variable_specifier: type_qualifier_list cfa_function_return asm_name_opt  */
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12706 "Parser/parser.cc"
    break;

  case 458: /* cfa_function_declaration: type_qualifier_list cfa_function_specifier  */
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12712 "Parser/parser.cc"
    break;

  case 459: /* cfa_function_declaration: declaration_qualifier_list cfa_function_specifier  */
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12718 "Parser/parser.cc"
    break;

  case 460: /* cfa_function_declaration: declaration_qualifier_list type_qualifier_list cfa_function_specifier  */
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12724 "Parser/parser.cc"
    break;

  case 461: /* cfa_function_declaration: cfa_function_declaration ',' identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12735 "Parser/parser.cc"
    break;

  case 462: /* cfa_function_specifier: '[' ']' identifier '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12741 "Parser/parser.cc"
    break;

  case 463: /* cfa_function_specifier: '[' ']' TYPEDEFname '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12747 "Parser/parser.cc"
    break;

  case 464: /* cfa_function_specifier: cfa_abstract_tuple identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12753 "Parser/parser.cc"
    break;

  case 465: /* cfa_function_specifier: cfa_function_return identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12759 "Parser/parser.cc"
    break;

  case 466: /* cfa_function_return: '[' cfa_parameter_list ']'  */
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12765 "Parser/parser.cc"
    break;

  case 467: /* cfa_function_return: '[' cfa_parameter_list ',' cfa_abstract_parameter_list ']'  */
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12771 "Parser/parser.cc"
    break;

  case 468: /* cfa_typedef_declaration: TYPEDEF attribute_list_opt cfa_variable_specifier  */
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12780 "Parser/parser.cc"
    break;

  case 469: /* cfa_typedef_declaration: TYPEDEF attribute_list_opt cfa_function_specifier  */
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12789 "Parser/parser.cc"
    break;

  case 470: /* cfa_typedef_declaration: cfa_typedef_declaration ',' attribute_list_opt identifier  */
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneType( (yyvsp[0].tok) )->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12798 "Parser/parser.cc"
    break;

  case 471: /* typedef_declaration: TYPEDEF attribute_list_opt type_specifier declarator  */
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef()->addQualifiers( (yyvsp[-2].decl) ); // watchout frees $3 and $4
		}
#line 12809 "Parser/parser.cc"
    break;

  case 472: /* typedef_declaration: typedef_declaration ',' attribute_list_opt declarator  */
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef()->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12818 "Parser/parser.cc"
    break;

  case 473: /* typedef_declaration: type_qualifier_list TYPEDEF type_specifier declarator  */
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12824 "Parser/parser.cc"
    break;

  case 474: /* typedef_declaration: type_specifier TYPEDEF declarator  */
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12830 "Parser/parser.cc"
    break;

  case 475: /* typedef_declaration: type_specifier TYPEDEF type_qualifier_list declarator  */
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12836 "Parser/parser.cc"
    break;

  case 476: /* typedef_expression: TYPEDEF identifier '=' assignment_expression  */
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12842 "Parser/parser.cc"
    break;

  case 477: /* typedef_expression: typedef_expression ',' identifier '=' assignment_expression  */
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12848 "Parser/parser.cc"
    break;

  case 478: /* c_declaration: declaration_specifier declaring_list  */
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distTypeSpec( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 481: /* c_declaration: sue_declaration_specifier  */
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12869 "Parser/parser.cc"
    break;

  case 482: /* declaring_list: variable_declarator asm_name_opt initializer_opt  */
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12875 "Parser/parser.cc"
    break;

  case 483: /* declaring_list: variable_type_redeclarator asm_name_opt initializer_opt  */
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12881 "Parser/parser.cc"
    break;

  case 484: /* declaring_list: general_function_declarator asm_name_opt  */
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12887 "Parser/parser.cc"
    break;

  case 485: /* declaring_list: general_function_declarator asm_name_opt '=' VOID  */
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12893 "Parser/parser.cc"
    break;

  case 486: /* declaring_list: declaring_list ',' attribute_list_opt declarator asm_name_opt initializer_opt  */
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12899 "Parser/parser.cc"
    break;

  case 492: /* declaration_specifier: sue_declaration_specifier invalid_types  */
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12909 "Parser/parser.cc"
    break;

  case 505: /* type_qualifier_list_opt: %empty  */
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12915 "Parser/parser.cc"
    break;

  case 507: /* type_qualifier_list: type_qualifier attribute_list_opt  */
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12921 "Parser/parser.cc"
    break;

  case 508: /* type_qualifier_list: type_qualifier_list type_qualifier attribute_list_opt  */
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12927 "Parser/parser.cc"
    break;

  case 509: /* type_qualifier: type_qualifier_name  */
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12933 "Parser/parser.cc"
    break;

  case 510: /* type_qualifier_name: CONST  */
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12939 "Parser/parser.cc"
    break;

  case 511: /* type_qualifier_name: RESTRICT  */
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12945 "Parser/parser.cc"
    break;

  case 512: /* type_qualifier_name: VOLATILE  */
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12951 "Parser/parser.cc"
    break;

  case 513: /* type_qualifier_name: ATOMIC  */
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12957 "Parser/parser.cc"
    break;

  case 514: /* type_qualifier_name: forall  */
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12963 "Parser/parser.cc"
    break;

  case 515: /* forall: FORALL '(' type_parameter_list ')'  */
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12969 "Parser/parser.cc"
    break;

  case 517: /* declaration_qualifier_list: type_qualifier_list storage_class_list  */
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12975 "Parser/parser.cc"
    break;

  case 518: /* declaration_qualifier_list: declaration_qualifier_list type_qualifier_list storage_class_list  */
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12981 "Parser/parser.cc"
    break;

  case 519: /* storage_class_list: storage_class attribute_list_opt  */
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12987 "Parser/parser.cc"
    break;

  case 520: /* storage_class_list: storage_class_list storage_class attribute_list_opt  */
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12993 "Parser/parser.cc"
    break;

  case 521: /* storage_class: EXTERN  */
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12999 "Parser/parser.cc"
    break;

  case 522: /* storage_class: STATIC  */
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 13005 "Parser/parser.cc"
    break;

  case 523: /* storage_class: AUTO  */
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 13011 "Parser/parser.cc"
    break;

  case 524: /* storage_class: REGISTER  */
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 13017 "Parser/parser.cc"
    break;

  case 525: /* storage_class: THREADLOCALGCC  */
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 13023 "Parser/parser.cc"
    break;

  case 526: /* storage_class: THREADLOCALC11  */
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 13029 "Parser/parser.cc"
    break;

  case 527: /* storage_class: INLINE  */
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 13035 "Parser/parser.cc"
    break;

  case 528: /* storage_class: FORTRAN  */
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 13041 "Parser/parser.cc"
    break;

  case 529: /* storage_class: NORETURN  */
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 13047 "Parser/parser.cc"
    break;

  case 530: /* basic_type_name: basic_type_name_type  */
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 13053 "Parser/parser.cc"
    break;

  case 531: /* basic_type_name_type: VOID  */
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 13059 "Parser/parser.cc"
    break;

  case 532: /* basic_type_name_type: BOOL  */
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 13065 "Parser/parser.cc"
    break;

  case 533: /* basic_type_name_type: CHAR  */
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 13071 "Parser/parser.cc"
    break;

  case 534: /* basic_type_name_type: INT  */
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 13077 "Parser/parser.cc"
    break;

  case 535: /* basic_type_name_type: INT128  */
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 13083 "Parser/parser.cc"
    break;

  case 536: /* basic_type_name_type: UINT128  */
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 13089 "Parser/parser.cc"
    break;

  case 537: /* basic_type_name_type: FLOAT  */
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 13095 "Parser/parser.cc"
    break;

  case 538: /* basic_type_name_type: DOUBLE  */
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 13101 "Parser/parser.cc"
    break;

  case 539: /* basic_type_name_type: FLOAT80  */
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 13107 "Parser/parser.cc"
    break;

  case 540: /* basic_type_name_type: uuFLOAT128  */
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 13113 "Parser/parser.cc"
    break;

  case 541: /* basic_type_name_type: FLOAT16  */
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 13119 "Parser/parser.cc"
    break;

  case 542: /* basic_type_name_type: FLOAT32  */
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 13125 "Parser/parser.cc"
    break;

  case 543: /* basic_type_name_type: FLOAT32X  */
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 13131 "Parser/parser.cc"
    break;

  case 544: /* basic_type_name_type: FLOAT64  */
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 13137 "Parser/parser.cc"
    break;

  case 545: /* basic_type_name_type: FLOAT64X  */
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 13143 "Parser/parser.cc"
    break;

  case 546: /* basic_type_name_type: FLOAT128  */
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 13149 "Parser/parser.cc"
    break;

  case 547: /* basic_type_name_type: FLOAT128X  */
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 13155 "Parser/parser.cc"
    break;

  case 548: /* basic_type_name_type: FLOAT32X4  */
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 13161 "Parser/parser.cc"
    break;

  case 549: /* basic_type_name_type: FLOAT64X2  */
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 13167 "Parser/parser.cc"
    break;

  case 550: /* basic_type_name_type: SVFLOAT32  */
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 13173 "Parser/parser.cc"
    break;

  case 551: /* basic_type_name_type: SVFLOAT64  */
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 13179 "Parser/parser.cc"
    break;

  case 552: /* basic_type_name_type: SVBOOL  */
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 13185 "Parser/parser.cc"
    break;

  case 553: /* basic_type_name_type: DECIMAL32  */
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13191 "Parser/parser.cc"
    break;

  case 554: /* basic_type_name_type: DECIMAL64  */
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13197 "Parser/parser.cc"
    break;

  case 555: /* basic_type_name_type: DECIMAL128  */
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13203 "Parser/parser.cc"
    break;

  case 556: /* basic_type_name_type: COMPLEX  */
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 13209 "Parser/parser.cc"
    break;

  case 557: /* basic_type_name_type: IMAGINARY  */
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 13215 "Parser/parser.cc"
    break;

  case 558: /* basic_type_name_type: SIGNED  */
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 13221 "Parser/parser.cc"
    break;

  case 559: /* basic_type_name_type: UNSIGNED  */
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 13227 "Parser/parser.cc"
    break;

  case 560: /* basic_type_name_type: SHORT  */
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 13233 "Parser/parser.cc"
    break;

  case 561: /* basic_type_name_type: LONG  */
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 13239 "Parser/parser.cc"
    break;

  case 562: /* basic_type_name_type: VA_LIST  */
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 13245 "Parser/parser.cc"
    break;

  case 563: /* basic_type_name_type: AUTO_TYPE  */
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 13251 "Parser/parser.cc"
    break;

  case 565: /* vtable_opt: %empty  */
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 13257 "Parser/parser.cc"
    break;

  case 567: /* vtable: VTABLE '(' type_name ')' default_opt  */
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 13263 "Parser/parser.cc"
    break;

  case 568: /* default_opt: %empty  */
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 13269 "Parser/parser.cc"
    break;

  case 569: /* default_opt: DEFAULT  */
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13275 "Parser/parser.cc"
    break;

  case 571: /* basic_declaration_specifier: declaration_qualifier_list basic_type_specifier  */
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13281 "Parser/parser.cc"
    break;

  case 572: /* basic_declaration_specifier: basic_declaration_specifier storage_class attribute_list_opt  */
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13287 "Parser/parser.cc"
    break;

  case 573: /* basic_declaration_specifier: basic_declaration_specifier storage_class type_qualifier_list  */
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13293 "Parser/parser.cc"
    break;

  case 574: /* basic_declaration_specifier: basic_declaration_specifier storage_class basic_type_specifier  */
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 13299 "Parser/parser.cc"
    break;

  case 575: /* basic_type_specifier: direct_type attribute_list_opt  */
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13305 "Parser/parser.cc"
    break;

  case 576: /* basic_type_specifier: type_qualifier_list_opt indirect_type attribute_list  */
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13311 "Parser/parser.cc"
    break;

  case 577: /* basic_type_specifier: type_qualifier_list_opt indirect_type type_qualifier_list_opt  */
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13317 "Parser/parser.cc"
    break;

  case 579: /* direct_type: type_qualifier_list basic_type_name  */
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13323 "Parser/parser.cc"
    break;

  case 580: /* direct_type: direct_type type_qualifier  */
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13329 "Parser/parser.cc"
    break;

  case 581: /* direct_type: direct_type basic_type_name  */
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 13335 "Parser/parser.cc"
    break;

  case 582: /* indirect_type: TYPEOF '(' type ')'  */
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13341 "Parser/parser.cc"
    break;

  case 583: /* indirect_type: TYPEOF '(' comma_expression ')'  */
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 13347 "Parser/parser.cc"
    break;

  case 584: /* indirect_type: BASETYPEOF '(' type ')'  */
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 13353 "Parser/parser.cc"
    break;

  case 585: /* indirect_type: BASETYPEOF '(' comma_expression ')'  */
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 13359 "Parser/parser.cc"
    break;

  case 586: /* indirect_type: ZERO_T  */
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 13365 "Parser/parser.cc"
    break;

  case 587: /* indirect_type: ONE_T  */
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 13371 "Parser/parser.cc"
    break;

  case 589: /* sue_declaration_specifier: declaration_qualifier_list sue_type_specifier  */
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13377 "Parser/parser.cc"
    break;

  case 590: /* sue_declaration_specifier: sue_declaration_specifier storage_class  */
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13383 "Parser/parser.cc"
    break;

  case 591: /* sue_declaration_specifier: sue_declaration_specifier storage_class type_qualifier_list  */
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13389 "Parser/parser.cc"
    break;

  case 593: /* $@1: %empty  */
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 13395 "Parser/parser.cc"
    break;

  case 594: /* sue_type_specifier: type_qualifier_list $@1 elaborated_type  */
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13401 "Parser/parser.cc"
    break;

  case 595: /* sue_type_specifier: sue_type_specifier type_qualifier  */
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 13410 "Parser/parser.cc"
    break;

  case 597: /* sue_declaration_specifier_nobody: declaration_qualifier_list sue_type_specifier_nobody  */
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13416 "Parser/parser.cc"
    break;

  case 598: /* sue_declaration_specifier_nobody: sue_declaration_specifier_nobody storage_class  */
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13422 "Parser/parser.cc"
    break;

  case 599: /* sue_declaration_specifier_nobody: sue_declaration_specifier_nobody storage_class type_qualifier_list  */
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13428 "Parser/parser.cc"
    break;

  case 601: /* sue_type_specifier_nobody: type_qualifier_list elaborated_type_nobody  */
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13434 "Parser/parser.cc"
    break;

  case 602: /* sue_type_specifier_nobody: sue_type_specifier_nobody type_qualifier  */
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13440 "Parser/parser.cc"
    break;

  case 603: /* type_declaration_specifier: type_type_specifier attribute_list_opt  */
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13446 "Parser/parser.cc"
    break;

  case 604: /* type_declaration_specifier: declaration_qualifier_list type_type_specifier attribute_list_opt  */
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13452 "Parser/parser.cc"
    break;

  case 605: /* type_declaration_specifier: type_declaration_specifier storage_class attribute_list_opt  */
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13458 "Parser/parser.cc"
    break;

  case 606: /* type_declaration_specifier: type_declaration_specifier storage_class type_qualifier_list  */
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13464 "Parser/parser.cc"
    break;

  case 607: /* type_type_specifier: type_name  */
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 13470 "Parser/parser.cc"
    break;

  case 608: /* type_type_specifier: type_qualifier_list type_name  */
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 13476 "Parser/parser.cc"
    break;

  case 609: /* type_type_specifier: type_type_specifier type_qualifier  */
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13482 "Parser/parser.cc"
    break;

  case 610: /* type_name: TYPEDEFname  */
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 13488 "Parser/parser.cc"
    break;

  case 611: /* type_name: '.' TYPEDEFname  */
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 13494 "Parser/parser.cc"
    break;

  case 612: /* type_name: type_name '.' TYPEDEFname  */
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 13500 "Parser/parser.cc"
    break;

  case 614: /* type_name: '.' typegen_name  */
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 13506 "Parser/parser.cc"
    break;

  case 615: /* type_name: type_name '.' typegen_name  */
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 13512 "Parser/parser.cc"
    break;

  case 616: /* typegen_name: TYPEGENname  */
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 13518 "Parser/parser.cc"
    break;

  case 617: /* typegen_name: TYPEGENname '(' ')'  */
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 13524 "Parser/parser.cc"
    break;

  case 618: /* typegen_name: TYPEGENname '(' type_list ')'  */
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13530 "Parser/parser.cc"
    break;

  case 623: /* $@2: %empty  */
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 13536 "Parser/parser.cc"
    break;

  case 624: /* aggregate_type: aggregate_key attribute_list_opt $@2 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), nullptr, (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13542 "Parser/parser.cc"
    break;

  case 625: /* $@3: %empty  */
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 13551 "Parser/parser.cc"
    break;

  case 626: /* aggregate_type: aggregate_key attribute_list_opt identifier attribute_list_opt $@3 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13559 "Parser/parser.cc"
    break;

  case 627: /* $@4: %empty  */
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 13568 "Parser/parser.cc"
    break;

  case 628: /* aggregate_type: aggregate_key attribute_list_opt TYPEDEFname attribute_list_opt $@4 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-7].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13577 "Parser/parser.cc"
    break;

  case 629: /* $@5: %empty  */
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 13586 "Parser/parser.cc"
    break;

  case 630: /* aggregate_type: aggregate_key attribute_list_opt TYPEGENname attribute_list_opt $@5 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-7].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13595 "Parser/parser.cc"
    break;

  case 632: /* type_parameters_opt: %empty  */
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13601 "Parser/parser.cc"
    break;

  case 633: /* type_parameters_opt: '(' type_list ')'  */
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13607 "Parser/parser.cc"
    break;

  case 634: /* aggregate_type_nobody: aggregate_key attribute_list_opt identifier  */
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13617 "Parser/parser.cc"
    break;

  case 635: /* aggregate_type_nobody: aggregate_key attribute_list_opt type_name  */
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13636 "Parser/parser.cc"
    break;

  case 638: /* aggregate_data: STRUCT vtable_opt  */
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 13642 "Parser/parser.cc"
    break;

  case 639: /* aggregate_data: UNION  */
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 13648 "Parser/parser.cc"
    break;

  case 640: /* aggregate_data: EXCEPTION  */
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 13654 "Parser/parser.cc"
    break;

  case 641: /* aggregate_control: MONITOR  */
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13660 "Parser/parser.cc"
    break;

  case 642: /* aggregate_control: MUTEX STRUCT  */
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13666 "Parser/parser.cc"
    break;

  case 643: /* aggregate_control: GENERATOR  */
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 13672 "Parser/parser.cc"
    break;

  case 644: /* aggregate_control: MUTEX GENERATOR  */
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13681 "Parser/parser.cc"
    break;

  case 645: /* aggregate_control: COROUTINE  */
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 13687 "Parser/parser.cc"
    break;

  case 646: /* aggregate_control: MUTEX COROUTINE  */
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13696 "Parser/parser.cc"
    break;

  case 647: /* aggregate_control: THREAD  */
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 13702 "Parser/parser.cc"
    break;

  case 648: /* aggregate_control: MUTEX THREAD  */
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13711 "Parser/parser.cc"
    break;

  case 649: /* field_declaration_list_opt: %empty  */
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13717 "Parser/parser.cc"
    break;

  case 650: /* field_declaration_list_opt: field_declaration_list_opt attribute_list_opt field_declaration  */
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.decl) = (yyvsp[-2].decl) ? (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13723 "Parser/parser.cc"
    break;

  case 651: /* field_declaration: type_specifier field_declaring_list_opt ';'  */
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 13729 "Parser/parser.cc"
    break;

  case 652: /* field_declaration: type_specifier field_declaring_list_opt '}'  */
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13738 "Parser/parser.cc"
    break;

  case 653: /* field_declaration: EXTENSION type_specifier field_declaring_list_opt ';'  */
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13744 "Parser/parser.cc"
    break;

  case 654: /* field_declaration: STATIC type_specifier field_declaring_list_opt ';'  */
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13750 "Parser/parser.cc"
    break;

  case 655: /* field_declaration: INLINE attribute_list_opt type_specifier field_abstract_list_opt ';'  */
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distTypeSpec( (yyvsp[-2].decl), (yyvsp[-1].decl) );				// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13763 "Parser/parser.cc"
    break;

  case 656: /* field_declaration: INLINE attribute_list_opt aggregate_control ';'  */
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13769 "Parser/parser.cc"
    break;

  case 659: /* field_declaration: EXTENSION cfa_field_declaring_list ';'  */
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13775 "Parser/parser.cc"
    break;

  case 660: /* field_declaration: INLINE attribute_list_opt cfa_field_abstract_list ';'  */
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13781 "Parser/parser.cc"
    break;

  case 663: /* field_declaring_list_opt: %empty  */
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13787 "Parser/parser.cc"
    break;

  case 666: /* field_declaring_list: field_declaring_list_opt ',' attribute_list_opt field_declarator  */
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13793 "Parser/parser.cc"
    break;

  case 667: /* field_declarator: bit_subrange_size  */
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13799 "Parser/parser.cc"
    break;

  case 668: /* field_declarator: variable_declarator bit_subrange_size_opt  */
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13805 "Parser/parser.cc"
    break;

  case 669: /* field_declarator: variable_type_redeclarator bit_subrange_size_opt  */
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13811 "Parser/parser.cc"
    break;

  case 670: /* field_declarator: function_type_redeclarator bit_subrange_size_opt  */
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13817 "Parser/parser.cc"
    break;

  case 671: /* field_abstract_list_opt: %empty  */
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13823 "Parser/parser.cc"
    break;

  case 673: /* field_abstract_list_opt: field_abstract_list_opt ',' attribute_list_opt field_abstract  */
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13829 "Parser/parser.cc"
    break;

  case 675: /* cfa_field_declaring_list: cfa_abstract_declarator_tuple identifier_or_type_name  */
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13835 "Parser/parser.cc"
    break;

  case 676: /* cfa_field_declaring_list: cfa_field_declaring_list ',' identifier_or_type_name  */
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13841 "Parser/parser.cc"
    break;

  case 678: /* cfa_field_abstract_list: cfa_field_abstract_list ','  */
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13847 "Parser/parser.cc"
    break;

  case 679: /* bit_subrange_size_opt: %empty  */
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13853 "Parser/parser.cc"
    break;

  case 681: /* bit_subrange_size: ':' assignment_expression  */
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13859 "Parser/parser.cc"
    break;

  case 682: /* enum_type: ENUM attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, false )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13870 "Parser/parser.cc"
    break;

  case 683: /* enum_type: ENUM enumerator_type attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-7].decl) && ((yyvsp[-7].decl)->storageClasses.val != 0 || (yyvsp[-7].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, true, (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13884 "Parser/parser.cc"
    break;

  case 684: /* $@6: %empty  */
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 1" ); }
#line 13890 "Parser/parser.cc"
    break;

  case 685: /* enum_type: ENUM attribute_list_opt identifier attribute_list_opt $@6 hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) ->addQualifiers( (yyvsp[-7].decl) ))->addQualifiers( (yyvsp[0].decl) ); }
#line 13896 "Parser/parser.cc"
    break;

  case 686: /* enum_type: ENUM attribute_list_opt typedef_name attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13902 "Parser/parser.cc"
    break;

  case 687: /* $@7: %empty  */
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13913 "Parser/parser.cc"
    break;

  case 688: /* enum_type: ENUM enumerator_type attribute_list_opt identifier attribute_list_opt $@7 hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, true, (yyvsp[-10].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13919 "Parser/parser.cc"
    break;

  case 689: /* enum_type: ENUM enumerator_type attribute_list_opt typedef_name attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, true, (yyvsp[-9].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13925 "Parser/parser.cc"
    break;

  case 691: /* enumerator_type: '(' ')'  */
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13931 "Parser/parser.cc"
    break;

  case 692: /* enumerator_type: '(' cfa_abstract_parameter_declaration ')'  */
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13937 "Parser/parser.cc"
    break;

  case 693: /* hide_opt: %empty  */
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13943 "Parser/parser.cc"
    break;

  case 694: /* hide_opt: '!'  */
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13949 "Parser/parser.cc"
    break;

  case 695: /* enum_type_nobody: ENUM attribute_list_opt identifier  */
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13958 "Parser/parser.cc"
    break;

  case 696: /* enum_type_nobody: ENUM attribute_list_opt type_name  */
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13967 "Parser/parser.cc"
    break;

  case 697: /* enumerator_list: %empty  */
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13973 "Parser/parser.cc"
    break;

  case 698: /* enumerator_list: visible_hide_opt identifier_or_type_name enumerator_value_opt  */
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13979 "Parser/parser.cc"
    break;

  case 699: /* enumerator_list: INLINE type_name  */
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13989 "Parser/parser.cc"
    break;

  case 700: /* enumerator_list: enumerator_list ',' visible_hide_opt identifier_or_type_name enumerator_value_opt  */
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13995 "Parser/parser.cc"
    break;

  case 701: /* enumerator_list: enumerator_list ',' INLINE type_name  */
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 14001 "Parser/parser.cc"
    break;

  case 703: /* visible_hide_opt: '^'  */
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 14007 "Parser/parser.cc"
    break;

  case 704: /* enumerator_value_opt: %empty  */
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 14013 "Parser/parser.cc"
    break;

  case 705: /* enumerator_value_opt: '=' constant_expression  */
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 14019 "Parser/parser.cc"
    break;

  case 706: /* enumerator_value_opt: '=' '{' initializer_list_opt comma_opt '}'  */
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 14025 "Parser/parser.cc"
    break;

  case 707: /* parameter_list_ellipsis_opt: %empty  */
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 14031 "Parser/parser.cc"
    break;

  case 708: /* parameter_list_ellipsis_opt: ELLIPSIS  */
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14037 "Parser/parser.cc"
    break;

  case 710: /* parameter_list_ellipsis_opt: parameter_list ',' ELLIPSIS  */
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 14043 "Parser/parser.cc"
    break;

  case 712: /* parameter_list: attribute_list parameter_declaration  */
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14049 "Parser/parser.cc"
    break;

  case 714: /* parameter_list: attribute_list abstract_parameter_declaration  */
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14055 "Parser/parser.cc"
    break;

  case 715: /* parameter_list: parameter_list ',' attribute_list_opt parameter_declaration  */
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 14061 "Parser/parser.cc"
    break;

  case 716: /* parameter_list: parameter_list ',' attribute_list_opt abstract_parameter_declaration  */
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 14067 "Parser/parser.cc"
    break;

  case 717: /* cfa_parameter_list_ellipsis_opt: %empty  */
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 14073 "Parser/parser.cc"
    break;

  case 718: /* cfa_parameter_list_ellipsis_opt: ELLIPSIS  */
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14079 "Parser/parser.cc"
    break;

  case 721: /* cfa_parameter_list_ellipsis_opt: cfa_parameter_list ',' cfa_abstract_parameter_list  */
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14085 "Parser/parser.cc"
    break;

  case 722: /* cfa_parameter_list_ellipsis_opt: cfa_parameter_list ',' ELLIPSIS  */
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 14091 "Parser/parser.cc"
    break;

  case 723: /* cfa_parameter_list_ellipsis_opt: cfa_abstract_parameter_list ',' ELLIPSIS  */
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 14097 "Parser/parser.cc"
    break;

  case 725: /* cfa_parameter_list: cfa_abstract_parameter_list ',' cfa_parameter_declaration  */
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14103 "Parser/parser.cc"
    break;

  case 726: /* cfa_parameter_list: cfa_parameter_list ',' cfa_parameter_declaration  */
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14109 "Parser/parser.cc"
    break;

  case 727: /* cfa_parameter_list: cfa_parameter_list ',' cfa_abstract_parameter_list ',' cfa_parameter_declaration  */
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 14115 "Parser/parser.cc"
    break;

  case 729: /* cfa_abstract_parameter_list: cfa_abstract_parameter_list ',' cfa_abstract_parameter_declaration  */
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14121 "Parser/parser.cc"
    break;

  case 730: /* parameter_declaration: declaration_specifier_nobody identifier_parameter_declarator default_initializer_opt  */
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14127 "Parser/parser.cc"
    break;

  case 731: /* parameter_declaration: declaration_specifier_nobody type_parameter_redeclarator default_initializer_opt  */
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14133 "Parser/parser.cc"
    break;

  case 732: /* abstract_parameter_declaration: declaration_specifier_nobody default_initializer_opt  */
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14139 "Parser/parser.cc"
    break;

  case 733: /* abstract_parameter_declaration: declaration_specifier_nobody abstract_parameter_declarator default_initializer_opt  */
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14145 "Parser/parser.cc"
    break;

  case 735: /* cfa_parameter_declaration: cfa_identifier_parameter_declarator_no_tuple identifier_or_type_name default_initializer_opt  */
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 14151 "Parser/parser.cc"
    break;

  case 736: /* cfa_parameter_declaration: cfa_abstract_tuple identifier_or_type_name default_initializer_opt  */
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 14157 "Parser/parser.cc"
    break;

  case 737: /* cfa_parameter_declaration: type_qualifier_list cfa_abstract_tuple identifier_or_type_name default_initializer_opt  */
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14163 "Parser/parser.cc"
    break;

  case 742: /* cfa_abstract_parameter_declaration: type_qualifier_list cfa_abstract_tuple  */
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14169 "Parser/parser.cc"
    break;

  case 744: /* identifier_list: identifier  */
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14175 "Parser/parser.cc"
    break;

  case 745: /* identifier_list: identifier_list ',' identifier  */
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 14181 "Parser/parser.cc"
    break;

  case 747: /* type_no_function: type_specifier abstract_declarator  */
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14187 "Parser/parser.cc"
    break;

  case 750: /* type: attribute_list type_no_function  */
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14193 "Parser/parser.cc"
    break;

  case 752: /* type: attribute_list cfa_abstract_function  */
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14199 "Parser/parser.cc"
    break;

  case 753: /* initializer_opt: %empty  */
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 14205 "Parser/parser.cc"
    break;

  case 754: /* initializer_opt: simple_assignment_operator initializer  */
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 14211 "Parser/parser.cc"
    break;

  case 755: /* initializer_opt: '=' VOID  */
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 14217 "Parser/parser.cc"
    break;

  case 756: /* initializer_opt: '{' initializer_list_opt comma_opt '}'  */
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 14223 "Parser/parser.cc"
    break;

  case 757: /* initializer: assignment_expression  */
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 14229 "Parser/parser.cc"
    break;

  case 758: /* initializer: '{' initializer_list_opt comma_opt '}'  */
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 14235 "Parser/parser.cc"
    break;

  case 759: /* initializer_list_opt: %empty  */
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 14241 "Parser/parser.cc"
    break;

  case 761: /* initializer_list_opt: designation initializer  */
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 14247 "Parser/parser.cc"
    break;

  case 762: /* initializer_list_opt: initializer_list_opt ',' initializer  */
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 14253 "Parser/parser.cc"
    break;

  case 763: /* initializer_list_opt: initializer_list_opt ',' designation initializer  */
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 14259 "Parser/parser.cc"
    break;

  case 765: /* designation: identifier_at ':'  */
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 14265 "Parser/parser.cc"
    break;

  case 767: /* designator_list: designator_list designator  */
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 14271 "Parser/parser.cc"
    break;

  case 768: /* designator: '.' identifier_at  */
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 14277 "Parser/parser.cc"
    break;

  case 769: /* designator: '[' constant_expression ']'  */
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14283 "Parser/parser.cc"
    break;

  case 770: /* designator: '[' subrange ']'  */
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14289 "Parser/parser.cc"
    break;

  case 771: /* designator: '[' constant_expression ELLIPSIS constant_expression ']'  */
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 14295 "Parser/parser.cc"
    break;

  case 772: /* designator: '.' '[' field_name_list ']'  */
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14301 "Parser/parser.cc"
    break;

  case 774: /* type_parameter_list: type_parameter_list ',' type_parameter  */
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14307 "Parser/parser.cc"
    break;

  case 775: /* type_initializer_opt: %empty  */
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14313 "Parser/parser.cc"
    break;

  case 776: /* type_initializer_opt: '=' type  */
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 14319 "Parser/parser.cc"
    break;

  case 777: /* $@8: %empty  */
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" ); }
#line 14325 "Parser/parser.cc"
    break;

  case 778: /* type_parameter: type_class identifier_or_type_name $@8 type_initializer_opt assertion_list_opt  */
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 14331 "Parser/parser.cc"
    break;

  case 779: /* $@9: %empty  */
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 14337 "Parser/parser.cc"
    break;

  case 780: /* type_parameter: identifier_or_type_name new_type_class $@9 type_initializer_opt assertion_list_opt  */
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 14343 "Parser/parser.cc"
    break;

  case 781: /* type_parameter: '[' identifier_or_type_name ']' assertion_list_opt  */
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-2].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-2].tok) )->addAssertions( (yyvsp[0].decl) );
		}
#line 14352 "Parser/parser.cc"
    break;

  case 782: /* type_parameter: assertion_list  */
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( "" ) )->addAssertions( (yyvsp[0].decl) ); }
#line 14358 "Parser/parser.cc"
    break;

  case 783: /* type_parameter: ENUM '(' identifier_or_type_name ')' identifier_or_type_name new_type_class type_initializer_opt assertion_list_opt  */
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 14368 "Parser/parser.cc"
    break;

  case 784: /* new_type_class: %empty  */
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 14374 "Parser/parser.cc"
    break;

  case 785: /* new_type_class: '&'  */
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 14380 "Parser/parser.cc"
    break;

  case 786: /* new_type_class: '*'  */
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 14386 "Parser/parser.cc"
    break;

  case 787: /* new_type_class: ELLIPSIS  */
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 14392 "Parser/parser.cc"
    break;

  case 788: /* type_class: OTYPE  */
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
#line 14398 "Parser/parser.cc"
    break;

  case 789: /* type_class: DTYPE  */
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
#line 14404 "Parser/parser.cc"
    break;

  case 790: /* type_class: FTYPE  */
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 14410 "Parser/parser.cc"
    break;

  case 791: /* type_class: TTYPE  */
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
#line 14416 "Parser/parser.cc"
    break;

  case 792: /* assertion_list_opt: %empty  */
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14422 "Parser/parser.cc"
    break;

  case 795: /* assertion_list: assertion_list assertion  */
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 14428 "Parser/parser.cc"
    break;

  case 796: /* assertion: '|' identifier_or_type_name '(' type_list ')'  */
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14434 "Parser/parser.cc"
    break;

  case 797: /* assertion: '|' '{' trait_declaration_list '}'  */
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14440 "Parser/parser.cc"
    break;

  case 798: /* type_list: type  */
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14446 "Parser/parser.cc"
    break;

  case 800: /* type_list: type_list ',' type  */
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14452 "Parser/parser.cc"
    break;

  case 801: /* type_list: type_list ',' assignment_expression  */
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 14458 "Parser/parser.cc"
    break;

  case 802: /* type_declaring_list: OTYPE type_declarator  */
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 14464 "Parser/parser.cc"
    break;

  case 803: /* type_declaring_list: storage_class_list OTYPE type_declarator  */
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14470 "Parser/parser.cc"
    break;

  case 804: /* type_declaring_list: type_declaring_list ',' type_declarator  */
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 14476 "Parser/parser.cc"
    break;

  case 805: /* type_declarator: type_declarator_name assertion_list_opt  */
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 14482 "Parser/parser.cc"
    break;

  case 806: /* type_declarator: type_declarator_name assertion_list_opt '=' type  */
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 14488 "Parser/parser.cc"
    break;

  case 807: /* type_declarator_name: identifier_or_type_name  */
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 14497 "Parser/parser.cc"
    break;

  case 808: /* type_declarator_name: identifier_or_type_name '(' type_parameter_list ')'  */
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 14506 "Parser/parser.cc"
    break;

  case 809: /* trait_specifier: TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' '}'  */
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 14515 "Parser/parser.cc"
    break;

  case 810: /* trait_specifier: forall TRAIT identifier_or_type_name '{' '}'  */
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 14521 "Parser/parser.cc"
    break;

  case 811: /* trait_specifier: TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' trait_declaration_list '}'  */
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 14530 "Parser/parser.cc"
    break;

  case 812: /* trait_specifier: forall TRAIT identifier_or_type_name '{' trait_declaration_list '}'  */
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 14536 "Parser/parser.cc"
    break;

  case 814: /* trait_declaration_list: trait_declaration_list trait_declaration  */
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 14542 "Parser/parser.cc"
    break;

  case 819: /* cfa_trait_declaring_list: cfa_trait_declaring_list ',' identifier_or_type_name  */
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 14548 "Parser/parser.cc"
    break;

  case 820: /* trait_declaring_list: type_specifier_nobody declarator  */
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14554 "Parser/parser.cc"
    break;

  case 821: /* trait_declaring_list: trait_declaring_list ',' declarator  */
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 14560 "Parser/parser.cc"
    break;

  case 822: /* trait_declaring_list: error  */
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 14566 "Parser/parser.cc"
    break;

  case 824: /* translation_unit: external_definition_list  */
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 14572 "Parser/parser.cc"
    break;

  case 825: /* external_definition_list_opt: %empty  */
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14578 "Parser/parser.cc"
    break;

  case 827: /* external_definition_list: attribute_list_opt push external_definition pop  */
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 14584 "Parser/parser.cc"
    break;

  case 828: /* external_definition_list: external_definition_list attribute_list_opt push external_definition pop  */
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-4].decl) ? (yyvsp[-4].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl)->addQualifiers( (yyvsp[-3].decl) ); }
#line 14590 "Parser/parser.cc"
    break;

  case 829: /* up: %empty  */
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 14596 "Parser/parser.cc"
    break;

  case 830: /* down: %empty  */
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 14602 "Parser/parser.cc"
    break;

  case 831: /* external_definition: DIRECTIVE  */
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 14608 "Parser/parser.cc"
    break;

  case 832: /* external_definition: declaration  */
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 14624 "Parser/parser.cc"
    break;

  case 833: /* external_definition: IDENTIFIER IDENTIFIER  */
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 14630 "Parser/parser.cc"
    break;

  case 834: /* external_definition: IDENTIFIER type_qualifier  */
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 14636 "Parser/parser.cc"
    break;

  case 835: /* external_definition: IDENTIFIER storage_class  */
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 14642 "Parser/parser.cc"
    break;

  case 836: /* external_definition: IDENTIFIER basic_type_name  */
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14648 "Parser/parser.cc"
    break;

  case 837: /* external_definition: IDENTIFIER TYPEDEFname  */
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14654 "Parser/parser.cc"
    break;

  case 838: /* external_definition: IDENTIFIER TYPEGENname  */
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14660 "Parser/parser.cc"
    break;

  case 840: /* external_definition: EXTENSION external_definition  */
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 14669 "Parser/parser.cc"
    break;

  case 841: /* external_definition: ASM '(' string_literal ')' ';'  */
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 14675 "Parser/parser.cc"
    break;

  case 842: /* $@10: %empty  */
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14684 "Parser/parser.cc"
    break;

  case 843: /* external_definition: EXTERN STRINGliteral $@10 up external_definition down  */
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 14694 "Parser/parser.cc"
    break;

  case 844: /* $@11: %empty  */
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14703 "Parser/parser.cc"
    break;

  case 845: /* external_definition: EXTERN STRINGliteral $@11 '{' up external_definition_list_opt down '}'  */
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14713 "Parser/parser.cc"
    break;

  case 846: /* $@12: %empty  */
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14724 "Parser/parser.cc"
    break;

  case 847: /* external_definition: type_qualifier_list $@12 '{' up external_definition_list_opt down '}'  */
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14734 "Parser/parser.cc"
    break;

  case 848: /* $@13: %empty  */
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14745 "Parser/parser.cc"
    break;

  case 849: /* external_definition: declaration_qualifier_list $@13 '{' up external_definition_list_opt down '}'  */
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14755 "Parser/parser.cc"
    break;

  case 850: /* $@14: %empty  */
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14766 "Parser/parser.cc"
    break;

  case 851: /* external_definition: declaration_qualifier_list type_qualifier_list $@14 '{' up external_definition_list_opt down '}'  */
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14776 "Parser/parser.cc"
    break;

  case 852: /* external_definition: ';'  */
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14782 "Parser/parser.cc"
    break;

  case 854: /* external_function_definition: function_declarator compound_statement  */
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14788 "Parser/parser.cc"
    break;

  case 855: /* external_function_definition: KR_function_declarator KR_parameter_list_opt compound_statement  */
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14794 "Parser/parser.cc"
    break;

  case 856: /* with_clause_opt: %empty  */
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14800 "Parser/parser.cc"
    break;

  case 857: /* with_clause_opt: WITH '(' type_list ')' attribute_list_opt  */
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14812 "Parser/parser.cc"
    break;

  case 858: /* function_definition: cfa_function_declaration with_clause_opt compound_statement  */
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14823 "Parser/parser.cc"
    break;

  case 859: /* function_definition: declaration_specifier function_declarator with_clause_opt compound_statement  */
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14832 "Parser/parser.cc"
    break;

  case 860: /* function_definition: declaration_specifier function_type_redeclarator with_clause_opt compound_statement  */
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14841 "Parser/parser.cc"
    break;

  case 861: /* function_definition: type_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14847 "Parser/parser.cc"
    break;

  case 862: /* function_definition: declaration_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14853 "Parser/parser.cc"
    break;

  case 863: /* function_definition: declaration_qualifier_list type_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14859 "Parser/parser.cc"
    break;

  case 864: /* function_definition: declaration_specifier KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14868 "Parser/parser.cc"
    break;

  case 865: /* function_definition: type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14874 "Parser/parser.cc"
    break;

  case 866: /* function_definition: declaration_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14880 "Parser/parser.cc"
    break;

  case 867: /* function_definition: declaration_qualifier_list type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14886 "Parser/parser.cc"
    break;

  case 872: /* subrange: constant_expression '~' constant_expression  */
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14892 "Parser/parser.cc"
    break;

  case 873: /* asm_name_opt: %empty  */
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14898 "Parser/parser.cc"
    break;

  case 874: /* asm_name_opt: ASM '(' string_literal ')' attribute_list_opt  */
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14908 "Parser/parser.cc"
    break;

  case 875: /* attribute_list_opt: %empty  */
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14914 "Parser/parser.cc"
    break;

  case 878: /* attribute_list: attribute_list attribute  */
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14920 "Parser/parser.cc"
    break;

  case 879: /* attribute: ATTRIBUTE '(' '(' attribute_name_list ')' ')'  */
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14926 "Parser/parser.cc"
    break;

  case 880: /* attribute: ATTRIBUTE '(' attribute_name_list ')'  */
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14932 "Parser/parser.cc"
    break;

  case 881: /* attribute: ATTR attribute_name_list ']'  */
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14938 "Parser/parser.cc"
    break;

  case 882: /* attribute: C23_ATTRIBUTE  */
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14944 "Parser/parser.cc"
    break;

  case 884: /* attribute_name_list: attribute_name_list ',' attribute_name  */
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14950 "Parser/parser.cc"
    break;

  case 885: /* attribute_name: %empty  */
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14956 "Parser/parser.cc"
    break;

  case 886: /* attribute_name: attr_name  */
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14962 "Parser/parser.cc"
    break;

  case 887: /* attribute_name: attr_name '(' argument_expression_list_opt ')'  */
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14968 "Parser/parser.cc"
    break;

  case 889: /* attr_name: FALLTHROUGH  */
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14974 "Parser/parser.cc"
    break;

  case 890: /* attr_name: CONST  */
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14980 "Parser/parser.cc"
    break;

  case 891: /* paren_identifier: identifier_at  */
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14986 "Parser/parser.cc"
    break;

  case 892: /* paren_identifier: '?' identifier  */
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14992 "Parser/parser.cc"
    break;

  case 893: /* paren_identifier: '(' paren_identifier ')'  */
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14998 "Parser/parser.cc"
    break;

  case 894: /* variable_declarator: paren_identifier attribute_list_opt  */
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15004 "Parser/parser.cc"
    break;

  case 896: /* variable_declarator: variable_array attribute_list_opt  */
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15010 "Parser/parser.cc"
    break;

  case 897: /* variable_declarator: variable_function attribute_list_opt  */
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15016 "Parser/parser.cc"
    break;

  case 898: /* variable_ptr: ptrref_operator variable_declarator  */
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15022 "Parser/parser.cc"
    break;

  case 899: /* variable_ptr: ptrref_operator attribute_list variable_declarator  */
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15028 "Parser/parser.cc"
    break;

  case 900: /* variable_ptr: ptrref_operator type_qualifier_list variable_declarator  */
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15034 "Parser/parser.cc"
    break;

  case 901: /* variable_ptr: '(' variable_ptr ')' attribute_list_opt  */
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15040 "Parser/parser.cc"
    break;

  case 902: /* variable_ptr: '(' attribute_list variable_ptr ')' attribute_list_opt  */
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15046 "Parser/parser.cc"
    break;

  case 903: /* variable_array: paren_identifier array_dimension  */
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15052 "Parser/parser.cc"
    break;

  case 904: /* variable_array: '(' variable_ptr ')' array_dimension  */
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15058 "Parser/parser.cc"
    break;

  case 905: /* variable_array: '(' attribute_list variable_ptr ')' array_dimension  */
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15064 "Parser/parser.cc"
    break;

  case 906: /* variable_array: '(' variable_array ')' multi_array_dimension  */
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15070 "Parser/parser.cc"
    break;

  case 907: /* variable_array: '(' attribute_list variable_array ')' multi_array_dimension  */
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15076 "Parser/parser.cc"
    break;

  case 908: /* variable_array: '(' variable_array ')'  */
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15082 "Parser/parser.cc"
    break;

  case 909: /* variable_array: '(' attribute_list variable_array ')'  */
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15088 "Parser/parser.cc"
    break;

  case 910: /* variable_function: '(' variable_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15094 "Parser/parser.cc"
    break;

  case 911: /* variable_function: '(' attribute_list variable_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15100 "Parser/parser.cc"
    break;

  case 912: /* variable_function: '(' variable_function ')'  */
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15106 "Parser/parser.cc"
    break;

  case 913: /* variable_function: '(' attribute_list variable_function ')'  */
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15112 "Parser/parser.cc"
    break;

  case 914: /* function_declarator: function_no_ptr attribute_list_opt  */
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15118 "Parser/parser.cc"
    break;

  case 916: /* function_declarator: function_array attribute_list_opt  */
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15124 "Parser/parser.cc"
    break;

  case 917: /* function_no_ptr: paren_identifier '(' parameter_list_ellipsis_opt ')'  */
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15130 "Parser/parser.cc"
    break;

  case 918: /* function_no_ptr: '(' function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15136 "Parser/parser.cc"
    break;

  case 919: /* function_no_ptr: '(' attribute_list function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15142 "Parser/parser.cc"
    break;

  case 920: /* function_no_ptr: '(' function_no_ptr ')'  */
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15148 "Parser/parser.cc"
    break;

  case 921: /* function_no_ptr: '(' attribute_list function_no_ptr ')'  */
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15154 "Parser/parser.cc"
    break;

  case 922: /* function_ptr: ptrref_operator function_declarator  */
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15160 "Parser/parser.cc"
    break;

  case 923: /* function_ptr: ptrref_operator attribute_list function_declarator  */
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15166 "Parser/parser.cc"
    break;

  case 924: /* function_ptr: ptrref_operator type_qualifier_list function_declarator  */
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15172 "Parser/parser.cc"
    break;

  case 925: /* function_ptr: '(' function_ptr ')' attribute_list_opt  */
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15178 "Parser/parser.cc"
    break;

  case 926: /* function_ptr: '(' attribute_list function_ptr ')' attribute_list_opt  */
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15184 "Parser/parser.cc"
    break;

  case 927: /* function_array: '(' function_ptr ')' array_dimension  */
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15190 "Parser/parser.cc"
    break;

  case 928: /* function_array: '(' attribute_list function_ptr ')' array_dimension  */
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15196 "Parser/parser.cc"
    break;

  case 929: /* function_array: '(' function_array ')' multi_array_dimension  */
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15202 "Parser/parser.cc"
    break;

  case 930: /* function_array: '(' attribute_list function_array ')' multi_array_dimension  */
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15208 "Parser/parser.cc"
    break;

  case 931: /* function_array: '(' function_array ')'  */
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15214 "Parser/parser.cc"
    break;

  case 932: /* function_array: '(' attribute_list function_array ')'  */
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15220 "Parser/parser.cc"
    break;

  case 936: /* KR_function_no_ptr: paren_identifier '(' identifier_list ')'  */
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 15226 "Parser/parser.cc"
    break;

  case 937: /* KR_function_no_ptr: '(' KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15232 "Parser/parser.cc"
    break;

  case 938: /* KR_function_no_ptr: '(' attribute_list KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15238 "Parser/parser.cc"
    break;

  case 939: /* KR_function_no_ptr: '(' KR_function_no_ptr ')'  */
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15244 "Parser/parser.cc"
    break;

  case 940: /* KR_function_no_ptr: '(' attribute_list KR_function_no_ptr ')'  */
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15250 "Parser/parser.cc"
    break;

  case 941: /* KR_function_ptr: ptrref_operator KR_function_declarator  */
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15256 "Parser/parser.cc"
    break;

  case 942: /* KR_function_ptr: ptrref_operator attribute_list KR_function_declarator  */
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15262 "Parser/parser.cc"
    break;

  case 943: /* KR_function_ptr: ptrref_operator type_qualifier_list KR_function_declarator  */
#line 3729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15268 "Parser/parser.cc"
    break;

  case 944: /* KR_function_ptr: '(' KR_function_ptr ')'  */
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15274 "Parser/parser.cc"
    break;

  case 945: /* KR_function_ptr: '(' attribute_list KR_function_ptr ')'  */
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15280 "Parser/parser.cc"
    break;

  case 946: /* KR_function_array: '(' KR_function_ptr ')' array_dimension  */
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15286 "Parser/parser.cc"
    break;

  case 947: /* KR_function_array: '(' attribute_list KR_function_ptr ')' array_dimension  */
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15292 "Parser/parser.cc"
    break;

  case 948: /* KR_function_array: '(' KR_function_array ')' multi_array_dimension  */
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15298 "Parser/parser.cc"
    break;

  case 949: /* KR_function_array: '(' attribute_list KR_function_array ')' multi_array_dimension  */
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15304 "Parser/parser.cc"
    break;

  case 950: /* KR_function_array: '(' KR_function_array ')'  */
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15310 "Parser/parser.cc"
    break;

  case 951: /* KR_function_array: '(' attribute_list KR_function_array ')'  */
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15316 "Parser/parser.cc"
    break;

  case 952: /* paren_type: typedef_name  */
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 15325 "Parser/parser.cc"
    break;

  case 953: /* paren_type: '(' paren_type ')'  */
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15331 "Parser/parser.cc"
    break;

  case 954: /* variable_type_redeclarator: paren_type attribute_list_opt  */
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15337 "Parser/parser.cc"
    break;

  case 956: /* variable_type_redeclarator: variable_type_array attribute_list_opt  */
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15343 "Parser/parser.cc"
    break;

  case 957: /* variable_type_redeclarator: variable_type_function attribute_list_opt  */
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15349 "Parser/parser.cc"
    break;

  case 958: /* variable_type_ptr: ptrref_operator variable_type_redeclarator  */
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15355 "Parser/parser.cc"
    break;

  case 959: /* variable_type_ptr: ptrref_operator attribute_list variable_type_redeclarator  */
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15361 "Parser/parser.cc"
    break;

  case 960: /* variable_type_ptr: ptrref_operator type_qualifier_list variable_type_redeclarator  */
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15367 "Parser/parser.cc"
    break;

  case 961: /* variable_type_ptr: '(' variable_type_ptr ')' attribute_list_opt  */
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15373 "Parser/parser.cc"
    break;

  case 962: /* variable_type_ptr: '(' attribute_list variable_type_ptr ')' attribute_list_opt  */
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15379 "Parser/parser.cc"
    break;

  case 963: /* variable_type_array: paren_type array_dimension  */
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15385 "Parser/parser.cc"
    break;

  case 964: /* variable_type_array: '(' variable_type_ptr ')' array_dimension  */
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15391 "Parser/parser.cc"
    break;

  case 965: /* variable_type_array: '(' attribute_list variable_type_ptr ')' array_dimension  */
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15397 "Parser/parser.cc"
    break;

  case 966: /* variable_type_array: '(' variable_type_array ')' multi_array_dimension  */
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15403 "Parser/parser.cc"
    break;

  case 967: /* variable_type_array: '(' attribute_list variable_type_array ')' multi_array_dimension  */
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15409 "Parser/parser.cc"
    break;

  case 968: /* variable_type_array: '(' variable_type_array ')'  */
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15415 "Parser/parser.cc"
    break;

  case 969: /* variable_type_array: '(' attribute_list variable_type_array ')'  */
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15421 "Parser/parser.cc"
    break;

  case 970: /* variable_type_function: '(' variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15427 "Parser/parser.cc"
    break;

  case 971: /* variable_type_function: '(' attribute_list variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15433 "Parser/parser.cc"
    break;

  case 972: /* variable_type_function: '(' variable_type_function ')'  */
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15439 "Parser/parser.cc"
    break;

  case 973: /* variable_type_function: '(' attribute_list variable_type_function ')'  */
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15445 "Parser/parser.cc"
    break;

  case 974: /* function_type_redeclarator: function_type_no_ptr attribute_list_opt  */
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15451 "Parser/parser.cc"
    break;

  case 976: /* function_type_redeclarator: function_type_array attribute_list_opt  */
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15457 "Parser/parser.cc"
    break;

  case 977: /* function_type_no_ptr: paren_type '(' parameter_list_ellipsis_opt ')'  */
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15463 "Parser/parser.cc"
    break;

  case 978: /* function_type_no_ptr: '(' function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15469 "Parser/parser.cc"
    break;

  case 979: /* function_type_no_ptr: '(' attribute_list function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15475 "Parser/parser.cc"
    break;

  case 980: /* function_type_no_ptr: '(' function_type_no_ptr ')'  */
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15481 "Parser/parser.cc"
    break;

  case 981: /* function_type_no_ptr: '(' attribute_list function_type_no_ptr ')'  */
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15487 "Parser/parser.cc"
    break;

  case 982: /* function_type_ptr: ptrref_operator function_type_redeclarator  */
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15493 "Parser/parser.cc"
    break;

  case 983: /* function_type_ptr: ptrref_operator attribute_list function_type_redeclarator  */
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15499 "Parser/parser.cc"
    break;

  case 984: /* function_type_ptr: ptrref_operator type_qualifier_list function_type_redeclarator  */
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15505 "Parser/parser.cc"
    break;

  case 985: /* function_type_ptr: '(' function_type_ptr ')' attribute_list_opt  */
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15511 "Parser/parser.cc"
    break;

  case 986: /* function_type_ptr: '(' attribute_list function_type_ptr ')' attribute_list_opt  */
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15517 "Parser/parser.cc"
    break;

  case 987: /* function_type_array: '(' function_type_ptr ')' array_dimension  */
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15523 "Parser/parser.cc"
    break;

  case 988: /* function_type_array: '(' attribute_list function_type_ptr ')' array_dimension  */
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15529 "Parser/parser.cc"
    break;

  case 989: /* function_type_array: '(' function_type_array ')' multi_array_dimension  */
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15535 "Parser/parser.cc"
    break;

  case 990: /* function_type_array: '(' attribute_list function_type_array ')' multi_array_dimension  */
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15541 "Parser/parser.cc"
    break;

  case 991: /* function_type_array: '(' function_type_array ')'  */
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15547 "Parser/parser.cc"
    break;

  case 992: /* function_type_array: '(' attribute_list function_type_array ')'  */
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15553 "Parser/parser.cc"
    break;

  case 993: /* identifier_parameter_declarator: paren_identifier attribute_list_opt  */
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15559 "Parser/parser.cc"
    break;

  case 994: /* identifier_parameter_declarator: '&' MUTEX paren_identifier attribute_list_opt  */
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15566 "Parser/parser.cc"
    break;

  case 996: /* identifier_parameter_declarator: identifier_parameter_array attribute_list_opt  */
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15572 "Parser/parser.cc"
    break;

  case 997: /* identifier_parameter_declarator: identifier_parameter_function attribute_list_opt  */
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15578 "Parser/parser.cc"
    break;

  case 998: /* identifier_parameter_ptr: ptrref_operator identifier_parameter_declarator  */
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15584 "Parser/parser.cc"
    break;

  case 999: /* identifier_parameter_ptr: ptrref_operator attribute_list identifier_parameter_declarator  */
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15590 "Parser/parser.cc"
    break;

  case 1000: /* identifier_parameter_ptr: ptrref_operator type_qualifier_list identifier_parameter_declarator  */
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15596 "Parser/parser.cc"
    break;

  case 1001: /* identifier_parameter_ptr: '(' identifier_parameter_ptr ')' attribute_list_opt  */
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15602 "Parser/parser.cc"
    break;

  case 1002: /* identifier_parameter_array: paren_identifier array_parameter_dimension  */
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15608 "Parser/parser.cc"
    break;

  case 1003: /* identifier_parameter_array: '(' identifier_parameter_ptr ')' array_dimension  */
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15614 "Parser/parser.cc"
    break;

  case 1004: /* identifier_parameter_array: '(' identifier_parameter_array ')' multi_array_dimension  */
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15620 "Parser/parser.cc"
    break;

  case 1005: /* identifier_parameter_array: '(' identifier_parameter_array ')'  */
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15626 "Parser/parser.cc"
    break;

  case 1006: /* identifier_parameter_function: paren_identifier '(' parameter_list_ellipsis_opt ')'  */
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15632 "Parser/parser.cc"
    break;

  case 1007: /* identifier_parameter_function: '(' identifier_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15638 "Parser/parser.cc"
    break;

  case 1008: /* identifier_parameter_function: '(' identifier_parameter_function ')'  */
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15644 "Parser/parser.cc"
    break;

  case 1009: /* type_parameter_redeclarator: typedef_name attribute_list_opt  */
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15650 "Parser/parser.cc"
    break;

  case 1010: /* type_parameter_redeclarator: '&' MUTEX typedef_name attribute_list_opt  */
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15657 "Parser/parser.cc"
    break;

  case 1012: /* type_parameter_redeclarator: type_parameter_array attribute_list_opt  */
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15663 "Parser/parser.cc"
    break;

  case 1013: /* type_parameter_redeclarator: type_parameter_function attribute_list_opt  */
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15669 "Parser/parser.cc"
    break;

  case 1014: /* typedef_name: TYPEDEFname  */
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15675 "Parser/parser.cc"
    break;

  case 1015: /* typedef_name: TYPEGENname  */
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15681 "Parser/parser.cc"
    break;

  case 1016: /* type_parameter_ptr: ptrref_operator type_parameter_redeclarator  */
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15687 "Parser/parser.cc"
    break;

  case 1017: /* type_parameter_ptr: ptrref_operator attribute_list type_parameter_redeclarator  */
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15693 "Parser/parser.cc"
    break;

  case 1018: /* type_parameter_ptr: ptrref_operator type_qualifier_list type_parameter_redeclarator  */
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15699 "Parser/parser.cc"
    break;

  case 1019: /* type_parameter_ptr: '(' type_parameter_ptr ')' attribute_list_opt  */
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15705 "Parser/parser.cc"
    break;

  case 1020: /* type_parameter_array: typedef_name array_parameter_dimension  */
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15711 "Parser/parser.cc"
    break;

  case 1021: /* type_parameter_array: '(' type_parameter_ptr ')' array_parameter_dimension  */
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15717 "Parser/parser.cc"
    break;

  case 1022: /* type_parameter_function: typedef_name '(' parameter_list_ellipsis_opt ')'  */
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15723 "Parser/parser.cc"
    break;

  case 1023: /* type_parameter_function: '(' type_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15729 "Parser/parser.cc"
    break;

  case 1025: /* abstract_declarator: abstract_array attribute_list_opt  */
#line 3990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15735 "Parser/parser.cc"
    break;

  case 1026: /* abstract_declarator: abstract_function attribute_list_opt  */
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15741 "Parser/parser.cc"
    break;

  case 1027: /* abstract_ptr: ptrref_operator attribute_list_opt  */
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15747 "Parser/parser.cc"
    break;

  case 1028: /* abstract_ptr: ptrref_operator type_qualifier_list  */
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15753 "Parser/parser.cc"
    break;

  case 1029: /* abstract_ptr: ptrref_operator abstract_declarator  */
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15759 "Parser/parser.cc"
    break;

  case 1030: /* abstract_ptr: ptrref_operator attribute_list abstract_declarator  */
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) )->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 15765 "Parser/parser.cc"
    break;

  case 1031: /* abstract_ptr: ptrref_operator type_qualifier_list abstract_declarator  */
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15771 "Parser/parser.cc"
    break;

  case 1032: /* abstract_ptr: '(' abstract_ptr ')' attribute_list_opt  */
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15777 "Parser/parser.cc"
    break;

  case 1034: /* abstract_array: '(' abstract_ptr ')' array_dimension  */
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15783 "Parser/parser.cc"
    break;

  case 1035: /* abstract_array: '(' abstract_array ')' multi_array_dimension  */
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15789 "Parser/parser.cc"
    break;

  case 1036: /* abstract_array: '(' abstract_array ')'  */
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15795 "Parser/parser.cc"
    break;

  case 1037: /* abstract_function: '(' parameter_list_ellipsis_opt ')'  */
#line 4022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15801 "Parser/parser.cc"
    break;

  case 1038: /* abstract_function: '(' abstract_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15807 "Parser/parser.cc"
    break;

  case 1039: /* abstract_function: '(' abstract_function ')'  */
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15813 "Parser/parser.cc"
    break;

  case 1040: /* array_dimension: '[' ']'  */
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15819 "Parser/parser.cc"
    break;

  case 1041: /* array_dimension: '[' ']' multi_array_dimension  */
#line 4034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15825 "Parser/parser.cc"
    break;

  case 1042: /* array_dimension: '[' assignment_expression ',' ']'  */
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15831 "Parser/parser.cc"
    break;

  case 1043: /* array_dimension: '[' assignment_expression ',' comma_expression ']'  */
#line 4040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15837 "Parser/parser.cc"
    break;

  case 1044: /* array_dimension: '[' array_type_list ']'  */
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15843 "Parser/parser.cc"
    break;

  case 1046: /* array_type_list: basic_type_name  */
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15849 "Parser/parser.cc"
    break;

  case 1047: /* array_type_list: type_name  */
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15855 "Parser/parser.cc"
    break;

  case 1049: /* array_type_list: array_type_list ',' basic_type_name  */
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15861 "Parser/parser.cc"
    break;

  case 1050: /* array_type_list: array_type_list ',' type_name  */
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15867 "Parser/parser.cc"
    break;

  case 1052: /* upupeq: '~'  */
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15873 "Parser/parser.cc"
    break;

  case 1053: /* upupeq: ErangeUpLe  */
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15879 "Parser/parser.cc"
    break;

  case 1054: /* multi_array_dimension: '[' assignment_expression ']'  */
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15885 "Parser/parser.cc"
    break;

  case 1055: /* multi_array_dimension: '[' '*' ']'  */
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15891 "Parser/parser.cc"
    break;

  case 1056: /* multi_array_dimension: multi_array_dimension '[' assignment_expression ']'  */
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15897 "Parser/parser.cc"
    break;

  case 1057: /* multi_array_dimension: multi_array_dimension '[' '*' ']'  */
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15903 "Parser/parser.cc"
    break;

  case 1058: /* abstract_parameter_declarator_opt: %empty  */
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15909 "Parser/parser.cc"
    break;

  case 1061: /* abstract_parameter_declarator: '&' MUTEX attribute_list_opt  */
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15916 "Parser/parser.cc"
    break;

  case 1062: /* abstract_parameter_declarator: abstract_parameter_array attribute_list_opt  */
#line 4128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15922 "Parser/parser.cc"
    break;

  case 1063: /* abstract_parameter_declarator: abstract_parameter_function attribute_list_opt  */
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15928 "Parser/parser.cc"
    break;

  case 1064: /* abstract_parameter_ptr: ptrref_operator attribute_list_opt  */
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15934 "Parser/parser.cc"
    break;

  case 1065: /* abstract_parameter_ptr: ptrref_operator type_qualifier_list  */
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15940 "Parser/parser.cc"
    break;

  case 1066: /* abstract_parameter_ptr: ptrref_operator abstract_parameter_declarator  */
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15946 "Parser/parser.cc"
    break;

  case 1067: /* abstract_parameter_ptr: ptrref_operator type_qualifier_list abstract_parameter_declarator  */
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15952 "Parser/parser.cc"
    break;

  case 1068: /* abstract_parameter_ptr: '(' abstract_parameter_ptr ')' attribute_list_opt  */
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15958 "Parser/parser.cc"
    break;

  case 1070: /* abstract_parameter_array: '(' abstract_parameter_ptr ')' array_parameter_dimension  */
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15964 "Parser/parser.cc"
    break;

  case 1071: /* abstract_parameter_array: '(' abstract_parameter_array ')' multi_array_dimension  */
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15970 "Parser/parser.cc"
    break;

  case 1072: /* abstract_parameter_array: '(' abstract_parameter_array ')'  */
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15976 "Parser/parser.cc"
    break;

  case 1073: /* abstract_parameter_function: '(' parameter_list_ellipsis_opt ')'  */
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15982 "Parser/parser.cc"
    break;

  case 1074: /* abstract_parameter_function: '(' abstract_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15988 "Parser/parser.cc"
    break;

  case 1075: /* abstract_parameter_function: '(' abstract_parameter_function ')'  */
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15994 "Parser/parser.cc"
    break;

  case 1077: /* array_parameter_dimension: array_parameter_1st_dimension multi_array_dimension  */
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 16000 "Parser/parser.cc"
    break;

  case 1079: /* array_parameter_1st_dimension: '[' ']'  */
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 16006 "Parser/parser.cc"
    break;

  case 1080: /* array_parameter_1st_dimension: '[' push type_qualifier_list '*' pop ']'  */
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 16012 "Parser/parser.cc"
    break;

  case 1081: /* array_parameter_1st_dimension: '[' push type_qualifier_list pop ']'  */
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 16018 "Parser/parser.cc"
    break;

  case 1082: /* array_parameter_1st_dimension: '[' push type_qualifier_list assignment_expression pop ']'  */
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 16024 "Parser/parser.cc"
    break;

  case 1083: /* array_parameter_1st_dimension: '[' push STATIC type_qualifier_list_opt assignment_expression pop ']'  */
#line 4190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 16030 "Parser/parser.cc"
    break;

  case 1084: /* array_parameter_1st_dimension: '[' push type_qualifier_list STATIC assignment_expression pop ']'  */
#line 4192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 16036 "Parser/parser.cc"
    break;

  case 1086: /* variable_abstract_declarator: variable_abstract_array attribute_list_opt  */
#line 4207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 16042 "Parser/parser.cc"
    break;

  case 1087: /* variable_abstract_declarator: variable_abstract_function attribute_list_opt  */
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 16048 "Parser/parser.cc"
    break;

  case 1088: /* variable_abstract_ptr: ptrref_operator attribute_list_opt  */
#line 4214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 16054 "Parser/parser.cc"
    break;

  case 1089: /* variable_abstract_ptr: ptrref_operator type_qualifier_list  */
#line 4216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 16060 "Parser/parser.cc"
    break;

  case 1090: /* variable_abstract_ptr: ptrref_operator variable_abstract_declarator  */
#line 4218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16066 "Parser/parser.cc"
    break;

  case 1091: /* variable_abstract_ptr: ptrref_operator type_qualifier_list variable_abstract_declarator  */
#line 4220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 16072 "Parser/parser.cc"
    break;

  case 1092: /* variable_abstract_ptr: '(' variable_abstract_ptr ')' attribute_list_opt  */
#line 4222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 16078 "Parser/parser.cc"
    break;

  case 1094: /* variable_abstract_array: '(' variable_abstract_ptr ')' array_dimension  */
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 16084 "Parser/parser.cc"
    break;

  case 1095: /* variable_abstract_array: '(' variable_abstract_array ')' multi_array_dimension  */
#line 4230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 16090 "Parser/parser.cc"
    break;

  case 1096: /* variable_abstract_array: '(' variable_abstract_array ')'  */
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 16096 "Parser/parser.cc"
    break;

  case 1097: /* variable_abstract_function: '(' variable_abstract_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 16102 "Parser/parser.cc"
    break;

  case 1098: /* variable_abstract_function: '(' variable_abstract_function ')'  */
#line 4239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 16108 "Parser/parser.cc"
    break;

  case 1101: /* cfa_identifier_parameter_declarator_tuple: type_qualifier_list cfa_abstract_tuple  */
#line 4249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 16114 "Parser/parser.cc"
    break;

  case 1104: /* cfa_identifier_parameter_declarator_no_tuple: type_qualifier_list cfa_identifier_parameter_array  */
#line 4256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 16120 "Parser/parser.cc"
    break;

  case 1105: /* cfa_identifier_parameter_ptr: ptrref_operator type_specifier_nobody  */
#line 4262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16126 "Parser/parser.cc"
    break;

  case 1106: /* cfa_identifier_parameter_ptr: ptrref_operator attribute_list type_specifier_nobody  */
#line 4264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 16132 "Parser/parser.cc"
    break;

  case 1107: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator type_specifier_nobody  */
#line 4266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16138 "Parser/parser.cc"
    break;

  case 1108: /* cfa_identifier_parameter_ptr: ptrref_operator cfa_abstract_function  */
#line 4268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16144 "Parser/parser.cc"
    break;

  case 1109: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator cfa_abstract_function  */
#line 4270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16150 "Parser/parser.cc"
    break;

  case 1110: /* cfa_identifier_parameter_ptr: ptrref_operator cfa_identifier_parameter_declarator_tuple  */
#line 4272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16156 "Parser/parser.cc"
    break;

  case 1111: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator cfa_identifier_parameter_declarator_tuple  */
#line 4274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16162 "Parser/parser.cc"
    break;

  case 1112: /* cfa_identifier_parameter_array: '[' ']' type_specifier_nobody  */
#line 4281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16168 "Parser/parser.cc"
    break;

  case 1113: /* cfa_identifier_parameter_array: '[' ']' cfa_abstract_tuple  */
#line 4283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16174 "Parser/parser.cc"
    break;

  case 1114: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension type_specifier_nobody  */
#line 4285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16180 "Parser/parser.cc"
    break;

  case 1115: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension cfa_abstract_tuple  */
#line 4287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16186 "Parser/parser.cc"
    break;

  case 1116: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension type_specifier_nobody  */
#line 4289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16192 "Parser/parser.cc"
    break;

  case 1117: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension cfa_abstract_tuple  */
#line 4291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16198 "Parser/parser.cc"
    break;

  case 1118: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension type_specifier_nobody  */
#line 4293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16204 "Parser/parser.cc"
    break;

  case 1119: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension cfa_abstract_tuple  */
#line 4295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16210 "Parser/parser.cc"
    break;

  case 1120: /* cfa_identifier_parameter_array: multi_array_dimension type_specifier_nobody  */
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16216 "Parser/parser.cc"
    break;

  case 1121: /* cfa_identifier_parameter_array: multi_array_dimension cfa_abstract_tuple  */
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16222 "Parser/parser.cc"
    break;

  case 1122: /* cfa_identifier_parameter_array: '[' ']' cfa_identifier_parameter_ptr  */
#line 4302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16228 "Parser/parser.cc"
    break;

  case 1123: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension cfa_identifier_parameter_ptr  */
#line 4304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16234 "Parser/parser.cc"
    break;

  case 1124: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16240 "Parser/parser.cc"
    break;

  case 1125: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16246 "Parser/parser.cc"
    break;

  case 1126: /* cfa_identifier_parameter_array: multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16252 "Parser/parser.cc"
    break;

  case 1127: /* cfa_array_parameter_1st_dimension: '[' type_qualifier_list '*' ']'  */
#line 4315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 16258 "Parser/parser.cc"
    break;

  case 1128: /* cfa_array_parameter_1st_dimension: '[' type_qualifier_list assignment_expression ']'  */
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 16264 "Parser/parser.cc"
    break;

  case 1129: /* cfa_array_parameter_1st_dimension: '[' declaration_qualifier_list assignment_expression ']'  */
#line 4322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 16270 "Parser/parser.cc"
    break;

  case 1130: /* cfa_array_parameter_1st_dimension: '[' declaration_qualifier_list type_qualifier_list assignment_expression ']'  */
#line 4324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 16276 "Parser/parser.cc"
    break;

  case 1132: /* cfa_abstract_declarator_tuple: type_qualifier_list cfa_abstract_tuple  */
#line 4351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 16282 "Parser/parser.cc"
    break;

  case 1136: /* cfa_abstract_ptr: ptrref_operator type_specifier  */
#line 4362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16288 "Parser/parser.cc"
    break;

  case 1137: /* cfa_abstract_ptr: ptrref_operator attribute_list type_specifier  */
#line 4364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 16294 "Parser/parser.cc"
    break;

  case 1138: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator type_specifier  */
#line 4366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16300 "Parser/parser.cc"
    break;

  case 1139: /* cfa_abstract_ptr: ptrref_operator cfa_abstract_function  */
#line 4368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16306 "Parser/parser.cc"
    break;

  case 1140: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator cfa_abstract_function  */
#line 4370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16312 "Parser/parser.cc"
    break;

  case 1141: /* cfa_abstract_ptr: ptrref_operator cfa_abstract_declarator_tuple  */
#line 4372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16318 "Parser/parser.cc"
    break;

  case 1142: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator cfa_abstract_declarator_tuple  */
#line 4374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16324 "Parser/parser.cc"
    break;

  case 1143: /* cfa_abstract_array: '[' ']' type_specifier  */
#line 4381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16330 "Parser/parser.cc"
    break;

  case 1144: /* cfa_abstract_array: '[' ']' multi_array_dimension type_specifier  */
#line 4383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16336 "Parser/parser.cc"
    break;

  case 1145: /* cfa_abstract_array: multi_array_dimension type_specifier  */
#line 4385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16342 "Parser/parser.cc"
    break;

  case 1146: /* cfa_abstract_array: '[' ']' cfa_abstract_ptr  */
#line 4387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16348 "Parser/parser.cc"
    break;

  case 1147: /* cfa_abstract_array: '[' ']' multi_array_dimension cfa_abstract_ptr  */
#line 4389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16354 "Parser/parser.cc"
    break;

  case 1148: /* cfa_abstract_array: multi_array_dimension cfa_abstract_ptr  */
#line 4391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16360 "Parser/parser.cc"
    break;

  case 1149: /* cfa_abstract_tuple: '[' cfa_abstract_parameter_list ']'  */
#line 4396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 16366 "Parser/parser.cc"
    break;

  case 1150: /* cfa_abstract_tuple: '[' type_specifier_nobody ELLIPSIS ']'  */
#line 4398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 16372 "Parser/parser.cc"
    break;

  case 1151: /* cfa_abstract_tuple: '[' type_specifier_nobody ELLIPSIS constant_expression ']'  */
#line 4400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 16378 "Parser/parser.cc"
    break;

  case 1152: /* cfa_abstract_function: '[' ']' '(' cfa_parameter_list_ellipsis_opt ')'  */
#line 4405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 16384 "Parser/parser.cc"
    break;

  case 1153: /* cfa_abstract_function: cfa_abstract_tuple '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 4407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 16390 "Parser/parser.cc"
    break;

  case 1154: /* cfa_abstract_function: cfa_function_return '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 4409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 16396 "Parser/parser.cc"
    break;

  case 1157: /* default_initializer_opt: %empty  */
#line 4433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 16402 "Parser/parser.cc"
    break;

  case 1158: /* default_initializer_opt: '=' assignment_expression  */
#line 4435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 16408 "Parser/parser.cc"
    break;


#line 16412 "Parser/parser.cc"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

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
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
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
  ++yynerrs;

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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 4438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
