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
    GENERIC = 365,                 /* GENERIC  */
    STATICASSERT = 366,            /* STATICASSERT  */
    IDENTIFIER = 367,              /* IDENTIFIER  */
    TYPEDIMname = 368,             /* TYPEDIMname  */
    TYPEDEFname = 369,             /* TYPEDEFname  */
    TYPEGENname = 370,             /* TYPEGENname  */
    TIMEOUT = 371,                 /* TIMEOUT  */
    WAND = 372,                    /* WAND  */
    WOR = 373,                     /* WOR  */
    CATCH = 374,                   /* CATCH  */
    RECOVER = 375,                 /* RECOVER  */
    CATCHRESUME = 376,             /* CATCHRESUME  */
    FIXUP = 377,                   /* FIXUP  */
    FINALLY = 378,                 /* FINALLY  */
    INTEGERconstant = 379,         /* INTEGERconstant  */
    CHARACTERconstant = 380,       /* CHARACTERconstant  */
    STRINGliteral = 381,           /* STRINGliteral  */
    DIRECTIVE = 382,               /* DIRECTIVE  */
    C23_ATTRIBUTE = 383,           /* C23_ATTRIBUTE  */
    FLOATING_DECIMALconstant = 384, /* FLOATING_DECIMALconstant  */
    FLOATING_FRACTIONconstant = 385, /* FLOATING_FRACTIONconstant  */
    FLOATINGconstant = 386,        /* FLOATINGconstant  */
    ARROW = 387,                   /* ARROW  */
    ICR = 388,                     /* ICR  */
    DECR = 389,                    /* DECR  */
    LS = 390,                      /* LS  */
    RS = 391,                      /* RS  */
    LE = 392,                      /* LE  */
    GE = 393,                      /* GE  */
    EQ = 394,                      /* EQ  */
    NE = 395,                      /* NE  */
    ANDAND = 396,                  /* ANDAND  */
    OROR = 397,                    /* OROR  */
    ATTR = 398,                    /* ATTR  */
    ELLIPSIS = 399,                /* ELLIPSIS  */
    EXPassign = 400,               /* EXPassign  */
    MULTassign = 401,              /* MULTassign  */
    DIVassign = 402,               /* DIVassign  */
    MODassign = 403,               /* MODassign  */
    PLUSassign = 404,              /* PLUSassign  */
    MINUSassign = 405,             /* MINUSassign  */
    LSassign = 406,                /* LSassign  */
    RSassign = 407,                /* RSassign  */
    ANDassign = 408,               /* ANDassign  */
    ERassign = 409,                /* ERassign  */
    ORassign = 410,                /* ORassign  */
    ErangeUpLt = 411,              /* ErangeUpLt  */
    ErangeUpLe = 412,              /* ErangeUpLe  */
    ErangeEq = 413,                /* ErangeEq  */
    ErangeNe = 414,                /* ErangeNe  */
    ErangeDownGt = 415,            /* ErangeDownGt  */
    ErangeDownGe = 416,            /* ErangeDownGe  */
    ErangeDownEq = 417,            /* ErangeDownEq  */
    ErangeDownNe = 418,            /* ErangeDownNe  */
    ATassign = 419,                /* ATassign  */
    THEN = 420                     /* THEN  */
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

#line 777 "Parser/parser.cc"

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
  YYSYMBOL_GENERIC = 110,                  /* GENERIC  */
  YYSYMBOL_STATICASSERT = 111,             /* STATICASSERT  */
  YYSYMBOL_IDENTIFIER = 112,               /* IDENTIFIER  */
  YYSYMBOL_TYPEDIMname = 113,              /* TYPEDIMname  */
  YYSYMBOL_TYPEDEFname = 114,              /* TYPEDEFname  */
  YYSYMBOL_TYPEGENname = 115,              /* TYPEGENname  */
  YYSYMBOL_TIMEOUT = 116,                  /* TIMEOUT  */
  YYSYMBOL_WAND = 117,                     /* WAND  */
  YYSYMBOL_WOR = 118,                      /* WOR  */
  YYSYMBOL_CATCH = 119,                    /* CATCH  */
  YYSYMBOL_RECOVER = 120,                  /* RECOVER  */
  YYSYMBOL_CATCHRESUME = 121,              /* CATCHRESUME  */
  YYSYMBOL_FIXUP = 122,                    /* FIXUP  */
  YYSYMBOL_FINALLY = 123,                  /* FINALLY  */
  YYSYMBOL_INTEGERconstant = 124,          /* INTEGERconstant  */
  YYSYMBOL_CHARACTERconstant = 125,        /* CHARACTERconstant  */
  YYSYMBOL_STRINGliteral = 126,            /* STRINGliteral  */
  YYSYMBOL_DIRECTIVE = 127,                /* DIRECTIVE  */
  YYSYMBOL_C23_ATTRIBUTE = 128,            /* C23_ATTRIBUTE  */
  YYSYMBOL_FLOATING_DECIMALconstant = 129, /* FLOATING_DECIMALconstant  */
  YYSYMBOL_FLOATING_FRACTIONconstant = 130, /* FLOATING_FRACTIONconstant  */
  YYSYMBOL_FLOATINGconstant = 131,         /* FLOATINGconstant  */
  YYSYMBOL_ARROW = 132,                    /* ARROW  */
  YYSYMBOL_ICR = 133,                      /* ICR  */
  YYSYMBOL_DECR = 134,                     /* DECR  */
  YYSYMBOL_LS = 135,                       /* LS  */
  YYSYMBOL_RS = 136,                       /* RS  */
  YYSYMBOL_LE = 137,                       /* LE  */
  YYSYMBOL_GE = 138,                       /* GE  */
  YYSYMBOL_EQ = 139,                       /* EQ  */
  YYSYMBOL_NE = 140,                       /* NE  */
  YYSYMBOL_ANDAND = 141,                   /* ANDAND  */
  YYSYMBOL_OROR = 142,                     /* OROR  */
  YYSYMBOL_ATTR = 143,                     /* ATTR  */
  YYSYMBOL_ELLIPSIS = 144,                 /* ELLIPSIS  */
  YYSYMBOL_EXPassign = 145,                /* EXPassign  */
  YYSYMBOL_MULTassign = 146,               /* MULTassign  */
  YYSYMBOL_DIVassign = 147,                /* DIVassign  */
  YYSYMBOL_MODassign = 148,                /* MODassign  */
  YYSYMBOL_PLUSassign = 149,               /* PLUSassign  */
  YYSYMBOL_MINUSassign = 150,              /* MINUSassign  */
  YYSYMBOL_LSassign = 151,                 /* LSassign  */
  YYSYMBOL_RSassign = 152,                 /* RSassign  */
  YYSYMBOL_ANDassign = 153,                /* ANDassign  */
  YYSYMBOL_ERassign = 154,                 /* ERassign  */
  YYSYMBOL_ORassign = 155,                 /* ORassign  */
  YYSYMBOL_ErangeUpLt = 156,               /* ErangeUpLt  */
  YYSYMBOL_ErangeUpLe = 157,               /* ErangeUpLe  */
  YYSYMBOL_ErangeEq = 158,                 /* ErangeEq  */
  YYSYMBOL_ErangeNe = 159,                 /* ErangeNe  */
  YYSYMBOL_ErangeDownGt = 160,             /* ErangeDownGt  */
  YYSYMBOL_ErangeDownGe = 161,             /* ErangeDownGe  */
  YYSYMBOL_ErangeDownEq = 162,             /* ErangeDownEq  */
  YYSYMBOL_ErangeDownNe = 163,             /* ErangeDownNe  */
  YYSYMBOL_ATassign = 164,                 /* ATassign  */
  YYSYMBOL_THEN = 165,                     /* THEN  */
  YYSYMBOL_166_ = 166,                     /* '}'  */
  YYSYMBOL_167_ = 167,                     /* '('  */
  YYSYMBOL_168_ = 168,                     /* '@'  */
  YYSYMBOL_169_ = 169,                     /* ')'  */
  YYSYMBOL_170_ = 170,                     /* '.'  */
  YYSYMBOL_171_ = 171,                     /* '['  */
  YYSYMBOL_172_ = 172,                     /* ']'  */
  YYSYMBOL_173_ = 173,                     /* ','  */
  YYSYMBOL_174_ = 174,                     /* ':'  */
  YYSYMBOL_175_ = 175,                     /* '{'  */
  YYSYMBOL_176_ = 176,                     /* '`'  */
  YYSYMBOL_177_ = 177,                     /* '^'  */
  YYSYMBOL_178_ = 178,                     /* '*'  */
  YYSYMBOL_179_ = 179,                     /* '&'  */
  YYSYMBOL_180_ = 180,                     /* '+'  */
  YYSYMBOL_181_ = 181,                     /* '-'  */
  YYSYMBOL_182_ = 182,                     /* '!'  */
  YYSYMBOL_183_ = 183,                     /* '~'  */
  YYSYMBOL_184_ = 184,                     /* '\\'  */
  YYSYMBOL_185_ = 185,                     /* '/'  */
  YYSYMBOL_186_ = 186,                     /* '%'  */
  YYSYMBOL_187_ = 187,                     /* '<'  */
  YYSYMBOL_188_ = 188,                     /* '>'  */
  YYSYMBOL_189_ = 189,                     /* '|'  */
  YYSYMBOL_190_ = 190,                     /* '?'  */
  YYSYMBOL_191_ = 191,                     /* '='  */
  YYSYMBOL_192_ = 192,                     /* ';'  */
  YYSYMBOL_YYACCEPT = 193,                 /* $accept  */
  YYSYMBOL_push = 194,                     /* push  */
  YYSYMBOL_pop = 195,                      /* pop  */
  YYSYMBOL_constant = 196,                 /* constant  */
  YYSYMBOL_quasi_keyword = 197,            /* quasi_keyword  */
  YYSYMBOL_identifier = 198,               /* identifier  */
  YYSYMBOL_identifier_at = 199,            /* identifier_at  */
  YYSYMBOL_identifier_or_type_name = 200,  /* identifier_or_type_name  */
  YYSYMBOL_string_literal = 201,           /* string_literal  */
  YYSYMBOL_string_literal_list = 202,      /* string_literal_list  */
  YYSYMBOL_primary_expression = 203,       /* primary_expression  */
  YYSYMBOL_generic_assoc_list = 204,       /* generic_assoc_list  */
  YYSYMBOL_generic_association = 205,      /* generic_association  */
  YYSYMBOL_postfix_expression = 206,       /* postfix_expression  */
  YYSYMBOL_field_name_list = 207,          /* field_name_list  */
  YYSYMBOL_field = 208,                    /* field  */
  YYSYMBOL_field_name = 209,               /* field_name  */
  YYSYMBOL_fraction_constants_opt = 210,   /* fraction_constants_opt  */
  YYSYMBOL_unary_expression = 211,         /* unary_expression  */
  YYSYMBOL_ptrref_operator = 212,          /* ptrref_operator  */
  YYSYMBOL_unary_operator = 213,           /* unary_operator  */
  YYSYMBOL_cast_expression = 214,          /* cast_expression  */
  YYSYMBOL_qualifier_cast_list = 215,      /* qualifier_cast_list  */
  YYSYMBOL_cast_modifier = 216,            /* cast_modifier  */
  YYSYMBOL_exponential_expression = 217,   /* exponential_expression  */
  YYSYMBOL_multiplicative_expression = 218, /* multiplicative_expression  */
  YYSYMBOL_additive_expression = 219,      /* additive_expression  */
  YYSYMBOL_shift_expression = 220,         /* shift_expression  */
  YYSYMBOL_relational_expression = 221,    /* relational_expression  */
  YYSYMBOL_equality_expression = 222,      /* equality_expression  */
  YYSYMBOL_AND_expression = 223,           /* AND_expression  */
  YYSYMBOL_exclusive_OR_expression = 224,  /* exclusive_OR_expression  */
  YYSYMBOL_inclusive_OR_expression = 225,  /* inclusive_OR_expression  */
  YYSYMBOL_logical_AND_expression = 226,   /* logical_AND_expression  */
  YYSYMBOL_logical_OR_expression = 227,    /* logical_OR_expression  */
  YYSYMBOL_conditional_expression = 228,   /* conditional_expression  */
  YYSYMBOL_constant_expression = 229,      /* constant_expression  */
  YYSYMBOL_argument_expression_list_opt = 230, /* argument_expression_list_opt  */
  YYSYMBOL_argument_expression_list = 231, /* argument_expression_list  */
  YYSYMBOL_argument_expression = 232,      /* argument_expression  */
  YYSYMBOL_assignment_expression = 233,    /* assignment_expression  */
  YYSYMBOL_assignment_expression_opt = 234, /* assignment_expression_opt  */
  YYSYMBOL_assignment_operator = 235,      /* assignment_operator  */
  YYSYMBOL_simple_assignment_operator = 236, /* simple_assignment_operator  */
  YYSYMBOL_compound_assignment_operator = 237, /* compound_assignment_operator  */
  YYSYMBOL_tuple = 238,                    /* tuple  */
  YYSYMBOL_tuple_expression_list = 239,    /* tuple_expression_list  */
  YYSYMBOL_comma_expression = 240,         /* comma_expression  */
  YYSYMBOL_comma_expression_opt = 241,     /* comma_expression_opt  */
  YYSYMBOL_statement = 242,                /* statement  */
  YYSYMBOL_labelled_statement = 243,       /* labelled_statement  */
  YYSYMBOL_compound_statement = 244,       /* compound_statement  */
  YYSYMBOL_statement_decl_list = 245,      /* statement_decl_list  */
  YYSYMBOL_statement_decl = 246,           /* statement_decl  */
  YYSYMBOL_statement_list_nodecl = 247,    /* statement_list_nodecl  */
  YYSYMBOL_expression_statement = 248,     /* expression_statement  */
  YYSYMBOL_selection_statement = 249,      /* selection_statement  */
  YYSYMBOL_conditional_declaration = 250,  /* conditional_declaration  */
  YYSYMBOL_case_value = 251,               /* case_value  */
  YYSYMBOL_case_value_list = 252,          /* case_value_list  */
  YYSYMBOL_case_label = 253,               /* case_label  */
  YYSYMBOL_case_label_list = 254,          /* case_label_list  */
  YYSYMBOL_case_clause = 255,              /* case_clause  */
  YYSYMBOL_switch_clause_list_opt = 256,   /* switch_clause_list_opt  */
  YYSYMBOL_switch_clause_list = 257,       /* switch_clause_list  */
  YYSYMBOL_iteration_statement = 258,      /* iteration_statement  */
  YYSYMBOL_for_control_expression_list = 259, /* for_control_expression_list  */
  YYSYMBOL_for_control_expression = 260,   /* for_control_expression  */
  YYSYMBOL_enum_key = 261,                 /* enum_key  */
  YYSYMBOL_updown = 262,                   /* updown  */
  YYSYMBOL_updownS = 263,                  /* updownS  */
  YYSYMBOL_updownEq = 264,                 /* updownEq  */
  YYSYMBOL_jump_statement = 265,           /* jump_statement  */
  YYSYMBOL_with_statement = 266,           /* with_statement  */
  YYSYMBOL_mutex_statement = 267,          /* mutex_statement  */
  YYSYMBOL_when_clause = 268,              /* when_clause  */
  YYSYMBOL_when_clause_opt = 269,          /* when_clause_opt  */
  YYSYMBOL_cast_expression_list = 270,     /* cast_expression_list  */
  YYSYMBOL_timeout = 271,                  /* timeout  */
  YYSYMBOL_wor = 272,                      /* wor  */
  YYSYMBOL_waitfor = 273,                  /* waitfor  */
  YYSYMBOL_wor_waitfor_clause = 274,       /* wor_waitfor_clause  */
  YYSYMBOL_waitfor_statement = 275,        /* waitfor_statement  */
  YYSYMBOL_wand = 276,                     /* wand  */
  YYSYMBOL_waituntil = 277,                /* waituntil  */
  YYSYMBOL_waituntil_clause = 278,         /* waituntil_clause  */
  YYSYMBOL_wand_waituntil_clause = 279,    /* wand_waituntil_clause  */
  YYSYMBOL_wor_waituntil_clause = 280,     /* wor_waituntil_clause  */
  YYSYMBOL_waituntil_statement = 281,      /* waituntil_statement  */
  YYSYMBOL_corun_statement = 282,          /* corun_statement  */
  YYSYMBOL_cofor_statement = 283,          /* cofor_statement  */
  YYSYMBOL_exception_statement = 284,      /* exception_statement  */
  YYSYMBOL_handler_clause = 285,           /* handler_clause  */
  YYSYMBOL_handler_predicate_opt = 286,    /* handler_predicate_opt  */
  YYSYMBOL_handler_key = 287,              /* handler_key  */
  YYSYMBOL_finally_clause = 288,           /* finally_clause  */
  YYSYMBOL_exception_declaration = 289,    /* exception_declaration  */
  YYSYMBOL_enable_disable_statement = 290, /* enable_disable_statement  */
  YYSYMBOL_enable_disable_key = 291,       /* enable_disable_key  */
  YYSYMBOL_asm_statement = 292,            /* asm_statement  */
  YYSYMBOL_asm_volatile_opt = 293,         /* asm_volatile_opt  */
  YYSYMBOL_asm_operands_opt = 294,         /* asm_operands_opt  */
  YYSYMBOL_asm_operands_list = 295,        /* asm_operands_list  */
  YYSYMBOL_asm_operand = 296,              /* asm_operand  */
  YYSYMBOL_asm_clobbers_list_opt = 297,    /* asm_clobbers_list_opt  */
  YYSYMBOL_asm_label_list = 298,           /* asm_label_list  */
  YYSYMBOL_declaration_list_opt = 299,     /* declaration_list_opt  */
  YYSYMBOL_declaration_list = 300,         /* declaration_list  */
  YYSYMBOL_KR_parameter_list_opt = 301,    /* KR_parameter_list_opt  */
  YYSYMBOL_KR_parameter_list = 302,        /* KR_parameter_list  */
  YYSYMBOL_local_label_declaration_opt = 303, /* local_label_declaration_opt  */
  YYSYMBOL_local_label_declaration_list = 304, /* local_label_declaration_list  */
  YYSYMBOL_local_label_list = 305,         /* local_label_list  */
  YYSYMBOL_declaration = 306,              /* declaration  */
  YYSYMBOL_static_assert = 307,            /* static_assert  */
  YYSYMBOL_cfa_declaration = 308,          /* cfa_declaration  */
  YYSYMBOL_cfa_variable_declaration = 309, /* cfa_variable_declaration  */
  YYSYMBOL_cfa_variable_specifier = 310,   /* cfa_variable_specifier  */
  YYSYMBOL_cfa_function_declaration = 311, /* cfa_function_declaration  */
  YYSYMBOL_cfa_function_specifier = 312,   /* cfa_function_specifier  */
  YYSYMBOL_cfa_function_return = 313,      /* cfa_function_return  */
  YYSYMBOL_cfa_typedef_declaration = 314,  /* cfa_typedef_declaration  */
  YYSYMBOL_typedef_declaration = 315,      /* typedef_declaration  */
  YYSYMBOL_typedef_expression = 316,       /* typedef_expression  */
  YYSYMBOL_c_declaration = 317,            /* c_declaration  */
  YYSYMBOL_declaring_list = 318,           /* declaring_list  */
  YYSYMBOL_general_function_declarator = 319, /* general_function_declarator  */
  YYSYMBOL_declaration_specifier = 320,    /* declaration_specifier  */
  YYSYMBOL_invalid_types = 321,            /* invalid_types  */
  YYSYMBOL_declaration_specifier_nobody = 322, /* declaration_specifier_nobody  */
  YYSYMBOL_type_specifier = 323,           /* type_specifier  */
  YYSYMBOL_type_specifier_nobody = 324,    /* type_specifier_nobody  */
  YYSYMBOL_type_qualifier_list_opt = 325,  /* type_qualifier_list_opt  */
  YYSYMBOL_type_qualifier_list = 326,      /* type_qualifier_list  */
  YYSYMBOL_type_qualifier = 327,           /* type_qualifier  */
  YYSYMBOL_type_qualifier_name = 328,      /* type_qualifier_name  */
  YYSYMBOL_forall = 329,                   /* forall  */
  YYSYMBOL_declaration_qualifier_list = 330, /* declaration_qualifier_list  */
  YYSYMBOL_storage_class_list = 331,       /* storage_class_list  */
  YYSYMBOL_storage_class = 332,            /* storage_class  */
  YYSYMBOL_basic_type_name = 333,          /* basic_type_name  */
  YYSYMBOL_basic_type_name_type = 334,     /* basic_type_name_type  */
  YYSYMBOL_vtable_opt = 335,               /* vtable_opt  */
  YYSYMBOL_vtable = 336,                   /* vtable  */
  YYSYMBOL_default_opt = 337,              /* default_opt  */
  YYSYMBOL_basic_declaration_specifier = 338, /* basic_declaration_specifier  */
  YYSYMBOL_basic_type_specifier = 339,     /* basic_type_specifier  */
  YYSYMBOL_direct_type = 340,              /* direct_type  */
  YYSYMBOL_indirect_type = 341,            /* indirect_type  */
  YYSYMBOL_sue_declaration_specifier = 342, /* sue_declaration_specifier  */
  YYSYMBOL_sue_type_specifier = 343,       /* sue_type_specifier  */
  YYSYMBOL_344_1 = 344,                    /* $@1  */
  YYSYMBOL_sue_declaration_specifier_nobody = 345, /* sue_declaration_specifier_nobody  */
  YYSYMBOL_sue_type_specifier_nobody = 346, /* sue_type_specifier_nobody  */
  YYSYMBOL_type_declaration_specifier = 347, /* type_declaration_specifier  */
  YYSYMBOL_type_type_specifier = 348,      /* type_type_specifier  */
  YYSYMBOL_type_name = 349,                /* type_name  */
  YYSYMBOL_typegen_name = 350,             /* typegen_name  */
  YYSYMBOL_elaborated_type = 351,          /* elaborated_type  */
  YYSYMBOL_elaborated_type_nobody = 352,   /* elaborated_type_nobody  */
  YYSYMBOL_aggregate_type = 353,           /* aggregate_type  */
  YYSYMBOL_354_2 = 354,                    /* $@2  */
  YYSYMBOL_355_3 = 355,                    /* $@3  */
  YYSYMBOL_356_4 = 356,                    /* $@4  */
  YYSYMBOL_357_5 = 357,                    /* $@5  */
  YYSYMBOL_type_parameters_opt = 358,      /* type_parameters_opt  */
  YYSYMBOL_aggregate_type_nobody = 359,    /* aggregate_type_nobody  */
  YYSYMBOL_aggregate_key = 360,            /* aggregate_key  */
  YYSYMBOL_aggregate_data = 361,           /* aggregate_data  */
  YYSYMBOL_aggregate_control = 362,        /* aggregate_control  */
  YYSYMBOL_field_declaration_list_opt = 363, /* field_declaration_list_opt  */
  YYSYMBOL_field_declaration = 364,        /* field_declaration  */
  YYSYMBOL_field_declaring_list_opt = 365, /* field_declaring_list_opt  */
  YYSYMBOL_field_declaring_list = 366,     /* field_declaring_list  */
  YYSYMBOL_field_declarator = 367,         /* field_declarator  */
  YYSYMBOL_field_abstract_list_opt = 368,  /* field_abstract_list_opt  */
  YYSYMBOL_field_abstract = 369,           /* field_abstract  */
  YYSYMBOL_cfa_field_declaring_list = 370, /* cfa_field_declaring_list  */
  YYSYMBOL_cfa_field_abstract_list = 371,  /* cfa_field_abstract_list  */
  YYSYMBOL_bit_subrange_size_opt = 372,    /* bit_subrange_size_opt  */
  YYSYMBOL_bit_subrange_size = 373,        /* bit_subrange_size  */
  YYSYMBOL_enum_type = 374,                /* enum_type  */
  YYSYMBOL_375_6 = 375,                    /* $@6  */
  YYSYMBOL_376_7 = 376,                    /* $@7  */
  YYSYMBOL_enumerator_type = 377,          /* enumerator_type  */
  YYSYMBOL_hide_opt = 378,                 /* hide_opt  */
  YYSYMBOL_enum_type_nobody = 379,         /* enum_type_nobody  */
  YYSYMBOL_enumerator_list = 380,          /* enumerator_list  */
  YYSYMBOL_visible_hide_opt = 381,         /* visible_hide_opt  */
  YYSYMBOL_enumerator_value_opt = 382,     /* enumerator_value_opt  */
  YYSYMBOL_parameter_list_ellipsis_opt = 383, /* parameter_list_ellipsis_opt  */
  YYSYMBOL_parameter_list = 384,           /* parameter_list  */
  YYSYMBOL_cfa_parameter_list_ellipsis_opt = 385, /* cfa_parameter_list_ellipsis_opt  */
  YYSYMBOL_cfa_parameter_list = 386,       /* cfa_parameter_list  */
  YYSYMBOL_cfa_abstract_parameter_list = 387, /* cfa_abstract_parameter_list  */
  YYSYMBOL_parameter_declaration = 388,    /* parameter_declaration  */
  YYSYMBOL_abstract_parameter_declaration = 389, /* abstract_parameter_declaration  */
  YYSYMBOL_cfa_parameter_declaration = 390, /* cfa_parameter_declaration  */
  YYSYMBOL_cfa_abstract_parameter_declaration = 391, /* cfa_abstract_parameter_declaration  */
  YYSYMBOL_identifier_list = 392,          /* identifier_list  */
  YYSYMBOL_type_no_function = 393,         /* type_no_function  */
  YYSYMBOL_type = 394,                     /* type  */
  YYSYMBOL_initializer_opt = 395,          /* initializer_opt  */
  YYSYMBOL_initializer = 396,              /* initializer  */
  YYSYMBOL_initializer_list_opt = 397,     /* initializer_list_opt  */
  YYSYMBOL_designation = 398,              /* designation  */
  YYSYMBOL_designator_list = 399,          /* designator_list  */
  YYSYMBOL_designator = 400,               /* designator  */
  YYSYMBOL_type_parameter_list = 401,      /* type_parameter_list  */
  YYSYMBOL_type_initializer_opt = 402,     /* type_initializer_opt  */
  YYSYMBOL_type_parameter = 403,           /* type_parameter  */
  YYSYMBOL_404_8 = 404,                    /* $@8  */
  YYSYMBOL_405_9 = 405,                    /* $@9  */
  YYSYMBOL_new_type_class = 406,           /* new_type_class  */
  YYSYMBOL_type_class = 407,               /* type_class  */
  YYSYMBOL_assertion_list_opt = 408,       /* assertion_list_opt  */
  YYSYMBOL_assertion_list = 409,           /* assertion_list  */
  YYSYMBOL_assertion = 410,                /* assertion  */
  YYSYMBOL_type_list = 411,                /* type_list  */
  YYSYMBOL_type_declaring_list = 412,      /* type_declaring_list  */
  YYSYMBOL_type_declarator = 413,          /* type_declarator  */
  YYSYMBOL_type_declarator_name = 414,     /* type_declarator_name  */
  YYSYMBOL_trait_specifier = 415,          /* trait_specifier  */
  YYSYMBOL_trait_declaration_list = 416,   /* trait_declaration_list  */
  YYSYMBOL_trait_declaration = 417,        /* trait_declaration  */
  YYSYMBOL_cfa_trait_declaring_list = 418, /* cfa_trait_declaring_list  */
  YYSYMBOL_trait_declaring_list = 419,     /* trait_declaring_list  */
  YYSYMBOL_translation_unit = 420,         /* translation_unit  */
  YYSYMBOL_external_definition_list_opt = 421, /* external_definition_list_opt  */
  YYSYMBOL_external_definition_list = 422, /* external_definition_list  */
  YYSYMBOL_up = 423,                       /* up  */
  YYSYMBOL_down = 424,                     /* down  */
  YYSYMBOL_external_definition = 425,      /* external_definition  */
  YYSYMBOL_426_10 = 426,                   /* $@10  */
  YYSYMBOL_427_11 = 427,                   /* $@11  */
  YYSYMBOL_428_12 = 428,                   /* $@12  */
  YYSYMBOL_429_13 = 429,                   /* $@13  */
  YYSYMBOL_430_14 = 430,                   /* $@14  */
  YYSYMBOL_external_function_definition = 431, /* external_function_definition  */
  YYSYMBOL_with_clause_opt = 432,          /* with_clause_opt  */
  YYSYMBOL_function_definition = 433,      /* function_definition  */
  YYSYMBOL_declarator = 434,               /* declarator  */
  YYSYMBOL_subrange = 435,                 /* subrange  */
  YYSYMBOL_asm_name_opt = 436,             /* asm_name_opt  */
  YYSYMBOL_attribute_list_opt = 437,       /* attribute_list_opt  */
  YYSYMBOL_attribute_list = 438,           /* attribute_list  */
  YYSYMBOL_attribute = 439,                /* attribute  */
  YYSYMBOL_attribute_name_list = 440,      /* attribute_name_list  */
  YYSYMBOL_attribute_name = 441,           /* attribute_name  */
  YYSYMBOL_attr_name = 442,                /* attr_name  */
  YYSYMBOL_paren_identifier = 443,         /* paren_identifier  */
  YYSYMBOL_variable_declarator = 444,      /* variable_declarator  */
  YYSYMBOL_variable_ptr = 445,             /* variable_ptr  */
  YYSYMBOL_variable_array = 446,           /* variable_array  */
  YYSYMBOL_variable_function = 447,        /* variable_function  */
  YYSYMBOL_function_declarator = 448,      /* function_declarator  */
  YYSYMBOL_function_no_ptr = 449,          /* function_no_ptr  */
  YYSYMBOL_function_ptr = 450,             /* function_ptr  */
  YYSYMBOL_function_array = 451,           /* function_array  */
  YYSYMBOL_KR_function_declarator = 452,   /* KR_function_declarator  */
  YYSYMBOL_KR_function_no_ptr = 453,       /* KR_function_no_ptr  */
  YYSYMBOL_KR_function_ptr = 454,          /* KR_function_ptr  */
  YYSYMBOL_KR_function_array = 455,        /* KR_function_array  */
  YYSYMBOL_paren_type = 456,               /* paren_type  */
  YYSYMBOL_variable_type_redeclarator = 457, /* variable_type_redeclarator  */
  YYSYMBOL_variable_type_ptr = 458,        /* variable_type_ptr  */
  YYSYMBOL_variable_type_array = 459,      /* variable_type_array  */
  YYSYMBOL_variable_type_function = 460,   /* variable_type_function  */
  YYSYMBOL_function_type_redeclarator = 461, /* function_type_redeclarator  */
  YYSYMBOL_function_type_no_ptr = 462,     /* function_type_no_ptr  */
  YYSYMBOL_function_type_ptr = 463,        /* function_type_ptr  */
  YYSYMBOL_function_type_array = 464,      /* function_type_array  */
  YYSYMBOL_identifier_parameter_declarator = 465, /* identifier_parameter_declarator  */
  YYSYMBOL_identifier_parameter_ptr = 466, /* identifier_parameter_ptr  */
  YYSYMBOL_identifier_parameter_array = 467, /* identifier_parameter_array  */
  YYSYMBOL_identifier_parameter_function = 468, /* identifier_parameter_function  */
  YYSYMBOL_type_parameter_redeclarator = 469, /* type_parameter_redeclarator  */
  YYSYMBOL_typedef_name = 470,             /* typedef_name  */
  YYSYMBOL_type_parameter_ptr = 471,       /* type_parameter_ptr  */
  YYSYMBOL_type_parameter_array = 472,     /* type_parameter_array  */
  YYSYMBOL_type_parameter_function = 473,  /* type_parameter_function  */
  YYSYMBOL_abstract_declarator = 474,      /* abstract_declarator  */
  YYSYMBOL_abstract_ptr = 475,             /* abstract_ptr  */
  YYSYMBOL_abstract_array = 476,           /* abstract_array  */
  YYSYMBOL_abstract_function = 477,        /* abstract_function  */
  YYSYMBOL_array_dimension = 478,          /* array_dimension  */
  YYSYMBOL_array_type_list = 479,          /* array_type_list  */
  YYSYMBOL_upupeq = 480,                   /* upupeq  */
  YYSYMBOL_multi_array_dimension = 481,    /* multi_array_dimension  */
  YYSYMBOL_abstract_parameter_declarator_opt = 482, /* abstract_parameter_declarator_opt  */
  YYSYMBOL_abstract_parameter_declarator = 483, /* abstract_parameter_declarator  */
  YYSYMBOL_abstract_parameter_ptr = 484,   /* abstract_parameter_ptr  */
  YYSYMBOL_abstract_parameter_array = 485, /* abstract_parameter_array  */
  YYSYMBOL_abstract_parameter_function = 486, /* abstract_parameter_function  */
  YYSYMBOL_array_parameter_dimension = 487, /* array_parameter_dimension  */
  YYSYMBOL_array_parameter_1st_dimension = 488, /* array_parameter_1st_dimension  */
  YYSYMBOL_variable_abstract_declarator = 489, /* variable_abstract_declarator  */
  YYSYMBOL_variable_abstract_ptr = 490,    /* variable_abstract_ptr  */
  YYSYMBOL_variable_abstract_array = 491,  /* variable_abstract_array  */
  YYSYMBOL_variable_abstract_function = 492, /* variable_abstract_function  */
  YYSYMBOL_cfa_identifier_parameter_declarator_tuple = 493, /* cfa_identifier_parameter_declarator_tuple  */
  YYSYMBOL_cfa_identifier_parameter_declarator_no_tuple = 494, /* cfa_identifier_parameter_declarator_no_tuple  */
  YYSYMBOL_cfa_identifier_parameter_ptr = 495, /* cfa_identifier_parameter_ptr  */
  YYSYMBOL_cfa_identifier_parameter_array = 496, /* cfa_identifier_parameter_array  */
  YYSYMBOL_cfa_array_parameter_1st_dimension = 497, /* cfa_array_parameter_1st_dimension  */
  YYSYMBOL_cfa_abstract_declarator_tuple = 498, /* cfa_abstract_declarator_tuple  */
  YYSYMBOL_cfa_abstract_declarator_no_tuple = 499, /* cfa_abstract_declarator_no_tuple  */
  YYSYMBOL_cfa_abstract_ptr = 500,         /* cfa_abstract_ptr  */
  YYSYMBOL_cfa_abstract_array = 501,       /* cfa_abstract_array  */
  YYSYMBOL_cfa_abstract_tuple = 502,       /* cfa_abstract_tuple  */
  YYSYMBOL_cfa_abstract_function = 503,    /* cfa_abstract_function  */
  YYSYMBOL_comma_opt = 504,                /* comma_opt  */
  YYSYMBOL_default_initializer_opt = 505   /* default_initializer_opt  */
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
#define YYLAST   31977

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  193
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  313
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1156
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2269

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   420


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
     749,   751,   756,   757,   768,   773,   778,   779,   784,   786,
     788,   794,   796,   798,   800,   802,   822,   825,   827,   829,
     831,   833,   835,   837,   839,   841,   843,   845,   847,   856,
     857,   861,   862,   864,   866,   868,   870,   872,   877,   879,
     881,   889,   890,   898,   901,   902,   904,   909,   925,   927,
     929,   931,   933,   935,   937,   939,   944,   946,   949,   951,
     956,   958,   963,   964,   966,   970,   971,   972,   973,   977,
     978,   980,   982,   984,   986,   988,   990,   992,   999,  1000,
    1001,  1002,  1006,  1007,  1011,  1012,  1017,  1018,  1020,  1022,
    1027,  1028,  1030,  1035,  1036,  1038,  1043,  1044,  1046,  1048,
    1050,  1055,  1056,  1058,  1063,  1064,  1069,  1070,  1075,  1076,
    1081,  1082,  1087,  1088,  1093,  1094,  1096,  1101,  1106,  1107,
    1111,  1113,  1118,  1121,  1124,  1129,  1130,  1138,  1144,  1145,
    1149,  1150,  1154,  1155,  1159,  1160,  1161,  1162,  1163,  1164,
    1165,  1166,  1167,  1168,  1169,  1175,  1178,  1180,  1182,  1184,
    1189,  1190,  1192,  1194,  1199,  1200,  1206,  1207,  1213,  1214,
    1215,  1216,  1217,  1218,  1219,  1220,  1221,  1222,  1223,  1224,
    1225,  1226,  1228,  1229,  1235,  1237,  1247,  1249,  1257,  1258,
    1263,  1265,  1267,  1269,  1271,  1276,  1278,  1280,  1286,  1315,
    1318,  1320,  1322,  1332,  1334,  1336,  1341,  1346,  1348,  1350,
    1352,  1360,  1361,  1363,  1367,  1369,  1373,  1375,  1376,  1378,
    1380,  1385,  1386,  1390,  1395,  1396,  1400,  1402,  1407,  1409,
    1414,  1416,  1418,  1420,  1425,  1427,  1429,  1431,  1436,  1438,
    1443,  1444,  1466,  1468,  1472,  1475,  1477,  1480,  1482,  1485,
    1487,  1492,  1498,  1500,  1505,  1510,  1512,  1514,  1516,  1518,
    1523,  1525,  1528,  1530,  1535,  1541,  1544,  1547,  1549,  1554,
    1560,  1562,  1567,  1573,  1576,  1578,  1581,  1583,  1588,  1595,
    1598,  1600,  1605,  1611,  1613,  1618,  1624,  1627,  1631,  1642,
    1647,  1652,  1663,  1665,  1667,  1669,  1674,  1676,  1680,  1682,
    1684,  1686,  1691,  1693,  1698,  1700,  1702,  1704,  1707,  1711,
    1714,  1718,  1720,  1722,  1724,  1726,  1728,  1730,  1732,  1734,
    1736,  1738,  1743,  1749,  1757,  1762,  1763,  1767,  1768,  1773,
    1777,  1778,  1781,  1783,  1788,  1791,  1793,  1795,  1798,  1800,
    1805,  1810,  1811,  1815,  1820,  1822,  1827,  1829,  1834,  1836,
    1838,  1843,  1848,  1853,  1858,  1860,  1862,  1867,  1869,  1875,
    1876,  1880,  1881,  1882,  1883,  1887,  1892,  1893,  1895,  1897,
    1899,  1903,  1907,  1908,  1912,  1914,  1916,  1918,  1920,  1926,
    1927,  1933,  1934,  1938,  1939,  1944,  1946,  1955,  1956,  1958,
    1963,  1965,  1973,  1974,  1978,  1980,  1986,  1987,  1991,  1993,
    1997,  1999,  2003,  2004,  2008,  2009,  2013,  2014,  2015,  2019,
    2021,  2036,  2037,  2038,  2039,  2041,  2045,  2047,  2051,  2058,
    2060,  2062,  2064,  2072,  2074,  2079,  2080,  2082,  2084,  2086,
    2096,  2098,  2110,  2113,  2118,  2120,  2126,  2131,  2136,  2147,
    2154,  2159,  2161,  2163,  2169,  2171,  2176,  2178,  2179,  2180,
    2196,  2198,  2201,  2203,  2206,  2211,  2212,  2216,  2217,  2218,
    2219,  2228,  2229,  2230,  2239,  2240,  2241,  2245,  2246,  2247,
    2256,  2257,  2258,  2263,  2264,  2273,  2275,  2280,  2285,  2287,
    2289,  2291,  2298,  2303,  2308,  2309,  2311,  2321,  2323,  2328,
    2330,  2332,  2334,  2336,  2338,  2341,  2343,  2345,  2350,  2356,
    2358,  2360,  2362,  2364,  2366,  2368,  2370,  2372,  2374,  2376,
    2378,  2380,  2382,  2384,  2386,  2389,  2391,  2393,  2395,  2397,
    2399,  2401,  2403,  2405,  2407,  2409,  2411,  2413,  2415,  2417,
    2419,  2421,  2423,  2428,  2429,  2433,  2439,  2440,  2446,  2447,
    2449,  2451,  2453,  2458,  2461,  2463,  2468,  2469,  2471,  2473,
    2478,  2480,  2482,  2484,  2486,  2488,  2493,  2494,  2496,  2498,
    2503,  2505,  2504,  2508,  2516,  2517,  2519,  2521,  2526,  2527,
    2529,  2534,  2536,  2538,  2540,  2545,  2547,  2549,  2554,  2556,
    2558,  2560,  2561,  2563,  2568,  2570,  2572,  2577,  2578,  2582,
    2583,  2590,  2589,  2594,  2593,  2603,  2602,  2613,  2612,  2622,
    2627,  2628,  2633,  2639,  2657,  2658,  2662,  2664,  2666,  2671,
    2673,  2675,  2677,  2682,  2684,  2689,  2691,  2700,  2701,  2706,
    2708,  2713,  2715,  2717,  2726,  2728,  2729,  2730,  2732,  2734,
    2735,  2740,  2741,  2745,  2746,  2751,  2753,  2756,  2759,  2766,
    2767,  2768,  2773,  2778,  2780,  2786,  2787,  2793,  2794,  2798,
    2806,  2813,  2826,  2825,  2829,  2832,  2831,  2840,  2844,  2848,
    2850,  2856,  2857,  2862,  2867,  2876,  2877,  2879,  2885,  2887,
    2892,  2893,  2899,  2900,  2901,  2910,  2911,  2913,  2914,  2919,
    2920,  2922,  2923,  2925,  2927,  2933,  2934,  2936,  2937,  2938,
    2940,  2942,  2949,  2950,  2952,  2954,  2959,  2960,  2969,  2971,
    2976,  2978,  2983,  2984,  2986,  2989,  2991,  2995,  2996,  2997,
    2999,  3001,  3009,  3011,  3016,  3017,  3019,  3023,  3024,  3026,
    3027,  3033,  3034,  3035,  3036,  3040,  3041,  3046,  3047,  3048,
    3049,  3050,  3064,  3065,  3070,  3071,  3076,  3078,  3080,  3082,
    3084,  3107,  3108,  3114,  3115,  3121,  3120,  3125,  3124,  3128,
    3134,  3137,  3147,  3148,  3150,  3154,  3159,  3161,  3163,  3165,
    3171,  3172,  3176,  3177,  3182,  3184,  3191,  3193,  3194,  3196,
    3201,  3203,  3205,  3210,  3212,  3217,  3222,  3230,  3235,  3237,
    3242,  3247,  3248,  3253,  3254,  3258,  3259,  3260,  3266,  3268,
    3270,  3276,  3278,  3284,  3285,  3289,  3291,  3296,  3300,  3304,
    3306,  3318,  3320,  3322,  3324,  3326,  3328,  3330,  3331,  3336,
    3339,  3338,  3350,  3349,  3362,  3361,  3375,  3374,  3388,  3387,
    3400,  3405,  3411,  3413,  3419,  3420,  3431,  3438,  3443,  3449,
    3452,  3455,  3459,  3465,  3468,  3471,  3476,  3477,  3478,  3479,
    3483,  3491,  3492,  3504,  3505,  3509,  3510,  3515,  3517,  3519,
    3521,  3526,  3527,  3533,  3534,  3536,  3541,  3542,  3544,  3579,
    3581,  3584,  3589,  3591,  3592,  3594,  3599,  3601,  3603,  3605,
    3607,  3612,  3614,  3616,  3618,  3620,  3622,  3624,  3629,  3631,
    3633,  3635,  3644,  3646,  3647,  3652,  3654,  3656,  3658,  3660,
    3665,  3667,  3669,  3671,  3673,  3678,  3680,  3682,  3684,  3686,
    3688,  3700,  3701,  3702,  3706,  3708,  3710,  3712,  3714,  3719,
    3721,  3723,  3725,  3727,  3732,  3734,  3736,  3738,  3740,  3742,
    3754,  3759,  3764,  3766,  3767,  3769,  3774,  3776,  3778,  3780,
    3782,  3787,  3789,  3791,  3793,  3795,  3797,  3799,  3804,  3806,
    3808,  3810,  3819,  3821,  3822,  3827,  3829,  3831,  3833,  3835,
    3840,  3842,  3844,  3846,  3848,  3853,  3855,  3857,  3859,  3861,
    3863,  3873,  3875,  3878,  3879,  3881,  3886,  3888,  3890,  3892,
    3897,  3899,  3901,  3903,  3908,  3910,  3912,  3926,  3928,  3931,
    3932,  3934,  3939,  3941,  3946,  3948,  3950,  3952,  3957,  3959,
    3964,  3966,  3983,  3984,  3986,  3991,  3993,  3995,  3997,  3999,
    4001,  4006,  4007,  4009,  4011,  4016,  4018,  4020,  4026,  4028,
    4031,  4034,  4041,  4043,  4052,  4054,  4056,  4057,  4059,  4061,
    4065,  4067,  4072,  4074,  4076,  4078,  4113,  4114,  4118,  4119,
    4122,  4124,  4129,  4131,  4133,  4135,  4137,  4142,  4143,  4145,
    4147,  4152,  4154,  4156,  4162,  4163,  4165,  4174,  4177,  4179,
    4182,  4184,  4186,  4200,  4201,  4203,  4208,  4210,  4212,  4214,
    4216,  4221,  4222,  4224,  4226,  4231,  4233,  4241,  4242,  4243,
    4248,  4249,  4250,  4256,  4258,  4260,  4262,  4264,  4266,  4268,
    4275,  4277,  4279,  4281,  4283,  4285,  4287,  4289,  4291,  4293,
    4296,  4298,  4300,  4302,  4304,  4309,  4311,  4313,  4318,  4344,
    4345,  4347,  4351,  4352,  4356,  4358,  4360,  4362,  4364,  4366,
    4368,  4375,  4377,  4379,  4381,  4383,  4385,  4390,  4392,  4394,
    4399,  4401,  4403,  4421,  4423,  4428,  4429
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
  "ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER", "TYPEDIMname",
  "TYPEDEFname", "TYPEGENname", "TIMEOUT", "WAND", "WOR", "CATCH",
  "RECOVER", "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
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

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1940)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1155)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     259,   -59, -1940,  3231,   131,   459, -1940,   583, -1940,  2365,
   -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940,   389, -1940,    12, -1940,
   -1940, 12971, -1940,  3231,   216, -1940,  3231,  7712, 12971,  3941,
      23, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940,    60,  1151,    79, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940, -1940,   393,
     271, -1940, -1940, -1940, -1940, -1940, -1940,  6113,  6113, 12971,
     118,   151, 31803, -1940,   185, -1940, -1940,  4039, -1940,   780,
   15539, -1940, -1940,  3402, -1940, -1940, -1940, 18690, -1940,   220,
     330,   282,   670,   160, -1940,  4752,   291,   428,   436,   498,
    4908,   714,  1041, 13161,   583, -1940,   672, 19028,  2229,   583,
   -1940, -1940, -1940,  3449,   729, 11720, 12149,  1453,  3449,  1611,
     585, -1940, -1940, -1940, -1940,   583, -1940, -1940, -1940, -1940,
     606, -1940, -1940, -1940, -1940,   595,   610,   583, -1940,   583,
   22547, -1940, -1940, -1940, 26318,  6113, -1940, -1940,  6113,   333,
   -1940, -1940, 31161,   650, 31238,   657,   681, 31315, 31392,   699,
   31862, -1940, -1940, -1940, -1940, -1940, -1940, -1940, 31469, 31469,
   22881, 12191,  5111, -1940, -1940, -1940, -1940,  3402,   528, -1940,
     654,   685, -1940,  2519,  5934, 31315, 31315, -1940,   673,   551,
     809,   887,   718,  1022,   701,   696,   730,   747,   -29, -1940,
     738,   755, -1940, -1940, -1940,   770, -1940,   764, 26377,   790,
    2637, -1940, -1940, -1940, -1940,   172, 20665,   583,  3580, -1940,
   -1940,   812, -1940,   841,   871, -1940,   926, 31315, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940, 23052,  4093,  4039,   541,
     885,   891,   896,   904,   917,   924, -1940, -1940,   583, 14459,
   24981,   911, 23394,   927, -1940,  5294,  6558,   980, 21369, 17748,
    3449,  3449,  1004,  3449,  1141,  3449,  1958,   770, -1940, -1940,
     583, -1940,  1019,  1086, -1940, -1940, -1940, -1940, 26544,  6113,
   -1940, -1940, 26603,  6305, -1940, -1940, 15719,   949, -1940, 19366,
   -1940,  2092,  1611, 18859, -1940, -1940, 26770, -1940, -1940,   986,
   -1940, -1940, -1940,   996, -1940,  9064,  1150, 29077, -1940,  1035,
    6113,   610,  1040,  1046, -1940,   583,   583,  3402, -1940, -1940,
   -1940,  5077,  5387,  1001,  1108,    95,  1108, -1940,   583,   583,
      -4, 22319,   743,  1108, -1940,   583,   583,    -4,   583, -1940,
     583, -1940,  3466, -1940, -1940,  1054,  1059,  1611, 17915, 20841,
   18690, -1940,  4752,   583,  3449, -1940,  1662,   585,  1056,  1145,
   22319,  6113, -1940,  6113,   670, -1940, 13723, -1940,  2092,  1611,
    1077,  1145, 22319,  6113,   583, -1940, 28891, -1940, -1940, -1940,
   -1940,  2092, -1940, -1940, -1940, -1940,  1611, -1940,  1273,  1164,
    5012,  6113, -1940, 24489,  1133, -1940, -1940, -1940,  3941,   610,
   22433,  1120,  6914, 24430, 17915, 16619, 25365, -1940, 27955, -1940,
    6113,  1108,    -9,  1146, 23223, -1940,  5111, 23736, -1940, 26829,
   25365, -1940, -1940, 23907, -1940, 31315, -1940, -1940, -1940, -1940,
   -1940, -1940, 23736, -1940, -1940, 25866, 26829, 26829, 14639,  1465,
    1875, 23565,   765,  2433, -1940,   694,  1152,  1095, 27955,  1112,
    1194, -1940, -1940,  1172,  1185,  1200, 29154,  1204,  1202, 31315,
    3402, 31315,  3402, -1940, -1940,  2172, -1940, -1940,  7712,  2769,
   29231,  7712,  3402, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940,  1228, 31315, -1940, -1940, -1940, -1940,
   31315, 31315, 31315, 31315, 31315, 31315, 31315, 31315, 31315, 31315,
   31315, 31315, 31315, 31315, 31315, 31315, 31315, 31315, 31315, 29308,
   -1940,  7712,  5793, -1940, 31315, -1940, -1940,  6914, 28005, -1940,
    1231,  1257, -1940, -1940, -1940, -1940,  6113,  4674,   272,   779,
   -1940,  6113,   841, -1940,  1165, -1940, 15899, 25532,   797, 21369,
   -1940,  2092,  1611,  1262, -1940,   986,  3743,    27,   360, -1940,
     690,   585,  1251,   583,  2637,  1272,   841,  2637,  1302, -1940,
     916, -1940, 14819, -1940, -1940, -1940,   944, 25365, -1940,  4996,
    4039,  1305,  1315,  1328,  1335,  1337,  1350, -1940, -1940,   772,
    1360, -1940,   966,  1360,  5821, 24647,  1209, 16799, 24078,  1275,
   31546,  1369, -1940, 28165, 26770, -1940, -1940, 14999, -1940, 26996,
   -1940,  2092,  2092, 28543, -1940, -1940,   986, -1940, -1940, -1940,
   13536, 29385,  1527, 31315,  4640,   925,  1375, -1940,   583,   583,
    1375,   993, -1940,   583,   583,  1406,  1375, -1940,   583,   583,
   -1940,  1360, -1940, 29462, 16079, 27055, -1940, -1940,  6113, 25306,
    2092,  2092, -1940, -1940,  5821, -1940, 21545, -1940, 21545, -1940,
   27795, -1940, -1940,  1375, 16979, -1940, 26544, -1940, -1940, -1940,
     -44, 25925, 21721, -1940, -1940, -1940, -1940, -1940, 28841, -1940,
   -1940, 31623, -1940, 21957,  5781, 12191,  9064,  1405,  1419, -1940,
   -1940,  1417, 29077,   521, -1940, -1940, -1940, 23565,  1437, -1940,
     926, -1940,  3402,  6914,  1423,  5077,   866,  1438,  1447,  1449,
    1136,  1473,  1478,  1482,  1539,  1543,  1545,  4403,  5077, -1940,
   -1940, -1940,   583,  1443, 28208, -1940, -1940,  1406,   670, -1940,
   -1940,   610,  1145, 24823, -1940, -1940,   670, -1940, -1940,   610,
   -1940, -1940,  5639,  5867,  3466, -1940,  1158, -1940, -1940, -1940,
   -1940, 23565, 23565, -1940,  2092,   583,  6914, 16259,  4135, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940,   610,  1145,  1520,  1517,
   -1940, -1940,  3449,  1567,  1145, 22319, -1940, -1940,   610,  1145,
   -1940, -1940, 13212, -1940, -1940,  2092,  2092, -1940, -1940, -1940,
     471,   703,   471,   585,  1573, -1940, -1940, -1940, 25306,  1549,
    1583, -1940, -1940,  1002, 25157, -1940,  1663, 17915, -1940,  1566,
   -1940, -1940, -1940, 27231,  1585,  1589,  1245, 28692, 27283,  6113,
    1108, -1940, -1940, -1940, -1940,  1593, 25699,  1594,  1599,  1604,
   16439,  1602,  1612,  1613,  1614,  1617,  1616, 31315,  1622,  1630,
    1636, 27343, 31315, -1940, -1940,  2476, -1940, -1940, -1940, 31315,
   -1940, 20137,  1267, -1940, -1940,   583,   583, -1940,  1637,  1638,
   28923, 29231,  1608, -1940, 29000,  7712, 31315,  1641, -1940,  1644,
   -1940, -1940,  4348, -1940,  1639, -1940,  4348, -1940, -1940, -1940,
   -1940,  1317,  1620, -1940,  9064, -1940, -1940,   673,   673,   673,
     551,   551,   809,   809,   887,   887,   887,   887,   718,   718,
    1022,   701,   696,   730,   747, 31315,  1197, -1940,  4348, -1940,
   -1940, -1940, 21545,  1108, 12971, -1940,  6113,  1645, 18523,  1643,
   -1940, -1940, -1940, -1940, -1940,  2637, -1940, -1940,  1733, 26092,
   20313,  1801,  2293, -1940, -1940,   583,  1646,   583, -1940,   611,
    1640,  1076, 25365,  1099,  1633, -1940,   926, -1940, 23565, -1940,
   -1940, -1940,  1188,  1360, -1940,  1142,  1360, 24823, -1940, -1940,
    1406, 24823, -1940,  1406, -1940, 20489, -1940, 26544, -1940, -1940,
   15179,  1664, 24249,  1666,   770,  1669, 17159, -1940, -1940, -1940,
   -1940, -1940, -1940, 17748, 20489, 13536,  1665,  1155,  1678,  1684,
    1686,  1687,  1689,  1691,  1696, -1940,   982,  2899, -1940,  2787,
   -1940,  6020, -1940, -1940, -1940, 24823, -1940, -1940, -1940, -1940,
   -1940, -1940, 24823, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
    1406, -1940,  1694, 26603, 16799, -1940, -1940, -1940,  1375,  2092,
   -1940,  1366, -1940, -1940, -1940, -1940, -1940, -1940, -1940, 20489,
   -1940,  6113,  4348, -1940,  1677,   116,  1701,  1417, -1940,  9064,
    1709, -1940,  1929, 31315, -1940, -1940,  1154, -1940,  1707, 20489,
   -1940, -1940, 31315,  1161,  1708,  1710,  1731,  1170,  1732,  1736,
    1738,  1739,  1742,  1747,  1334,  1360, -1940, -1940,  1420,  1360,
   -1940, -1940,  1542,  1360, -1940, -1940, -1940, -1940, -1940, -1940,
    6914,  1880,  1360,   968, -1940,   770,  1388, -1940, -1940,   610,
    1749, -1940, -1940,  5639,  1171,  3466,  5639, -1940, 24823,  1178,
    1752,  1187,  1755, -1940,  1275,   367, -1940,   610, 17688, -1940,
     610,  1145,   367, -1940,   610, -1940, -1940, -1940, -1940, -1940,
     999, -1940,  3402, -1940, -1940,  6113,   583,  1850, -1940, -1940,
   -1940, 21721,  1108, -1940, 20489,   362,  1761, -1940, 25306,   362,
    3402, -1940,   362, -1940, 26151,   362, -1940, 31315, 31315, 31315,
   -1940, -1940, -1940, -1940,  1764,  1770,  1773,  1778,  1281, -1940,
    1068, -1940, -1940, -1940, 31315, 31315,  1774,  9064, -1940,  1780,
   -1940, -1940,  1780,  1785, -1940, -1940, -1940, -1940,  4721, -1940,
   -1940,  1402, -1940,    46, -1940,  1410, -1940, 29539, -1940,  1417,
   -1940, 31315,  1416,  1781, -1940,   367,  1787,   841, -1940, -1940,
   -1940,  6914, 27510, 17855, -1940,   144,   230, 23565,  1771, -1940,
    1771, -1940, -1940,   583,  1498, -1940,   611,  1640,  1640,   172,
   -1940, -1940,  1795,  6113,  1794, -1940, -1940,  1804, -1940,  1807,
   -1940, -1940, 24823, -1940, -1940,  1406, 24823, -1940,  1406,  1811,
    1812, -1940,  1825,  1822,  1823, -1940, -1940, -1940, -1940, -1940,
   -1940,  1826, 20489, 20489, -1940,  1831, -1940,  1607,  1360, -1940,
    1621,  1672,  1360, -1940,  2092,  8038,  2964, -1940,   583,   583,
   -1940, -1940, -1940,  4563,  1983,  6267, -1940, -1940,  1833,  1835,
   -1940, -1940, -1940, 21545, -1940,   670,  1439, 31315, -1940, 31315,
   -1940,  1841, -1940, 29077, -1940,   583, 20489,   583, -1940, -1940,
    1681,  1360, -1940,  1815,  1360, -1940, -1940,  1816,  1360, 24823,
   -1940, -1940,  1406, 24823, -1940, -1940,  1406, 24823, -1940, -1940,
    1406,  1108, -1940,  1406, -1940, 29616, -1940, 31315, -1940, 28372,
   -1940, -1940,  1223, -1940, -1940, -1940, -1940, -1940,   492, -1940,
   -1940, 18022,   367, -1940,   610, -1940, -1940,  1834,  1836,  1838,
     692, -1940, 25306, -1940, -1940,   251,   804, -1940, 12401,  6113,
   -1940, -1940, -1940,  1198,  1846,  1842,  1227, -1940,  1843, -1940,
   -1940, -1940, -1940,  1817,  1360, -1940, -1940, -1940, -1940, -1940,
    9064,  1417, 29539,  1847,  1849, -1940,  1893,  4348, -1940,  1893,
    1893, -1940,  4348,  4924,  5557, -1940, -1940, -1940,  1844, -1940,
   -1940, -1940, -1940,  6113, -1940, -1940, -1940, -1940,  6113, -1940,
    6914, -1940,  1229, 25365,   841,   841,  1640,  1795,  1851,  1853,
     585,   503,  1858,  1839,   611, 18189, -1940,  1856,  1862, -1940,
   -1940, -1940, 21017, 21193, -1940, -1940, -1940,  1864,   583, 24823,
   -1940, -1940,  1406, 24823, -1940, -1940, 24823, -1940, -1940,  1406,
   31315, 31315,  1863,  1867, -1940,  1871, -1940, -1940,  4563,  3005,
    4800,  6020, -1940, -1940, -1940,  1885, -1940, -1940,  1872, -1940,
   -1940, -1940, -1940, -1940, -1940,  1888, 24823, -1940, -1940,  1406,
   24823, -1940, -1940,  1406, 24823, -1940, -1940,  1406,  1890,  1897,
    1899,   670, -1940,  1457, -1940,   -50, -1940,   770,  1905, -1940,
   -1940, -1940,  1906, -1940, -1940, -1940,  1907, 19901, -1940, -1940,
    6113, -1940,  1911, -1940,   656,   129, 14279,  1914,  1917, 22129,
    1921,  1922,  3321,  3557,  4023, 29693,  1923,  3120,  1925,  1927,
   22129,  1931, -1940, -1940,   610, 31315, 31315,  2089,  1940,   392,
   -1940, 22710, 15359,  1942,  1946,  1930, -1940, -1940, -1940, -1940,
   -1940, -1940, -1940, -1940, -1940, -1940,  1565,    78, -1940,   155,
   -1940,    78, -1940, -1940, -1940, -1940, -1940,  3402, -1940, -1940,
   13349, 19197, -1940,   265,  1952,  1956, -1940, -1940, 31315, -1940,
   26151, 31315, 24823, -1940, -1940,  1406,  1417,  1960, -1940, -1940,
   -1940,  1494, -1940,  4348, -1940,  4348, -1940, -1940,  1962,   272,
   -1940, -1940, -1940, -1940, -1940, -1940,  1959,  1970,   611,   611,
     172,  6113,   583, 29770, -1940,  1795, -1940, 18356, -1940, -1940,
   -1940,  1964, -1940,  1971,  1976, -1940,  1981,  1990,  1991, -1940,
   -1940,  1989, -1940,  1993, -1940, -1940,  1994,   583,  1997,  2001,
    2006, -1940, -1940, -1940, -1940, -1940, 31315, -1940,  1996, -1940,
     914,   962,   972, 23565,   583,   583, 17915,   583, 26829,  1984,
     302,   355,  2082, 10158, -1940,   375,  6113, -1940, -1940,  7712,
     -47,    55, -1940, -1940, -1940, -1940, 14279, 31315,  2010,  2091,
   14098, 12591, -1940,  1987, -1940,  1992, 31315,  1995,  9064,  2003,
   31315,  2015, -1940,  2016, 23565, 31315, -1940, 12781,  2034, -1940,
    2017,   -28, -1940,   -15,  2084,    50,   583, -1940,  2021,  2022,
   22129, 22129, -1940, -1940,  2116, -1940, -1940,     9,     9,   625,
   13910, -1940,   583, -1940, -1940, -1940, -1940,  2045,  2049, -1940,
   -1940,  1501,  1506, -1940,  1771,   611,   583,  1795,  1795,   585,
    1839, -1940,  9064, -1940,  2050, -1940,   583,   583, -1940, -1940,
   -1940,  2046,  2048, -1940, -1940, -1940, -1940, -1940, -1940, -1940,
   -1940, -1940,  1907,  1907,  1907,  1232, -1940,  5516, 26829,  5516,
     384, -1940, -1940, -1940,  6147, 31315,  6461,   142, -1940, -1940,
   -1940,   476,  2052,  2052,  2052,  6113, -1940, -1940,  2055, -1940,
   -1940, -1940, -1940,  1946,  2058, 31315,   330,  2070,   498, 20078,
   26377,  1242,  2078, 22129,  2079, -1940, -1940, -1940, -1940,  1073,
   22129, 31315,  1499,   357, -1940, 31315, 11311, -1940, -1940,   397,
   -1940,  1417, -1940,  1254, -1940, -1940,  1263,  1271,   707, -1940,
   -1940, -1940, -1940,   610,  2034,  2080, -1940, -1940, 31315, -1940,
    2088,   926, -1940, 10976, 31315, 31315, -1940, -1940,   264,     9,
   -1940,   344, -1940, -1940, -1940, -1940, -1940, -1940, -1940,   841,
    1795, -1940,  2093,  2096, -1940,  1417,   583, -1940, -1940, -1940,
   -1940,   583,   583,   583, -1940,   432,  1213,  2065,   440, -1940,
     456, -1940,  6147,   844, -1940,  7112,  6147, -1940,   583, -1940,
   -1940, -1940, -1940, -1940, -1940, 22129, 22129,  1946, 21897,    54,
   29847,  2176, 22129, -1940, 31315, -1940, 29924,  2186,  2077, 11445,
   30001, 22129, 12781,  1946,   434,  4144,  2081, 31315, -1940,  2104,
      82, 22129, -1940, 22129, -1940,  2109, -1940, 27569,  2086,   926,
     723, -1940, -1940,  2111,  1510,  1274, 22129,  2114, 22129, 22129,
   22129, -1940,  2121,   583,   583,  2133, -1940, -1940, -1940, -1940,
   -1940,  1213,  2441,   457, -1940, -1940, -1940, -1940,   583,   583,
   -1940, -1940, -1940, -1940,  2129,  5516, -1940,  2200,  7531,    67,
   17342, -1940, 21999, -1940,   -33,  1292, 22129,  2220,   469,  2120,
      17, 22129, 31315,   434,  4144,  2112, 30083,  1173,  2092,  2122,
     477,  2230, -1940, 30160, -1940, -1940, -1940, -1940, 30237, 31315,
   31315,  1946,  2123, 17521, -1940, -1940, -1940, 27569,  2125,  6599,
   27736,  3402, -1940,  2139,  2126,   -17, -1940, 31315,  7712, -1940,
   -1940, 31315,    78, -1940, -1940,   583, -1940, -1940, -1940,  2145,
    2150,  2153,  1690, -1940, -1940,   583, -1940, -1940, -1940, -1940,
   22129, -1940,   -18, -1940,    80, -1940, -1940, -1940,  2157,   725,
   -1940, -1940, 22129, -1940,   -30, -1940, 22129, 31315,  2158, 30314,
   -1940, -1940, 30391, 30468, 31315, 31315,  5821,  1946, -1940,   770,
   30545, 30622, 22129,  2142,   481,  2149,   495,  1946, -1940, -1940,
    2167,   725,  2125, 31315,  2159,  5251,  4657, -1940, -1940, -1940,
    2160, -1940,  2223,  2169,   793,  2166, -1940, -1940,  2175,  1322,
     263, -1940,  1818,  1360, -1940, -1940,  1213, -1940, 31315, -1940,
   31315, -1940, -1940,  1610, 19554, 19731, -1940, 22129, -1940, -1940,
    1946, -1940, -1940,  1946,  2163,   608,  2164,   677,  1946, -1940,
   -1940,   585, -1940,  1946, -1940,  1946, -1940,  2173, 30699, 30776,
   30853, -1940,  1610,  2182, -1940,   610,  3836,  5639,   -17,  2181,
   31315,  2168,   -17,   -17, -1940, -1940, 22129,  2277, 24823, -1940,
   -1940,  1406, -1940, -1940, -1940,  1176, -1940,  1610, -1940, -1940,
   -1940,  2193, 30930, 31007, 31084, -1940, -1940,  1946, -1940,  1946,
   -1940,  1946, -1940,   610, -1940,  2190,   926,  2196, -1940,   857,
   -1940, -1940, 22129,  2197, 11796, 22129,  2201,  1176, -1940, -1940,
    1946, -1940,  1946, -1940,  1946,  2202, -1940,   926,  2208, -1940,
    2184,   926, -1940, -1940, -1940, 22129, -1940, -1940, 11950, -1940,
   -1940,  1523, 31315, -1940,  1323, -1940,   926,  6113,  2210,  2185,
   -1940, -1940,  1329, -1940, -1940,  2191,  6113, -1940, -1940
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
     873,     0,   880,   883,     0,   873,     2,   874,   875,   883,
     888,   887,    17,    22,    23,     9,    10,    11,    12,    13,
      14,    15,    16,    18,    21,   886,     0,   881,   884,     1,
       2,   503,   876,   883,     0,   879,   883,   158,   503,   873,
     519,   520,   521,   522,   523,   524,   525,   526,   527,   508,
     510,   509,   511,     0,     0,     0,   529,   531,   558,   532,
     559,   535,   536,   556,   557,   530,   554,   555,   533,   534,
     537,   538,   539,   540,   541,   542,   543,   544,   545,   546,
     547,   548,   549,   550,   551,   552,   553,   560,   561,   873,
     563,   637,   638,   641,   643,   639,   645,     0,     0,   503,
       0,     0,    17,   608,   614,   829,   104,     0,    20,     0,
     503,   102,   103,     0,   850,    19,   889,   503,   830,     0,
       0,   441,   751,   443,   455,   871,   442,   477,   478,     0,
       0,     0,     0,   591,   873,   507,   512,   503,   514,   873,
     576,   528,   562,   487,   568,   873,   489,   586,   488,   873,
     605,   611,   590,   617,   629,   873,   634,   635,   618,   688,
     444,   445,     3,   837,   851,     0,     0,   873,   913,   873,
     503,   931,   932,   933,   503,     0,  1132,  1133,     0,     0,
     878,   882,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    29,     4,     8,    25,     5,     6,     7,     0,     0,
     503,     0,     0,   105,   106,   107,   108,   162,    84,    28,
      85,    24,    46,    83,   109,     0,     0,   124,   126,   130,
     133,   136,   141,   144,   146,   148,   150,   152,   154,   165,
       0,   159,   160,   164,    30,     0,     3,     0,   503,   840,
       0,   640,   642,   644,   646,     0,   503,   873,   691,   636,
     564,   805,   800,   790,     0,   838,     0,     0,   519,   831,
     835,   836,   832,   512,   833,   834,   503,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   609,   612,   873,   503,
     503,   102,   503,     0,   736,     0,  1155,     0,   504,   503,
     514,   494,   568,   495,   594,   496,   873,   605,   598,   619,
     873,   620,     0,     0,   732,   737,   722,   726,   503,   738,
    1100,  1101,   503,   739,   741,   890,   503,     0,  1134,   591,
     497,   498,   873,   503,   920,   939,   503,  1139,  1131,  1129,
    1137,   438,   437,     0,   173,   757,   172,     0,   446,     0,
       0,     0,     0,     0,   453,   873,   873,     0,   436,  1012,
    1013,     0,     0,   476,   871,   873,   871,   893,   873,   873,
     486,   503,   873,   871,   953,   873,   873,   485,   873,   973,
     873,   950,     0,   584,   585,     0,     0,   503,   503,   503,
     503,   456,   871,   873,   515,   577,     0,   606,     0,   854,
     503,     0,   505,     0,   751,   457,   591,   569,   587,   873,
       0,   854,   503,     0,   873,   517,   873,   578,   579,   573,
     490,   588,   492,   493,   491,   593,   873,   607,   601,     0,
     621,     0,   825,   503,     2,   852,   912,   914,   873,     0,
     503,     0,     0,   591,   503,   503,   503,  1143,   591,  1146,
       0,   871,   871,     0,   503,    91,     0,   503,   100,   503,
     503,   109,    86,   503,    94,     0,    36,    40,    41,    37,
      38,    39,   503,    89,    90,   503,   503,   503,   503,   105,
     106,   503,     0,     0,   194,     0,     0,   744,   591,   635,
       0,   746,  1129,  1153,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    26,    58,     0,    64,    65,   158,     0,
       0,   158,     0,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   172,     0,   170,   171,    87,    88,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     885,     0,     0,   826,     0,   466,   467,     0,   591,   827,
       0,     0,   786,   788,   787,   789,     0,     0,   782,     0,
     771,     0,   780,   792,     0,   689,   503,   503,  1155,   504,
     568,   594,   873,     0,   738,   739,   691,   608,   614,   692,
     693,   694,     0,   873,     0,   803,   791,     0,     0,   157,
       0,   615,   503,   797,   747,   796,     0,   503,   749,     0,
       0,     0,     0,     0,     0,     0,     0,   891,   918,   873,
     929,   937,   942,   948,     0,   503,     0,   504,   503,   608,
       0,     0,  1141,   591,   503,  1144,  1053,   503,  1103,   504,
     500,   501,   502,   503,  1108,  1097,  1098,  1106,  1052,     2,
     503,     2,   103,     0,   873,   873,  1155,   993,   873,   873,
    1155,   873,  1009,   873,   873,  1076,  1155,  1058,   873,   873,
    1067,  1074,   730,     0,   503,   503,   599,  1102,   740,   504,
     595,   596,   600,   601,     0,   464,   503,  1147,   503,  1118,
     504,  1124,  1119,  1155,   503,  1112,   503,  1121,  1113,     2,
    1155,   503,   503,   922,   941,  1130,   499,  1135,   591,   921,
     940,     0,     2,    27,     0,     0,   757,    28,     0,   755,
     758,  1153,     0,     0,   764,   753,   752,   503,     0,   856,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   896,
     956,   980,   873,   482,     0,   892,   901,  1043,   751,   894,
     895,     0,   854,   503,   952,   961,   751,   954,   955,     0,
     972,   974,     0,     0,     0,   472,   873,   866,   868,   867,
     869,   503,   503,   575,   504,   574,     0,   503,     0,  1136,
    1140,  1138,   454,   506,   592,   827,     0,   854,     0,     0,
     447,   458,   516,     0,   854,   503,   602,   827,     0,   854,
     801,   518,   571,   572,   570,   589,   604,   603,   610,   613,
     608,   614,   632,   633,     0,   802,   706,   742,   504,     0,
     707,   709,   711,     0,   503,   216,   430,   503,   853,     0,
     428,   486,   485,   591,   102,     0,     0,   503,   503,     0,
     871,   449,     2,   450,   877,     0,   503,     0,     0,     0,
     503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   503,     0,   123,   122,     0,   119,   118,    31,     0,
      32,   503,   873,   745,  1022,   873,   873,  1031,     0,     0,
       0,  1154,     0,   185,     0,   158,     0,     0,    54,     0,
      55,    62,     0,    61,     0,    57,     0,    56,    60,   191,
     190,     0,     0,    53,   757,   166,   125,   127,   128,   129,
     131,   132,   134,   135,   139,   140,   137,   138,   142,   143,
     145,   147,   149,   151,   153,     0,     0,   161,     0,    33,
     474,   469,   503,   871,   503,   827,     0,     0,     0,     0,
     785,   784,   783,   777,   513,     0,   775,   793,   566,   503,
     503,   103,   873,   740,   690,   873,     0,   873,   682,   691,
     691,     0,   503,     0,     0,   440,     0,   616,   503,   748,
     750,   919,   873,   930,   938,   943,   949,   503,   923,   925,
     927,   503,   944,   946,   693,   503,  1110,   503,  1120,  1111,
     503,   102,   503,     0,   606,     0,   504,     2,     2,  1142,
    1145,  1099,  1104,   504,   503,   503,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1077,     0,   873,  1156,  1063,
    1062,   874,   996,  1014,  1064,   503,   991,  1000,   728,   994,
     995,   729,   503,  1007,  1018,  1010,  1011,   731,  1060,  1061,
    1075,  1148,     0,   503,   504,  1105,  1109,  1107,  1155,   597,
     632,     0,   724,   723,   727,   733,  1116,  1123,  1117,   503,
     734,     0,     0,   766,   157,     0,     0,  1153,   763,  1154,
       0,   759,     0,     0,   762,   765,     0,     2,     0,   503,
     468,   470,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   873,   906,   910,   951,   873,   966,
     970,   978,   873,   989,   898,   958,   982,   897,   957,   981,
       0,     0,  1038,     0,  1044,  1045,     0,   480,   857,     0,
       0,   481,   858,     0,     0,     0,     0,   473,   503,     0,
       0,     0,     0,   471,     0,   873,   859,     0,     0,   827,
       0,   854,   873,   860,     0,   625,   627,   623,   647,   915,
     873,   934,     0,   710,   712,     0,   873,   431,   429,  1055,
    1054,   503,   871,   451,   503,    92,     0,    96,   503,   101,
       0,    99,    95,    97,   503,     0,   113,     0,     0,     0,
     117,   121,   120,   195,     0,     0,     0,     0,  1026,  1025,
     874,  1027,  1023,  1024,     0,     0,     0,   757,   110,  1153,
     187,   186,  1153,     0,   163,    48,    49,    81,     0,    81,
      81,     0,    69,    71,    51,     0,    47,     0,    50,  1153,
     156,     0,     0,     0,   828,   873,     0,   790,   820,   815,
     816,     0,   504,     0,   811,     0,     0,   503,   773,   772,
     773,   567,   565,   873,  1063,   685,   691,   691,   691,     0,
     701,   700,  1153,     0,     0,   806,   804,     0,   839,     0,
     799,   798,   503,   924,   926,   928,   503,   945,   947,     0,
       0,   716,     0,   717,   718,  1114,  1122,  1115,  1125,  1126,
    1127,     0,   503,   503,     3,     0,  1071,   873,  1003,  1006,
     873,   873,  1070,  1073,   503,     3,     0,  1059,   873,   873,
     998,  1016,  1065,     0,   103,     0,   997,  1015,     0,     0,
    1149,   735,   465,   503,     3,   751,     0,     0,   767,     0,
     768,     0,   760,     0,   754,   873,   503,   873,     3,   475,
     873,   907,   911,   873,   967,   971,   979,   873,   990,   503,
     899,   902,   904,   503,   959,   962,   964,   503,   983,   985,
     987,   871,   483,  1039,  1051,     0,  1050,     0,  1042,     0,
     862,   975,     0,   581,   580,   583,   582,   828,   873,   863,
     808,     0,   873,   861,     0,   828,   864,     0,     0,     0,
     873,   708,   503,   743,   434,     0,   873,   218,   503,     0,
     452,     3,    93,  1056,     0,     0,     0,    42,     0,   114,
     116,   115,  1035,   873,  1034,  1037,  1029,  1028,   112,   111,
     757,  1153,  1154,     0,     0,    68,    78,     0,    72,    79,
      80,    63,     0,     0,     0,    59,   193,   192,     0,   155,
      34,   841,   828,     0,   779,   818,   795,   812,     0,   813,
       0,   814,     0,   503,   790,   790,   691,  1153,     0,     0,
     697,   691,     0,   702,   691,     0,   439,     0,     0,   916,
     935,  1150,   503,   503,  1128,     3,     3,     0,   873,   503,
     999,  1001,  1002,   503,  1017,  1019,   503,  1066,  1068,  1069,
       0,     0,   102,     0,     3,     0,   992,  1008,     0,     0,
       0,     0,  1004,  1020,   725,     0,   448,   770,     0,   870,
     756,   761,   855,     3,   872,     0,   503,   900,   903,   905,
     503,   960,   963,   965,   503,   984,   986,   988,     0,     0,
       0,   751,  1040,     0,  1046,     0,  1047,  1048,     0,   810,
     828,   865,     0,   647,   647,   647,   630,   503,   713,   714,
       0,   432,     0,   219,     0,     0,   503,     0,     0,   355,
       0,     0,     0,     0,     0,   196,     0,     0,     0,     0,
     355,     0,   403,   402,     0,   168,   168,   409,   608,   614,
     213,   503,   503,     0,   197,     0,   224,   198,   199,   200,
     201,   202,   203,   204,   205,   356,     0,   370,   206,   376,
     378,   381,   207,   208,   209,   210,   211,     0,   212,   220,
     591,   503,   222,     0,     0,     0,  1057,    98,     0,    35,
     503,     0,   503,  1030,  1032,  1033,  1153,     0,   189,   188,
      82,     0,    70,     0,    76,     0,    74,   167,     0,   782,
     817,   819,   794,   774,   778,   776,     0,     0,   691,   691,
       0,     0,   873,     0,   696,  1153,   807,     0,   917,   936,
     720,   719,   721,     0,     0,  1152,     0,     0,     0,     3,
       3,     0,  1079,     0,  1151,   769,     0,   873,     0,     0,
       0,   908,   968,   976,   484,  1041,     0,   845,     0,   847,
     873,   873,   873,   503,   873,   873,   503,   873,   503,     0,
       0,     0,   661,   591,   648,     0,     0,   435,   217,   158,
       0,     0,   343,   344,   221,   223,   503,     0,     0,     0,
     503,   503,   339,     0,   337,     0,     0,     0,   757,     0,
       0,     0,   334,     0,   503,     0,   382,   503,     0,   169,
       0,     0,   410,     0,     0,     0,   873,   228,     0,     0,
     355,   355,   361,   360,   355,   372,   371,   355,   355,     0,
     591,   433,   873,    52,    45,    43,    44,     0,     0,    66,
      73,     0,     0,   843,   773,   691,   873,  1153,  1153,   699,
     702,   680,   757,   703,     0,   809,   873,   873,  1005,  1021,
    1072,     0,     0,  1078,  1080,   459,   463,   909,   969,   977,
    1049,   849,   630,   630,   630,     0,   622,   661,   503,   661,
       0,   660,   659,   655,     0,     0,     0,     0,   662,   663,
     665,   873,   677,   677,   677,     0,   656,   673,     0,   347,
     348,   345,   346,   237,     0,     0,   239,   443,   238,   591,
     503,     0,     0,   355,     0,   322,   324,   323,   325,     0,
     355,   196,   277,     0,   270,     0,   196,   340,   338,     0,
     332,  1153,   341,     0,   336,   335,     0,     0,     0,   391,
     392,   393,   394,     0,   384,     0,   385,   349,     0,   350,
       0,     0,   375,     0,     0,     0,   364,   374,     0,   355,
     377,     0,   379,   401,   462,  1036,    67,    77,    75,   790,
    1153,   681,     0,     0,   698,  1153,   873,   461,   460,  1081,
    1082,   873,   873,   873,   631,     0,   669,   635,     0,   675,
       0,   657,     0,     0,   679,     0,     0,   650,   873,   649,
     666,   678,   667,   668,   674,   355,   355,   240,   591,     0,
       0,   258,   355,   326,     0,   327,     0,   266,     0,   196,
       0,   355,   503,   278,     0,   304,     0,     0,   333,     0,
       0,   355,   354,   355,   395,     0,   386,   503,     0,     0,
       0,   215,   214,   357,     0,     0,   355,     0,   355,   355,
     355,   781,     0,   873,   873,     0,   684,   626,   628,   624,
     652,     0,   873,     0,   670,  1091,   672,  1083,   873,   873,
     654,   676,   658,   651,     0,     0,   353,   229,     0,     0,
       0,   251,   355,   231,     0,     0,   355,   260,   275,   286,
     280,   355,   196,     0,   290,     0,     0,     0,   317,   281,
     279,   268,   271,     0,   328,   329,   330,   331,     0,     0,
     196,   305,     0,     0,   234,   352,   383,   503,   389,   396,
     504,   400,   351,     0,     0,   411,   362,     0,   158,   373,
     366,     0,   367,   365,   380,   873,   687,   683,   704,     0,
       0,     0,  1087,  1086,  1088,   873,   653,  1084,  1085,   664,
     355,   246,   241,   244,     0,   243,   250,   249,     0,   873,
     253,   252,   355,   262,     0,   259,   355,     0,     0,     0,
     267,   272,     0,     0,     0,   196,     0,   291,   318,   319,
       0,     0,   355,     0,   307,   308,   306,   309,   274,   342,
       0,   873,   389,     0,     0,     0,   873,   397,   398,   399,
       0,   404,     0,     0,     0,   412,   413,   358,     0,     0,
       0,   686,   873,  1094,  1096,  1089,     0,   230,     0,   248,
       0,   247,   233,   254,   503,   503,   263,   355,   264,   261,
     276,   289,   287,   283,   295,   293,   294,   292,   296,   273,
     320,   321,   288,   284,   285,   282,   269,     0,     0,     0,
       0,   236,   254,     0,   390,     0,  1087,   874,   411,     0,
       0,     0,   411,     0,   363,   359,   355,     0,   503,  1090,
    1092,  1093,   671,   242,   245,   873,     3,   255,   425,   424,
     265,     0,     0,     0,     0,   316,   314,   311,   315,   312,
     313,   310,     3,     0,   387,     0,     0,     0,   405,     0,
     414,   368,   355,     0,     0,   355,     0,   873,   303,   301,
     298,   302,   299,   300,   297,     0,   388,   417,     0,   415,
       0,   417,   369,  1095,   227,   355,   225,   232,     0,   235,
     418,     0,     0,   406,     0,   226,     0,     0,     0,     0,
     419,   420,     0,   416,   407,     0,     0,   408,   421
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1940,    42,  -116, -1940,    -3,   589,  2214,    83,   -31, -1940,
    -152, -1940,   774, -1940,  -702, -1143, -1940,   501,  3840,  6737,
   -1940,   391, -1940,  1910,  1007,  1177,  1190,   652,  1220,  1857,
    1852,  1854,  1855,  1859, -1940,  -120,  -255,  -472, -1940,  1860,
    9969,   827, -1940,  2180, -1940, -1940,    32,  4899, -1356,  2162,
   -1940,  1629, -1940,  1012,   173, -1940, -1940,   689,   266, -1940,
   -1925, -1894,   453,   233, -1940, -1940,   680,   466, -1940, -1612,
   -1380,   398, -1940, -1940, -1940,   281, -1505, -1940, -1940, -1534,
     542, -1940, -1940, -1940, -1940, -1940,   -27, -1532, -1940, -1940,
   -1940, -1940, -1940,   314,   564,   566,   394, -1940, -1940, -1940,
   -1940,  -611, -1940,   249,   192, -1940,   324, -1940,  -261, -1940,
   -1940, -1940,  1048, -1195,   902,  -188, -1940,   -94,   -26,   176,
    1279,   903,   907, -1940,   -49, -1940, -1940,   -14, -1940,  -184,
     928,  2524,  -368,  4316,  9298,  -446,   153,   122,   675,   403,
    3497, -1940, -1940,  2351, -1940,   644,  5181, -1940,  2306, -1940,
     225, -1940, -1940,  6233,   646,  5426,  2937,   -61,  2067,  -100,
   -1940, -1940, -1940, -1940, -1940,  -343,  8704,  8234, -1940,  -199,
      68, -1940,  -750, -1940,   468, -1940,   334,   778, -1940,   -68,
    -168, -1940, -1940, -1940, -1940,  -113,  8838, -1186,  1016,   706,
      86, -1940,  -441,  -916,  2265,  3149,  1518,  -660,  -231,   872,
     507,  -443,  -391,  -329,  -609,  1404, -1940,  1783,   419, -1208,
    1533, -1940, -1940,   868, -1940, -1213,  -151,  -272,  -692, -1940,
      76, -1940, -1940, -1083, -1176, -1940, -1940, -1940, -1075,  2498,
    -661, -1219,   -25, -1940, -1940, -1940, -1940, -1940, -1940,   256,
   -1313,  -536, -1939,   -87,  1765,  9103,     3,   331,  2464, -1940,
    4162,    75,  -347,  -331,  -328,    52,   -62,   -53,   -46,  2194,
       5,    25,    33,  -304,   447,  -295,  -280,  -271,   480,  -269,
    -264,  -248,  -267,  -606,  -598,  -570,  -249,   -36,  -561, -1940,
   -1940,  -779,  1631,  1632,  1634,  2206, -1940,   976,  7666, -1940,
    -613,  -619,  -600,  -577,  -534, -1940, -1175, -1897, -1880, -1879,
    -601,  -224,  -166,  -247, -1940,   -73,    87,  -123, -1940, 10552,
    2328,  -241,  -473
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    31,   333,   208,   209,   115,   116,  1573,   210,   211,
     212,  1396,  1397,   213,  1211,  1212,  1213,  1416,   214,   215,
     216,   217,   472,   473,   218,   219,   220,   221,   222,   223,
     224,   225,   226,   227,   228,   229,  1065,   230,   231,   232,
     474,  1730,   515,   337,   517,   234,   901,  1574,  1575,  1576,
    1577,  1578,  1386,  1387,  2224,  1579,  1580,  1824,  2073,  2074,
    2001,  2002,  2003,  2196,  2197,  1581,  1843,  1844,  2098,  1935,
    1936,  2029,  1582,  1583,  1584,  1585,  1586,  1964,  1968,  1748,
    1740,  1587,  1588,  1747,  1741,  1589,  1590,  1591,  1592,  1593,
    1594,  1595,  1864,  2114,  1865,  1866,  2038,  1596,  1597,  1598,
    1733,  2124,  2125,  2126,  2251,  2262,  2143,  2144,   429,   430,
    1156,  1157,  1385,   118,   119,   120,   121,   122,  1827,   284,
     317,   126,   127,   128,   129,   353,   354,   432,   410,   286,
     477,   287,   132,   478,   134,   135,   263,   289,   290,   139,
     140,   141,   249,   142,  1242,   291,   320,   145,   377,   146,
     321,   386,   293,   571,   295,   322,   235,   151,   152,   298,
     153,   814,  1379,  1377,  1378,  1684,   299,   300,   156,   157,
    1380,  1694,  1807,  1808,  1809,  1983,  1984,  1695,  1908,  1920,
    1810,   158,  1248,  1446,   247,  1251,   301,  1252,  1253,  1644,
    1006,   820,  1272,   302,   303,   821,   305,   306,   307,   823,
     594,   595,   338,   710,   711,   712,   713,   714,   559,  1444,
     560,  1240,  1238,   943,   561,   585,   586,   563,   596,   160,
     252,   253,   161,  1233,  1234,  1235,  1236,     4,  1367,  1368,
     934,  1431,   162,   549,   550,   388,   400,   793,   163,   341,
     164,   765,  1066,   782,     6,     7,     8,    26,    27,    28,
     165,   767,   357,   358,   359,   768,   167,   168,   169,   170,
     171,   172,   173,   362,   769,   364,   365,   366,   770,   368,
     369,   370,  1022,   647,   648,   649,  1023,   371,   652,   653,
     654,   873,   874,   875,   876,   746,  1116,  1357,   308,  1605,
     656,   657,   658,   659,   660,   661,  1986,  1987,  1988,  1989,
     634,   309,   310,   311,   312,   481,   328,   176,   177,   178,
     314,   882,   662
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      23,   479,   590,   790,   727,   123,    23,   852,   716,   773,
      32,   931,   123,   236,  1434,   573,  1052,   130,  1053,   646,
     728,  1012,   574,   729,   130,  1076,   894,   867,    23,   902,
      23,  1024,  1445,    23,  1008,    23,    23,   650,   344,  1735,
    1013,   667,  1009,   394,   327,   270,   422,   730,   277,  2082,
     487,   439,  2147,  1744,   271,  1371,   731,  1437,   635,  2075,
    1447,   272,   568,  1014,  1046,  1418,  1734,  1375,  2076,  1273,
    1010,   732,    38,   123,   255,  1602,  1870,  2081,  1868,  1011,
     733,  2139,   734,   166,  2059,   130,    25,   735,  1273,   562,
     166,   339,    25,  1191,    23,    23,    23,  1067,   342,  1845,
     752,  2060,  2061,   736,    23,  1559, -1012,  1354,     9,   194,
      23,  1027,   273,   538,    23,  1845,    25,  1034,   175,    25,
     543,   431,    23,   842,  1135,   175,  2138,    23,   424,   787,
      23,    29,   274,  1356,    23,   582,  1142,   589,  1998,  1999,
     275,   799,   681,  1273,   545,  1819,   687,   643,  1528,   239,
    1432,   166,  1871,   137,  2122, -1012,  1532,   625,   842,  2083,
     137,   539,  2148,  1273,  1869,  1319,  1998,  1999,  1742,   324,
   -1012,  -854,    23,  1028,     1,    23,  1879,  1031,  1423,    37,
     251,   254,   360,  1037,   136,   389,   175,  1081,   666,   401,
     869,   136,  1743,  1599,  1215,  1437,  1742,  1700,  1701,  1719,
    2089,  2075, -1012,   439,    23,   356,   601,   124,   343, -1012,
    1055,   574,   583,  1628,   124,   602,  1424,  1060,  2059,  1872,
    1743,   137,   603,     2,   175,   588,  1222,   240,  1127,  2000,
     424,  1634,  1635,  1705,  1947,  2060,  2061,    23,     3,  1878,
    1133,  2077,  1734,  1881,   574,    23,   245,  1821,  1273,  2195,
     651,   568,   136,  2140,  2141,   339,   147,  2033,   441,  -821,
    1317,   442,   423,   147,    23,    23,   744,   743,  1645,   748,
    2081,    32,  1745,   604,  1225,   124,   756,    23,  2195,  1622,
    1624,  1626,    23,    23,   568,   256,   103,   104,  1318,   270,
     947,    55,   434,   605,   848,  1219,  1746,  1530,   271,  1319,
     898,   606,  2081,  2227,   424,   272,    23,   780,  1917,   381,
      23,  1678,   431,   395,   947,  1918,    23,  1438,   257,   324,
      23,  1702,   667,   558,   147,   175,    32,  2016,  1130,  1132,
    1845,  1012,   707,   340,  1919,  -854,  1439,    23,     1,  1024,
      34,   431,   109,   635,    23,  2186,  1966,  1735,    23,    23,
    1013,  1704,   266,   431,   841,   843,   273,  1117,   809,  1559,
    1316,  1738,   398,   327,   179,  1121,  1273,  1273,   343,    23,
     667,   693,  1647,  1014,  1734,   699,   274,    23,  1084,    23,
    1967,   829,   667,  1071,   275,   180,  1012,     2,    23,    36,
      23,  1046,   683,    23,  1085,   147,   690,  1086,   327,  1008,
      23,   618,     3,  1440,   324,  1013,  1302,  1009,  1042,  1406,
    1273,  1407,   331,  1203,   546,   727,   940,    23,    23,  1182,
      23,  1087,  1441,   718,  1540,    23,  1970,   739,  1014,    23,
    1088,   728,   324,   562,   729,  1010,   562,    23,  1540, -1013,
     574,   635,  1739,  1541,  1285,  1089,     1,  1054,   794,   988,
     941,   942,  1767,  1768,  1090,    -3,  1091,  1751,   730,  -822,
     574,  1092,  1940,   956,   345,   343,   826,   731,   574,   666,
    1070,  1437,     1,  1027,   788,   345,   789,  1093,  1372,   800,
     568,   809,   732,   434,   831,  1938,   251,    23, -1013,    23,
    1946,   733,    23,   734,  1802,     2,    23,   815,   735,    23,
     568,  1000,   443, -1013,   251,   264,    36,   356,   568,   819,
       3,  1825,   434,  1640,   736,  1825,  1846,   666,  2130,  1256,
    1057,     2,   332,   840,   434,  1261,  1941,   266,   346,   666,
    1196,  1942,  1846,  -823,  1141, -1013,     3,  1197,     1,    23,
     957,   404, -1013,   589,    23,  1442,   406,  1803,  1815,   411,
       1,   416,   434,    23,    23,     1,  1889,  1815,    23,   266,
     246,    35,    36,  1284,  2023,  2028,   -23,  1816,   625,   666,
     869,     1,   791,    23,  1351,  1311,  1911,   363,   452,  1890,
     666,    23,   897,  2015,    23,  1064,   147,     2,  1411,  1948,
    1835,  1836,    24,   460,  1837,  1838,    23,    23,    24,     2,
      32,   346,     3,    32,     2,  1918,   518,   519,   651,   347,
     367,    23,    23,  1991,     3,   147,   751,  1933,  1314,     3,
       2,  1249,    24,   759,  1980,    24,   574,   147,   237,  1918,
    2065,  1302,  1992,  2092,  2093,     3,    32,    23,  1328,   937,
     939,    23,   869,   277,   946,   786,  -873,   744,  1993,  2066,
     869,   693,   699,  1494,   869,   147,  2091,   798,  -824,   398,
    2101,  2087,     1,   601,  2168,    23,   568,   558,   869, -1154,
     558,    23,   602,   518,  2108,   143,  1971,   148,  2170,   603,
    1250,  1004,   143,  1016,   148,   579,    24,    24,   618,  1078,
     348,  1072,  1073,   404,   406,  1435,   671,  1008,   416,   489,
     270,    23,   315,   707,   490,  1009,   138,   480,   423,   271,
     607,     2,  1074,   138,    24,  1621,   272,   372,   666,    23,
      23,   241,    23,  1391,   242,   243,     3,   244,    32,   521,
     604,  1059,  -497,  1010,    23,    23,   522,   523,   625,  2159,
    1322,    32,  1011,   143,  1061,   148,   431,  1846,   393,   618,
     605,  1048,  1300,  1163,  1306,   419,  1475,  1478,   606,    23,
      23,    23,   423,  1079,    24,   780,   574,    24,   635,     1,
    1301,     1,  1307,    23,   138,    23,  1084,  -695,    32,   421,
    1606,   869,     1,   988,  -695,   424,   618,   404,  1250,   693,
     699,  2202,  1085,   579,   562,  1086,   488,   667,  1152,   740,
     424,  1616,  1052,  1053,     1,  1220,   568,  2064,   384,  -422,
    -422,   493,  1104,  1107,   143,   324,   148,   446,     2,  1087,
       2,  1276,     1,  1699,   449,   491,  1321,    32,  1088,    24,
     492,     2,   741,     3,   334,     3,    23,   580,   739,  1120,
      32,  1465,  1466,  1089,  1229,   335,     3,  1254,   450,    32,
     869,     1,  1090,     2,  1091,   528,   529,   520,  1536,  1092,
    2204,   336,   518,   868,  2118,  -873,   455,   869,     3,   621,
     266,     2,  -873,   535,    24,  1093,  1953,   988,  -873,   363,
     534,  1942,  1008,     1,  1164,  1503,     3,  2135,   537,    23,
    1009,  -422,  2044,    23,   276,   104,   666,  2045,    24,   618,
       2,   707,    24,   666,  1631,   530,   531,   540,   123,  1224,
     753,   906,   832,  1199,   744,     3,  1202,   434,  1010,   536,
     130,  1761,  1162,  1762,  1496,    23,  1480,  1285,   541,    24,
    1054,    23,     2,    23,   862,  1259,   724,   574,   106,   977,
     542,  2064,    23,   744,   666,   863,   864,     3,   944,  2198,
    2199,   845,   945,   589,   849,   544,   851,  1184,  1413,   131,
     853,  1414,  2181,   384,   950,  -842,   131,  2182,   641,   856,
      -3,    24,   858,   859,   860,   111,   951,   568,  1428,   584,
      24,  1299,    24,   651,  1393,   651,   166,  1294,   643,   524,
     525,  1795,    24,     1,  1501,    49,    50,    51,    52,    53,
    1633,  2135,    23,   961,     1,   143,   963,   148,  1119,   812,
      24,  1452,   817,   607,    23,   744,    23,   237,    23,  1226,
     147,   175,   526,   527,    32,   175,  2240,   131,   558,    24,
     557,  2241,  1856,   423,   143,   607,   148,   744,   587,  1282,
    1283,     1,     2,  1137,  1229,   318,   143,  1905,   148,  1910,
    1140,     1,   194,     2,   608,  1144,   137,     3,    23,    23,
     609,   601,  1498,  1269,  1499,   610,   707,  1270,     3,    23,
     602,   792,     1,   611,   143,  1390,   148,   603,     1,   888,
    1792,   890,  1054,   626,   893,   965,   612,   136,    24,   966,
       2,   903,  1025,   613,   373,   374,   641,   375,   131,   638,
       2,  1429,   437,   376,   969,     3,   460,    23,   384,  1851,
     124,  1308,   618,   967,  1230,     3,   639,   968,  1309,  1326,
      23,     2,    23,    23,   663,  1354,    32,     2,  1793,    32,
    1674,   929,   666,   981,  1448,  1449,     3,   744,  1794,  1229,
     638,  1355,     3,  1381,  1315,    24,    24,     1,  -500,    23,
      24,  1356,    23,   689,    49,    50,    51,    52,    53,   147,
    1032,   532,   533,  1895,   641,   955,   547,    23,  1467,   702,
    1617,  1151,   715,    24,   742,  1152,    24,   693,   699,  1483,
     914,   915,   916,   917,  1105,  1108,    49,    50,    51,    52,
      53,   675,   676,    32,   707,   404,     2,   589,  1495,   589,
    1104,  1107,   717,   984,   621,    23,  1637,   720,   622,   106,
     740,     3,  1505,   721,   819,   342,   241,  1106,  1109,   242,
     243,   771,   244,  1300,  1306,   175,   772,  1818,    23,  1835,
    1836,   785,  1054,  1837,  1838,   871,   106,     1,  1384,   744,
     339,  1301,  1307,   741,   384,  1255,   111,   112,  1176,   945,
      23,   697,   797,  1180,   437,     1,  1933,    24,   677,   678,
    1998,  1999,   871,  1050,  1521,  1934,   744,     1,  1257,   651,
    1542,  1198,   945,   111,   112,  1604,  -499,  1229,   808,   104,
      49,    50,    51,    52,    53,  -501,     2,   103,   104,   131,
     878,   879,   384,    23,    49,    50,    51,    52,    53,   825,
      23,     3,    23,   753,     2,  1097,   776,   744,   779,  1266,
     125,  1080,   830,   744,  1230,   844,     2,   125,   131,     3,
     175,   870,  1025,  1325,   607,  1128,   641,   968,   423,   744,
     131,     3,   744,  1636,   948,   419,  1453,   753,  1128,   106,
     607,   744,   744,   109,   792,   881,     1,  1363,  1457,  1653,
    1654,   869,  1458,  1166,   106,  1262,  1365,   883,   131,   744,
     869,  1229,   123,   880,   318,   950,  1661,   621,  1663,   641,
     869,  1221,   479,   884,   130,  1758,   111,   951,   125,   885,
    1981,   638,   884,   518,   744,   707,    23,  1666,  1773,   285,
    1128,   111,   112,   886,   744,     2,  1609,  1374,  1632,   318,
    1610,  1904,   968,   904,  1774,   968,   935,   707,   106,  1230,
       3,  1929,   382,     1,    23,   869,   125,   677,  1161,    23,
      23,    23,   106,  1950,   936,  1518,   959,   869,    24,  1519,
      23,   954,  1951,  1520,   871,    23,   968,    23,   744,   143,
    1952,   148,   997,  2049,   869,   111,   112,   869,   871,  1901,
    1902,  1903,   744,  1299,   651,   651,  -498,   727,   175,   111,
     112,  2084,     2,   962,  1696,   869,    49,    50,    51,    52,
      53,   964,  1384,   728,   971,   175,   729,     3,  -123,  -123,
    -123,  -123,  -123,  -123,   972,    23,    23,    23,    23,  1216,
    1217,  2185,  2259,   384,    32,   869,  2256,   973,  2265,     1,
     730,  1339,  2266,  2117,   974,   744,   975,   394,   792,   731,
    1601,    49,    50,    51,    52,    53,  1629,   125,  1826,   976,
     123,  1630,  1826,   589,   732,    24,  1892,  1893,   907,   908,
     909,   620,   130,   733,    24,   734,   998,    23,  1312,  1313,
     735,   136,   175,  1781,  1782,  1017,   707,  1230,     2,    23,
      23,    23,   999,  1229,    23,  1656,   736,   707,   285,  1657,
    1358,  1359,  1658,     3,   124,  1084,   643,  2215,  1399,  1400,
    1401,  2219,  1105,  1108,  1421,  1422,  2128,   701,   143,   -18,
     148,  1085,  1425,  1422,  1086,  1408,  1409,  1343,  1430,  1422,
    1069,   744,  1668,  1068,    23,   285,  1669,    23,    23,  1907,
    1670,  1680,  1681,  1682,  1077,  1106,  1109,  1094,  1087,   138,
    1949,  1497,  1422,   147,  1082,  1696,  1095,  1088,  1096,   622,
      23,     1,    23,  1697,    49,    50,    51,    52,    53,  1675,
     869,  1230,  1089,   175,  1111,  1713,  1715,  1717,    23,   106,
    1723,  1090,  1098,  1091,  1921,  1921,  1921,  1099,  1092,  1972,
      24,  1100,   389,   401,  1975,  1835,  1836,  1828,   285,  1837,
    1838,  1828,  1738,  1739,  1093,   950,  1760,  1422,  1601,   641,
       2,   792,   869,  1887,  1422,   382,   111,   951,  1888,  1422,
      54,  1398,  1933,  2047,  2048,     3,     1,   842,   175,    23,
       1,  1939,  1138,    23,  1998,  1999,  2256,  2257,  1757,   136,
       1,   910,   911,    49,    50,    51,    52,    53,  1101,  1347,
    1419,  1420,  1102,   744,  1103,   707,   912,   913,  1149,   384,
    1880,  1882,   124,   131,  1770,  1909,    89,    90,    91,    92,
      93,    94,    95,    96,   175,     2,   394,   707,   707,     2,
    1155,  1383,  1139,  2072,    24,  1922,  1923,    23,  1148,     2,
       3,     1,   918,   919,     3,   547,  1150,  1159,  1158,  1394,
       1,  1160,  1165,  1167,     3,   697,   779,  1812,   727,   707,
      30,   147,  1168,  1169,  1469,  1170,   381,   395,   744,  1817,
    1200,  1171,  1172,  1173,   728,  1175,  1218,   729,  1473,   622,
    1174,  1177,   641,   175,    23,   425,    23,   175,   175,  1178,
       2,    23,   794,    23,   238,  1179,  1194,  1195,  1214,     2,
    1237,   730,    23,  1205,   175,     3,  1206,  1227,  1241,  1243,
     731,  1246,   579,  1230,     3,  1258,   398,   933,  1830,   476,
     707,   106,  1830,  1830,  1286,   732,  1278,   707,  1279,  1476,
    1960,  1280,    24,   641,   733,   285,   734,  1287,  1506,  1830,
    -165,   735,   744,  1288,   248,  1289,  1290,  1981,  1291,   136,
    1292,   744,   131,   136,   136,  1293,  1310,   736,   111,   112,
     707,   285,  1812,  1320,  1812,  1324,  1327,  1330,   589,  1331,
     136,   739,   124,  2193,  2041,  2072,   124,   124,  -122,  -122,
    -122,  -122,  -122,  -122,     1,     1,     1,     1,  1924,   392,
    1332,  1333,  1352,   124,   405,  1334,   285,  1335,  1336,    23,
     409,  1337,    23,    23,   418,    32,  1338,   175,  1361,    32,
     420,  1364,   707,   707,  1366,  2217,   791,  1389,  2043,   707,
    1392,   147,   426,  1402,   427,   147,   147,   452,   707,  1403,
     666,   822,  1404,     2,     2,     2,     2,  1405,   707,  1410,
     707,  1415,   147,  1412,   678,   285,  1433,   285,     3,     3,
       3,     3,  1443,   707,  2041,   707,   707,   707,  1451,  1454,
     719,    49,    50,    51,    52,    53,  1456,    24,    24,  1455,
    1459,  1460,  1510,  1514,  1612,  2188,   744,   744,   744,   744,
    1104,  1107,    23,  2174,  1461,  1462,  1463,  2258,  1464,   707,
    1468,  1489,  1492,   707,  1493,   381,   395,  1500,   707,  1533,
    1627,  1534,   576,  1535,  2123,  1607,  1608,  1611,   589,  1618,
     589,  1619,    24,  1620,  1642,  1648,  1638,    24,  1639,   175,
    1643,  1649,   143,  1655,   148,    -3,    23,     1,    23,  1662,
     607,    12,  2079,   614,  1665,    15,    16,    17,    18,    19,
      20,    21,    22,   270,  1664,   398,   285,  1667,   828,  1671,
     589,   673,   271,   138,  1830,   674,  1672,   707,  1673,   272,
    1812,  1677,  1679,   476,  1683,  2111,   476,  1698,   480,   707,
    2226,  1706,   476,   707,  1707,   372,     2,   696,  1710,  1711,
    1720,   476,  1724,    23,  1725,   136,  2235,   108,  1727,   707,
    1062,     3,  -502,  1732,   791,    49,    50,    51,    52,    53,
     722,   723,    23,    23,   -22,   476,  1736,  1398,   124,   869,
     745,  1752,  1737,   749,   750,  1753,  1759,   754,  1763,    24,
     757,   758,   589,   760,  1765,   761,  1766,  1313,    24,  1813,
    1776,    24,    24,    24,   707,  1777,    24,  2123,   783,    24,
    1778,  2123,  2123,  1859,  1860,  1861,  1862,  1863,   822,  1779,
    1780,  1783,  1791,  1785,   796,  1784,  1787,   147,   324,   801,
    1788,   804,  1814,    23,    23,  1789,  1801,  1699,  1832,  1847,
      32,   807,  1739,   707,  1848,  2238,   817,  1850,  1874,  1875,
     143,   739,   148,   827,    12,  1852,   349,   350,    15,    16,
      17,    18,    19,    20,    21,    22,  2250,  1854,  1855,  1867,
    2250,   285,  1559,   125,  1885,  1886,  1896,   125,  1899,   707,
    1900,   138,   707,   106,  1925,  2260,  1805,  1926,   693,   699,
      24,   175,   175,   258,    41,    42,    43,    44,    45,    46,
      47,    48,   707,   340,  1813,  1930,  1813,  1957,  1932,  1804,
     108,  1104,  1107,   740,    23,  1959,  1805,  1990,  2006,  1973,
     111,   112,  1974,    23,   285,  1963,  1830,  1830,  2011,  2012,
    2032,   822,   113,  2030,  2223,   384,  2037,  1814,  2042,  1814,
    2046,  2051,  2070,   285,    12,    24,   741,  2055,    15,    16,
      17,    18,    19,    20,    21,    22,   891,   136,   136,  2058,
     744,   403,  2086,  2088,  2095,  2100,    49,    50,    51,    52,
      53,   325,  2102,  2120,  2132,  2109,   131,  2113,  2121,  2133,
     124,   124,  2134,  2142,   361,  2167,  2151,   390,  2175,    24,
      24,   402,  2169,  2171,  2178,  2179,  2180,   673,   285,  2183,
    2261,  2205,  1154,   892,  2184,   958,  2201,  2203,   960,  2268,
     143,  2213,   148,  2216,   143,   143,   148,   148,   285,  2222,
    2218,  2228,  1105,  1108,  2237,  2239,  2243,  2247,  2249,   147,
     147,   143,     1,   148,   978,  2252,  2253,  2264,    10,  2263,
    1118,   138,   865,  2267,  1755,   138,   138,   921,  1122,   822,
     922,   920,   923,  1731,   516,  1106,  1109,   924,  1543,  1834,
    2248,   927,   138,  2034,    24,  2212,  2194,  1858,  2022,  1020,
    1026,  2187,  2094,  1029,  1030,  1136,  1033,   125,  1035,  1036,
    1969,     2,    24,  1038,  1039,   792,  2173,  1143,  1955,    24,
    1956,  2112,  2220,  2254,   106,  2172,     3,  1603,  2127,  1689,
    1690,   250,  1813,   285,  1691,   330,    49,    50,    51,    52,
      53,   866,   413,   784,    49,    50,    51,    52,    53,    11,
     950,   325,    24,  2069,   641,  1692,  1800,  1641,   822,  1749,
    2192,   111,   951,  1323,   131,  1814,  1894,    12,  1239,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    49,
      50,    51,    52,    53,  1181,   822,  1075,  1764,     5,   822,
     181,  1676,  1185,  1186,   384,  1187,     0,  1110,     0,     0,
       0,   933,   125,   694,    24,    24,     0,   700,     0,     0,
       1,    24,     0,   822,     0,     0,     0,     0,     0,     0,
      24,   745,    33,     0,     0,     0,     0,     0,     0,     0,
      24,     0,    24,   822,     0,     0,   325,     0,     0,   708,
     822,     0,     0,     0,     0,    24,     0,    24,    24,    24,
       0,   285,   285,   740,     0,     0,     0,     0,   755,     2,
       0,     0,     0,     0,   325,  1145,  1146,  1147,     0,     0,
       0,     0,   106,     0,     3,     0,   143,     0,   148,     0,
     795,    24,   285,     0,   598,    24,   741,     0,     0,     0,
      24,     0,     0,   792,     0,   285,     0,     0,  1981,     0,
     637,     0,   744,     0,  1797,     0,  1799,   138,     0,   111,
     112,     0,     0,  1105,  1108,     0,     0,     0,     0,     0,
    2119,     0,     0,     0,   131,     0,     0,  1189,   131,   131,
    1192,  1193,     0,     0,     0,     0,   822,     0,     0,   494,
     125,   495,   496,   497,     0,   131,  1106,  1109,     0,    24,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,    24,     0,     0,     0,    24,     0,     0,     0,     0,
       0,     0,     0,   877,     0,  2160,   498,     0,     0,   499,
     500,    24,     0,     0,   501,   502,     0,     0,     0,     0,
     836,   551,     0,     0,     0,     0,     0,     0,   781,   552,
     553,   554,   555,     0,     0,     0,     0,  1020,     0,     0,
    1245,     0,  1247,     0,     0,     0,  1906,     0,     0,     0,
       0,     0,     0,   836,   125,     0,    24,  1263,     0,     0,
       0,   285,   285,     0,     0,     0,     0,     0,  1360,    12,
       0,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,   330,     0,  1369,     0,     0,  1373,
       0,     0,   847,  1376,     0,    24,     0,     0,   598,     0,
     822,   854,  1297,     0,   822,     0,     0,    54,   143,   143,
     148,   148,     0,   694,   700,     0,     0,     0,     0,   330,
      49,    50,    51,    52,    53,     0,   628,     0,   556,     0,
       0,    24,     0,     0,    24,   979,     0,     0,   982,   138,
     138,     0,     0,     0,     0,   125,   557,     0,     0,     0,
       0,     0,   679,     0,    24,     0,   685,    93,    94,    95,
      96,     0,     0,     0,     0,     0,    24,     0,     0,     0,
       0,   285,     0,     0,     0,    24,     0,   822,     0,  1340,
       0,   822,     0,  1344,     0,   822,     0,  1348,     0,     0,
     131,     0,     0,     0,     0,     0,     0,     0,     0,   382,
     125,    12,     0,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,   895,     0,   637,     0,     0,     0,    12,
    1539,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,  1382,     0,     0,  1063,     0,
     708,  1388,     0,     0,     0,   970,   125,     0,   106,   836,
       0,   694,   700,     0,     0,     0,   755,     0,     0,     0,
     896,  1051,     0,     0,     0,     0,     0,     0,     0,   836,
       0,     0,     0,     0,   640,   108,     0,   836,   641,     0,
       0,     0,     0,     0,     0,   111,   642,     0,   150,     0,
       0,     0,     0,     0,     0,   150,     0,   113,     1,     0,
       0,     0,     0,     0,     0,   125,     0,   822,     0,   125,
     125,   822,     0,  1047,   822,     0,     0,     0,     0,     0,
       0,     0,     0,  1531,     0,     0,   125,     0,  1297,     0,
       0,    12,     0,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,     0,   822,     0,     0,     2,   822,   382,
       0,     0,   822,     0,     0,     0,   150,     0,     0,     0,
       0,     0,     3,     0,     0,   598,     0,   297,     0,     0,
       0,     0,  1470,     0,   150,  1474,  1477,     0,     0,     0,
       0,     0,     0,  1486,  1487,     0,  1296,   108,     0,     0,
     387,     0,   131,   131,   150,     0,    12,   877,   877,     0,
      15,    16,    17,    18,    19,    20,    21,    22,     0,   113,
    1502,   628,  1504,     0,     0,  1507,     0,     0,  1511,   598,
     598,     0,  1515,     0,     0,     0,  1210,   150,   382,   125,
    1210,   150,     0,     0,     0,   836,     0,    12,   708,   349,
     350,    15,    16,    17,    18,    19,    20,    21,    22,     0,
     822,  1296,   108,    30,     0,     0,     0,   297,     0,   986,
       0,     0,  1210,     0,     0,  1537,     0,     0,     0,     0,
       0,  1388,     0,     0,   113,     0,     0,  1002,     0,     0,
       0,     0,     0,     0,     0,     0,   781,     0,  1613,     0,
       0,     0,  1296,   108,  1703,   150,     0,     0,  1264,     0,
       0,  1267,   564,   150,     0,   581,     0,     0,     0,  1045,
       0,     0,     0,  1728,     0,   113,     0,  1223,     0,     0,
     476,     0,     0,   297,     0,  1721,     0,   382,     0,     0,
    1056,     0,     0,     0,     0,     0,   297,   150,     0,   297,
       0,   125,     0,     0,     0,   387,   150,     0,     0,     0,
       0,     0,    12,  1474,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    10,   150,     0,     0,     0,   150,
    1274,     0,     0,   297,     0,   836,   387,     0,     0,   304,
     150,     0,     0,   150,     0,     0,     0,     0,     0,  1274,
       0,     0,     0,     0,     0,     0,  1210,     0,     0,     0,
       0,     0,     0,   708,     0,     0,  1063,     0,     0,     0,
     598,     0,     0,   755,     0,     0,   598,     0,   150,     0,
    1341,     0,     0,     0,  1345,     0,     0,     0,  1349,     0,
       0,     0,  1722,     0,     0,   150,   150,   150,     0,     0,
    1047,     0,     0,     0,  1274,    11,     0,   150,     0,  1820,
    1822,     0,     0,   387,     0,     0,     0,     0,     0,   150,
       0,     0,     0,    12,  1274,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,   813,     0,     0,
     150,     0,     0,     0,     0,     0,     0,   150,     0,     0,
     387,   150,   297,   150,     0,   387,     0,     0,  1883,     0,
       0,   297,     0,     0,   297,     0,   150,   150,     0,     0,
     297,     0,     0,     0,   877,     0,   877,     0,     0,   297,
       0,     0,   150,   150,   150,   297,     0,  1771,   297,     0,
       0,   708,     0,     0,     0,   387,     0,     0,     0,     0,
       0,     0,  1210,   125,   125,     0,     0,     0,   304,  1274,
       0,     0,  1786,    12,     0,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,  1537,  1537,  1537,     0,  1796,
     238,     0,  1798,   258,    41,    42,    43,    44,    45,    46,
      47,    48,  1231,     0,     0,   304,     0,     0,     0,     0,
       0,     0,     0,   986,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,   387,     0,     0,     0,     0,
       0,     0,  1954,  1471,     0,     0,     0,     0,     0,     0,
       0,  1873,     0,   297,   150,     0,   387,     0,     0,     0,
       0,  1275,     0,  1712,    12,     0,  1045,  1884,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,   304,   297,
       0,  1891,     0,     0,   150,     0,  1508,     0,     0,  1512,
       0,  1897,  1898,  1516,     0,     0,     0,  1274,  1274,     0,
       0,   581,   150,     0,   994,   297,     0,     0,     0,     0,
     387,   150,     0,     0,   297,   598,   387,   986,     0,     0,
     150,     0,     0,     0,     0,     0,   745,   150,    12,     0,
     349,   350,    15,    16,    17,    18,    19,    20,    21,    22,
       0,  1274,     0,     0,     0,     0,     0,     0,     0,   265,
       0,   297,   150,     0,     0,     0,   387,   106,     0,  1614,
       0,   813,     0,   150,     0,   150,     0,   387,     0,     0,
       0,   297,     0,   150,   708,     0,     0,     0,   150,   150,
     385,  1210,     0,   762,   108,   387,  1210,  1210,  1210,     0,
       0,     0,   408,   412,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,   297,     0,   113,     0,     0,     0,
       0,  1976,  1231,     0,     0,     0,  1977,  1978,  1979,    12,
       0,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,  1115,     0,  1995,     0,     0,     0,   461,     0,     0,
     150,     0,    12,     0,   577,   578,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,   822,     0,   297,   297,
       0,  1709,     0,     0,   297,   304,     0,     0,     0,     0,
       0,     0,  1726,     0,     0,     0,     0,  1651,     0,     0,
       0,     0,   150,     0,     0,     0,     0,     0,  2056,  2057,
       0,   304,     0,     0,     0,     0,     0,  2063,     0,  1714,
     109,     0,     0,  2067,  2068,   387,     0,  1231,     0,     0,
       0,   150,   579,     0,   150,     0,     0,     0,     0,     0,
     387,   598,     0,     0,   150,   150,   304,     0,     0,     0,
       0,     0,     0,   150,     0,   385,     0,   297,     0,     0,
       0,     0,     0,     0,   390,   402,     0,     0,   150,     0,
       0,     0,     0,     0,  2214,     0,     0,     0,   150,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,     0,
    2131,     0,     0,     0,     0,   304,     0,   304,     0,     0,
    2136,     0,     0,     0,     0,     0,     0,  1210,     0,  1210,
       0,     0,  2236,     0,  2145,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,    12,     0,   349,   350,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,   150,
       0,   150,     0,     0,     0,   150,  2145,     0,     0,     0,
       0,  2063,     0,     0,     0,     0,   150,   150,     0,     0,
       0,     0,     0,   385,     0,  1231,     0,  2189,     0,   150,
       0,     0,  1876,  1877,     0,   297,     0,     0,     0,     0,
       0,     0,     0,     0,   150,     0,     0,     0,   150,     0,
       0,     0,   150,     0,   150,   579,   304,   297,     0,   297,
     385,     0,   708,   994,     0,   385,     0,     0,     0,     0,
     387,   150,   150,     0,   795,     0,     0,     0,    12,     0,
     349,   350,    15,    16,    17,    18,    19,    20,    21,    22,
    2225,     0,   150,     0,     0,     0,     0,     0,     0,   150,
       0,     0,     0,  1153,     0,   385,     0,   106,     0,  1231,
     150,   994,     0,     0,     0,     0,   708,     0,     0,  2245,
       0,     0,  2225,     0,     0,  1931,   150,     0,     0,     0,
       0,     0,  1937,  2115,   108,     0,     0,   744,     0,     0,
       0,   598,     0,  2245,   111,   112,   150,     0,     0,     0,
       1,     0,   445,     0,   448,     0,   113,   451,   454,     0,
       0,     0,     0,     0,     0,  1962,     0,     0,   463,   464,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
       0,     0,   598,    12,     0,   451,   451,    15,    16,    17,
      18,    19,    20,    21,    22,   150,   385,     0,     0,     2,
       0,     0,     0,     0,     0,   150,     0,     0,     0,     0,
       0,   304,     0,     0,     3,     0,     0,  1996,  1997,     0,
       0,     0,     0,     0,  2007,     0,     0,   451,   150,     0,
       0,   150,     0,  2021,     0,   150,    49,    50,    51,    52,
      53,   150,  1985,  2035,   385,  2036,     0,     0,     1,     0,
     385,     0,   451,     0,     0,     0,   385,     0,  2050,     0,
    2052,  2053,  2054,     0,   304,    12,     0,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,    12,     0,   304,     0,    15,    16,    17,    18,    19,
      20,    21,    22,     0,  2080,     0,   385,     2,  2085,   387,
     150,  1231,     1,  2090,   297,     0,     0,   385,     0,     0,
     106,     0,     3,     0,     0,     0,  1450,  1985,  1985,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,   150,
     461,  1716,     0,   150,     0,    12,   107,   108,   304,    15,
      16,    17,    18,    19,    20,    21,    22,   111,   112,   150,
     150,     2,     0,     0,     0,     0,     0,     0,   304,   113,
       0,     0,  2137,     0,   106,     0,     3,     0,     0,     0,
       0,  1114,     0,     0,  2146,  1985,     0,    12,  2149,  1134,
     150,    15,    16,    17,    18,    19,    20,    21,    22,     0,
     107,   108,     0,   150,  2166,     0,     0,     0,  1985,   269,
       0,   111,   112,     0,     0,     0,   150,     0,     0,     0,
     150,     0,     0,   113,   150,     0,     0,     0,     0,     0,
       0,     0,   355,     0,     0,     0,  1527,     0,     0,   385,
    1835,  1836,  2024,  2025,  1837,  1838,  2026,  2027,   150,  2200,
       0,   451,     0,   304,     0,   385,     0,   869,     0,   150,
       0,  1985,  1985,     0,     0,   297,     0,  1933,     0,     0,
     385,     0,     0,     0,     0,     0,  -197,     0,  2190,     0,
       0,     0,  1985,     0,     0,     0,     0,   133,  2221,     0,
       0,     0,     0,     0,   133,     0,     0,     0,     0,     0,
     451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
     451,   451,   451,   451,   451,   451,   451,   451,   451,     0,
     150,     0,  1985,     0,  2242,     0,     0,  2246,     0,     0,
       0,     0,   150,     0,     0,     0,     0,     0,     0,   150,
     150,     0,     0,     0,     0,     0,   150,  2255,     0,     0,
     150,     0,     0,   150,     0,   133,    49,    50,    51,    52,
      53,     0,     0,     0,     0,     0,   288,     0,     0,     0,
       0,   304,   304,   319,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   150,     0,     0,     0,   150,   645,     0,
       0,   150,     0,   396,     0,     0,     0,     0,     0,     0,
      12,     0,   304,     0,    15,    16,    17,    18,    19,    20,
      21,    22,  1207,     0,   150,   304,     0,  1208,     0,  1209,
       0,  2039,     0,   297,     0,     0,   433,     0,     0,     0,
     438,     0,     0,   385,     0,     0,     0,     0,     0,     0,
     385,     0,     0,   451,     0,     0,     0,     0,   297,   297,
       0,     0,     0,   726,   355,    12,   108,   349,   350,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
       0,  1538,     0,     0,   766,     0,     0,   387,   150,     0,
       0,   385,     0,     0,   106,     0,     0,   150,     0,   150,
       0,     0,     0,     0,   548,     0,     0,     0,     0,     0,
       0,  2039,   569,     0,     0,     0,     0,     0,     0,     0,
     351,   108,     0,     0,     0,     0,     0,  1769,     0,     0,
       0,   111,   112,   599,   150,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   766,   617,   623,     0,   629,     0,
       0,     0,     0,     0,     0,   669,     0,     0,     0,     0,
       0,   304,   304,     0,     0,     0,     0,     0,     0,     0,
     297,     0,     0,   150,   680,   150,     0,     0,   680,     0,
     387,     0,   288,     0,     0,     0,     0,     0,     0,   698,
       0,     0,   623,   297,     0,     0,     0,   297,   297,     0,
       0,     0,     0,    49,    50,    51,    52,    53,     0,     0,
       0,   297,     0,     0,   297,     0,     0,     0,   737,     0,
      49,    50,    51,    52,    53,    12,     0,   433,     0,    15,
      16,    17,    18,    19,    20,    21,    22,   387,   764,     0,
       0,     0,     0,   774,   698,   288,   319,   451,     0,     0,
       0,     0,   451,     0,   106,     0,   433,     0,     0,   766,
       0,     0,     0,     0,     0,     0,     0,     0,   433,     1,
     451,   304,   802,     0,     0,     0,     0,   805,     0,   385,
    1488,   108,   806,     0,     0,   150,     1,     0,     0,   818,
       0,   111,   112,     0,     0,     0,   433,     0,     0,     0,
     833,   569,    12,   113,   349,   350,    15,    16,    17,    18,
      19,    20,    21,    22,     0,   451,   387,   150,     2,    12,
       0,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,   106,     0,     3,   569,     2,    12,     0,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,   106,     0,
       3,     0,  1007,     0,     0,     0,   645,   640,   108,     0,
       0,   641,     0,    49,    50,    51,    52,    53,   111,   642,
       0,     0,     0,     0,  2115,   108,     0,     0,   744,     0,
     113,     0,   451,    12,     0,   111,   112,    15,    16,    17,
      18,    19,    20,    21,    22,  1207,     0,   113,     0,   938,
    1208,     0,  1209,     0,     0,     0,  1526,     0,     0,   342,
       0,     0,     0,     0,    12,   387,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,   297,     0,     0,   297,
       0,     0,   617,   629,     0,   766,     0,  1083,     0,   108,
       0,     0,  1417,     0,   150,     0,     0,     0,     0,   355,
     355,     0,     0,     0,     0,     0,     0,     0,   288,     0,
       0,     0,    12,   451,   349,   350,    15,    16,    17,    18,
      19,    20,    21,    22,  1124,   766,   766,     0,     0,     0,
       0,   680,     0,     0,   996,     0,     0,     0,   766,     0,
     623,   106,     0,   617,     0,     0,     0,     0,     0,  1003,
       0,     0,     0,  2099,   387,     0,   818,     0,     0,     0,
    1019,     0,     0,     0,     0,     0,     0,  1303,   108,     0,
       0,     0,     0,     0,   150,     0,     0,   387,   111,  1304,
    1044,   629,     0,     0,     0,     0,     0,  1049,     0,     0,
     113,     0,   288,     0,   288,     0,     0,     0,     0,     0,
     569,     0,   680,     0,     0,     0,     0,   623,   569,    49,
      50,    51,    52,    53,     0,     0,     0,   451,   451,   451,
      12,     0,   349,   350,    15,    16,    17,    18,    19,    20,
      21,    22,     0,  2161,   451,   451,    12,     0,     0,     0,
      15,    16,    17,    18,    19,    20,    21,    22,  1207,   106,
       0,     0,     0,  1208,     0,  1209,     0,     0,     0,     0,
       0,   451,     0,     0,     0,     0,     0,     0,     0,   818,
       0,     0,     0,     0,     0,   351,   108,     0,     0,  1125,
       0,   150,   150,     0,     0,     0,   111,   112,     0,     0,
       0,     0,   108,   617,     0,  1623,     0,   385,   113,   475,
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
       0,   433,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,    12,   150,   810,   811,    15,    16,
      17,    18,    19,    20,    21,    22,     0,   106,     0,     0,
     818,     0,     0,   698,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   698,     0,     0,     1,   451,     0,   451,
       0,     0,     0,   107,   108,     0,   569,  1007,     0,     0,
       0,     0,     0,     0,   111,   112,     0,   623,     0,  1298,
       0,   645,   109,   645,     0,     0,   113,   818,  1188,    12,
     385,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,   144,     0,     0,     0,     0,     0,   106,   144,
       3,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   351,   108,     0,   385,   288,     0,
     133,     0,     0,     0,  1232,   111,   112,     0,     0,     0,
       0,     0,     0,     0,     0,   680,   818,   113,  1244,     0,
       0,     0,   766,     0,     0,     0,     0,     0,   486,     0,
     144,   109,   201,     0,     0,  1362,     0,   766,   766,     0,
       0,   292,     0,   818,     0,     0,     0,   818,     0,     0,
       0,   288,     0,   680,     0,     0,  1044,     0,   629,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   397,     0,
     288,   818,     0,     0,     0,     0,   385,     0,     0,     0,
       1,     0,  1295,     0,     0,     0,     0,     0,     0,     0,
       0,   818,     0,   475,     0,     0,   475,     0,   818,     0,
       0,   144,   475,     0,     0,     0,     0,     0,     0,   680,
       0,   475,     0,    12,     0,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,   288,     0,     0,     0,     2,
       0,     0,     0,     0,     0,   475,   451,     0,     0,     0,
       0,     0,   106,   766,     3,   288,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,    12,     0,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,  2115,   108,
       0,     0,   744,     0,     0,   385,     0,   570,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,   926,     0,
       0,   113,     0,     0,   818,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1232,     0,     0,   149,  1485,     0,
     292,   639,     0,   630,   149,  1007,     1,   645,     0,     0,
     397,     0,     0,     0,     0,     0,     0,   569,     0,     0,
     288,     0,     0,   451,   818,     0,     0,     0,     0,   630,
       0,     0,     0,   630,     0,     0,     0,   292,     0,    12,
       0,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,     0,   106,     0,
       3,     0,     0,     0,     0,     0,   296,   385,     0,     0,
       0,     0,   144,     0,     0,     0,     0,     0,     0,  1232,
       0,     0,     0,     0,   351,   108,     0,     0,     0,     0,
     292,     0,     0,   399,     0,   111,   112,     0,     0,     0,
       0,   144,     0,     0,     0,     0,     0,   113,   818,     0,
       0,     0,   818,   144,     0,     0,     0,   803,     0,     0,
       0,     0,     0,     0,     0,     0,   149,     0,   288,   288,
       0,     0,   766,     0,   570,     0,     0,     0,     0,     0,
     774,   144,     0,     0,     0,   397,   292,     0,     0,     0,
       0,  1490,     0,     0,     0,     0,     0,     0,    12,   288,
     349,   350,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,   288,     0,     0,     0,     0,     0,     0,   292,
    1007,  1298,   645,   645,     0,   818,     0,   106,     0,   818,
       0,     0,     0,   818,     0,     0,     0,     0,     0,    12,
    1129,  1131,   572,    15,    16,    17,    18,    19,    20,    21,
      22,  1207,     0,  1804,   108,     0,  1208,  1232,  1209,     0,
    1805,     0,     0,     0,   111,   112,     0,     0,   818,     0,
       0,     0,     0,     0,  1600,   296,   113,     0,   632,     0,
       0,     0,     0,     0,   451,   399,     0,     0,     1,     0,
       0,     0,     0,     0,     0,   108,     0,     0,  1625,     0,
       0,     0,     0,     0,   632,     0,     0,     0,   632,     0,
       0,     0,   296,     0,     0,     0,     0,   292,   630,     0,
       0,    12,     0,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     2,     0,     0,
       0,  1232,     0,   292,     0,     0,     0,     0,   288,   288,
     106,     0,     3,     0,     0,   818,     0,   149,     0,   818,
       0,     0,   818,     0,     0,     0,   630,     0,     0,   397,
       0,     0,     0,     0,     0,   296,   762,   108,   292,     0,
       0,     0,     0,     0,   630,     0,   149,   111,   112,     0,
       0,   570,   818,     0,     0,     0,   818,     0,   149,   113,
     818,     0,     0,     0,     0,     0,     0,     0,   451,     0,
       0,     0,     0,     0,     0,   292,   630,     0,     0,   572,
       0,     0,     0,  1693,  1811,     0,   149,   570,     0,   570,
     399,   296,  1600,     0,     0,   292,     0,   630,     0,     0,
       0,     0,     0,   292,     0,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,     0,   451,   288,     0,
       0,     0,     0,    12,   296,   276,   104,    15,    16,    17,
      18,    19,    20,    21,    22,    12,     0,   808,   104,    15,
      16,    17,    18,    19,    20,    21,    22,  1750,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   818,     0,
       0,     0,     0,    12,   570,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     1,     0,     0,   108,
       0,     0,  1062,   451,     0,     0,     0,     0,   292,  1811,
       0,  1811,     0,  1232,   928,     0,  1913,     0,  1811,     0,
       0,     0,     0,     0,     0,     0,   144,     0,   451,    12,
     451,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,   109,   296,   632,     0,     2,     0,     0,     0,     0,
       0,     0,   698,     0,     0,   570,     0,     0,   106,     0,
       3,     0,     0,     0,     0,     0,     0,     0,   296,     0,
     451,     0,  1829,     0,     0,     0,  1829,  1829,     0,     0,
       0,   292,     0,     0,   762,   108,     0,     0,     0,     0,
       0,   632,     0,  1829,   399,   111,   112,     0,     0,     0,
       0,     0,   570,   296,     0,     0,     0,   113,     0,   632,
       0,     0,     0,     0,     0,     0,   572,     0,     0,     0,
       0,     0,     0,     0,  1994,     0,     0,  1811,  1811,   503,
     504,   505,   506,   507,   508,   509,   510,   511,   512,   513,
     296,   632,   451,     0,     0,     0,     0,     0,   334,     1,
       0,     0,   572,     0,   572,     0,     0,     0,     0,     0,
     296,     0,   632,   570,     0,   144,     0,     0,   296,   630,
       0,     0,  1915,     0,     0,   514,     0,     0,     0,     0,
     630,   570,    12,     0,   349,   350,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,  1928,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,  1811,   570,     0,
       0,   106,   570,     3,     0,     0,   570,     0,   630,     0,
       0,   292,     0,   630,     0,     0,     0,     0,     0,   572,
       0,     0,     0,     0,     0,   570,   570,  1303,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,  1304,
       0,   766,     0,   296,     0,     0,   570,     0,     0,     0,
     113,     0,     0,   570,     0,     0,     0,     0,     0,     0,
       0,   149,     0,     0,   630,    12,     1,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,     0,     0,     0,
     570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     572,     0,     0,     0,  1523,  2017,     0,     0,  1829,    12,
     570,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,  2040,     0,     2,   296,  1124,   766,     0,
      49,    50,    51,    52,    53,     0,     0,     0,   106,     0,
       3,     0,     0,     0,     0,     0,     0,   572,  2062,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   570,
       0,     0,     0,     0,  1804,   108,     0,     0,     0,   630,
       0,     0,     0,     0,     0,   111,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   113,   766,   766,
       0,     0,   570,   294,     0,   570,     1,     0,     0,   570,
       0,     0,     0,  2040,     0,     0,     0,     0,   572,     0,
     149,     0,     0,     0,   632,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   632,   572,     0,     0,    12,
       0,   349,   350,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,   572,     0,     0,     0,   572,   106,     0,
       3,   572,     0,   632,   630,     0,   296,    12,   632,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,     0,
     572,   572,  2176,     0,  1303,   108,     0,     0,     0,     0,
       0,     0,     0,   570,     0,   111,  1304,   570,     0,     0,
       0,   572,     0,     0,     0,     0,     0,   113,   572,     0,
    1829,  1829,     0,   570,   570,     0,     0,     0,     0,   632,
     475,     0,   689,     0,    49,    50,    51,    52,    53,     0,
       0,     0,     0,     0,     0,   572,     0,     0,     0,     0,
       0,     0,     0,     0,   570,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   818,   572,     0,   570,     0,     0,
       0,     0,   294,     0,     0,   631,     0,     0,     0,     0,
     570,     0,   670,     0,   570,     0,     0,     0,   570,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       1,   631,     0,     0,     0,   631,     0,     0,     0,   294,
       0,     0,   630,     0,   572,     0,     0,     0,     0,     0,
       0,     0,     0,   570,   632,     0,     0,     0,     0,   144,
       0,     0,     0,    12,     0,   349,   350,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,   572,     0,     2,
     572,     0,     0,     0,   572,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     3,  1823,  1831,     0,     0,  1823,
    1842,     0,   294,     0,     0,  1849,     0,     0,     0,  1853,
       0,     0,     0,     0,  1857,     0,  1842,     0,  1804,   108,
       0,     0,     0,     0,     0,     0,   630,     0,     0,   111,
     112,     0,     0,   570,   570,     0,     0,     0,     0,     0,
     570,   113,     0,     0,   570,     0,     0,   570,     0,   632,
       0,     0,     0,     0,     0,     0,     0,     0,   294,     0,
      12,     0,   349,   350,    15,    16,    17,    18,    19,    20,
      21,    22,     0,     0,     0,     0,     0,   570,   572,     0,
       0,   570,   572,     0,     0,   570,     0,     0,     0,   106,
       0,   294,     0,     0,     0,     0,     0,     0,   572,   572,
       0,    12,     0,   349,   350,    15,    16,    17,    18,    19,
      20,    21,    22,     0,  1927,   640,   108,   144,     0,   641,
       0,     0,     0,     0,     0,     0,   111,   642,     0,   572,
     106,     0,     0,     0,  1943,  1945,     0,     0,   113,   643,
       0,     0,   572,   292,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   572,  2115,   108,   117,   572,
     744,     0,     0,   572,  1965,   117,     0,   111,   112,     0,
       0,     0,   397,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,   570,     0,     0,     0,   632,     0,   294,
     631,     0,     0,     0,     0,     0,     0,     0,   572,     0,
       0,     0,     0,     0,   149,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   294,     0,     0,   630,  2005,
       0,     0,     0,  2008,     0,  2010,   117,     0,  2014,  2020,
       0,  1842,     0,     0,   267,     0,  2031,   282,   631,     0,
       0,   670,     0,     0,   117,     0,     0,     0,     0,     0,
     294,     0,     0,     0,     0,     0,   631,   352,     0,     0,
     380,     0,     0,     0,   117,     0,     0,     0,     0,     0,
       0,   632,     0,     0,     0,     0,     0,   144,   572,   572,
       0,   144,   144,     0,     0,   572,     0,   294,   631,   572,
       0,     0,   572,     0,     0,     0,     0,     0,   144,     0,
       0,   436,     0,     0,     0,  2097,     0,   294,     0,   631,
       0,     0,  2104,     0,     0,   294,     0,  2106,  2107,     0,
       0,     0,   572,     0,     0,     0,   572,   471,     0,     0,
     572,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2129,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   149,     0,     0,   436,     0,     0,     0,     0,
       0,     0,     0,   567,     0,     0,  2150,     0,  2153,     0,
       0,  2155,  2157,  2158,     0,     0,     0,     0,   296,  2163,
    2165,     0,     0,   471,   267,   267,     0,     0,     0,     0,
     294,   397,     0,     0,     0,     0,   282,   436,     0,   282,
       0,     0,     0,   644,     0,   665,    12,   399,   349,   350,
      15,    16,    17,    18,    19,    20,    21,    22,   572,     0,
       0,     0,     0,     0,     0,   567,     0,     0,     0,   567,
       0,     0,     0,   282,     0,   106,   380,     0,     0,     0,
     267,     0,     0,   436,     0,     0,     0,  2207,  2209,  2211,
       0,     0,     0,   632,     0,     0,     0,     0,     0,     0,
       0,   762,   108,   294,     0,     0,     0,     0,   352,   352,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,  2230,  2232,  2234,   113,     0,     0,     0,     0,   763,
       0,     0,     0,     0,     0,     0,   567,   117,     0,     0,
       0,     0,     0,   144,     0,    49,    50,    51,    52,    53,
       0,     0,   149,   380,     0,     0,   149,   149,   630,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   763,
       0,   631,   282,   436,     0,   838,     0,     0,     0,     0,
       0,   471,   631,     0,   471,     0,   436,   436,     0,     0,
     471,     0,     0,     0,     0,     0,     0,     0,     0,   471,
       0,     0,   436,   436,   436,   282,     0,     0,   471,     0,
       0,     0,     0,     0,   872,   838,     0,     0,   630,     0,
     631,     0,     0,   294,    12,   631,   349,   350,    15,    16,
      17,    18,    19,    20,    21,    22,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   399,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   631,     0,     0,  1804,
     108,     0,     0,     0,   763,   838,     0,     0,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,   282,   567,   952,   665,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   144,   144,     0,     0,   282,
       0,     0,     0,     0,   436,     0,   267,   267,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   567,     0,   992,     0,     0,     0,     0,     0,
     838,   436,     0,     0,   282,  2018,   665,     0,   149,   570,
       0,   631,     0,     0,     0,     0,     0,   644,     0,     0,
       0,   644,     0,   632,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,   567,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   567,     0,   567,     0,   665,     0,     0,
       0,   282,     0,   567,     0,     0,     0,     0,   436,   567,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
     763,     0,   352,   632,     0,     0,   631,     0,     0,     0,
       0,     0,     0,     0,   352,   352,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   763,
     763,   763,     0,     0,     0,     0,     0,     0,   471,   471,
       0,     0,     0,   763,   282,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2071,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     149,   149,     0,     0,     0,   436,     0,     0,     0,     0,
       0,     0,     0,   436,     0,     0,   182,   282,     0,   183,
       0,   184,   185,     0,   186,     0,     0,     0,   436,     0,
       0,     0,     0,     0,   631,     0,     0,     0,   872,   872,
       0,   187,     0,     0,   572,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,   567,
       0,   117,   106,     0,     0,   436,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   567,   952,   631,   952,
       0,     0,     0,     0,     0,     0,     0,   174,   200,   436,
       0,   109,   201,     0,   174,   471,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
       0,     0,   567,     0,   567,     0,     0,   282,     0,   282,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   567,   644,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   644,     0,  1305,     0,
       0,     0,     0,     0,     0,   174,     0,   182,     0,     0,
     183,     0,   184,   185,     0,   186,     0,     0,     0,     0,
     567,   992,     0,   326,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,   567,     0,     0,     0,
       0,     0,     0,   174,     0,   294,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   567,     0,     0,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,     0,   198,   199,   763,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     763,     0,   763,   763,     0,     0,   326,     0,     0,     0,
       0,     0,     0,     0,     0,   436,     0,     0,     0,   200,
     631,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   567,     0,
       0,   567,   207,     0,   174,     0,     0,     0,     0,     0,
       0,   436,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   872,     0,   872,     0,     0,
       0,     0,   326,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   624,     0,     0,     0,
       0,     0,   655,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   763,   838,
     436,     0,     0,     0,   471,     0,     0,     0,   686,     0,
       0,   952,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   567,
     567,   747,     0,     0,     0,     0,     0,     0,   747,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1305,     0,  1305,  1481,     0,     0,   326,     0,     0,     0,
     567,    49,    50,    51,    52,    53,     0,     0,     0,     0,
       0,     0,     0,   567,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,     0,     0,   183,     0,   184,   185,
       0,   186,   326,     0,     0,     0,     0,     0,   436,     0,
     326,     0,     0,   326,     0,   326,   326,     0,   187,   326,
       0,     0,     0,     0,     0,   471,     0,     0,   326,     0,
     952,   326,   326,   326,     0,     0,     0,   326,     0,     0,
       0,     0,     0,   747,     0,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
       0,   198,   199,     0,     0,     0,     0,   763,     0,   106,
     436,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     631,     0,   436,     0,     0,     0,     0,     0,     0,   567,
     567,     0,     0,     0,     0,   200,     0,     0,   109,   201,
       0,     0,     0,     0,     0,   202,  1482,   112,   203,   204,
     205,   206,     0,     0,     0,  1305,     0,  1305,  1305,     0,
       0,     0,     0,     0,   655,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   326,     0,   155,     0,     0,     0,     0,
     631,     0,   155,     0,   436,   747,   980,     0,   747,   983,
       0,   987,     0,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   655,     0,   471,   282,
     655,   655,     0,     0,     0,     0,     0,   655,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1040,     0,     0,
       0,     0,     0,   155,     0,     0,     0,   380,   117,     0,
       0,     0,     0,     0,     0,     0,     0,   436,     0,     0,
       0,   155,     0,     0,     0,     0,     0,   624,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,     0,     0,     0,     0,     0,     0,     0,
     414,     0,     0,   326,   436,     0,     0,     0,     0,     0,
       0,     0,   747,     0,     0,     0,   747,     0,     0,     0,
       0,     0,     0,     0,   155,     0,     0,     0,   155,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,     0,     0,   436,     0,     0,     0,  1806,
     838,     0,   747,     0,   155,     0,     0,   326,   326,     0,
       0,     0,     0,   471,     0,     0,     0,   471,   471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   471,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,   155,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   380,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,     0,     0,     0,   326,     0,     0,     0,     0,     0,
       0,     0,   326,     0,   155,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   624,     0,     0,
       0,     0,     0,     0,  1806,   436,  1806,   747,   747,     0,
       0,  1806,     0,  1806,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   155,     0,     0,
     155,     0,     0,     0,     0,     0,   838,   436,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   155,     0,     0,     0,     0,
     174,     0,     0,     0,   174,     0,     0,     0,     0,     0,
       0,     0,   155,     0,   155,   987,   655,     0,   655,     0,
     155,     0,     0,     0,   155,     0,     0,     0,   326,     0,
       0,     0,     0,     0,   326,     0,   155,     0,   747,  1265,
       0,   747,  1268,  1982,     0,     0,     0,     0,     0,  1806,
       0,     0,  1806,  1806,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   155,   838,     0,     0,   155,     0,
     155,   655,     0,   655,     0,     0,     0,     0,   155,   471,
       0,   155,     0,   155,   155,   655,     0,   155,     0,     0,
       0,     0,     0,     0,   436,     0,   155,     0,     0,   155,
     155,   155,     0,     0,     0,   155,     0,     0,     0,   987,
       0,     0,     0,     0,     0,     0,     0,     0,  1982,  1982,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1806,     0,     0,   154,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,   747,
       0,     0,     0,   747,     0,     0,     0,     0,     0,     0,
     747,  1342,     0,     0,   747,  1346,     0,     0,   747,  1350,
       0,     0,     0,     0,   436,     0,  2116,   838,  1353,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     747,     0,     0,     0,     0,     0,     0,     0,     0,  1982,
       0,     0,     0,   154,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,     0,     0,     0,     0,     0,     0,     0,
     326,   154,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2116,  2116,   747,     0,   747,     0,   155,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   159,
       0,     0,     0,  1982,   154,     0,   159,     0,   154,     0,
       0,   436,   436,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
       0,     0,     0,   326,   154,     0,     0,     0,     0,     0,
     655,     0,     0,  2116,   763,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   155,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   159,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,   155,     0,   747,  1472,   159,   655,   655,  1479,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     154,     0,     0,     0,     0,   159,     0,     0,     0,     0,
       0,     0,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   747,  1509,     0,   747,
    1513,     0,     0,   747,  1517,   155,   155,     0,   159,     0,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   154,   747,   155,
     154,     0,     0,     0,     0,     0,     0,   174,   159,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,     0,     0,     0,     0,   655,
       0,   155,     0,     0,     0,   154,     0,     0,     0,   747,
    1615,   155,   155,     0,     0,     0,   159,     0,     0,     0,
     155,     0,   154,     0,   154,     0,     0,     0,     0,     0,
     154,     0,     0,     0,   154,   155,     0,     0,     0,     0,
       0,     0,     0,     0,   159,     0,   154,     0,     0,   326,
       0,     0,     0,     0,     0,     0,     0,     0,   159,   182,
       0,   174,   183,     0,   184,   185,     0,   186,     0,     0,
       0,     0,     0,     0,   154,     0,     0,     0,   154,     0,
     154,     0,     0,     0,   187,     0,     0,     0,   154,     0,
       0,   154,     0,   154,   154,     0,     0,   154,     0,     0,
       0,   159,     0,     0,   159,     0,   154,     0,   155,   154,
     154,   154,     0,   188,   189,   154,   703,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   155,   198,   199,   159,
       0,     0,   155,   326,     0,   106,     0,     0,     0,     0,
     268,     0,   174,     0,     0,     0,   159,     0,   159,     0,
     323,     0,     0,     0,   159,     0,     0,     0,   159,     0,
       0,   200,   108,     0,   704,   705,     0,   326,     0,   706,
     159,   202,   111,   112,   203,   204,   205,   206,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   159,     0,
       0,     0,   159,     0,   159,     0,   326,     0,     0,     0,
       0,     0,   159,     0,     0,   159,     0,   159,   159,     0,
       0,   159,     0,     0,     0,     0,     0,     0,     0,     0,
     159,   154,     0,   159,   159,   159,     0,     0,     0,   159,
       0,     0,     0,   174,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   326,
       0,     0,     0,     0,   326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   597,
     600,     0,   174,     0,     0,     0,   174,   174,     0,     0,
       0,     0,     0,     0,     0,   633,     0,     0,     0,     0,
     326,     0,     0,   174,     0,   154,     0,     0,     0,     0,
     262,     0,     0,     0,     0,     0,     0,     0,   155,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   383,     0,     0,     0,   159,     0,     0,     0,     0,
       0,     0,     0,   407,     0,   415,     0,   417,     0,     0,
       0,     0,     0,     0,   725,   738,     0,     0,     0,     0,
       0,     0,   159,     0,   326,     0,     0,     0,     0,     0,
       0,   155,     0,     0,     0,   154,   154,   747,     0,     0,
     775,     0,     0,   600,     0,     0,     0,     0,   459,     0,
       0,     0,     0,     0,     0,     0,   174,     0,     0,   154,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   824,     0,     0,   159,
       0,   154,     0,     0,     0,     0,     0,     0,     0,   837,
       0,   154,   154,     0,     0,     0,     0,   846,     0,     0,
     154,     0,     0,   597,     0,   159,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   154,     0,     0,     0,     0,
       0,     0,   747,     0,   837,     0,     0,     0,     0,   747,
       0,     0,     0,     0,     0,     0,   383,     0,     0,     0,
       0,     0,   672,     0,   417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   159,
     159,     0,     0,     0,     0,     0,     0,   383,     0,   415,
     417,     0,   155,   326,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   159,     0,     0,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,   747,   747,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     747,     0,     0,     0,     0,   159,   154,     0,     0,     0,
     633,     0,   154,     0,     0,   159,   159,   155,     0,     0,
       0,     0,     0,     0,   159,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   383,     0,   415,   417,     0,   159,
       0,     0,     0,   326,     0,   747,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   747,     0,
       0,   383,     0,     0,     0,     0,   383,     0,     0,     0,
       0,     0,     0,   824,     0,     0,     0,  1021,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   155,   159,     0,     0,     0,   383,     0,     0,     0,
     155,   747,   747,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   747,  2191,
     159,     0,   747,     0,     0,   155,   159,     0,     0,     0,
     174,   174,     0,     0,     0,     0,     0,     0,     0,     0,
     597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   155,     0,     0,     0,     0,
       0,     0,   747,     0,   155,     0,   383,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   824,     0,     0,     0,
       0,     0,     0,     0,     0,  1123,  1126,   383,     0,   672,
     417,     0,     0,     0,   597,   597,     0,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   383,     0,   155,     0,     0,
     155,   383,   155,     0,     0,     0,     0,   383,     0,   672,
     417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     155,   154,     0,     0,   155,   155,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   155,     0,
       0,   155,     0,     0,     0,     0,     0,   383,   672,     0,
       0,     0,     0,     0,   824,  1190,     0,     0,   383,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   383,     0,     0,     0,
       0,   459,     0,     0,     0,     0,   233,     0,     0,     0,
       0,     0,   159,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   155,     0,     0,   383,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   824,     0,     0,     0,     0,     0,     0,
       0,     0,   383,     0,   155,   597,     0,     0,     0,     0,
       0,   597,   383,     0,     0,   159,     0,     0,     0,   283,
     824,     0,     0,     0,   824,     0,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
     383,     0,     0,   383,   383,     0,     0,     0,   824,     0,
       0,     0,     0,     0,     0,     0,   383,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   824,     0,
       0,   383,     0,     0,     0,   824,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   154,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   378,     0,     0,     0,     0,     0,     0,     0,     0,
     485,    49,    50,    51,    52,    53,   155,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,  -504,  -504,     0,  -504,    87,     0,    88,     0,     0,
    -504,     0,     0,     0,     0,     0,   159,     0,     0,     0,
       0,   824,     0,     0,     0,   593,     0,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,     0,   616,     0,
     154,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,   104,     0,   154,     0,     0,     0,     0,
       0,   159,     0,     0,     0,   283,     0,     0,     0,     0,
       0,     0,     0,     0,   383,     0,     0,     0,     0,   106,
       0,   383,     0,     0,   709,   154,   709,     0,     0,     0,
       0,     0,     0,     0,   154,     0,     0,   383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   692,
       0,     0,     0,     0,     0,     0,   111,   112,     0,     0,
     597,     0,   383,     0,     0,     0,     0,   383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   824,     0,     0,     0,   824,
       0,     0,     0,     0,     0,   159,     0,     0,   155,   155,
       0,     0,     0,     0,   159,     0,     0,   154,     0,     0,
     154,     0,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   835,     0,     0,     0,  1491,   159,
     154,     0,     0,     0,   154,   154,     0,     0,     0,     0,
       0,     0,     0,   383,   855,     0,     0,     0,   154,     0,
       0,   154,     0,     0,     0,     0,     0,   616,     0,   159,
       0,     0,   824,     0,     0,     0,   824,     0,   159,     0,
     824,     0,     0,     0,     0,     0,     0,     0,   887,     0,
     889,     0,     0,     0,     0,     0,     0,   233,     0,   900,
     233,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   905,     0,   383,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
     233,     0,     0,   930,     0,     0,     0,     0,     0,     0,
       0,   159,     0,     0,   159,     0,   159,     0,     0,     0,
     383,     0,     0,     0,   154,   283,     0,     0,     0,     0,
       0,     0,   383,     0,   159,     0,   597,     0,   159,   159,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   616,   159,     0,     0,   159,     0,     0,     0,     0,
       0,     0,   824,     0,     0,     0,   824,     0,     0,   824,
       0,     0,     0,     0,     0,     0,   993,   995,     0,   283,
       0,     0,     0,   383,     0,     0,   616,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   824,
     283,     0,  1018,   824,     0,     0,     0,   824,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   283,     0,     0,   159,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   154,     0,     0,     0,
       0,     0,     0,   283,     0,     0,     0,     0,     0,     0,
       0,     0,   313,     0,     0,     0,     0,     0,   159,   329,
     835,     0,     0,     0,   485,   709,     0,     0,     0,     0,
       0,   709,     0,     0,     0,   391,   593,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1113,     0,   824,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   440,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   283,     0,     0,     0,
       0,     0,   482,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     159,     0,     0,     0,     0,     0,   597,     0,   383,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   575,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   329,   283,
       0,     0,     0,     0,     0,     0,     0,   597,     0,     0,
       0,   313,     0,     0,   636,     0,     0,     0,  1183,     0,
     668,     0,     0,     0,     0,     0,     0,     0,   154,   154,
     900,     0,     0,   900,   233,  1204,     0,     0,     0,     0,
     682,     0,     0,     0,   688,     0,     0,     0,   313,     0,
       0,   695,     0,   709,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   383,     0,
       0,     0,     0,     0,     0,     0,     0,  1912,     0,  1916,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   313,   329,     0,     0,     0,     0,  1260,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   391,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   616,
       0,     0,     0,     0,     0,  1281,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1961,     0,     0,
       0,     0,   159,   159,     0,     0,     0,   575,   329,     0,
     839,   383,     0,     0,  1708,     0,   329,     0,     0,   482,
       0,   482,   329,     0,     0,   329,     0,     0,     0,     0,
       0,     0,     0,   993,   482,     0,     0,   482,   482,   482,
     575,     0,     0,   329,     0,     0,     0,     0,     0,     0,
     695,   182,     0,     0,   183,     0,   184,   185,   709,   186,
       0,     0,     0,     0,     0,     0,     0,     0,   383,     0,
       0,  1329,     0,     0,  1545,     0,   187,  1547,     0,  1548,
       0,     0,  1549,  1550,  1551,  1552,  1553,  1554,  1555,  1556,
    1557,  1558,  1559,  -355,  -355,  1560,  1561,  1562,  1563,  1564,
    1565,  1566,     0,  1567,     0,   188,   189,     0,   703,   191,
    1568,  1569,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,  1570,     0,   195,   196,   197,     0,   198,
     199,     0,     0,     0,     0,     0,     0,   106,   313,   636,
       0,   953,     0,     0,     0,     0,     0,   383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1571,   313,     0,   109,   201,     0,   329,
       0,   424,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,     0,     0,     0,     0,   709,   989,  -196,   668,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,  1001,     0,     0,     0,     0,  1427,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   593,     0,     0,     0,
       0,     0,     0,   383,     0,     0,   575,   636,  1123,  2177,
       0,     0,     0,     0,     0,     0,   383,     0,   313,     0,
     313,     0,     0,     0,     0,     0,   575,     0,  1058,     0,
       0,     0,     0,     0,   575,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1484,     0,     0,     0,     0,   329,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   824,   709,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   383,   417,     0,     0,     0,
       0,     0,     0,   329,   329,     0,  1524,     0,  1525,   313,
       0,     0,     0,     0,     0,     0,     0,     0,   383,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     383,     0,     0,     0,     0,     0,   182,     0,     0,   183,
       0,   184,   185,     0,   186,     0,     0,     0,     0,   709,
       0,  1427,     0,     0,     0,     0,     0,     0,     0,     0,
     329,   187,     0,     0,     0,     0,     0,     0,   482,     0,
       0,     0,   575,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,  1659,
    1660,     0,   106,     0,     0,     0,     0,     0,    49,    50,
      51,    52,    53,     0,     0,     0,     0,  1835,  1836,     0,
       0,  1837,  1838,     0,   383,     0,     0,     0,   200,  1944,
       0,   109,   201,     0,   313,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
     182,   989,     0,   183,     0,   184,   185,     0,   186,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,     0,
     329,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,  1729,  1729,     0,   313,     0,  1277,
       0,   616,   575,     0,   636,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   313,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,  1754,   198,   199,
    1756,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,   989,   953,     0,     0,     0,
       0,  1835,  1836,     0,     0,  1837,  1838,     0,     0,     0,
       0,   313,   200,  2013,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1790,     0,     0,     0,     0,
       0,     0,   593,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   233,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   709,     0,     0,
       0,     0,     0,   593,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   575,     0,     0,   313,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   482,     0,     0,     0,
       0,     0,     0,    49,    50,    51,    52,    53,     0,     0,
      55,   709,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,  1914,     0,     0,    87,     0,    88,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   329,
       0,     0,     0,     0,     0,     0,     0,  2244,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -873,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,   313,     0,  1958,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,     0,
       0,  -873,     0,     0,  -873,     0,  -873,  -873,     0,  -873,
       0,     0,     0,     3,     0,   313,     0,     0,     0,     0,
       0,     0,     0,     0,  -873,     1,  -873,  -873,   313,  -873,
    -256,  -256,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
    -873,  -873,     0,  -873,     0,  -873,  -873,     0,  -873,  -873,
    -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,     2,  -873,  -873,  -873,     0,  -873,
    -873,     0,     0,     0,     0,     0,     0,  -873,     0,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2244,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -256,  -873,     0,     0,  -873,  -873,  -873,     0,
       0,  -873,     0,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
       0,     0,     0,     0,     0,     0,     0,     0,  -873,     0,
       0,     0,     0,     0,     0,   329,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -873,     0,     0,  -873,     0,
    -873,  -873,     0,  -873,   313,   313,     0,   233,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -873,     1,
    -873,  -873,     0,  -873,  -257,  -257,  -873,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,  -873,  -873,     0,  -873,     0,  -873,
    -873,     0,  -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,  -873,  -873,  -873,  -873,     2,  -873,
    -873,  -873,     0,  -873,  -873,     0,     0,     0,     0,   482,
       0,  -873,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -257,  -873,     0,     0,
    -873,  -873,     0,   482,   313,  -873,     0,  -873,  -873,  -873,
    -873,  -873,  -873,  -873,     0,     0,     0,     0,     0,     0,
       0,     0,  -873,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391,   258,    41,    42,    43,    44,    45,    46,
      47,    48,   482,     0,     0,     0,     0,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,   373,   374,     0,   375,    87,     0,    88,     0,
       0,   376,     0,     0,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   329,     0,     0,     0,     0,
     482,     0,     0,     0,     0,   695,   182,     0,     0,   183,
       0,   184,   185,     0,   186,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,   329,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   391,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,  -479,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,  -479,     0,     0,     0,     0,     0,     0,     0,     0,
     482,     0,     0,     0,     0,     0,     0,     0,   200,   483,
       0,   109,   201,     0,   484,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
       0,   391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,  1544,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,    89,    90,    91,    92,    93,
      94,    95,    96,    97,     0,     0,     0,    98,     0,  1545,
     391,  1546,  1547,     0,  1548,     0,     0,  1549,  1550,  1551,
    1552,  1553,  1554,  1555,  1556,  1557,  1558,  1559,  -355,  -355,
    1560,  1561,  1562,  1563,  1564,  1565,  1566,     0,  1567,   482,
     188,   189,   101,   703,   191,  1568,  1569,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,  1570,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1571,     0,
       0,   109,  1572,     0,     0,     0,   424,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,   482,
       0,     0,   695,  -196,    39,   258,    41,    42,    43,    44,
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
       0,     0,     0,     0,     0,     0,     0,  1835,  1836,     0,
       0,  1837,  1838,     0,     0,     0,     0,     0,   200,  1839,
    1840,   109,  1572,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
       0,     0,     0,  1841,    39,   258,    41,    42,    43,    44,
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
       0,     0,     0,     0,     0,     0,     0,  1835,  1836,     0,
       0,  1837,  1838,     0,     0,     0,     0,     0,   200,  1839,
       0,   109,  1572,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
       0,     0,     0,  1841,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,    97,     0,     0,     0,    98,     0,     0,
       0,    99,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   100,     0,
       0,     0,   101,   102,     0,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   108,
       0,   109,   110,     0,     0,     0,     0,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,   114,   378,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -504,  -504,     0,  -504,    87,     0,
      88,     0,     0,  -504,     0,    49,    50,    51,    52,    53,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,  -504,  -504,     0,  -504,    87,
       0,    88,     0,    12,  -504,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,   108,
       0,   109,   379,     0,     0,     0,  -844,     0,     0,   111,
     112,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,   378,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -504,  -504,     0,  -504,    87,     0,    88,     0,
       0,  -504,     0,     0,     0,     0,     0,     0,     0,     0,
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
     379,     0,     0,     0,     0,     0,     0,   111,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
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
       0,     0,     0,     0,     0,     0,     0,     0,    12,     0,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
       0,     0,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     3,
     816,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1005,   108,  -705,   109,   641,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -504,  -504,     0,  -504,
      87,     0,    88,     0,     0,  -504,     0,     0,     0,     0,
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
     107,   108,     0,   109,   379,     0,     0,     0,  -848,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -504,  -504,     0,  -504,    87,     0,    88,
       0,     0,  -504,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    12,     0,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,   108,     0,
     109,   379,     0,     0,     0,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,    39,   258,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,    89,    90,    91,    92,    93,    94,    95,    96,
      97,     0,     0,     0,    98,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   101,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
       0,   198,   199,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,  1833,   109,  1572,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,    39,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,    89,    90,    91,    92,    93,    94,    95,
      96,    97,     0,     0,     0,    98,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     101,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   109,
    1572,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     279,   615,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     627,   861,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     279,   691,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     279,   949,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     627,  1043,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   483,     0,   109,
     279,   280,   484,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     279,   280,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     279,   691,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     279,   949,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     627,  1043,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     279,   615,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     627,   861,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     627,     0,     0,     0,     0,     0,   202,   834,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     990,     0,     0,     0,     0,     0,   202,   991,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     627,     0,     0,     0,     0,     0,   202,   281,   112,   203,
     204,   205,   206,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,  2078,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,
       0,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,  2110,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,  1228,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,  -503,  -503,     0,  -503,    87,     0,    88,     0,     0,
    -503,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,   103,   104,     0,    87,     0,    88,     0,     0,
       0,     0,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1370,     0,  1228,     0,   109,   110,
       0,     0,   103,   104,     0,     0,   111,   112,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -503,  -503,
       0,  -503,    87,     0,    88,     0,     0,  -503,   109,   278,
      90,    91,    92,    93,    94,    95,    96,     0,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,   103,
     104,     0,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1436,     0,  1228,     0,   109,   110,     0,     0,   103,
     104,     0,     0,   111,   112,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,  -503,  -503,     0,  -503,    87,
       0,    88,     0,     0,  -503,   109,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1529,     0,
    1228,     0,   109,   110,     0,     0,     0,     0,     0,     0,
     111,   112,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,  -503,  -503,     0,  -503,    87,     0,    88,     0,
       0,  -503,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1646,     0,  1228,     0,   109,
     110,     0,     0,     0,     0,     0,     0,   111,   112,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,  -503,
    -503,     0,  -503,    87,     0,    88,     0,     0,  -503,     0,
     278,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1775,     0,  1228,     0,   109,   110,     0,     0,
       0,     0,     0,     0,   111,   112,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,  -503,  -503,     0,  -503,
      87,     0,    88,     0,     0,  -503,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   110,     0,     0,     0,     0,     0,
       0,   111,   112,    49,    50,    51,    52,    53,    54,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,     0,     0,    87,     0,    88,
       0,     0,     0,     0,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    12,     0,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,     0,     0,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,   108,     0,
     109,   316,     0,     0,     0,     0,     0,     0,   111,   112,
       0,     0,    49,    50,    51,    52,    53,    54,     0,    55,
     113,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    12,     0,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     3,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,   108,     0,   109,
       0,     0,     0,     0,     0,     0,     0,   111,   112,     0,
       0,    49,    50,    51,    52,    53,    54,     0,    55,   113,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,    89,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      12,     0,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,   108,     0,   109,   110,
       0,     0,     0,  -846,     0,     0,   111,   112,     0,     0,
      49,    50,    51,    52,    53,    54,     0,    55,   113,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
       0,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,   108,     0,   109,   110,     0,
       0,     0,     0,     0,     0,   111,   112,     0,     0,    49,
      50,    51,    52,    53,     0,     0,    55,   113,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,  -504,
    -504,     0,  -504,    87,     0,    88,     0,     0,  -504,     0,
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
       0,     0,     0,   107,   108,     0,   109,   692,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,    39,   258,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,     0,
       0,    87,     0,    88,     0,     0,     0,     0,    89,    90,
      91,    92,    93,    94,    95,    96,    97,     0,     0,     0,
      98,     0,     0,     0,     0,     0,     0,     0,  -423,  -423,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -423,     0,     0,     0,   109,   110,     0,     0,     0,     0,
       0,     0,   111,   112,    39,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,    97,     0,     0,     0,    98,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   110,     0,  1685,     0,  1686,     0,     0,   111,
     112,  1687,     0,     0,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,  1688,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   850,     0,     0,     0,     0,     0,     0,   111,
     112,   378,   258,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,  -504,  -504,     0,  -504,    87,     0,    88,     0,     0,
    -504,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   103,   104,    87,     0,    88,     0,     0,     0,
       0,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   379,
       0,   103,   104,     0,     0,     0,   111,   112,     0,     0,
       0,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       3,   816,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   871,     0,  -705,   109,   744,     0,
       0,     0,     0,     0,     0,   111,   112,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     3,   816,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     950,     0,  -705,   109,   641,     0,     0,     0,     0,     0,
       0,   111,   112,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,  1271,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -715,   109,
     777,     0,     0,     0,     0,     0,     0,   111,   112,   258,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,   278,
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
       0,     0,     0,     0,   565,   109,   566,     0,     0,     0,
       0,     0,     0,   111,   112,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   777,   778,     0,     0,     0,     0,     0,   111,
     112,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,  1650,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   777,     0,
       0,     0,     0,     0,     0,   111,   112,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,  1652,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   777,     0,     0,     0,     0,     0,
       0,   111,   112,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     664,     0,     0,     0,     0,     0,     0,   111,   112,   258,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
       0,     0,    87,     0,    88,     0,     0,     0,     0,   278,
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
       0,     0,     0,     0,     0,   109,   777,     0,     0,     0,
       0,     0,     0,   111,   112,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,   278,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   566,     0,     0,     0,     0,     0,     0,   111,
     112,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    -504,  -504,     0,  -504,    87,     0,    88,     0,     0,  -504,
       0,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,   103,   104,     0,    87,     0,    88,  1708,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,     0,     0,   183,     0,   184,
     185,     0,   186,     0,     0,     0,     0,   109,   379,   456,
       0,   457,   458,     0,     0,   111,   112,  1545,     0,   187,
    1547,     0,  1548,  1998,  1999,  1549,  1550,  1551,  1552,  1553,
    1554,  1555,  1556,  1557,  1558,  1559,     0,     0,  1560,  1561,
    1562,  1563,  1564,  1565,  1566,     0,  1567,     0,   188,   189,
       0,   703,   191,  1568,  1569,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,  1570,     0,   195,   196,
     197,   -17,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,  1708,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1571,     0,     0,   109,
     201,     0,     0,     0,   424,     0,   202,   111,   112,   203,
     204,   205,   206,     0,   182,     0,     0,   183,     0,   184,
     185,  -196,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1545,     0,   187,
    1547,     0,  1548,     0,     0,  1549,  1550,  1551,  1552,  1553,
    1554,  1555,  1556,  1557,  1558,  1559,     0,     0,  1560,  1561,
    1562,  1563,  1564,  1565,  1566,     0,  1567,     0,   188,   189,
       0,   703,   191,  1568,  1569,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,  1570,     0,   195,   196,
     197,     0,   198,   199,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1571,     0,     0,   109,
     201,     0,     0,     0,   424,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,     0,     0,     0,     0,     0,
       0,  -196,   428,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,     0,     0,    87,     0,    88,     0,
       0,     0,     0,    89,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -426,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   104,     0,   428,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,   109,
      87,     0,    88,     0,  -426,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -427,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
     428,   258,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,   109,    87,     0,    88,     0,  -427,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,  -426,    49,    50,    51,    52,    53,    54,   465,
      55,   466,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,     0,     0,   182,     0,    87,   183,    88,
     184,   185,     0,   186,    89,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   467,     0,     0,     0,  1559,     0,  -355,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,     0,   198,   199,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1571,     0,     0,
     109,   468,     0,     0,     0,   424,     0,   202,   111,   112,
     469,   470,   205,   206,    49,    50,    51,    52,    53,    54,
     465,    55,   466,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   182,     0,    87,   183,
      88,   184,   185,     0,   186,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   467,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   109,   468,     0,     0,     0,   424,     0,   202,   111,
     112,   469,   470,   205,   206,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,    89,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     1,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       2,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,     0,   106,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,   591,   109,   592,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,   182,     0,
      87,   183,    88,   184,   185,     0,   186,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     2,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     3,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   109,   592,     0,     0,     0,   424,     0,
     202,   111,   112,   203,   204,   205,   206,    49,    50,    51,
      52,    53,    54,     0,    55,     0,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,     0,     0,   182,
       0,    87,   183,    88,   184,   185,     0,   186,   278,    90,
      91,    92,    93,    94,    95,    96,     0,     0,     0,     0,
       0,     0,     0,     1,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     2,   195,   196,   197,     0,   198,   199,     0,
       0,     0,     0,     0,     0,   106,     0,     3,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   109,   627,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,    49,    50,
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
       0,     0,   200,     0,     0,   109,   592,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,   182,     0,    87,   183,    88,   184,   185,     0,   186,
      89,    90,    91,    92,    93,    94,    95,    96,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,     0,   198,
     199,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   468,     0,     0,
       0,   424,     0,   202,   111,   112,   203,   204,   205,   206,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   182,     0,    87,   183,    88,   184,   185,     0,
     186,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,     0,
     198,   199,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   592,     0,
       0,     0,   424,     0,   202,   111,   112,   203,   204,   205,
     206,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,   278,    90,    91,    92,    93,    94,    95,    96,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
       0,   198,   199,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   182,     0,    87,   183,    88,   184,
     185,     0,   186,   278,    90,    91,    92,    93,    94,    95,
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
     627,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,   378,   258,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,  -504,  -504,     0,  -504,    87,     0,    88,
       0,     0,  -504,   258,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,     0,     0,   103,   104,    87,     0,    88,     0,
       0,     0,     0,   278,    90,    91,    92,    93,    94,    95,
      96,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,    12,     0,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     3,   816,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -705,   109,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,     0,     0,    87,     0,    88,     0,     0,     0,
       0,   278,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
       0,   619,   104,    15,    16,    17,    18,    19,    20,    21,
      22,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   985,     0,     0,   109,   684,     0,
       0,     0,     0,     0,     0,   111,   112,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     3,   816,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -705,   109,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,     0,     0,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    12,     0,   619,   104,    15,    16,    17,
      18,    19,    20,    21,    22,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   620,     0,     0,     0,     0,     0,     0,   111,
     112,   258,    41,    42,    43,    44,    45,    46,    47,    48,
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
     258,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    55,   109,    56,    57,
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
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     3,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   316,     0,     0,     0,
       0,     0,     0,   111,   112,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,   278,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,   566,     0,     0,     0,     0,     0,     0,
     111,   112,    49,    50,    51,    52,    53,    54,     0,    55,
       0,    56,    57,    58,    59,    60,    61,    62,    63,    64,
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
     850,     0,     0,     0,     0,     0,     0,   111,   112,    49,
      50,    51,    52,    53,    54,     0,    55,     0,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,     0,
       0,     0,     0,    87,     0,    88,     0,     0,     0,     0,
      89,    90,    91,    92,    93,    94,    95,    96,    49,    50,
      51,    52,    53,    54,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,     0,     0,
     103,   104,    87,     0,    88,     0,     0,     0,     0,    89,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   857,   109,   850,     0,   103,
     104,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   985,     0,     0,   109,   620,     0,     0,     0,
       0,     0,     0,   111,   112,    49,    50,    51,    52,    53,
      54,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,   278,    90,    91,    92,
      93,    94,    95,    96,    49,    50,    51,    52,    53,    54,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,     0,     0,   103,   104,    87,     0,
      88,     0,     0,     0,     0,    89,    90,    91,    92,    93,
      94,    95,    96,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,  1395,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   985,
       0,     0,   109,   684,     0,   103,   104,     0,     0,     0,
     111,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,   850,     0,     0,     0,     0,     0,     0,   111,
     112,    49,    50,    51,    52,    53,    54,     0,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,     0,     0,    87,     0,    88,     0,     0,
       0,     0,    89,    90,    91,    92,    93,    94,    95,    96,
      49,    50,    51,    52,    53,    54,     0,    55,     0,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
       0,     0,   103,   104,    87,     0,    88,     0,     0,     0,
       0,    89,    90,    91,    92,    93,    94,    95,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,   435,
       0,   103,   104,     0,     0,     0,   111,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,   110,     0,
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
       0,     0,     0,     0,   109,   435,     0,   103,   104,     0,
       0,     0,   111,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   684,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,   701,     0,   103,   104,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     850,     0,     0,     0,     0,     0,     0,   111,   112,    49,
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
     103,   104,    87,     0,    88,     0,     0,     0,     0,   278,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   664,     0,   103,
     104,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   566,     0,     0,     0,
       0,     0,     0,   111,   112,   258,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -504,  -504,     0,  -504,    87,     0,
      88,     0,     0,  -504,     0,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,   103,   104,    89,    90,    91,
      92,    93,    94,    95,    96,     0,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,   103,   104,     0,
      87,   109,    88,     0,     0,     0,     0,    89,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   316,     0,     0,   103,   104,     0,
       0,   111,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   620,     0,     0,     0,     0,     0,
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
     109,   932,     0,   103,   104,     0,     0,     0,   111,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     850,     0,     0,     0,     0,     0,     0,   111,   112,    49,
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
     103,   104,    87,     0,    88,     0,     0,     0,     0,   278,
      90,    91,    92,    93,    94,    95,    96,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,   692,     0,   103,
     104,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,    49,    50,
      51,    52,    53,   111,   112,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -504,  -504,
       0,  -504,    87,     0,    88,     0,     0,  -504,    49,    50,
      51,    52,    53,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -504,  -504,
       0,  -504,    87,     0,    88,     0,     0,  -504,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,   109,   692,     0,     0,     0,
       0,     0,     0,   111,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,   932,     0,    49,    50,
      51,    52,    53,   111,   112,    55,     0,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,  -504,  -504,
       0,  -504,    87,     0,    88,     0,     0,  -504,    55,     0,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,     0,     0,   182,     0,    87,   183,    88,   184,   185,
       0,   186,     0,     0,     0,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,   109,     0,   195,   196,   197,
       0,   198,   199,   111,   112,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,   109,   201,
    1112,     0,     0,     0,     0,   202,   281,   112,   203,   204,
     205,   206,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,   182,     0,    87,
     183,    88,   184,   185,     0,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,     0,   198,   199,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,    49,    50,    51,    52,
      53,    54,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,     0,   278,    90,    91,
      92,    93,    94,    95,    96,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     3,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,    51,    52,    53,
      54,     0,    55,   109,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,     0,     0,     0,     0,    87,
       0,    88,     0,     0,     0,     0,    89,    90,    91,    92,
      93,    94,    95,    96,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
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
      83,    84,    85,    86,  -504,  -504,     0,  -504,    87,     0,
      88,     0,     0,  -504,    49,    50,    51,    52,    53,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,  -503,  -503,     0,  -503,    87,     0,
      88,     0,     0,  -503,     0,   103,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,   182,     0,
       0,   183,     0,   184,   185,     0,   186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     3,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,  1196,     0,   109,   201,     0,     0,     0,  1197,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,   899,     0,
     109,   201,  1201,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   109,   201,     0,
       0,     0,   706,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   109,   201,     0,     0,     0,   424,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   899,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,   109,   201,
       0,     0,   925,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   109,   201,  1015,     0,     0,
       0,     0,   202,   281,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   109,   201,  1041,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,  1426,     0,   109,
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   201,  1522,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   109,   201,     0,     0,     0,  1718,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
     109,   201,     0,     0,     0,  1772,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,  2004,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,  2009,     0,   109,   201,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,     0,   198,   199,     0,     0,   182,     0,
       0,   183,   106,   184,   185,     0,   186,  2096,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,   200,  2019,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,  2103,     0,
     109,   201,     0,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,  2105,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,  2152,     0,   109,   201,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,  2154,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,  2156,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,  2162,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
    2164,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,  2206,     0,   109,
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,  2208,     0,   109,   201,     0,     0,
       0,     0,     0,   202,   111,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,   182,   198,   199,   183,     0,
     184,   185,     0,   186,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,  2210,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   111,   112,   203,   204,   205,   206,     0,     0,   188,
     189,     0,   190,   191,   103,   104,    15,    16,    17,    18,
      19,    20,    21,    22,   192,   193,   194,     0,     0,   195,
     196,   197,   182,   198,   199,   183,     0,   184,   185,     0,
     186,   106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,  2229,     0,
     109,   201,     0,     0,     0,     0,     0,   202,   111,   112,
     203,   204,   205,   206,     0,     0,   188,   189,     0,   190,
     191,   103,   104,    15,    16,    17,    18,    19,    20,    21,
      22,   192,   193,   194,     0,     0,   195,   196,   197,   182,
     198,   199,   183,     0,   184,   185,     0,   186,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,  2231,     0,   109,   201,     0,
       0,     0,     0,     0,   202,   111,   112,   203,   204,   205,
     206,     0,     0,   188,   189,     0,   190,   191,   103,   104,
      15,    16,    17,    18,    19,    20,    21,    22,   192,   193,
     194,     0,     0,   195,   196,   197,   182,   198,   199,   183,
       0,   184,   185,     0,   186,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,  2233,     0,   109,   201,     0,     0,     0,     0,
       0,   202,   111,   112,   203,   204,   205,   206,     0,     0,
     188,   189,     0,   190,   191,   103,   104,    15,    16,    17,
      18,    19,    20,    21,    22,   192,   193,   194,     0,     0,
     195,   196,   197,   182,   198,   199,   183,     0,   184,   185,
       0,   186,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   444,     0,
       0,   109,   201,     0,     0,     0,     0,     0,   202,   111,
     112,   203,   204,   205,   206,     0,     0,   188,   189,     0,
     190,   191,   103,   104,    15,    16,    17,    18,    19,    20,
      21,    22,   192,   193,   194,     0,     0,   195,   196,   197,
     182,   198,   199,   183,     0,   184,   185,     0,   186,   106,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   447,     0,     0,   109,   201,
       0,     0,     0,     0,     0,   202,   111,   112,   203,   204,
     205,   206,     0,     0,   188,   189,     0,   190,   191,   103,
     104,    15,    16,    17,    18,    19,    20,    21,    22,   192,
     193,   194,     0,     0,   195,   196,   197,   182,   198,   199,
     183,     0,   184,   185,     0,   186,   106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   109,   201,     0,     0,     0,
       0,     0,   202,   111,   112,   203,   204,   205,   206,     0,
       0,   188,   189,     0,   190,   191,   103,   104,    15,    16,
      17,    18,    19,    20,    21,    22,   192,   193,   194,     0,
       0,   195,   196,   197,   182,   198,   199,   183,     0,   184,
     185,     0,   186,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
       0,     0,   109,   201,     0,     0,     0,     0,     0,   202,
     111,   112,   203,   204,   205,   206,     0,     0,   188,   189,
       0,   190,   191,   103,   104,    15,    16,    17,    18,    19,
      20,    21,    22,   192,   193,   194,     0,     0,   195,   196,
     197,   182,   198,   199,   183,     0,   184,   185,     0,   186,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   462,     0,     0,   109,
     201,     0,     0,     0,     0,     0,   202,   111,   112,   203,
     204,   205,   206,     0,     0,   188,   189,     0,   190,   191,
     103,   104,    15,    16,    17,    18,    19,    20,    21,    22,
     192,   193,   194,     0,     0,   195,   196,   197,   182,   198,
     199,   183,     0,   184,   185,     0,   186,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   109,   201,     0,     0,
       0,     0,     0,   202,   281,   112,   203,   204,   205,   206,
       0,     0,   188,   189,     0,   190,   191,   103,   104,    15,
      16,    17,    18,    19,    20,    21,    22,   192,   193,   194,
       0,     0,   195,   196,   197,     0,   198,   199,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   109,   201,     0,     0,     0,     0,     0,
     202,   834,   112,   203,   204,   205,   206,   258,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,     0,     0,     0,     0,
      87,     0,    88,     0,     0,     0,   258,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,   259,     0,   260,   261,    87,
       0,    88,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   456,     0,   457,   458
};

static const yytype_int16 yycheck[] =
{
       3,   200,   257,   394,   351,    31,     9,   450,   337,   377,
       7,   547,    38,    38,  1227,   246,   676,    31,   678,   286,
     351,   640,   246,   351,    38,   717,   498,   473,    31,   501,
      33,   644,  1240,    36,   640,    38,    39,   286,   125,  1571,
     640,   288,   640,   137,   117,   107,   162,   351,   109,    82,
     202,   174,    82,  1587,   107,  1138,   351,  1233,   282,  1998,
    1246,   107,   246,   640,   665,  1208,  1571,  1142,     1,   985,
     640,   351,    30,    99,    99,  1388,    91,  2002,   106,   640,
     351,     1,   351,    31,  1981,    99,     3,   351,  1004,   240,
      38,    95,     9,   872,    97,    98,    99,   706,   107,  1711,
     361,  1981,  1981,   351,   107,    96,    79,   157,   167,   126,
     113,   645,   107,   142,   117,  1727,    33,   651,    31,    36,
     236,   170,   125,   167,   785,    38,   144,   130,   175,   390,
     133,     0,   107,   183,   137,   248,   797,   257,    84,    85,
     107,   402,   308,  1059,   238,   192,   312,   191,  1367,   126,
    1225,    99,   167,    31,   171,   128,  1375,   280,   167,   192,
      38,   190,   192,  1079,   192,   183,    84,    85,   118,   117,
     143,   175,   175,   646,    79,   178,   167,   650,   132,   167,
      97,    98,   130,   656,    31,   133,    99,   723,   288,   137,
     173,    38,   142,  1388,   896,  1371,   118,    68,    69,  1555,
     183,  2140,   175,   326,   207,   130,   268,    31,   125,   182,
     683,   435,   248,  1432,    38,   268,   170,   690,  2115,   169,
     142,    99,   268,   128,   137,   256,   928,   167,   764,   175,
     175,  1444,  1445,  1546,  1846,  2115,  2115,   240,   143,  1744,
     776,   174,  1747,  1748,   468,   248,   167,   192,  1164,  2143,
     286,   435,    99,   173,   174,    95,    31,   175,   175,     0,
     144,   178,   167,    38,   267,   268,   171,   354,  1454,   356,
    2195,   268,   117,   268,   935,    99,   363,   280,  2172,  1422,
    1423,  1424,   285,   286,   468,   167,   114,   115,   172,   351,
     562,    20,   170,   268,   446,   904,   141,  1372,   351,   183,
     499,   268,  2227,  2197,   175,   351,   309,   380,   166,   133,
     313,  1530,   361,   137,   586,   173,   319,   173,   167,   267,
     323,   192,   569,   240,    99,   238,   323,  1939,   771,   772,
    1942,   950,   335,   173,   192,   175,   192,   340,    79,   952,
       9,   390,   170,   567,   347,    82,    82,  1879,   351,   352,
     950,  1546,   167,   402,   441,   442,   351,   748,   419,    96,
    1062,    97,   137,   436,    33,   756,  1282,  1283,   285,   372,
     617,   319,  1455,   950,  1879,   323,   351,   380,   725,   382,
     116,   430,   629,   712,   351,   169,  1005,   128,   391,   173,
     393,   992,   309,   396,   725,   170,   313,   725,   471,  1005,
     403,   279,   143,   173,   352,  1005,  1019,  1005,   663,  1188,
    1326,  1190,   192,   885,   238,   762,   144,   420,   421,   865,
     423,   725,   192,   340,   173,   428,    82,   352,  1005,   432,
     725,   762,   380,   584,   762,  1005,   587,   440,   173,    79,
     664,   665,    98,   192,  1005,   725,    79,   678,   396,   615,
     178,   179,  1638,  1639,   725,   173,   725,   192,   762,     0,
     684,   725,  1842,   576,   173,   382,   424,   762,   692,   569,
     711,  1647,    79,  1007,   391,   173,   393,   725,  1139,   403,
     664,   542,   762,   361,   432,  1841,   403,   490,   128,   492,
    1846,   762,   495,   762,   192,   128,   499,   421,   762,   502,
     684,   624,   169,   143,   421,   102,   173,   432,   692,   423,
     143,  1706,   390,    10,   762,  1710,  1711,   617,  2052,   962,
     686,   128,   192,   440,   402,   968,   169,   167,   173,   629,
     168,   174,  1727,   166,   795,   175,   143,   175,    79,   542,
     576,   138,   182,   663,   547,  1237,   143,   192,   173,   146,
      79,   148,   430,   556,   557,    79,  1764,   173,   561,   167,
     167,   172,   173,  1004,  1944,  1945,   174,   192,   691,   669,
     173,    79,   396,   576,  1110,  1048,   192,   130,   187,  1765,
     680,   584,   499,  1939,   587,   705,   361,   128,  1197,   192,
     156,   157,     3,   190,   160,   161,   599,   600,     9,   128,
     597,   173,   143,   600,   128,   173,   215,   216,   644,   173,
     130,   614,   615,   173,   143,   390,   360,   183,  1059,   143,
     128,    10,    33,   367,   192,    36,   850,   402,    39,   173,
     173,  1244,   192,  2013,  2014,   143,   633,   640,  1079,   556,
     557,   644,   173,   704,   561,   389,   175,   171,   192,   192,
     173,   599,   600,  1313,   173,   430,  2012,   401,   166,   434,
     183,   192,    79,   725,   183,   668,   850,   584,   173,   166,
     587,   674,   725,   282,  2030,    31,  1889,    31,   183,   725,
     177,   639,    38,   641,    38,   182,    97,    98,   566,   720,
     192,   170,   171,   290,   291,  1231,   293,  1303,   295,   171,
     762,   704,   113,   706,   176,  1303,    31,   200,   167,   762,
     169,   128,   191,    38,   125,  1417,   762,     3,   818,   722,
     723,    65,   725,  1164,    68,    69,   143,    71,   725,   178,
     725,   689,     3,  1303,   737,   738,   185,   186,   861,  2095,
    1069,   738,  1303,    99,   702,    99,   795,  1942,    76,   627,
     725,   668,  1019,   840,  1021,   170,  1290,  1291,   725,   762,
     763,   764,   167,   721,   175,   838,   990,   178,   992,    79,
    1019,    79,  1021,   776,    99,   778,  1123,   166,   775,   173,
    1393,   173,    79,   949,   173,   175,   664,   384,   177,   737,
     738,   183,  1123,   182,   945,  1123,   207,  1044,   173,   352,
     175,  1410,  1462,  1463,    79,   925,   990,  1982,   133,    84,
      85,   126,   737,   738,   170,   763,   170,   167,   128,  1123,
     128,   987,    79,   167,   167,   171,  1067,   824,  1123,   240,
     176,   128,   352,   143,   164,   143,   839,   248,   763,   753,
     837,  1282,  1283,  1123,   938,   175,   143,   960,   167,   846,
     173,    79,  1123,   128,  1123,   137,   138,   184,   166,  1123,
     183,   191,   471,   169,  2039,   175,   167,   173,   143,   280,
     167,   128,   182,   177,   285,  1123,   169,  1043,   175,   432,
     179,   174,  1488,    79,   842,  1326,   143,  2062,   141,   892,
    1488,   166,   169,   896,   114,   115,   996,   174,   309,   777,
     128,   904,   313,  1003,  1440,   187,   188,   169,   934,   934,
     167,   520,   432,   881,   171,   143,   884,   795,  1488,   189,
     934,  1623,   839,  1625,  1315,   928,  1294,  1488,   173,   340,
    1161,   934,   128,   936,   169,   966,   347,  1161,   141,   167,
     170,  2116,   945,   171,  1044,   180,   181,   143,   169,  2144,
    2145,   444,   173,  1073,   447,   191,   449,   871,  1199,    31,
     453,  1202,   169,   288,   167,   175,    38,   174,   171,   462,
     166,   382,   465,   466,   467,   178,   179,  1161,  1219,   167,
     391,  1017,   393,  1019,  1168,  1021,   934,     5,   191,   180,
     181,  1683,   403,    79,  1323,    13,    14,    15,    16,    17,
    1443,  2176,  1005,   584,    79,   361,   587,   361,   752,   420,
     421,  1252,   423,   169,  1017,   171,  1019,   428,  1021,   936,
     795,   934,   135,   136,  1021,   938,   169,    99,   945,   440,
     189,   174,  1724,   167,   390,   169,   390,   171,   167,   997,
     998,    79,   128,   787,  1138,   117,   402,  1797,   402,  1799,
     794,    79,   126,   128,   169,   799,   934,   143,  1061,  1062,
     169,  1123,  1317,   977,  1319,   169,  1069,   981,   143,  1072,
    1123,   396,    79,   169,   430,  1162,   430,  1123,    79,   490,
     166,   492,  1313,   172,   495,   169,   169,   934,   499,   173,
     128,   502,   167,   169,    53,    54,   171,    56,   170,   172,
     128,  1221,   174,    62,   597,   143,   703,  1110,   433,  1718,
     934,  1025,   990,   169,   938,   143,   167,   173,  1032,  1077,
    1123,   128,  1125,  1126,   144,   157,  1123,   128,   166,  1126,
    1521,   542,  1232,   167,  1247,  1248,   143,   171,   166,  1233,
     172,   173,   143,   144,  1061,   556,   557,    79,   144,  1152,
     561,   183,  1155,   167,    13,    14,    15,    16,    17,   934,
     167,   139,   140,  1772,   171,   576,   238,  1170,  1284,   173,
    1411,   169,    22,   584,   173,   173,   587,  1125,  1126,  1295,
     528,   529,   530,   531,   737,   738,    13,    14,    15,    16,
      17,   172,   173,  1190,  1197,   792,   128,  1317,  1314,  1319,
    1125,  1126,   167,   614,   615,  1208,  1447,   167,   280,   141,
     763,   143,  1328,   167,  1128,   107,    65,   737,   738,    68,
      69,   167,    71,  1490,  1491,  1138,   167,  1699,  1231,   156,
     157,   175,  1463,   160,   161,   167,   141,    79,  1155,   171,
      95,  1490,  1491,   763,   569,   169,   178,   179,   857,   173,
    1253,   323,   175,   862,   326,    79,   183,   668,   172,   173,
      84,    85,   167,   674,  1351,   192,   171,    79,   169,  1305,
    1386,   880,   173,   178,   179,  1391,     3,  1371,   114,   115,
      13,    14,    15,    16,    17,   144,   128,   114,   115,   361,
     178,   179,   617,  1296,    13,    14,    15,    16,    17,   166,
    1303,   143,  1305,   167,   128,   169,   378,   171,   380,   167,
      31,   722,   192,   171,  1138,   169,   128,    38,   390,   143,
    1233,   169,   167,   169,   169,   167,   171,   173,   167,   171,
     402,   143,   171,  1446,   169,   170,  1253,   167,   167,   141,
     169,   171,   171,   170,   669,   173,    79,   169,  1262,  1465,
    1466,   173,  1266,   846,   141,   167,   169,   172,   430,   171,
     173,  1455,  1388,   169,   436,   167,  1482,   778,  1484,   171,
     173,   174,  1571,   173,  1388,  1616,   178,   179,    99,   175,
     167,   172,   173,   992,   171,  1388,  1389,  1503,  1643,   110,
     167,   178,   179,   191,   171,   128,   169,  1141,   169,   471,
     173,   169,   173,   175,  1645,   173,   175,  1410,   141,  1233,
     143,   169,   133,    79,  1417,   173,   137,   172,   173,  1422,
    1423,  1424,   141,   169,   167,  1339,   175,   173,   839,  1343,
    1433,   169,   169,  1347,   167,  1438,   173,  1440,   171,   795,
     169,   795,   167,   169,   173,   178,   179,   173,   167,  1792,
    1793,  1794,   171,  1489,  1490,  1491,     3,  1804,  1371,   178,
     179,   169,   128,   191,  1537,   173,    13,    14,    15,    16,
      17,   169,  1389,  1804,   169,  1388,  1804,   143,    13,    14,
      15,    16,    17,    18,   169,  1488,  1489,  1490,  1491,   172,
     173,   169,   169,   818,  1491,   173,   173,   169,   169,    79,
    1804,   167,   173,  2039,   169,   171,   169,  1601,   833,  1804,
    1388,    13,    14,    15,    16,    17,  1433,   238,  1706,   169,
    1546,  1438,  1710,  1643,  1804,   936,  1767,  1768,   521,   522,
     523,   171,  1546,  1804,   945,  1804,   167,  1540,   172,   173,
    1804,  1388,  1455,  1659,  1660,    18,  1549,  1371,   128,  1552,
    1553,  1554,   624,  1647,  1557,  1469,  1804,  1560,   279,  1473,
     172,   173,  1476,   143,  1388,  1912,   191,  2178,  1177,  1178,
    1179,  2182,  1125,  1126,   172,   173,  2048,   171,   934,   174,
     934,  1912,   172,   173,  1912,  1194,  1195,   167,   172,   173,
     173,   171,  1506,   174,  1597,   316,  1510,  1600,  1601,  1798,
    1514,  1533,  1534,  1535,   167,  1125,  1126,   169,  1912,   934,
    1851,   172,   173,  1388,   191,  1688,   169,  1912,   169,   691,
    1623,    79,  1625,  1540,    13,    14,    15,    16,    17,   172,
     173,  1455,  1912,  1546,   191,  1552,  1553,  1554,  1641,   141,
    1557,  1912,   169,  1912,  1812,  1813,  1814,   169,  1912,  1890,
    1061,   169,  1600,  1601,  1895,   156,   157,  1706,   379,   160,
     161,  1710,    97,    98,  1912,   167,   172,   173,  1546,   171,
     128,   996,   173,   172,   173,   396,   178,   179,   172,   173,
      18,  1174,   183,   173,   174,   143,    79,   167,  1601,  1692,
      79,   192,   175,  1696,    84,    85,   173,   174,  1612,  1546,
      79,   524,   525,    13,    14,    15,    16,    17,   169,   167,
    1209,  1210,   169,   171,   169,  1718,   526,   527,   169,  1044,
    1747,  1748,  1546,   795,  1641,  1798,    64,    65,    66,    67,
      68,    69,    70,    71,  1647,   128,  1830,  1740,  1741,   128,
      77,  1152,   175,  1998,  1155,  1813,  1814,  1750,   175,   128,
     143,    79,   532,   533,   143,   827,   173,   172,   192,  1170,
      79,   172,   169,   169,   143,   837,   838,  1692,  2115,  1772,
       5,  1546,   173,   169,   167,   173,  1600,  1601,   171,  1696,
     172,   169,   169,   169,  2115,   169,   166,  2115,   167,   861,
     173,   169,   171,  1706,  1797,   166,  1799,  1710,  1711,   169,
     128,  1804,  1750,  1806,    39,   169,   169,   169,   169,   128,
     167,  2115,  1815,   172,  1727,   143,   172,   172,    85,    18,
    2115,   175,   182,  1647,   143,   192,  1601,   548,  1706,   200,
    1833,   141,  1710,  1711,   169,  2115,   172,  1840,   172,   167,
    1871,   172,  1253,   171,  2115,   566,  2115,   169,   167,  1727,
     173,  2115,   171,   169,    89,   169,   169,   167,   169,  1706,
     169,   171,   934,  1710,  1711,   169,   172,  2115,   178,   179,
    1873,   592,  1797,   172,  1799,   166,   169,   169,  1998,   169,
    1727,  1806,  1706,  2138,  1957,  2140,  1710,  1711,    13,    14,
      15,    16,    17,    18,    79,    79,    79,    79,  1815,   134,
     169,   169,    22,  1727,   139,   169,   627,   169,   169,  1912,
     145,   169,  1915,  1916,   149,  1912,   169,  1830,   169,  1916,
     155,   169,  1925,  1926,   169,  2180,  1750,    77,  1959,  1932,
     169,  1706,   167,   169,   169,  1710,  1711,  1546,  1941,   169,
    2040,   423,   169,   128,   128,   128,   128,   169,  1951,   175,
    1953,   166,  1727,   173,   173,   676,   169,   678,   143,   143,
     143,   143,   191,  1966,  2037,  1968,  1969,  1970,   173,   175,
     341,    13,    14,    15,    16,    17,   169,  1388,  1389,   175,
     169,   169,   167,   167,   167,   167,   171,   171,   171,   171,
    1915,  1916,  1995,  2113,   169,   173,   173,  2252,   172,  2002,
     169,    18,   169,  2006,   169,  1829,  1830,   166,  2011,   175,
     166,   175,   247,   175,  2045,   169,   174,   174,  2138,   172,
    2140,   172,  1433,   130,   166,   169,   175,  1438,   175,  1942,
     191,   169,  1388,   169,  1388,   172,  2039,    79,  2041,   172,
     169,   112,  2000,   278,   172,   116,   117,   118,   119,   120,
     121,   122,   123,  2115,   169,  1830,   777,   169,   429,   169,
    2180,   296,  2115,  1388,  1942,   300,   169,  2070,   169,  2115,
    1995,   166,   166,   444,   167,  2033,   447,   166,  1571,  2082,
    2196,   167,   453,  2086,   167,     3,   128,   322,   167,   167,
     167,   462,   167,  2096,   167,  1942,  2212,   168,   167,  2102,
     171,   143,   144,    14,  1928,    13,    14,    15,    16,    17,
     345,   346,  2115,  2116,   174,   486,   174,  1610,  1942,   173,
     355,   169,   192,   358,   359,   169,   166,   362,   166,  1540,
     365,   366,  2252,   368,   175,   370,   166,   173,  1549,  1692,
     169,  1552,  1553,  1554,  2147,   169,  1557,  2178,   383,  1560,
     169,  2182,  2183,   119,   120,   121,   122,   123,   640,   169,
     169,   172,   166,   169,   399,   172,   169,  1942,  2116,   404,
     169,   406,  1692,  2176,  2177,   169,   192,   167,    87,   192,
    2177,   416,    98,  2186,   192,  2216,  1597,   192,   167,   167,
    1546,  2116,  1546,   428,   112,   192,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,  2237,   192,   192,   192,
    2241,   932,    96,   934,   169,   166,   166,   938,   172,  2222,
     172,  1546,  2225,   141,   169,  2256,   174,   169,  2176,  2177,
    1641,  2144,  2145,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  2245,   173,  1797,   167,  1799,   167,   169,   167,
     168,  2176,  2177,  1806,  2257,   167,   174,   192,    82,   166,
     178,   179,   166,  2266,   985,  1874,  2144,  2145,    82,   192,
     166,   753,   190,   192,  2188,  1600,   167,  1797,   192,  1799,
     169,   167,    82,  1004,   112,  1696,  1806,   166,   116,   117,
     118,   119,   120,   121,   122,   123,   124,  2144,  2145,   166,
     171,    72,    82,   183,   192,   183,    13,    14,    15,    16,
      17,   117,    82,   174,   169,   192,  1388,   192,   192,   169,
    2144,  2145,   169,   166,   130,   183,   168,   133,   169,  1740,
    1741,   137,   183,   166,   174,   112,   167,   572,  1059,   173,
    2257,   168,   824,   171,   169,   580,   183,   183,   583,  2266,
    1706,   169,  1706,   172,  1710,  1711,  1710,  1711,  1079,    82,
     192,   168,  1915,  1916,   174,   169,   169,   166,   166,  2144,
    2145,  1727,    79,  1727,   609,   167,   192,   192,    13,   169,
     751,  1706,   472,   192,  1610,  1710,  1711,   535,   759,   871,
     536,   534,   537,  1566,   214,  1915,  1916,   538,  1386,  1710,
    2227,   541,  1727,  1950,  1815,  2172,  2140,  1727,  1942,   644,
     645,  2130,  2014,   648,   649,   786,   651,  1138,   653,   654,
    1878,   128,  1833,   658,   659,  1750,  2112,   798,  1864,  1840,
    1864,  2037,  2183,  2241,   141,  2111,   143,  1389,  2047,  1537,
    1537,    90,  1995,  1164,  1537,   117,    13,    14,    15,    16,
      17,    18,   146,   386,    13,    14,    15,    16,    17,    94,
     167,   267,  1873,  1995,   171,  1537,  1688,  1451,   950,  1597,
    2136,   178,   179,  1069,  1546,  1995,  1770,   112,   945,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    13,
      14,    15,    16,    17,    18,   977,   713,  1629,     0,   981,
      36,  1525,   871,   871,  1829,   871,    -1,   742,    -1,    -1,
      -1,  1232,  1233,   319,  1925,  1926,    -1,   323,    -1,    -1,
      79,  1932,    -1,  1005,    -1,    -1,    -1,    -1,    -1,    -1,
    1941,   766,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1951,    -1,  1953,  1025,    -1,    -1,   352,    -1,    -1,   335,
    1032,    -1,    -1,    -1,    -1,  1966,    -1,  1968,  1969,  1970,
      -1,  1282,  1283,  2116,    -1,    -1,    -1,    -1,   362,   128,
      -1,    -1,    -1,    -1,   380,   810,   811,   812,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,  1942,    -1,  1942,    -1,
     396,  2002,  1313,    -1,   266,  2006,  2116,    -1,    -1,    -1,
    2011,    -1,    -1,  1928,    -1,  1326,    -1,    -1,   167,    -1,
     282,    -1,   171,    -1,  1686,    -1,  1688,  1942,    -1,   178,
     179,    -1,    -1,  2176,  2177,    -1,    -1,    -1,    -1,    -1,
    2041,    -1,    -1,    -1,  1706,    -1,    -1,   872,  1710,  1711,
     875,   876,    -1,    -1,    -1,    -1,  1128,    -1,    -1,   130,
    1371,   132,   133,   134,    -1,  1727,  2176,  2177,    -1,  2070,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1388,    -1,    -1,
      -1,  2082,    -1,    -1,    -1,  2086,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   477,    -1,  2096,   167,    -1,    -1,   170,
     171,  2102,    -1,    -1,   175,   176,    -1,    -1,    -1,    -1,
     435,    64,    -1,    -1,    -1,    -1,    -1,    -1,   380,    72,
      73,    74,    75,    -1,    -1,    -1,    -1,   952,    -1,    -1,
     955,    -1,   957,    -1,    -1,    -1,  1798,    -1,    -1,    -1,
      -1,    -1,    -1,   468,  1455,    -1,  2147,   972,    -1,    -1,
      -1,  1462,  1463,    -1,    -1,    -1,    -1,    -1,  1119,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,   436,    -1,  1137,    -1,    -1,  1140,
      -1,    -1,   444,  1144,    -1,  2186,    -1,    -1,   450,    -1,
    1262,   453,  1017,    -1,  1266,    -1,    -1,    18,  2144,  2145,
    2144,  2145,    -1,   599,   600,    -1,    -1,    -1,    -1,   471,
      13,    14,    15,    16,    17,    -1,   282,    -1,   171,    -1,
      -1,  2222,    -1,    -1,  2225,   609,    -1,    -1,   612,  2144,
    2145,    -1,    -1,    -1,    -1,  1546,   189,    -1,    -1,    -1,
      -1,    -1,   308,    -1,  2245,    -1,   312,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,  2257,    -1,    -1,    -1,
      -1,  1572,    -1,    -1,    -1,  2266,    -1,  1339,    -1,  1094,
      -1,  1343,    -1,  1098,    -1,  1347,    -1,  1102,    -1,    -1,
    1942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,
    1601,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,   567,    -1,    -1,    -1,   112,
    1382,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,  1150,    -1,    -1,   704,    -1,
     706,  1156,    -1,    -1,    -1,   597,  1647,    -1,   141,   664,
      -1,   737,   738,    -1,    -1,    -1,   730,    -1,    -1,    -1,
     171,   676,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,
      -1,    -1,    -1,    -1,   167,   168,    -1,   692,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    38,    -1,   190,    79,    -1,
      -1,    -1,    -1,    -1,    -1,  1706,    -1,  1469,    -1,  1710,
    1711,  1473,    -1,   665,  1476,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1374,    -1,    -1,  1727,    -1,  1243,    -1,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,  1506,    -1,    -1,   128,  1510,  1750,
      -1,    -1,  1514,    -1,    -1,    -1,    99,    -1,    -1,    -1,
      -1,    -1,   143,    -1,    -1,   717,    -1,   110,    -1,    -1,
      -1,    -1,  1287,    -1,   117,  1290,  1291,    -1,    -1,    -1,
      -1,    -1,    -1,  1298,  1299,    -1,   167,   168,    -1,    -1,
     133,    -1,  2144,  2145,   137,    -1,   112,   871,   872,    -1,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,   190,
    1325,   567,  1327,    -1,    -1,  1330,    -1,    -1,  1333,   771,
     772,    -1,  1337,    -1,    -1,    -1,   892,   170,  1829,  1830,
     896,   174,    -1,    -1,    -1,   850,    -1,   112,   904,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
    1612,   167,   168,  1368,    -1,    -1,    -1,   200,    -1,   615,
      -1,    -1,   928,    -1,    -1,  1380,    -1,    -1,    -1,    -1,
      -1,  1386,    -1,    -1,   190,    -1,    -1,   633,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,  1403,    -1,
      -1,    -1,   167,   168,  1545,   238,    -1,    -1,   972,    -1,
      -1,   975,   245,   246,    -1,   248,    -1,    -1,    -1,   665,
      -1,    -1,    -1,  1564,    -1,   190,    -1,   932,    -1,    -1,
    1571,    -1,    -1,   266,    -1,    85,    -1,  1928,    -1,    -1,
     686,    -1,    -1,    -1,    -1,    -1,   279,   280,    -1,   282,
      -1,  1942,    -1,    -1,    -1,   288,   289,    -1,    -1,    -1,
      -1,    -1,   112,  1468,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    13,   308,    -1,    -1,    -1,   312,
     985,    -1,    -1,   316,    -1,   990,   319,    -1,    -1,   110,
     323,    -1,    -1,   326,    -1,    -1,    -1,    -1,    -1,  1004,
      -1,    -1,    -1,    -1,    -1,    -1,  1062,    -1,    -1,    -1,
      -1,    -1,    -1,  1069,    -1,    -1,  1072,    -1,    -1,    -1,
     962,    -1,    -1,  1087,    -1,    -1,   968,    -1,   361,    -1,
    1094,    -1,    -1,    -1,  1098,    -1,    -1,    -1,  1102,    -1,
      -1,    -1,   192,    -1,    -1,   378,   379,   380,    -1,    -1,
     992,    -1,    -1,    -1,  1059,    94,    -1,   390,    -1,  1700,
    1701,    -1,    -1,   396,    -1,    -1,    -1,    -1,    -1,   402,
      -1,    -1,    -1,   112,  1079,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,   420,    -1,    -1,
     423,    -1,    -1,    -1,    -1,    -1,    -1,   430,    -1,    -1,
     433,   434,   435,   436,    -1,   438,    -1,    -1,  1749,    -1,
      -1,   444,    -1,    -1,   447,    -1,   449,   450,    -1,    -1,
     453,    -1,    -1,    -1,  1188,    -1,  1190,    -1,    -1,   462,
      -1,    -1,   465,   466,   467,   468,    -1,  1642,   471,    -1,
      -1,  1197,    -1,    -1,    -1,   478,    -1,    -1,    -1,    -1,
      -1,    -1,  1208,  2144,  2145,    -1,    -1,    -1,   279,  1164,
      -1,    -1,  1667,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,  1680,  1681,  1682,    -1,  1684,
    1685,    -1,  1687,     4,     5,     6,     7,     8,     9,    10,
      11,    12,   938,    -1,    -1,   316,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   548,    -1,    -1,    -1,    -1,
      -1,    -1,  1863,  1287,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1736,    -1,   566,   567,    -1,   569,    -1,    -1,    -1,
      -1,   987,    -1,   192,   112,    -1,   992,  1752,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,   379,   592,
      -1,  1766,    -1,    -1,   597,    -1,  1330,    -1,    -1,  1333,
      -1,  1776,  1777,  1337,    -1,    -1,    -1,  1282,  1283,    -1,
      -1,   614,   615,    -1,   617,   618,    -1,    -1,    -1,    -1,
     623,   624,    -1,    -1,   627,  1237,   629,  1043,    -1,    -1,
     633,    -1,    -1,    -1,    -1,    -1,  1811,   640,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,  1326,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,   664,   665,    -1,    -1,    -1,   669,   141,    -1,  1403,
      -1,   674,    -1,   676,    -1,   678,    -1,   680,    -1,    -1,
      -1,   684,    -1,   686,  1410,    -1,    -1,    -1,   691,   692,
     133,  1417,    -1,   167,   168,   698,  1422,  1423,  1424,    -1,
      -1,    -1,   145,   146,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   717,    -1,   190,    -1,    -1,    -1,
      -1,  1896,  1138,    -1,    -1,    -1,  1901,  1902,  1903,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   744,    -1,  1918,    -1,    -1,    -1,   190,    -1,    -1,
     753,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,  2188,    -1,   771,   772,
      -1,  1549,    -1,    -1,   777,   566,    -1,    -1,    -1,    -1,
      -1,    -1,  1560,    -1,    -1,    -1,    -1,  1462,    -1,    -1,
      -1,    -1,   795,    -1,    -1,    -1,    -1,    -1,  1973,  1974,
      -1,   592,    -1,    -1,    -1,    -1,    -1,  1982,    -1,   192,
     170,    -1,    -1,  1988,  1989,   818,    -1,  1233,    -1,    -1,
      -1,   824,   182,    -1,   827,    -1,    -1,    -1,    -1,    -1,
     833,  1443,    -1,    -1,   837,   838,   627,    -1,    -1,    -1,
      -1,    -1,    -1,   846,    -1,   288,    -1,   850,    -1,    -1,
      -1,    -1,    -1,    -1,  1600,  1601,    -1,    -1,   861,    -1,
      -1,    -1,    -1,    -1,  2175,    -1,    -1,    -1,   871,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   319,    -1,    -1,    -1,
    2055,    -1,    -1,    -1,    -1,   676,    -1,   678,    -1,    -1,
    2065,    -1,    -1,    -1,    -1,    -1,    -1,  1623,    -1,  1625,
      -1,    -1,  2213,    -1,  2079,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,   932,
      -1,   934,    -1,    -1,    -1,   938,  2111,    -1,    -1,    -1,
      -1,  2116,    -1,    -1,    -1,    -1,   949,   950,    -1,    -1,
      -1,    -1,    -1,   396,    -1,  1371,    -1,  2132,    -1,   962,
      -1,    -1,  1740,  1741,    -1,   968,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   977,    -1,    -1,    -1,   981,    -1,
      -1,    -1,   985,    -1,   987,   182,   777,   990,    -1,   992,
     433,    -1,  1718,   996,    -1,   438,    -1,    -1,    -1,    -1,
    1003,  1004,  1005,    -1,  1750,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
    2195,    -1,  1025,    -1,    -1,    -1,    -1,    -1,    -1,  1032,
      -1,    -1,    -1,   824,    -1,   478,    -1,   141,    -1,  1455,
    1043,  1044,    -1,    -1,    -1,    -1,  1772,    -1,    -1,  2224,
      -1,    -1,  2227,    -1,    -1,  1833,  1059,    -1,    -1,    -1,
      -1,    -1,  1840,   167,   168,    -1,    -1,   171,    -1,    -1,
      -1,  1683,    -1,  2248,   178,   179,  1079,    -1,    -1,    -1,
      79,    -1,   182,    -1,   184,    -1,   190,   187,   188,    -1,
      -1,    -1,    -1,    -1,    -1,  1873,    -1,    -1,   198,   199,
      -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,    -1,
      -1,    -1,  1724,   112,    -1,   215,   216,   116,   117,   118,
     119,   120,   121,   122,   123,  1128,   569,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,
      -1,   932,    -1,    -1,   143,    -1,    -1,  1925,  1926,    -1,
      -1,    -1,    -1,    -1,  1932,    -1,    -1,   257,  1161,    -1,
      -1,  1164,    -1,  1941,    -1,  1168,    13,    14,    15,    16,
      17,  1174,  1906,  1951,   617,  1953,    -1,    -1,    79,    -1,
     623,    -1,   282,    -1,    -1,    -1,   629,    -1,  1966,    -1,
    1968,  1969,  1970,    -1,   985,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
      -1,   112,    -1,  1004,    -1,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,  2002,    -1,   669,   128,  2006,  1232,
    1233,  1647,    79,  2011,  1237,    -1,    -1,   680,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,  1249,  1981,  1982,    -1,
      -1,    -1,    -1,    -1,    -1,   698,    -1,    -1,    -1,  1262,
     703,   178,    -1,  1266,    -1,   112,   167,   168,  1059,   116,
     117,   118,   119,   120,   121,   122,   123,   178,   179,  1282,
    1283,   128,    -1,    -1,    -1,    -1,    -1,    -1,  1079,   190,
      -1,    -1,  2070,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,   744,    -1,    -1,  2082,  2039,    -1,   112,  2086,   114,
    1313,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
     167,   168,    -1,  1326,  2102,    -1,    -1,    -1,  2062,   107,
      -1,   178,   179,    -1,    -1,    -1,  1339,    -1,    -1,    -1,
    1343,    -1,    -1,   190,  1347,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,  1359,    -1,    -1,   802,
     156,   157,   158,   159,   160,   161,   162,   163,  1371,  2147,
      -1,   471,    -1,  1164,    -1,   818,    -1,   173,    -1,  1382,
      -1,  2115,  2116,    -1,    -1,  1388,    -1,   183,    -1,    -1,
     833,    -1,    -1,    -1,    -1,    -1,   192,    -1,  2132,    -1,
      -1,    -1,  2136,    -1,    -1,    -1,    -1,    31,  2186,    -1,
      -1,    -1,    -1,    -1,    38,    -1,    -1,    -1,    -1,    -1,
     520,   521,   522,   523,   524,   525,   526,   527,   528,   529,
     530,   531,   532,   533,   534,   535,   536,   537,   538,    -1,
    1443,    -1,  2176,    -1,  2222,    -1,    -1,  2225,    -1,    -1,
      -1,    -1,  1455,    -1,    -1,    -1,    -1,    -1,    -1,  1462,
    1463,    -1,    -1,    -1,    -1,    -1,  1469,  2245,    -1,    -1,
    1473,    -1,    -1,  1476,    -1,    99,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,    -1,
      -1,  1282,  1283,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1506,    -1,    -1,    -1,  1510,   286,    -1,
      -1,  1514,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,  1313,    -1,   116,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,  1537,  1326,    -1,   129,    -1,   131,
      -1,  1957,    -1,  1546,    -1,    -1,   170,    -1,    -1,    -1,
     174,    -1,    -1,   996,    -1,    -1,    -1,    -1,    -1,    -1,
    1003,    -1,    -1,   663,    -1,    -1,    -1,    -1,  1571,  1572,
      -1,    -1,    -1,   351,   352,   112,   168,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
      -1,  1382,    -1,    -1,   372,    -1,    -1,  1600,  1601,    -1,
      -1,  1044,    -1,    -1,   141,    -1,    -1,  1610,    -1,  1612,
      -1,    -1,    -1,    -1,   238,    -1,    -1,    -1,    -1,    -1,
      -1,  2037,   246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,    -1,    -1,    -1,    -1,  1640,    -1,    -1,
      -1,   178,   179,   267,  1647,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   190,   432,   279,   280,    -1,   282,    -1,
      -1,    -1,    -1,    -1,    -1,   289,    -1,    -1,    -1,    -1,
      -1,  1462,  1463,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1683,    -1,    -1,  1686,   308,  1688,    -1,    -1,   312,    -1,
    1693,    -1,   316,    -1,    -1,    -1,    -1,    -1,    -1,   323,
      -1,    -1,   326,  1706,    -1,    -1,    -1,  1710,  1711,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,  1724,    -1,    -1,  1727,    -1,    -1,    -1,   352,    -1,
      13,    14,    15,    16,    17,   112,    -1,   361,    -1,   116,
     117,   118,   119,   120,   121,   122,   123,  1750,   372,    -1,
      -1,    -1,    -1,   377,   378,   379,   380,   857,    -1,    -1,
      -1,    -1,   862,    -1,   141,    -1,   390,    -1,    -1,   547,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   402,    79,
     880,  1572,   406,    -1,    -1,    -1,    -1,   411,    -1,  1232,
     167,   168,   416,    -1,    -1,  1798,    79,    -1,    -1,   423,
      -1,   178,   179,    -1,    -1,    -1,   430,    -1,    -1,    -1,
     434,   435,   112,   190,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,   925,  1829,  1830,   128,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   141,    -1,   143,   468,   128,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   141,    -1,
     143,    -1,   640,    -1,    -1,    -1,   644,   167,   168,    -1,
      -1,   171,    -1,    13,    14,    15,    16,    17,   178,   179,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,   171,    -1,
     190,    -1,   992,   112,    -1,   178,   179,   116,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,   190,    -1,   175,
     129,    -1,   131,    -1,    -1,    -1,  1359,    -1,    -1,   107,
      -1,    -1,    -1,    -1,   112,  1928,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,  1939,    -1,    -1,  1942,
      -1,    -1,   566,   567,    -1,   723,    -1,   725,    -1,   168,
      -1,    -1,   171,    -1,  1957,    -1,    -1,    -1,    -1,   737,
     738,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,
      -1,    -1,   112,  1073,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   762,   763,   764,    -1,    -1,    -1,
      -1,   615,    -1,    -1,   618,    -1,    -1,    -1,   776,    -1,
     624,   141,    -1,   627,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,  2016,  2017,    -1,   640,    -1,    -1,    -1,
     644,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
      -1,    -1,    -1,    -1,  2037,    -1,    -1,  2040,   178,   179,
     664,   665,    -1,    -1,    -1,    -1,    -1,   671,    -1,    -1,
     190,    -1,   676,    -1,   678,    -1,    -1,    -1,    -1,    -1,
     684,    -1,   686,    -1,    -1,    -1,    -1,   691,   692,    13,
      14,    15,    16,    17,    -1,    -1,    -1,  1177,  1178,  1179,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,  2096,  1194,  1195,   112,    -1,    -1,    -1,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   141,
      -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,
      -1,  1221,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,    -1,   763,
      -1,  2144,  2145,    -1,    -1,    -1,   178,   179,    -1,    -1,
      -1,    -1,   168,   777,    -1,   171,    -1,  1600,   190,   200,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,   795,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,   112,  2188,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,   141,    -1,    -1,
     824,    -1,    -1,   827,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   837,    -1,    -1,    79,  1317,    -1,  1319,
      -1,    -1,    -1,   167,   168,    -1,   850,  1005,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,    -1,   861,    -1,  1017,
      -1,  1019,   170,  1021,    -1,    -1,   190,   871,   872,   112,
    1693,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    -1,    -1,    -1,    -1,    -1,   141,    38,
     143,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,  1750,   932,    -1,
     934,    -1,    -1,    -1,   938,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   949,   950,   190,   952,    -1,
      -1,    -1,  1110,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      99,   170,   171,    -1,    -1,  1123,    -1,  1125,  1126,    -1,
      -1,   110,    -1,   977,    -1,    -1,    -1,   981,    -1,    -1,
      -1,   985,    -1,   987,    -1,    -1,   990,    -1,   992,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
    1004,  1005,    -1,    -1,    -1,    -1,  1829,    -1,    -1,    -1,
      79,    -1,  1016,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1025,    -1,   444,    -1,    -1,   447,    -1,  1032,    -1,
      -1,   170,   453,    -1,    -1,    -1,    -1,    -1,    -1,  1043,
      -1,   462,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,  1059,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,   486,  1546,    -1,    -1,    -1,
      -1,    -1,   141,  1231,   143,  1079,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,   112,    -1,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   167,   168,
      -1,    -1,   171,    -1,    -1,  1928,    -1,   246,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   539,    -1,
      -1,   190,    -1,    -1,  1128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1138,    -1,    -1,    31,  1296,    -1,
     279,   167,    -1,   282,    38,  1303,    79,  1305,    -1,    -1,
     289,    -1,    -1,    -1,    -1,    -1,    -1,  1161,    -1,    -1,
    1164,    -1,    -1,  1643,  1168,    -1,    -1,    -1,    -1,   308,
      -1,    -1,    -1,   312,    -1,    -1,    -1,   316,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,   110,  2040,    -1,    -1,
      -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,    -1,  1233,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
     379,    -1,    -1,   137,    -1,   178,   179,    -1,    -1,    -1,
      -1,   390,    -1,    -1,    -1,    -1,    -1,   190,  1262,    -1,
      -1,    -1,  1266,   402,    -1,    -1,    -1,   406,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,  1282,  1283,
      -1,    -1,  1440,    -1,   423,    -1,    -1,    -1,    -1,    -1,
    1294,   430,    -1,    -1,    -1,   434,   435,    -1,    -1,    -1,
      -1,  1305,    -1,    -1,    -1,    -1,    -1,    -1,   112,  1313,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,  1326,    -1,    -1,    -1,    -1,    -1,    -1,   468,
    1488,  1489,  1490,  1491,    -1,  1339,    -1,   141,    -1,  1343,
      -1,    -1,    -1,  1347,    -1,    -1,    -1,    -1,    -1,   112,
     771,   772,   246,   116,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,   167,   168,    -1,   129,  1371,   131,    -1,
     174,    -1,    -1,    -1,   178,   179,    -1,    -1,  1382,    -1,
      -1,    -1,    -1,    -1,  1388,   279,   190,    -1,   282,    -1,
      -1,    -1,    -1,    -1,  1874,   289,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,   171,    -1,
      -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,   312,    -1,
      -1,    -1,   316,    -1,    -1,    -1,    -1,   566,   567,    -1,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,  1455,    -1,   592,    -1,    -1,    -1,    -1,  1462,  1463,
     141,    -1,   143,    -1,    -1,  1469,    -1,   361,    -1,  1473,
      -1,    -1,  1476,    -1,    -1,    -1,   615,    -1,    -1,   618,
      -1,    -1,    -1,    -1,    -1,   379,   167,   168,   627,    -1,
      -1,    -1,    -1,    -1,   633,    -1,   390,   178,   179,    -1,
      -1,   640,  1506,    -1,    -1,    -1,  1510,    -1,   402,   190,
    1514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1998,    -1,
      -1,    -1,    -1,    -1,    -1,   664,   665,    -1,    -1,   423,
      -1,    -1,    -1,  1537,  1692,    -1,   430,   676,    -1,   678,
     434,   435,  1546,    -1,    -1,   684,    -1,   686,    -1,    -1,
      -1,    -1,    -1,   692,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,  2047,  1572,    -1,
      -1,    -1,    -1,   112,   468,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   112,    -1,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,  1601,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1612,    -1,
      -1,    -1,    -1,   112,   753,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    79,    -1,    -1,   168,
      -1,    -1,   171,  2113,    -1,    -1,    -1,    -1,   777,  1797,
      -1,  1799,    -1,  1647,   171,    -1,  1804,    -1,  1806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,  2138,   112,
    2140,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   170,   566,   567,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,  1686,    -1,    -1,   824,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,
    2180,    -1,  1706,    -1,    -1,    -1,  1710,  1711,    -1,    -1,
      -1,   850,    -1,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,   615,    -1,  1727,   618,   178,   179,    -1,    -1,    -1,
      -1,    -1,   871,   627,    -1,    -1,    -1,   190,    -1,   633,
      -1,    -1,    -1,    -1,    -1,    -1,   640,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1912,    -1,    -1,  1915,  1916,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     664,   665,  2252,    -1,    -1,    -1,    -1,    -1,   164,    79,
      -1,    -1,   676,    -1,   678,    -1,    -1,    -1,    -1,    -1,
     684,    -1,   686,   932,    -1,   934,    -1,    -1,   692,   938,
      -1,    -1,  1806,    -1,    -1,   191,    -1,    -1,    -1,    -1,
     949,   950,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,  1830,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1995,   977,    -1,
      -1,   141,   981,   143,    -1,    -1,   985,    -1,   987,    -1,
      -1,   990,    -1,   992,    -1,    -1,    -1,    -1,    -1,   753,
      -1,    -1,    -1,    -1,    -1,  1004,  1005,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
      -1,  2039,    -1,   777,    -1,    -1,  1025,    -1,    -1,    -1,
     190,    -1,    -1,  1032,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   795,    -1,    -1,  1043,   112,    79,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
    1059,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     824,    -1,    -1,    -1,  1355,  1939,    -1,    -1,  1942,   112,
    1079,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,  1957,    -1,   128,   850,  2115,  2116,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,   141,    -1,
     143,    -1,    -1,    -1,    -1,    -1,    -1,   871,  1982,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1128,
      -1,    -1,    -1,    -1,   167,   168,    -1,    -1,    -1,  1138,
      -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,  2176,  2177,
      -1,    -1,  1161,   110,    -1,  1164,    79,    -1,    -1,  1168,
      -1,    -1,    -1,  2037,    -1,    -1,    -1,    -1,   932,    -1,
     934,    -1,    -1,    -1,   938,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   949,   950,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   977,    -1,    -1,    -1,   981,   141,    -1,
     143,   985,    -1,   987,  1233,    -1,   990,   112,   992,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
    1004,  1005,  2116,    -1,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1262,    -1,   178,   179,  1266,    -1,    -1,
      -1,  1025,    -1,    -1,    -1,    -1,    -1,   190,  1032,    -1,
    2144,  2145,    -1,  1282,  1283,    -1,    -1,    -1,    -1,  1043,
    1571,    -1,   167,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,  1059,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1313,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2188,  1079,    -1,  1326,    -1,    -1,
      -1,    -1,   279,    -1,    -1,   282,    -1,    -1,    -1,    -1,
    1339,    -1,   289,    -1,  1343,    -1,    -1,    -1,  1347,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,   308,    -1,    -1,    -1,   312,    -1,    -1,    -1,   316,
      -1,    -1,  1371,    -1,  1128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1382,  1138,    -1,    -1,    -1,    -1,  1388,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,  1161,    -1,   128,
    1164,    -1,    -1,    -1,  1168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,  1706,  1707,    -1,    -1,  1710,
    1711,    -1,   379,    -1,    -1,  1716,    -1,    -1,    -1,  1720,
      -1,    -1,    -1,    -1,  1725,    -1,  1727,    -1,   167,   168,
      -1,    -1,    -1,    -1,    -1,    -1,  1455,    -1,    -1,   178,
     179,    -1,    -1,  1462,  1463,    -1,    -1,    -1,    -1,    -1,
    1469,   190,    -1,    -1,  1473,    -1,    -1,  1476,    -1,  1233,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,    -1,  1506,  1262,    -1,
      -1,  1510,  1266,    -1,    -1,  1514,    -1,    -1,    -1,   141,
      -1,   468,    -1,    -1,    -1,    -1,    -1,    -1,  1282,  1283,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,  1825,   167,   168,  1546,    -1,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,  1313,
     141,    -1,    -1,    -1,  1845,  1846,    -1,    -1,   190,   191,
      -1,    -1,  1326,  1572,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1339,   167,   168,    31,  1343,
     171,    -1,    -1,  1347,  1875,    38,    -1,   178,   179,    -1,
      -1,    -1,  1601,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,  1612,    -1,    -1,    -1,  1371,    -1,   566,
     567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1382,    -1,
      -1,    -1,    -1,    -1,  1388,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,  1647,  1930,
      -1,    -1,    -1,  1934,    -1,  1936,    99,    -1,  1939,  1940,
      -1,  1942,    -1,    -1,   107,    -1,  1947,   110,   615,    -1,
      -1,   618,    -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,
     627,    -1,    -1,    -1,    -1,    -1,   633,   130,    -1,    -1,
     133,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,
      -1,  1455,    -1,    -1,    -1,    -1,    -1,  1706,  1462,  1463,
      -1,  1710,  1711,    -1,    -1,  1469,    -1,   664,   665,  1473,
      -1,    -1,  1476,    -1,    -1,    -1,    -1,    -1,  1727,    -1,
      -1,   174,    -1,    -1,    -1,  2016,    -1,   684,    -1,   686,
      -1,    -1,  2023,    -1,    -1,   692,    -1,  2028,  2029,    -1,
      -1,    -1,  1506,    -1,    -1,    -1,  1510,   200,    -1,    -1,
    1514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2051,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1546,    -1,    -1,   238,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   246,    -1,    -1,  2087,    -1,  2089,    -1,
      -1,  2092,  2093,  2094,    -1,    -1,    -1,    -1,  1572,  2100,
    2101,    -1,    -1,   266,   267,   268,    -1,    -1,    -1,    -1,
     777,  1830,    -1,    -1,    -1,    -1,   279,   280,    -1,   282,
      -1,    -1,    -1,   286,    -1,   288,   112,  1601,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,  1612,    -1,
      -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,   312,
      -1,    -1,    -1,   316,    -1,   141,   319,    -1,    -1,    -1,
     323,    -1,    -1,   326,    -1,    -1,    -1,  2168,  2169,  2170,
      -1,    -1,    -1,  1647,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,   168,   850,    -1,    -1,    -1,    -1,   351,   352,
      -1,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2202,  2203,  2204,   190,    -1,    -1,    -1,    -1,   372,
      -1,    -1,    -1,    -1,    -1,    -1,   379,   380,    -1,    -1,
      -1,    -1,    -1,  1942,    -1,    13,    14,    15,    16,    17,
      -1,    -1,  1706,   396,    -1,    -1,  1710,  1711,  1957,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1727,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   432,
      -1,   938,   435,   436,    -1,   438,    -1,    -1,    -1,    -1,
      -1,   444,   949,    -1,   447,    -1,   449,   450,    -1,    -1,
     453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,
      -1,    -1,   465,   466,   467,   468,    -1,    -1,   471,    -1,
      -1,    -1,    -1,    -1,   477,   478,    -1,    -1,  2037,    -1,
     987,    -1,    -1,   990,   112,   992,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,  1830,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1043,    -1,    -1,   167,
     168,    -1,    -1,    -1,   547,   548,    -1,    -1,    -1,    -1,
     178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   190,   566,   567,   568,   569,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2144,  2145,    -1,    -1,   592,
      -1,    -1,    -1,    -1,   597,    -1,   599,   600,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   615,    -1,   617,    -1,    -1,    -1,    -1,    -1,
     623,   624,    -1,    -1,   627,  1939,   629,    -1,  1942,  2188,
      -1,  1138,    -1,    -1,    -1,    -1,    -1,   640,    -1,    -1,
      -1,   644,    -1,  1957,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   664,   665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   676,    -1,   678,    -1,   680,    -1,    -1,
      -1,   684,    -1,   686,    -1,    -1,    -1,    -1,   691,   692,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   717,    -1,    -1,    -1,    -1,    -1,
     723,    -1,   725,  2037,    -1,    -1,  1233,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   737,   738,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,
     763,   764,    -1,    -1,    -1,    -1,    -1,    -1,   771,   772,
      -1,    -1,    -1,   776,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2144,  2145,    -1,    -1,    -1,   838,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   846,    -1,    -1,    55,   850,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,   861,    -1,
      -1,    -1,    -1,    -1,  1371,    -1,    -1,    -1,   871,   872,
      -1,    80,    -1,    -1,  2188,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,   932,
      -1,   934,   141,    -1,    -1,   938,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   949,   950,  1455,   952,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,   167,   962,
      -1,   170,   171,    -1,    38,   968,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   985,    -1,   987,    -1,    -1,   990,    -1,   992,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1004,  1005,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1019,    -1,  1021,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    55,    -1,    -1,
      58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
    1043,  1044,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,  1059,    -1,    -1,    -1,
      -1,    -1,    -1,   137,    -1,  1572,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1079,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    -1,   133,   134,  1110,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
    1123,    -1,  1125,  1126,    -1,    -1,   200,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1138,    -1,    -1,    -1,   167,
    1647,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    -1,    -1,  1161,    -1,
      -1,  1164,   190,    -1,   238,    -1,    -1,    -1,    -1,    -1,
      -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1188,    -1,  1190,    -1,    -1,
      -1,    -1,   266,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,
      -1,    -1,   286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1231,  1232,
    1233,    -1,    -1,    -1,  1237,    -1,    -1,    -1,   312,    -1,
      -1,  1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1282,
    1283,   355,    -1,    -1,    -1,    -1,    -1,    -1,   362,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1303,    -1,  1305,     5,    -1,    -1,   380,    -1,    -1,    -1,
    1313,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1326,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,    61,
      -1,    63,   436,    -1,    -1,    -1,    -1,    -1,  1371,    -1,
     444,    -1,    -1,   447,    -1,   449,   450,    -1,    80,   453,
      -1,    -1,    -1,    -1,    -1,  1388,    -1,    -1,   462,    -1,
    1393,   465,   466,   467,    -1,    -1,    -1,   471,    -1,    -1,
      -1,    -1,    -1,   477,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,  1440,    -1,   141,
    1443,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1957,    -1,  1455,    -1,    -1,    -1,    -1,    -1,    -1,  1462,
    1463,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,    -1,  1488,    -1,  1490,  1491,    -1,
      -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   597,    -1,    31,    -1,    -1,    -1,    -1,
    2037,    -1,    38,    -1,  1537,   609,   610,    -1,   612,   613,
      -1,   615,    -1,  1546,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   640,    -1,  1571,  1572,
     644,   645,    -1,    -1,    -1,    -1,    -1,   651,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   661,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,  1600,  1601,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1610,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,   691,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,    -1,    -1,   717,  1647,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   726,    -1,    -1,    -1,   730,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1683,    -1,    -1,    -1,    -1,  1688,    -1,    -1,    -1,  1692,
    1693,    -1,   766,    -1,   200,    -1,    -1,   771,   772,    -1,
      -1,    -1,    -1,  1706,    -1,    -1,    -1,  1710,  1711,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1724,    -1,    -1,  1727,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1750,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     266,    -1,    -1,    -1,   838,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   846,    -1,   280,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,
      -1,    -1,    -1,    -1,  1797,  1798,  1799,   871,   872,    -1,
      -1,  1804,    -1,  1806,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,
     326,    -1,    -1,    -1,    -1,    -1,  1829,  1830,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   361,    -1,    -1,    -1,    -1,
     934,    -1,    -1,    -1,   938,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   378,    -1,   380,   949,   950,    -1,   952,    -1,
     386,    -1,    -1,    -1,   390,    -1,    -1,    -1,   962,    -1,
      -1,    -1,    -1,    -1,   968,    -1,   402,    -1,   972,   973,
      -1,   975,   976,  1906,    -1,    -1,    -1,    -1,    -1,  1912,
      -1,    -1,  1915,  1916,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   430,  1928,    -1,    -1,   434,    -1,
     436,  1005,    -1,  1007,    -1,    -1,    -1,    -1,   444,  1942,
      -1,   447,    -1,   449,   450,  1019,    -1,   453,    -1,    -1,
      -1,    -1,    -1,    -1,  1957,    -1,   462,    -1,    -1,   465,
     466,   467,    -1,    -1,    -1,   471,    -1,    -1,    -1,  1043,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1981,  1982,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1995,    -1,    -1,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,  1083,
      -1,    -1,    -1,  1087,    -1,    -1,    -1,    -1,    -1,    -1,
    1094,  1095,    -1,    -1,  1098,  1099,    -1,    -1,  1102,  1103,
      -1,    -1,    -1,    -1,  2037,    -1,  2039,  2040,  1112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2062,
      -1,    -1,    -1,    99,  1138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1174,   137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2115,  2116,  1188,    -1,  1190,    -1,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      -1,    -1,    -1,  2136,   170,    -1,    38,    -1,   174,    -1,
      -1,  2144,  2145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1233,
      -1,    -1,    -1,  1237,   200,    -1,    -1,    -1,    -1,    -1,
    1244,    -1,    -1,  2176,  2177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   691,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   717,    -1,  1287,  1288,   117,  1290,  1291,  1292,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     266,    -1,    -1,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   280,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1330,  1331,    -1,  1333,
    1334,    -1,    -1,  1337,  1338,   771,   772,    -1,   170,    -1,
      -1,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   323,  1362,   795,
     326,    -1,    -1,    -1,    -1,    -1,    -1,  1371,   200,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1388,    -1,    -1,    -1,    -1,  1393,
      -1,   827,    -1,    -1,    -1,   361,    -1,    -1,    -1,  1403,
    1404,   837,   838,    -1,    -1,    -1,   238,    -1,    -1,    -1,
     846,    -1,   378,    -1,   380,    -1,    -1,    -1,    -1,    -1,
     386,    -1,    -1,    -1,   390,   861,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   266,    -1,   402,    -1,    -1,  1443,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   280,    55,
      -1,  1455,    58,    -1,    60,    61,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,   430,    -1,    -1,    -1,   434,    -1,
     436,    -1,    -1,    -1,    80,    -1,    -1,    -1,   444,    -1,
      -1,   447,    -1,   449,   450,    -1,    -1,   453,    -1,    -1,
      -1,   323,    -1,    -1,   326,    -1,   462,    -1,   934,   465,
     466,   467,    -1,   109,   110,   471,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,   962,   133,   134,   361,
      -1,    -1,   968,  1537,    -1,   141,    -1,    -1,    -1,    -1,
     107,    -1,  1546,    -1,    -1,    -1,   378,    -1,   380,    -1,
     117,    -1,    -1,    -1,   386,    -1,    -1,    -1,   390,    -1,
      -1,   167,   168,    -1,   170,   171,    -1,  1571,    -1,   175,
     402,   177,   178,   179,   180,   181,   182,   183,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,   430,    -1,
      -1,    -1,   434,    -1,   436,    -1,  1610,    -1,    -1,    -1,
      -1,    -1,   444,    -1,    -1,   447,    -1,   449,   450,    -1,
      -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     462,   597,    -1,   465,   466,   467,    -1,    -1,    -1,   471,
      -1,    -1,    -1,  1647,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1683,
      -1,    -1,    -1,    -1,  1688,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,
     267,    -1,  1706,    -1,    -1,    -1,  1710,  1711,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,
    1724,    -1,    -1,  1727,    -1,   691,    -1,    -1,    -1,    -1,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   133,    -1,    -1,    -1,   597,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,    -1,   147,    -1,   149,    -1,    -1,
      -1,    -1,    -1,    -1,   351,   352,    -1,    -1,    -1,    -1,
      -1,    -1,   624,    -1,  1798,    -1,    -1,    -1,    -1,    -1,
      -1,  1237,    -1,    -1,    -1,   771,   772,  1811,    -1,    -1,
     377,    -1,    -1,   380,    -1,    -1,    -1,    -1,   190,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1830,    -1,    -1,   795,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   423,    -1,    -1,   691,
      -1,   827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,
      -1,   837,   838,    -1,    -1,    -1,    -1,   444,    -1,    -1,
     846,    -1,    -1,   450,    -1,   717,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,
      -1,    -1,  1906,    -1,   471,    -1,    -1,    -1,    -1,  1913,
      -1,    -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,    -1,
      -1,    -1,   294,    -1,   296,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1942,   771,
     772,    -1,    -1,    -1,    -1,    -1,    -1,   319,    -1,   321,
     322,    -1,  1388,  1957,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   795,    -1,    -1,    -1,    -1,   934,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1981,  1982,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1994,    -1,    -1,    -1,    -1,   827,   962,    -1,    -1,    -1,
     567,    -1,   968,    -1,    -1,   837,   838,  1443,    -1,    -1,
      -1,    -1,    -1,    -1,   846,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   396,    -1,   398,   399,    -1,   861,
      -1,    -1,    -1,  2037,    -1,  2039,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2062,    -1,
      -1,   433,    -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,
      -1,    -1,    -1,   640,    -1,    -1,    -1,   644,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1537,   934,    -1,    -1,    -1,   478,    -1,    -1,    -1,
    1546,  2115,  2116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2132,  2133,
     962,    -1,  2136,    -1,    -1,  1571,   968,    -1,    -1,    -1,
    2144,  2145,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,
      -1,    -1,  2176,    -1,  1610,    -1,   548,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   753,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   762,   763,   569,    -1,   571,
     572,    -1,    -1,    -1,   771,   772,    -1,    -1,  1174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   617,    -1,  1683,    -1,    -1,
    1686,   623,  1688,    -1,    -1,    -1,    -1,   629,    -1,   631,
     632,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1706,  1237,    -1,    -1,  1710,  1711,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1724,    -1,
      -1,  1727,    -1,    -1,    -1,    -1,    -1,   669,   670,    -1,
      -1,    -1,    -1,    -1,   871,   872,    -1,    -1,   680,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   698,    -1,    -1,    -1,
      -1,   703,    -1,    -1,    -1,    -1,    37,    -1,    -1,    -1,
      -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1798,    -1,    -1,   737,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   950,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   764,    -1,  1830,   962,    -1,    -1,    -1,    -1,
      -1,   968,   774,    -1,    -1,  1237,    -1,    -1,    -1,   110,
     977,    -1,    -1,    -1,   981,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1388,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,   805,   806,    -1,    -1,    -1,  1005,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   818,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1025,    -1,
      -1,   833,    -1,    -1,    -1,  1032,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1443,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     201,    13,    14,    15,    16,    17,  1942,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,  1388,    -1,    -1,    -1,
      -1,  1128,    -1,    -1,    -1,   266,    -1,    -1,    -1,    -1,
      -1,  1537,    -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,
    1546,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,   115,    -1,  1571,    -1,    -1,    -1,    -1,
      -1,  1443,    -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,   141,
      -1,  1003,    -1,    -1,   335,  1601,   337,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1610,    -1,    -1,  1019,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,
      -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    -1,
    1237,    -1,  1044,    -1,    -1,    -1,    -1,  1049,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1262,    -1,    -1,    -1,  1266,
      -1,    -1,    -1,    -1,    -1,  1537,    -1,    -1,  2144,  2145,
      -1,    -1,    -1,    -1,  1546,    -1,    -1,  1683,    -1,    -1,
    1686,    -1,  1688,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   435,    -1,    -1,    -1,  1305,  1571,
    1706,    -1,    -1,    -1,  1710,  1711,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1125,   455,    -1,    -1,    -1,  1724,    -1,
      -1,  1727,    -1,    -1,    -1,    -1,    -1,   468,    -1,  1601,
      -1,    -1,  1339,    -1,    -1,    -1,  1343,    -1,  1610,    -1,
    1347,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   489,    -1,
     491,    -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,   500,
     501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   515,    -1,  1188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1798,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     541,    -1,    -1,   544,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1683,    -1,    -1,  1686,    -1,  1688,    -1,    -1,    -1,
    1232,    -1,    -1,    -1,  1830,   566,    -1,    -1,    -1,    -1,
      -1,    -1,  1244,    -1,  1706,    -1,  1443,    -1,  1710,  1711,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   592,  1724,    -1,    -1,  1727,    -1,    -1,    -1,    -1,
      -1,    -1,  1469,    -1,    -1,    -1,  1473,    -1,    -1,  1476,
      -1,    -1,    -1,    -1,    -1,    -1,   617,   618,    -1,   620,
      -1,    -1,    -1,  1295,    -1,    -1,   627,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1506,
     641,    -1,   643,  1510,    -1,    -1,    -1,  1514,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   664,    -1,    -1,  1798,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1942,    -1,    -1,    -1,
      -1,    -1,    -1,   684,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,  1830,   117,
     701,    -1,    -1,    -1,   705,   706,    -1,    -1,    -1,    -1,
      -1,   712,    -1,    -1,    -1,   133,   717,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   744,    -1,  1612,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,   200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1942,    -1,    -1,    -1,    -1,    -1,  1683,    -1,  1490,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   246,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,   850,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1724,    -1,    -1,
      -1,   279,    -1,    -1,   282,    -1,    -1,    -1,   869,    -1,
     288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2144,  2145,
     881,    -1,    -1,   884,   885,   886,    -1,    -1,    -1,    -1,
     308,    -1,    -1,    -1,   312,    -1,    -1,    -1,   316,    -1,
      -1,   319,    -1,   904,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,    -1,  1806,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   379,   380,    -1,    -1,    -1,    -1,   968,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   396,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   990,
      -1,    -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,  2144,  2145,    -1,    -1,    -1,   435,   436,    -1,
     438,  1693,    -1,    -1,    18,    -1,   444,    -1,    -1,   447,
      -1,   449,   450,    -1,    -1,   453,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1044,   462,    -1,    -1,   465,   466,   467,
     468,    -1,    -1,   471,    -1,    -1,    -1,    -1,    -1,    -1,
     478,    55,    -1,    -1,    58,    -1,    60,    61,  1069,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1750,    -1,
      -1,  1082,    -1,    -1,    78,    -1,    80,    81,    -1,    83,
      -1,    -1,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,    -1,   107,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,    -1,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,   566,   567,
      -1,   569,    -1,    -1,    -1,    -1,    -1,  1829,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   592,    -1,   170,   171,    -1,   597,
      -1,   175,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,    -1,    -1,    -1,    -1,  1197,   615,   192,   617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,
      -1,   629,    -1,    -1,    -1,    -1,  1217,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1237,    -1,    -1,    -1,
      -1,    -1,    -1,  1915,    -1,    -1,   664,   665,  2115,  2116,
      -1,    -1,    -1,    -1,    -1,    -1,  1928,    -1,   676,    -1,
     678,    -1,    -1,    -1,    -1,    -1,   684,    -1,   686,    -1,
      -1,    -1,    -1,    -1,   692,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1295,    -1,    -1,    -1,    -1,   717,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2188,  1323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2017,  2018,    -1,    -1,    -1,
      -1,    -1,    -1,   771,   772,    -1,  1357,    -1,  1359,   777,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2040,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2062,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,  1410,
      -1,  1412,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     838,    80,    -1,    -1,    -1,    -1,    -1,    -1,   846,    -1,
      -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,  1480,
    1481,    -1,   141,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,   156,   157,    -1,
      -1,   160,   161,    -1,  2176,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,   932,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
      55,   949,    -1,    58,    -1,    60,    61,    -1,    63,    -1,
      -1,    -1,    -1,    -1,   962,    -1,    -1,    -1,    -1,    -1,
     968,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1565,  1566,    -1,   985,    -1,   987,
      -1,  1572,   990,    -1,   992,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,   110,  1004,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,  1608,   133,   134,
    1611,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1043,  1044,    -1,    -1,    -1,
      -1,   156,   157,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,  1059,   167,   168,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,  1079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1676,    -1,    -1,    -1,    -1,
      -1,    -1,  1683,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1699,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,    -1,
      -1,    -1,    -1,  1724,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1161,    -1,    -1,  1164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,  1772,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,  1805,    -1,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1237,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1282,  1283,    -1,  1868,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    55,    -1,    -1,    58,    -1,    60,    61,    -1,    63,
      -1,    -1,    -1,   143,    -1,  1313,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    79,    80,    81,  1326,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,    -1,   107,    -1,   109,   110,    -1,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,   167,    -1,    -1,   170,   171,    18,    -1,
      -1,   175,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,
      -1,    -1,    -1,    -1,    -1,  1443,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,
      60,    61,    -1,    63,  1462,  1463,    -1,  2048,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
      80,    81,    -1,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,    -1,   107,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,  1537,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,   167,    -1,    -1,
     170,   171,    -1,  1571,  1572,   175,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1600,     4,     5,     6,     7,     8,     9,    10,
      11,    12,  1610,    -1,    -1,    -1,    -1,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    -1,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1683,    -1,    -1,    -1,    -1,
    1688,    -1,    -1,    -1,    -1,  1693,    55,    -1,    -1,    58,
      -1,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,  1724,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,  1750,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   169,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1798,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,   173,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
      -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    78,
    1928,    80,    81,    -1,    83,    -1,    -1,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,    -1,   107,  1957,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,  2037,
      -1,    -1,  2040,   192,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
     169,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   192,     3,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   192,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,   111,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,   127,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,   192,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,   112,    62,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,   175,    -1,    -1,   178,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,   169,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   190,     4,     5,     6,
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
      -1,    -1,    -1,   190,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     190,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    76,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,   169,   170,   171,
      -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
     171,   172,   173,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,     1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,   114,   115,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   166,    -1,     1,    -1,   170,   171,
      -1,    -1,   114,   115,    -1,    -1,   178,   179,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,   170,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,   114,
     115,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   166,    -1,     1,    -1,   170,   171,    -1,    -1,   114,
     115,    -1,    -1,   178,   179,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    56,    57,
      -1,    59,    -1,    -1,    62,   170,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   166,    -1,
       1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    56,    57,    -1,    59,    -1,
      -1,    62,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,     1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   166,    -1,     1,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    56,
      57,    -1,    59,    -1,    -1,    62,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     190,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   190,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,
      -1,    -1,    -1,   175,    -1,    -1,   178,   179,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   190,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,   168,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,   190,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   167,   168,    -1,   170,   171,    -1,    -1,
      -1,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   190,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,   111,    -1,    -1,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     166,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
      -1,    -1,   178,   179,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,     3,    -1,     5,    -1,    -1,   178,
     179,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   111,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,
      62,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,   114,   115,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,
      -1,   114,   115,    -1,    -1,    -1,   178,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,   169,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,     4,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   169,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,   172,    -1,    -1,    -1,    -1,    -1,   178,
     179,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
      -1,   178,   179,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
     171,    -1,    -1,    -1,    -1,    -1,    -1,   178,   179,     4,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    56,    57,    -1,    59,    -1,    -1,    62,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,   114,   115,    -1,    57,    -1,    59,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,    -1,    63,    -1,    -1,    -1,    -1,   170,   171,   112,
      -1,   114,   115,    -1,    -1,   178,   179,    78,    -1,    80,
      81,    -1,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    -1,    -1,    99,   100,
     101,   102,   103,   104,   105,    -1,   107,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,   174,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,    55,    -1,    -1,    58,    -1,    60,
      61,   192,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    80,
      81,    -1,    83,    -1,    -1,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    -1,    -1,    99,   100,
     101,   102,   103,   104,   105,    -1,   107,    -1,   109,   110,
      -1,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,    -1,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,
     171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,   180,
     181,   182,   183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   192,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    -1,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   114,   115,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,   170,
      57,    -1,    59,    -1,   175,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,   170,    57,    -1,    59,    -1,   175,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,   175,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    -1,    96,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,
     170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,
     180,   181,   182,   183,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,   110,    -1,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,
     179,   180,   181,   182,   183,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,   169,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,
     178,   179,   180,   181,   182,   183,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,   128,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
     177,   178,   179,   180,   181,   182,   183,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,   110,    -1,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,   128,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,
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
      -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    -1,    -1,
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
      -1,   175,    -1,   177,   178,   179,   180,   181,   182,   183,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,   175,    -1,   177,   178,   179,   180,   181,   182,
     183,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     171,    -1,    -1,    -1,    -1,    -1,   177,   178,   179,   180,
     181,   182,   183,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    -1,    59,
      -1,    -1,    62,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,   114,   115,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     170,   112,    -1,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,   128,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,   170,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    67,    68,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,    -1,
      -1,    -1,    -1,    -1,    -1,   178,   179,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   169,   170,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,   178,
     179,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,   170,    22,    23,
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
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,    -1,
     178,   179,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
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
      64,    65,    66,    67,    68,    69,    70,    71,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
     114,   115,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   169,   170,   171,    -1,   114,
     115,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,    -1,   170,   171,    -1,    -1,    -1,
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
      -1,    -1,    -1,   141,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,   114,
     115,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,   114,   115,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,   114,   115,    -1,
      57,   170,    59,    -1,    -1,    -1,    -1,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,    -1,    -1,   114,   115,    -1,
      -1,   178,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   171,    -1,   114,
     115,    -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,    13,    14,
      15,    16,    17,   178,   179,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,   170,   171,    -1,    -1,    -1,
      -1,    -1,    -1,   178,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   171,    -1,    13,    14,
      15,    16,    17,   178,   179,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    -1,    59,    -1,    -1,    62,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   170,    -1,   129,   130,   131,
      -1,   133,   134,   178,   179,    -1,    -1,    -1,    -1,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,
     172,    -1,    -1,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     178,   179,   180,   181,   182,   183,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,   170,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   170,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    -1,
      59,    -1,    -1,    62,    -1,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    58,    -1,    60,    61,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,   110,   143,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,    -1,
     170,   171,   172,    -1,    -1,    -1,    -1,   177,   178,   179,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,
      -1,    -1,   174,    -1,    -1,   177,   178,   179,   180,   181,
     182,   183,    -1,    -1,   109,   110,    -1,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,    -1,    -1,   129,   130,   131,    55,   133,   134,
      58,    -1,    60,    61,    -1,    63,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   167,    -1,    -1,   170,   171,   172,    -1,    -1,
      -1,    -1,   177,   178,   179,   180,   181,   182,   183,    -1,
      -1,   109,   110,    -1,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,    55,   133,   134,    58,    -1,    60,
      61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,
      -1,    -1,   170,   171,   172,    -1,    -1,    -1,    -1,   177,
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
      -1,    -1,    -1,   167,    -1,    -1,   170,   171,   172,    -1,
      -1,    -1,    -1,   177,   178,   179,   180,   181,   182,   183,
      -1,    -1,   109,   110,    -1,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,    55,   133,   134,    58,    -1,
      60,    61,    -1,    63,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,   175,    -1,
     177,   178,   179,   180,   181,   182,   183,    -1,    -1,   109,
     110,    -1,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,    -1,    -1,   129,
     130,   131,    55,   133,   134,    58,    -1,    60,    61,    -1,
      63,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,
     170,   171,    -1,    -1,    -1,   175,    -1,   177,   178,   179,
     180,   181,   182,   183,    -1,    -1,   109,   110,    -1,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,    55,
     133,   134,    58,    -1,    60,    61,    -1,    63,   141,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   167,    -1,   169,   170,   171,    -1,
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
     129,   130,   131,    -1,   133,   134,    -1,    -1,    55,    -1,
      -1,    58,   141,    60,    61,    -1,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,   167,   168,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,   170,   171,    -1,    -1,    -1,    -1,    -1,   177,   178,
     179,   180,   181,   182,   183,    -1,    -1,   109,   110,    -1,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
      55,   133,   134,    58,    -1,    60,    61,    -1,    63,   141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   167,    -1,    -1,   170,   171,
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
      -1,    -1,   129,   130,   131,    -1,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     167,    -1,    -1,   170,   171,    -1,    -1,    -1,    -1,    -1,
     177,   178,   179,   180,   181,   182,   183,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    -1,    -1,
      57,    -1,    59,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,   112,    -1,   114,   115,    57,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,   114,   115
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
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
     198,   124,   171,   198,   230,   124,   171,   200,   362,   168,
     233,   239,   230,   198,   175,   233,   214,   217,   217,   217,
     218,   218,   219,   219,   220,   220,   220,   220,   221,   221,
     222,   223,   224,   225,   226,   174,   240,   232,   171,   198,
     233,   434,   171,   313,   423,   175,   167,   200,   175,   200,
     144,   178,   179,   406,   169,   173,   200,   410,   169,   172,
     167,   179,   212,   502,   169,   198,   378,   470,   437,   175,
     437,   401,   191,   401,   169,   169,   173,   169,   173,   393,
     503,   169,   169,   169,   169,   169,   169,   167,   437,   478,
     481,   167,   478,   481,   198,   167,   324,   481,   495,   502,
     171,   178,   212,   233,   349,   233,   326,   167,   167,   323,
     500,   502,   324,   326,   194,   167,   383,   443,   466,   467,
     468,   471,   484,   485,   486,   172,   194,    18,   233,   326,
     437,   438,   465,   469,   483,   167,   437,   487,   505,   437,
     437,   505,   167,   437,   487,   437,   437,   505,   437,   437,
     481,   172,   229,   172,   326,   324,   493,   503,   200,   326,
     198,   387,   390,   390,   391,   505,   324,   495,   502,   194,
     505,   194,   171,   199,   228,   229,   435,   397,   174,   173,
     504,   396,   170,   171,   191,   400,   411,   167,   201,   194,
     198,   434,   191,   443,   445,   446,   447,   456,   458,   459,
     460,   462,   463,   464,   169,   169,   169,   169,   169,   169,
     169,   169,   169,   169,   444,   457,   461,   444,   457,   461,
     437,   191,   172,   233,   333,   349,   479,   395,   244,   432,
     383,   395,   244,   438,   443,   326,   438,   434,   167,   240,
     394,   240,   394,   434,   114,   423,   244,   432,   175,   175,
     432,   301,   423,   244,   432,   437,   437,   437,   175,   169,
     173,   169,   173,   388,   389,    77,   303,   304,   192,   172,
     172,   173,   200,   436,   194,   169,   393,   169,   173,   169,
     173,   169,   169,   169,   173,   169,   214,   169,   169,   169,
     214,    18,   328,   233,   383,   475,   476,   477,   326,   437,
     438,   474,   437,   437,   169,   169,   168,   175,   214,   239,
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
     210,   172,   173,   132,   170,   172,   168,   233,   504,   228,
     172,   424,   421,   169,   408,   434,   166,   417,   173,   192,
     173,   192,   411,   191,   402,   402,   376,   380,   378,   378,
     349,   173,   504,   200,   175,   175,   169,   383,   383,   169,
     169,   169,   173,   173,   172,   385,   385,   195,   169,   167,
     437,   478,   481,   167,   437,   487,   167,   437,   487,   481,
     325,     5,   178,   195,   233,   443,   437,   437,   167,    18,
     326,   438,   169,   169,   390,   195,   395,   172,   229,   229,
     166,   396,   437,   385,   437,   195,   167,   437,   478,   481,
     167,   437,   478,   481,   167,   437,   478,   481,   383,   383,
     383,   436,   172,   240,   233,   233,   333,   349,   424,   166,
     421,   244,   424,   175,   175,   175,   166,   437,   388,   389,
     173,   192,   195,   246,    18,    78,    80,    81,    83,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      99,   100,   101,   102,   103,   104,   105,   107,   114,   115,
     127,   167,   171,   200,   240,   241,   242,   243,   244,   248,
     249,   258,   265,   266,   267,   268,   269,   274,   275,   278,
     279,   280,   281,   282,   283,   284,   290,   291,   292,   306,
     326,   330,   433,   305,   195,   482,   483,   169,   174,   169,
     173,   174,   167,   437,   478,   481,   397,   504,   172,   172,
     130,   207,   208,   171,   208,   171,   208,   166,   424,   200,
     200,   434,   169,   394,   408,   408,   378,   504,   175,   175,
      10,   381,   166,   191,   382,   380,   166,   416,   169,   169,
     144,   387,   144,   195,   195,   169,   383,   383,   383,   233,
     233,   195,   172,   195,   169,   172,   195,   169,   383,   383,
     383,   169,   169,   169,   395,   172,   480,   166,   424,   166,
     363,   363,   363,   167,   358,     3,     5,    10,    80,   307,
     314,   315,   323,   326,   364,   370,   498,   200,   166,   167,
      68,    69,   192,   244,   306,   433,   167,   167,    18,   242,
     167,   167,   192,   200,   192,   200,   178,   200,   175,   241,
     167,    85,   192,   200,   167,   167,   242,   167,   244,   233,
     234,   234,    14,   293,   269,   280,   174,   192,    97,    98,
     273,   277,   118,   142,   272,   117,   141,   276,   272,   392,
     326,   192,   169,   169,   233,   205,   233,   383,   504,   166,
     172,   207,   207,   166,   406,   175,   166,   380,   380,   349,
     200,   437,   175,   229,   504,   166,   169,   169,   169,   169,
     169,   195,   195,   172,   172,   169,   437,   169,   169,   169,
     233,   166,   166,   166,   166,   411,   437,   323,   437,   323,
     370,   192,   192,   192,   167,   174,   212,   365,   366,   367,
     373,   443,   444,   457,   461,   173,   192,   200,   230,   192,
     244,   192,   244,   240,   250,   306,   308,   311,   317,   326,
     330,   240,    87,   169,   250,   156,   157,   160,   161,   168,
     169,   192,   240,   259,   260,   262,   306,   192,   192,   240,
     192,   397,   192,   240,   192,   192,   411,   240,   259,   119,
     120,   121,   122,   123,   285,   287,   288,   192,   106,   192,
      91,   167,   169,   437,   167,   167,   242,   242,   269,   167,
     279,   269,   279,   244,   437,   169,   166,   172,   172,   402,
     380,   437,   504,   504,   382,   397,   166,   437,   437,   172,
     172,   358,   358,   358,   169,   365,   323,   362,   371,   498,
     365,   192,   438,   443,   233,   326,   438,   166,   173,   192,
     372,   373,   372,   372,   200,   169,   169,   240,   326,   169,
     167,   242,   169,   183,   192,   262,   263,   242,   241,   192,
     263,   169,   174,   240,   168,   240,   241,   262,   192,   504,
     169,   169,   169,   169,   244,   287,   288,   167,   233,   167,
     201,     1,   242,   214,   270,   240,    82,   116,   271,   273,
      82,   408,   504,   166,   166,   504,   437,   437,   437,   437,
     192,   167,   212,   368,   369,   478,   489,   490,   491,   492,
     192,   173,   192,   192,   443,   437,   242,   242,    84,    85,
     175,   253,   254,   255,   169,   240,    82,   242,   240,   168,
     240,    82,   192,   168,   240,   241,   262,   326,   348,   168,
     240,   242,   260,   263,   158,   159,   162,   163,   263,   264,
     192,   240,   166,   175,   255,   242,   242,   167,   289,   324,
     326,   498,   192,   201,   169,   174,   169,   173,   174,   169,
     242,   167,   242,   242,   242,   166,   437,   437,   166,   490,
     491,   492,   326,   437,   489,   173,   192,   437,   437,   367,
      82,     1,   229,   251,   252,   435,     1,   174,     1,   194,
     242,   253,    82,   192,   169,   242,    82,   192,   183,   183,
     242,   241,   263,   263,   264,   192,    64,   240,   261,   349,
     183,   183,    82,   168,   240,   168,   240,   240,   241,   192,
       1,   194,   289,   192,   286,   167,   212,   434,   489,   198,
     174,   192,   171,   201,   294,   295,   296,   214,   230,   240,
     272,   437,   169,   169,   169,   489,   437,   242,   144,     1,
     173,   174,   166,   299,   300,   437,   242,    82,   192,   242,
     240,   168,   168,   240,   168,   240,   168,   240,   240,   241,
     198,   349,   168,   240,   168,   240,   242,   183,   183,   183,
     183,   166,   299,   286,   228,   169,   326,   438,   174,   112,
     167,   169,   174,   173,   169,   169,    82,   268,   167,   437,
     478,   481,   369,   229,   251,   254,   256,   257,   306,   306,
     242,   183,   183,   183,   183,   168,   168,   240,   168,   240,
     168,   240,   256,   169,   244,   294,   172,   229,   192,   294,
     296,   242,    82,   383,   247,   437,   195,   254,   168,   168,
     240,   168,   240,   168,   240,   195,   244,   174,   201,   169,
     169,   174,   242,   169,     1,   437,   242,   166,   247,   166,
     201,   297,   167,   192,   297,   242,   173,   174,   229,   169,
     201,   200,   298,   169,   192,   169,   173,   192,   200
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   193,   194,   195,   196,   196,   196,   196,   196,   197,
     197,   197,   197,   197,   197,   197,   197,   198,   198,   199,
     199,   200,   200,   200,   201,   202,   202,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   204,   204,   205,   205,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   206,
     206,   206,   206,   206,   206,   206,   206,   206,   206,   207,
     207,   208,   208,   208,   208,   208,   208,   208,   209,   209,
     209,   210,   210,   211,   211,   211,   211,   211,   211,   211,
     211,   211,   211,   211,   211,   211,   211,   211,   211,   211,
     211,   211,   212,   212,   212,   213,   213,   213,   213,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   215,   215,
     215,   215,   216,   216,   217,   217,   218,   218,   218,   218,
     219,   219,   219,   220,   220,   220,   221,   221,   221,   221,
     221,   222,   222,   222,   223,   223,   224,   224,   225,   225,
     226,   226,   227,   227,   228,   228,   228,   229,   230,   230,
     231,   231,   232,   232,   232,   233,   233,   233,   234,   234,
     235,   235,   236,   236,   237,   237,   237,   237,   237,   237,
     237,   237,   237,   237,   237,   238,   238,   238,   238,   238,
     239,   239,   239,   239,   240,   240,   241,   241,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   243,   243,   244,   244,   245,   245,
     246,   246,   246,   246,   246,   247,   247,   247,   248,   249,
     249,   249,   249,   249,   249,   249,   249,   250,   250,   250,
     250,   251,   251,   251,   252,   252,   253,   253,   253,   253,
     253,   254,   254,   255,   256,   256,   257,   257,   258,   258,
     258,   258,   258,   258,   258,   258,   258,   258,   258,   258,
     259,   259,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   260,
     260,   260,   260,   260,   260,   260,   260,   260,   260,   261,
     261,   261,   262,   262,   262,   262,   263,   263,   264,   264,
     264,   264,   265,   265,   265,   265,   265,   265,   265,   265,
     265,   265,   265,   265,   265,   265,   265,   265,   265,   265,
     265,   265,   266,   267,   268,   269,   269,   270,   270,   271,
     272,   272,   273,   273,   274,   274,   274,   274,   274,   274,
     275,   276,   276,   277,   278,   278,   279,   279,   280,   280,
     280,   281,   282,   283,   284,   284,   284,   285,   285,   286,
     286,   287,   287,   287,   287,   288,   289,   289,   289,   289,
     289,   290,   291,   291,   292,   292,   292,   292,   292,   293,
     293,   294,   294,   295,   295,   296,   296,   297,   297,   297,
     298,   298,   299,   299,   300,   300,   301,   301,   302,   302,
     303,   303,   304,   304,   305,   305,   306,   306,   306,   307,
     307,   308,   308,   308,   308,   308,   309,   309,   309,   310,
     310,   310,   310,   310,   310,   311,   311,   311,   311,   311,
     312,   312,   312,   312,   313,   313,   314,   314,   314,   315,
     315,   315,   315,   315,   316,   316,   317,   317,   317,   317,
     318,   318,   318,   318,   318,   319,   319,   320,   320,   320,
     320,   321,   321,   321,   322,   322,   322,   323,   323,   323,
     324,   324,   324,   325,   325,   326,   326,   327,   328,   328,
     328,   328,   328,   329,   330,   330,   330,   331,   331,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   333,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   335,   335,   336,   337,   337,   338,   338,
     338,   338,   338,   339,   339,   339,   340,   340,   340,   340,
     341,   341,   341,   341,   341,   341,   342,   342,   342,   342,
     343,   344,   343,   343,   345,   345,   345,   345,   346,   346,
     346,   347,   347,   347,   347,   348,   348,   348,   349,   349,
     349,   349,   349,   349,   350,   350,   350,   351,   351,   352,
     352,   354,   353,   355,   353,   356,   353,   357,   353,   353,
     358,   358,   359,   359,   360,   360,   361,   361,   361,   362,
     362,   362,   362,   362,   362,   362,   362,   363,   363,   364,
     364,   364,   364,   364,   364,   364,   364,   364,   364,   364,
     364,   365,   365,   366,   366,   367,   367,   367,   367,   368,
     368,   368,   369,   370,   370,   371,   371,   372,   372,   373,
     374,   374,   375,   374,   374,   376,   374,   374,   374,   377,
     377,   378,   378,   379,   379,   380,   380,   380,   380,   380,
     381,   381,   382,   382,   382,   383,   383,   383,   383,   384,
     384,   384,   384,   384,   384,   385,   385,   385,   385,   385,
     385,   385,   386,   386,   386,   386,   387,   387,   388,   388,
     389,   389,   390,   390,   390,   390,   390,   391,   391,   391,
     391,   391,   392,   392,   393,   393,   393,   394,   394,   394,
     394,   395,   395,   395,   395,   396,   396,   397,   397,   397,
     397,   397,   398,   398,   399,   399,   400,   400,   400,   400,
     400,   401,   401,   402,   402,   404,   403,   405,   403,   403,
     403,   403,   406,   406,   406,   406,   407,   407,   407,   407,
     408,   408,   409,   409,   410,   410,   411,   411,   411,   411,
     412,   412,   412,   413,   413,   414,   414,   415,   415,   415,
     415,   416,   416,   417,   417,   418,   418,   418,   419,   419,
     419,   420,   420,   421,   421,   422,   422,   423,   424,   425,
     425,   425,   425,   425,   425,   425,   425,   425,   425,   425,
     426,   425,   427,   425,   428,   425,   429,   425,   430,   425,
     425,   431,   431,   431,   432,   432,   433,   433,   433,   433,
     433,   433,   433,   433,   433,   433,   434,   434,   434,   434,
     435,   436,   436,   437,   437,   438,   438,   439,   439,   439,
     439,   440,   440,   441,   441,   441,   442,   442,   442,   443,
     443,   443,   444,   444,   444,   444,   445,   445,   445,   445,
     445,   446,   446,   446,   446,   446,   446,   446,   447,   447,
     447,   447,   448,   448,   448,   449,   449,   449,   449,   449,
     450,   450,   450,   450,   450,   451,   451,   451,   451,   451,
     451,   452,   452,   452,   453,   453,   453,   453,   453,   454,
     454,   454,   454,   454,   455,   455,   455,   455,   455,   455,
     456,   456,   457,   457,   457,   457,   458,   458,   458,   458,
     458,   459,   459,   459,   459,   459,   459,   459,   460,   460,
     460,   460,   461,   461,   461,   462,   462,   462,   462,   462,
     463,   463,   463,   463,   463,   464,   464,   464,   464,   464,
     464,   465,   465,   465,   465,   465,   466,   466,   466,   466,
     467,   467,   467,   467,   468,   468,   468,   469,   469,   469,
     469,   469,   470,   470,   471,   471,   471,   471,   472,   472,
     473,   473,   474,   474,   474,   475,   475,   475,   475,   475,
     475,   476,   476,   476,   476,   477,   477,   477,   478,   478,
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
       4,     5,     5,     4,     5,     5,     5,     4,     2,     2,
       3,     3,     1,     1,     1,     3,     1,     3,     3,     3,
       1,     3,     3,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     5,     4,     1,     0,     1,
       1,     3,     1,     4,     1,     1,     3,     6,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     4,     6,     6,
       1,     1,     3,     3,     1,     3,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     2,     6,     1,     2,
       2,     3,     2,     3,     2,     2,     3,     2,     2,     5,
       7,     5,    10,     7,     5,    10,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     2,     3,     3,     2,
       2,     1,     2,     2,     0,     1,     2,     3,     4,     6,
       5,     7,     6,     7,     7,     8,     4,     6,     5,     7,
       1,     3,     4,     5,     4,     3,     5,     1,     2,     3,
       3,     3,     5,     5,     5,     5,     3,     5,     5,     5,
       3,     4,     5,     5,     5,     5,     5,     7,     7,     7,
       7,     7,     7,     7,     2,     3,     4,     4,     4,     4,
       6,     6,     6,     6,     6,     6,     6,     3,     4,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     4,     2,     3,     3,     2,     3,     2,
       3,     3,     6,     2,     2,     3,     3,     3,     3,     3,
       3,     5,     5,     5,     4,     0,     1,     1,     3,     4,
       1,     1,     4,     6,     3,     5,     5,     5,     8,     9,
       1,     1,     1,     4,     3,     3,     1,     3,     1,     3,
       5,     1,     2,     5,     3,     3,     4,     6,     7,     0,
       2,     1,     1,     1,     1,     2,     1,     2,     2,     2,
       1,     3,     1,     1,     6,     8,    10,    12,    14,     0,
       1,     0,     1,     1,     3,     4,     7,     0,     1,     3,
       1,     3,     0,     1,     2,     2,     0,     1,     2,     3,
       0,     1,     3,     4,     1,     3,     2,     2,     2,     6,
       4,     1,     1,     1,     1,     1,     2,     3,     6,     3,
       3,     4,     5,     2,     3,     1,     2,     2,     3,     8,
       9,     9,     8,     8,     3,     5,     3,     3,     4,     4,
       4,     4,     3,     4,     4,     5,     2,     1,     1,     1,
       3,     3,     2,     4,     6,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     0,     1,     2,     3,     1,     1,     1,
       1,     1,     1,     4,     1,     2,     3,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     5,     0,     1,     1,     2,
       3,     3,     3,     2,     3,     3,     1,     2,     2,     2,
       4,     4,     4,     4,     1,     1,     1,     2,     2,     3,
       1,     0,     3,     2,     1,     2,     2,     3,     1,     2,
       2,     2,     3,     3,     3,     1,     2,     2,     1,     2,
       3,     1,     2,     3,     1,     3,     4,     1,     1,     1,
       1,     0,     8,     0,    10,     0,    10,     0,    10,     1,
       0,     3,     3,     3,     1,     1,     2,     1,     1,     1,
       2,     1,     2,     1,     2,     1,     2,     0,     3,     3,
       3,     4,     4,     5,     4,     2,     2,     3,     4,     2,
       2,     0,     1,     1,     4,     1,     2,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       8,     9,     0,    11,    10,     0,    12,    11,     1,     2,
       3,     0,     1,     3,     3,     0,     3,     2,     5,     4,
       1,     1,     0,     2,     5,     0,     1,     1,     3,     1,
       2,     1,     2,     4,     4,     0,     1,     1,     1,     3,
       3,     3,     1,     3,     3,     5,     1,     3,     3,     3,
       2,     3,     1,     3,     3,     4,     1,     1,     1,     1,
       2,     1,     1,     3,     1,     2,     1,     1,     2,     1,
       2,     0,     2,     2,     4,     1,     4,     0,     1,     2,
       3,     4,     2,     2,     1,     2,     2,     3,     3,     5,
       4,     1,     3,     0,     2,     0,     5,     0,     5,     4,
       1,     8,     0,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     5,     4,     1,     1,     3,     3,
       2,     3,     3,     2,     4,     1,     4,     7,     5,     8,
       6,     1,     2,     2,     2,     1,     1,     3,     2,     3,
       1,     0,     1,     0,     1,     4,     5,     0,     0,     1,
       1,     2,     2,     2,     2,     2,     2,     1,     2,     5,
       0,     6,     0,     8,     0,     7,     0,     7,     0,     8,
       1,     1,     2,     3,     0,     5,     3,     4,     4,     4,
       4,     5,     5,     5,     5,     6,     1,     1,     1,     1,
       3,     0,     5,     0,     1,     1,     2,     6,     4,     3,
       1,     1,     3,     0,     1,     4,     1,     1,     1,     1,
       2,     3,     2,     1,     2,     2,     2,     3,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     6,     7,
       3,     4,     2,     1,     2,     4,     6,     7,     3,     4,
       2,     3,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     1,     1,     1,     4,     6,     7,     3,     4,     2,
       3,     3,     3,     4,     4,     5,     4,     5,     3,     4,
       1,     3,     2,     1,     2,     2,     2,     3,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     6,     7,
       3,     4,     2,     1,     2,     4,     6,     7,     3,     4,
       2,     3,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     2,     4,     1,     2,     2,     2,     3,     3,     4,
       2,     4,     4,     3,     4,     6,     3,     2,     4,     1,
       2,     2,     1,     1,     2,     3,     3,     4,     2,     4,
       4,     6,     1,     2,     2,     2,     2,     2,     3,     3,
       4,     1,     4,     4,     3,     3,     6,     3,     2,     3,
       4,     5,     3,     1,     1,     1,     3,     3,     3,     5,
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
#line 10278 "Parser/parser.cc"
    break;

  case 3: /* pop: %empty  */
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 10284 "Parser/parser.cc"
    break;

  case 4: /* constant: INTEGERconstant  */
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 5: /* constant: FLOATING_DECIMALconstant  */
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 6: /* constant: FLOATING_FRACTIONconstant  */
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 7: /* constant: FLOATINGconstant  */
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 8: /* constant: CHARACTERconstant  */
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 20: /* identifier_at: '@'  */
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 10320 "Parser/parser.cc"
    break;

  case 24: /* string_literal: string_literal_list  */
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 25: /* string_literal_list: STRINGliteral  */
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 10332 "Parser/parser.cc"
    break;

  case 26: /* string_literal_list: string_literal_list STRINGliteral  */
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 10342 "Parser/parser.cc"
    break;

  case 27: /* primary_expression: IDENTIFIER  */
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 28: /* primary_expression: quasi_keyword  */
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 29: /* primary_expression: TYPEDIMname  */
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 31: /* primary_expression: '(' comma_expression ')'  */
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10366 "Parser/parser.cc"
    break;

  case 32: /* primary_expression: '(' compound_statement ')'  */
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 33: /* primary_expression: type_name '.' identifier  */
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, DeclarationNode::newFromTypeData( (yyvsp[-2].type) ), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 34: /* primary_expression: type_name '.' '[' field_name_list ']'  */
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10384 "Parser/parser.cc"
    break;

  case 35: /* primary_expression: GENERIC '(' assignment_expression ',' generic_assoc_list ')'  */
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 10394 "Parser/parser.cc"
    break;

  case 36: /* primary_expression: IDENTIFIER IDENTIFIER  */
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "expression" ); (yyval.expr) = nullptr; }
#line 10400 "Parser/parser.cc"
    break;

  case 37: /* primary_expression: IDENTIFIER type_qualifier  */
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 10406 "Parser/parser.cc"
    break;

  case 38: /* primary_expression: IDENTIFIER storage_class  */
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 10412 "Parser/parser.cc"
    break;

  case 39: /* primary_expression: IDENTIFIER basic_type_name  */
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10418 "Parser/parser.cc"
    break;

  case 40: /* primary_expression: IDENTIFIER TYPEDEFname  */
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10424 "Parser/parser.cc"
    break;

  case 41: /* primary_expression: IDENTIFIER TYPEGENname  */
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 10430 "Parser/parser.cc"
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
#line 10442 "Parser/parser.cc"
    break;

  case 44: /* generic_association: type_no_function ':' assignment_expression  */
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 10451 "Parser/parser.cc"
    break;

  case 45: /* generic_association: DEFAULT ':' assignment_expression  */
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 10457 "Parser/parser.cc"
    break;

  case 47: /* postfix_expression: postfix_expression '[' tuple_expression_list ']'  */
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), new ExpressionNode( build_tuple( yylloc, (yyvsp[-1].expr) ) ) ) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 48: /* postfix_expression: constant '[' assignment_expression ']'  */
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 49: /* postfix_expression: string_literal '[' assignment_expression ']'  */
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 50: /* postfix_expression: postfix_expression '{' argument_expression_list_opt '}'  */
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 10485 "Parser/parser.cc"
    break;

  case 51: /* postfix_expression: postfix_expression '(' argument_expression_list_opt ')'  */
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 10491 "Parser/parser.cc"
    break;

  case 52: /* postfix_expression: VA_ARG '(' primary_expression ',' declaration_specifier_nobody abstract_parameter_declarator_opt ')'  */
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_va_arg( yylloc, (yyvsp[-4].expr), ( (yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl) ) ) ); }
#line 10497 "Parser/parser.cc"
    break;

  case 53: /* postfix_expression: postfix_expression '`' identifier  */
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10503 "Parser/parser.cc"
    break;

  case 54: /* postfix_expression: constant '`' identifier  */
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10509 "Parser/parser.cc"
    break;

  case 55: /* postfix_expression: string_literal '`' identifier  */
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 10515 "Parser/parser.cc"
    break;

  case 56: /* postfix_expression: postfix_expression '.' identifier_or_type_name  */
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10521 "Parser/parser.cc"
    break;

  case 57: /* postfix_expression: postfix_expression '.' INTEGERconstant  */
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10527 "Parser/parser.cc"
    break;

  case 58: /* postfix_expression: postfix_expression FLOATING_FRACTIONconstant  */
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10533 "Parser/parser.cc"
    break;

  case 59: /* postfix_expression: postfix_expression '.' '[' field_name_list ']'  */
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10539 "Parser/parser.cc"
    break;

  case 60: /* postfix_expression: postfix_expression '.' aggregate_control  */
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 10545 "Parser/parser.cc"
    break;

  case 61: /* postfix_expression: postfix_expression ARROW identifier  */
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 10551 "Parser/parser.cc"
    break;

  case 62: /* postfix_expression: postfix_expression ARROW INTEGERconstant  */
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 10557 "Parser/parser.cc"
    break;

  case 63: /* postfix_expression: postfix_expression ARROW '[' field_name_list ']'  */
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10563 "Parser/parser.cc"
    break;

  case 64: /* postfix_expression: postfix_expression ICR  */
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 10569 "Parser/parser.cc"
    break;

  case 65: /* postfix_expression: postfix_expression DECR  */
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 10575 "Parser/parser.cc"
    break;

  case 66: /* postfix_expression: '(' type_no_function ')' '{' initializer_list_opt comma_opt '}'  */
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 10581 "Parser/parser.cc"
    break;

  case 67: /* postfix_expression: '(' type_no_function ')' '@' '{' initializer_list_opt comma_opt '}'  */
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 10587 "Parser/parser.cc"
    break;

  case 68: /* postfix_expression: '^' primary_expression '{' argument_expression_list_opt '}'  */
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 10597 "Parser/parser.cc"
    break;

  case 70: /* field_name_list: field_name_list ',' field  */
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 10603 "Parser/parser.cc"
    break;

  case 72: /* field: FLOATING_DECIMALconstant field  */
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10609 "Parser/parser.cc"
    break;

  case 73: /* field: FLOATING_DECIMALconstant '[' field_name_list ']'  */
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10615 "Parser/parser.cc"
    break;

  case 74: /* field: field_name '.' field  */
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10621 "Parser/parser.cc"
    break;

  case 75: /* field: field_name '.' '[' field_name_list ']'  */
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 76: /* field: field_name ARROW field  */
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 10633 "Parser/parser.cc"
    break;

  case 77: /* field: field_name ARROW '[' field_name_list ']'  */
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 10639 "Parser/parser.cc"
    break;

  case 78: /* field_name: INTEGERconstant fraction_constants_opt  */
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10645 "Parser/parser.cc"
    break;

  case 79: /* field_name: FLOATINGconstant fraction_constants_opt  */
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 10651 "Parser/parser.cc"
    break;

  case 80: /* field_name: identifier_at fraction_constants_opt  */
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 10659 "Parser/parser.cc"
    break;

  case 81: /* fraction_constants_opt: %empty  */
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10665 "Parser/parser.cc"
    break;

  case 82: /* fraction_constants_opt: fraction_constants_opt FLOATING_FRACTIONconstant  */
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 10674 "Parser/parser.cc"
    break;

  case 85: /* unary_expression: string_literal  */
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10680 "Parser/parser.cc"
    break;

  case 86: /* unary_expression: EXTENSION cast_expression  */
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 10686 "Parser/parser.cc"
    break;

  case 87: /* unary_expression: ptrref_operator cast_expression  */
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10706 "Parser/parser.cc"
    break;

  case 88: /* unary_expression: unary_operator cast_expression  */
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 89: /* unary_expression: ICR unary_expression  */
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 90: /* unary_expression: DECR unary_expression  */
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 10724 "Parser/parser.cc"
    break;

  case 91: /* unary_expression: SIZEOF unary_expression  */
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 92: /* unary_expression: SIZEOF '(' type_no_function ')'  */
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10736 "Parser/parser.cc"
    break;

  case 93: /* unary_expression: SIZEOF '(' attribute_list type_no_function ')'  */
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ) ) ) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 94: /* unary_expression: ALIGNOF unary_expression  */
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10748 "Parser/parser.cc"
    break;

  case 95: /* unary_expression: ALIGNOF '(' type_no_function ')'  */
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 96: /* unary_expression: SIZEOF '(' cfa_abstract_function ')'  */
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 97: /* unary_expression: ALIGNOF '(' cfa_abstract_function ')'  */
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10766 "Parser/parser.cc"
    break;

  case 98: /* unary_expression: OFFSETOF '(' type_no_function ',' identifier ')'  */
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 99: /* unary_expression: TYPEID '(' type ')'  */
#line 952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 10781 "Parser/parser.cc"
    break;

  case 100: /* unary_expression: COUNTOF unary_expression  */
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, new ast::TypeofType( maybeMoveBuild( (yyvsp[0].expr) ) ) ) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 101: /* unary_expression: COUNTOF '(' type_no_function ')'  */
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CountofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 10793 "Parser/parser.cc"
    break;

  case 102: /* ptrref_operator: '*'  */
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 10799 "Parser/parser.cc"
    break;

  case 103: /* ptrref_operator: '&'  */
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 10805 "Parser/parser.cc"
    break;

  case 104: /* ptrref_operator: ANDAND  */
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 10811 "Parser/parser.cc"
    break;

  case 105: /* unary_operator: '+'  */
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 10817 "Parser/parser.cc"
    break;

  case 106: /* unary_operator: '-'  */
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 10823 "Parser/parser.cc"
    break;

  case 107: /* unary_operator: '!'  */
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 10829 "Parser/parser.cc"
    break;

  case 108: /* unary_operator: '~'  */
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 10835 "Parser/parser.cc"
    break;

  case 110: /* cast_expression: '(' type_no_function ')' cast_expression  */
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 111: /* cast_expression: '(' aggregate_control '&' ')' cast_expression  */
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 112: /* cast_expression: '(' aggregate_control '*' ')' cast_expression  */
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 113: /* cast_expression: '(' VIRTUAL ')' cast_expression  */
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), nullptr ) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 114: /* cast_expression: '(' VIRTUAL type_no_function ')' cast_expression  */
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 115: /* cast_expression: '(' RETURN type_no_function ')' cast_expression  */
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::ReturnCast ) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 116: /* cast_expression: '(' COERCE type_no_function ')' cast_expression  */
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10877 "Parser/parser.cc"
    break;

  case 117: /* cast_expression: '(' qualifier_cast_list ')' cast_expression  */
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 10883 "Parser/parser.cc"
    break;

  case 125: /* exponential_expression: exponential_expression '\\' cast_expression  */
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 127: /* multiplicative_expression: multiplicative_expression '*' exponential_expression  */
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 128: /* multiplicative_expression: multiplicative_expression '/' exponential_expression  */
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 129: /* multiplicative_expression: multiplicative_expression '%' exponential_expression  */
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 131: /* additive_expression: additive_expression '+' multiplicative_expression  */
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 132: /* additive_expression: additive_expression '-' multiplicative_expression  */
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10919 "Parser/parser.cc"
    break;

  case 134: /* shift_expression: shift_expression LS additive_expression  */
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 135: /* shift_expression: shift_expression RS additive_expression  */
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10931 "Parser/parser.cc"
    break;

  case 137: /* relational_expression: relational_expression '<' shift_expression  */
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 138: /* relational_expression: relational_expression '>' shift_expression  */
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10943 "Parser/parser.cc"
    break;

  case 139: /* relational_expression: relational_expression LE shift_expression  */
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10949 "Parser/parser.cc"
    break;

  case 140: /* relational_expression: relational_expression GE shift_expression  */
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 142: /* equality_expression: equality_expression EQ relational_expression  */
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10961 "Parser/parser.cc"
    break;

  case 143: /* equality_expression: equality_expression NE relational_expression  */
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10967 "Parser/parser.cc"
    break;

  case 145: /* AND_expression: AND_expression '&' equality_expression  */
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10973 "Parser/parser.cc"
    break;

  case 147: /* exclusive_OR_expression: exclusive_OR_expression '^' AND_expression  */
#line 1071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10979 "Parser/parser.cc"
    break;

  case 149: /* inclusive_OR_expression: inclusive_OR_expression '|' exclusive_OR_expression  */
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 10985 "Parser/parser.cc"
    break;

  case 151: /* logical_AND_expression: logical_AND_expression ANDAND inclusive_OR_expression  */
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 10991 "Parser/parser.cc"
    break;

  case 153: /* logical_OR_expression: logical_OR_expression OROR logical_AND_expression  */
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 10997 "Parser/parser.cc"
    break;

  case 155: /* conditional_expression: logical_OR_expression '?' comma_expression ':' conditional_expression  */
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 11003 "Parser/parser.cc"
    break;

  case 156: /* conditional_expression: logical_OR_expression '?' ':' conditional_expression  */
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), nullptr, (yyvsp[0].expr) ) ); }
#line 11009 "Parser/parser.cc"
    break;

  case 158: /* argument_expression_list_opt: %empty  */
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11015 "Parser/parser.cc"
    break;

  case 161: /* argument_expression_list: argument_expression_list ',' argument_expression  */
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 162: /* argument_expression: '?'  */
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *new string( "2" ) ) ); }
#line 11027 "Parser/parser.cc"
    break;

  case 163: /* argument_expression: '?' identifier '=' assignment_expression  */
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11033 "Parser/parser.cc"
    break;

  case 166: /* assignment_expression: unary_expression assignment_operator assignment_expression  */
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 11045 "Parser/parser.cc"
    break;

  case 167: /* assignment_expression: unary_expression '=' '{' initializer_list_opt comma_opt '}'  */
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11051 "Parser/parser.cc"
    break;

  case 168: /* assignment_expression_opt: %empty  */
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11057 "Parser/parser.cc"
    break;

  case 172: /* simple_assignment_operator: '='  */
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 11063 "Parser/parser.cc"
    break;

  case 173: /* simple_assignment_operator: ATassign  */
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 11069 "Parser/parser.cc"
    break;

  case 174: /* compound_assignment_operator: EXPassign  */
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 11075 "Parser/parser.cc"
    break;

  case 175: /* compound_assignment_operator: MULTassign  */
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 11081 "Parser/parser.cc"
    break;

  case 176: /* compound_assignment_operator: DIVassign  */
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 11087 "Parser/parser.cc"
    break;

  case 177: /* compound_assignment_operator: MODassign  */
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 11093 "Parser/parser.cc"
    break;

  case 178: /* compound_assignment_operator: PLUSassign  */
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 11099 "Parser/parser.cc"
    break;

  case 179: /* compound_assignment_operator: MINUSassign  */
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 11105 "Parser/parser.cc"
    break;

  case 180: /* compound_assignment_operator: LSassign  */
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 11111 "Parser/parser.cc"
    break;

  case 181: /* compound_assignment_operator: RSassign  */
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 11117 "Parser/parser.cc"
    break;

  case 182: /* compound_assignment_operator: ANDassign  */
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 11123 "Parser/parser.cc"
    break;

  case 183: /* compound_assignment_operator: ERassign  */
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 11129 "Parser/parser.cc"
    break;

  case 184: /* compound_assignment_operator: ORassign  */
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 11135 "Parser/parser.cc"
    break;

  case 185: /* tuple: '[' ',' ']'  */
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Empty tuple is meaningless." ); (yyval.expr) = nullptr; }
#line 11141 "Parser/parser.cc"
    break;

  case 186: /* tuple: '[' assignment_expression ',' ']'  */
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-2].expr) ) ); }
#line 11147 "Parser/parser.cc"
    break;

  case 187: /* tuple: '[' '@' comma_opt ']'  */
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11153 "Parser/parser.cc"
    break;

  case 188: /* tuple: '[' assignment_expression ',' tuple_expression_list comma_opt ']'  */
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (yyvsp[-4].expr)->set_last( (yyvsp[-2].expr) ) ) ); }
#line 11159 "Parser/parser.cc"
    break;

  case 189: /* tuple: '[' '@' ',' tuple_expression_list comma_opt ']'  */
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11165 "Parser/parser.cc"
    break;

  case 191: /* tuple_expression_list: '@'  */
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11171 "Parser/parser.cc"
    break;

  case 192: /* tuple_expression_list: tuple_expression_list ',' assignment_expression  */
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 11177 "Parser/parser.cc"
    break;

  case 193: /* tuple_expression_list: tuple_expression_list ',' '@'  */
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 11183 "Parser/parser.cc"
    break;

  case 195: /* comma_expression: comma_expression ',' assignment_expression  */
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11189 "Parser/parser.cc"
    break;

  case 196: /* comma_expression_opt: %empty  */
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11195 "Parser/parser.cc"
    break;

  case 211: /* statement: enable_disable_statement  */
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 11201 "Parser/parser.cc"
    break;

  case 213: /* statement: DIRECTIVE  */
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 214: /* labelled_statement: identifier_or_type_name ':' attribute_list_opt statement  */
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 215: /* labelled_statement: identifier_or_type_name ':' attribute_list_opt error  */
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "syntx error, label \"%s\" must be associated with a statement, "
						   "where a declaration, case, or default is not a statement.\n"
						   "Move the label or terminate with a semicolon.", (yyvsp[-3].tok).str->c_str() );
			(yyval.stmt) = nullptr;
		}
#line 11224 "Parser/parser.cc"
    break;

  case 216: /* compound_statement: '{' '}'  */
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 11230 "Parser/parser.cc"
    break;

  case 217: /* compound_statement: '{' push local_label_declaration_opt statement_decl_list pop '}'  */
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 11236 "Parser/parser.cc"
    break;

  case 219: /* statement_decl_list: statement_decl_list statement_decl  */
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 11242 "Parser/parser.cc"
    break;

  case 220: /* statement_decl: attribute_list_opt declaration  */
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 221: /* statement_decl: attribute_list_opt EXTENSION declaration  */
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 222: /* statement_decl: attribute_list_opt function_definition  */
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 223: /* statement_decl: attribute_list_opt EXTENSION function_definition  */
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-2].decl), (yyvsp[0].decl) ); distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 224: /* statement_decl: attribute_list_opt statement  */
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11272 "Parser/parser.cc"
    break;

  case 225: /* statement_list_nodecl: attribute_list_opt statement  */
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11278 "Parser/parser.cc"
    break;

  case 226: /* statement_list_nodecl: statement_list_nodecl attribute_list_opt statement  */
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-2].stmt) ); (yyvsp[-2].stmt)->set_last( (yyvsp[0].stmt)->addQualifiers( (yyvsp[-1].decl) ) ); (yyval.stmt) = (yyvsp[-2].stmt); }
#line 11284 "Parser/parser.cc"
    break;

  case 227: /* statement_list_nodecl: statement_list_nodecl error  */
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations only allowed at the start of the switch body,"
						 " i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 11291 "Parser/parser.cc"
    break;

  case 228: /* expression_statement: comma_expression_opt ';'  */
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 11297 "Parser/parser.cc"
    break;

  case 229: /* selection_statement: IF '(' conditional_declaration ')' statement  */
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 11303 "Parser/parser.cc"
    break;

  case 230: /* selection_statement: IF '(' conditional_declaration ')' statement ELSE statement  */
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11309 "Parser/parser.cc"
    break;

  case 231: /* selection_statement: SWITCH '(' comma_expression ')' case_clause  */
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11315 "Parser/parser.cc"
    break;

  case 232: /* selection_statement: SWITCH '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}'  */
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 11329 "Parser/parser.cc"
    break;

  case 233: /* selection_statement: SWITCH '(' comma_expression ')' '{' error '}'  */
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11335 "Parser/parser.cc"
    break;

  case 234: /* selection_statement: CHOOSE '(' comma_expression ')' case_clause  */
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 235: /* selection_statement: CHOOSE '(' comma_expression ')' '{' push declaration_list_opt switch_clause_list_opt pop '}'  */
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (new StatementNode( (yyvsp[-3].decl) ))->set_last( sw ) ) ) : sw;
		}
#line 11350 "Parser/parser.cc"
    break;

  case 236: /* selection_statement: CHOOSE '(' comma_expression ')' '{' error '}'  */
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 11356 "Parser/parser.cc"
    break;

  case 237: /* conditional_declaration: comma_expression  */
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( nullptr, (yyvsp[0].expr) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 238: /* conditional_declaration: c_declaration  */
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11368 "Parser/parser.cc"
    break;

  case 239: /* conditional_declaration: cfa_declaration  */
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[0].decl), nullptr ); }
#line 11374 "Parser/parser.cc"
    break;

  case 240: /* conditional_declaration: declaration comma_expression  */
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctrl) = new CondCtrl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 241: /* case_value: constant_expression  */
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 11386 "Parser/parser.cc"
    break;

  case 242: /* case_value: constant_expression ELLIPSIS constant_expression  */
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 244: /* case_value_list: case_value  */
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 11398 "Parser/parser.cc"
    break;

  case 245: /* case_value_list: case_value_list ',' case_value  */
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 246: /* case_label: CASE error  */
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, case list missing after case." ); (yyval.clause) = nullptr; }
#line 11410 "Parser/parser.cc"
    break;

  case 247: /* case_label: CASE case_value_list ':'  */
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 11416 "Parser/parser.cc"
    break;

  case 248: /* case_label: CASE case_value_list error  */
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 11422 "Parser/parser.cc"
    break;

  case 249: /* case_label: DEFAULT ':'  */
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 11428 "Parser/parser.cc"
    break;

  case 250: /* case_label: DEFAULT error  */
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, colon missing after default." ); (yyval.clause) = nullptr; }
#line 11434 "Parser/parser.cc"
    break;

  case 252: /* case_label_list: case_label_list case_label  */
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 11440 "Parser/parser.cc"
    break;

  case 253: /* case_clause: case_label_list statement  */
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 11446 "Parser/parser.cc"
    break;

  case 254: /* switch_clause_list_opt: %empty  */
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 11452 "Parser/parser.cc"
    break;

  case 256: /* switch_clause_list: case_label_list statement_list_nodecl  */
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11458 "Parser/parser.cc"
    break;

  case 257: /* switch_clause_list: switch_clause_list case_label_list statement_list_nodecl  */
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 11464 "Parser/parser.cc"
    break;

  case 258: /* iteration_statement: WHILE '(' ')' statement  */
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11470 "Parser/parser.cc"
    break;

  case 259: /* iteration_statement: WHILE '(' ')' statement ELSE statement  */
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtrl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11479 "Parser/parser.cc"
    break;

  case 260: /* iteration_statement: WHILE '(' conditional_declaration ')' statement  */
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 261: /* iteration_statement: WHILE '(' conditional_declaration ')' statement ELSE statement  */
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 262: /* iteration_statement: DO statement WHILE '(' ')' ';'  */
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 263: /* iteration_statement: DO statement WHILE '(' ')' ELSE statement  */
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11506 "Parser/parser.cc"
    break;

  case 264: /* iteration_statement: DO statement WHILE '(' comma_expression ')' ';'  */
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 11512 "Parser/parser.cc"
    break;

  case 265: /* iteration_statement: DO statement WHILE '(' comma_expression ')' ELSE statement  */
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11518 "Parser/parser.cc"
    break;

  case 266: /* iteration_statement: FOR '(' ')' statement  */
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11524 "Parser/parser.cc"
    break;

  case 267: /* iteration_statement: FOR '(' ')' statement ELSE statement  */
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 11533 "Parser/parser.cc"
    break;

  case 268: /* iteration_statement: FOR '(' for_control_expression_list ')' statement  */
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 11539 "Parser/parser.cc"
    break;

  case 269: /* iteration_statement: FOR '(' for_control_expression_list ')' statement ELSE statement  */
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctrl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 271: /* for_control_expression_list: for_control_expression_list ':' for_control_expression  */
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11564 "Parser/parser.cc"
    break;

  case 272: /* for_control_expression: ';' comma_expression_opt ';' comma_expression_opt  */
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 273: /* for_control_expression: comma_expression ';' comma_expression_opt ';' comma_expression_opt  */
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = new ForCtrl( (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 11578 "Parser/parser.cc"
    break;

  case 274: /* for_control_expression: declaration comma_expression_opt ';' comma_expression_opt  */
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11584 "Parser/parser.cc"
    break;

  case 275: /* for_control_expression: '@' ';' comma_expression  */
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 11590 "Parser/parser.cc"
    break;

  case 276: /* for_control_expression: '@' ';' comma_expression ';' comma_expression  */
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 11596 "Parser/parser.cc"
    break;

  case 277: /* for_control_expression: comma_expression  */
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11602 "Parser/parser.cc"
    break;

  case 278: /* for_control_expression: updown comma_expression  */
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11608 "Parser/parser.cc"
    break;

  case 279: /* for_control_expression: comma_expression updownS comma_expression  */
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11614 "Parser/parser.cc"
    break;

  case 280: /* for_control_expression: '@' updownS comma_expression  */
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11623 "Parser/parser.cc"
    break;

  case 281: /* for_control_expression: comma_expression updownS '@'  */
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11632 "Parser/parser.cc"
    break;

  case 282: /* for_control_expression: comma_expression updownS comma_expression '~' comma_expression  */
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11638 "Parser/parser.cc"
    break;

  case 283: /* for_control_expression: '@' updownS comma_expression '~' comma_expression  */
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11647 "Parser/parser.cc"
    break;

  case 284: /* for_control_expression: comma_expression updownS '@' '~' comma_expression  */
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
		}
#line 11656 "Parser/parser.cc"
    break;

  case 285: /* for_control_expression: comma_expression updownS comma_expression '~' '@'  */
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11662 "Parser/parser.cc"
    break;

  case 286: /* for_control_expression: '@' updownS '@'  */
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11668 "Parser/parser.cc"
    break;

  case 287: /* for_control_expression: '@' updownS comma_expression '~' '@'  */
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11674 "Parser/parser.cc"
    break;

  case 288: /* for_control_expression: comma_expression updownS '@' '~' '@'  */
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11680 "Parser/parser.cc"
    break;

  case 289: /* for_control_expression: '@' updownS '@' '~' '@'  */
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctrl) = nullptr; }
#line 11686 "Parser/parser.cc"
    break;

  case 290: /* for_control_expression: comma_expression ';' comma_expression  */
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 11692 "Parser/parser.cc"
    break;

  case 291: /* for_control_expression: comma_expression ';' updown comma_expression  */
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11698 "Parser/parser.cc"
    break;

  case 292: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression  */
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11704 "Parser/parser.cc"
    break;

  case 293: /* for_control_expression: comma_expression ';' '@' updownS comma_expression  */
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11713 "Parser/parser.cc"
    break;

  case 294: /* for_control_expression: comma_expression ';' comma_expression updownS '@'  */
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11723 "Parser/parser.cc"
    break;

  case 295: /* for_control_expression: comma_expression ';' '@' updownS '@'  */
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11729 "Parser/parser.cc"
    break;

  case 296: /* for_control_expression: comma_expression ';' comma_expression updownEq comma_expression  */
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11735 "Parser/parser.cc"
    break;

  case 297: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression '~' comma_expression  */
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11741 "Parser/parser.cc"
    break;

  case 298: /* for_control_expression: comma_expression ';' '@' updownS comma_expression '~' comma_expression  */
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11750 "Parser/parser.cc"
    break;

  case 299: /* for_control_expression: comma_expression ';' comma_expression updownS '@' '~' comma_expression  */
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11760 "Parser/parser.cc"
    break;

  case 300: /* for_control_expression: comma_expression ';' comma_expression updownS comma_expression '~' '@'  */
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11766 "Parser/parser.cc"
    break;

  case 301: /* for_control_expression: comma_expression ';' '@' updownS comma_expression '~' '@'  */
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11775 "Parser/parser.cc"
    break;

  case 302: /* for_control_expression: comma_expression ';' comma_expression updownS '@' '~' '@'  */
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11785 "Parser/parser.cc"
    break;

  case 303: /* for_control_expression: comma_expression ';' '@' updownS '@' '~' '@'  */
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11791 "Parser/parser.cc"
    break;

  case 304: /* for_control_expression: declaration comma_expression  */
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 11797 "Parser/parser.cc"
    break;

  case 305: /* for_control_expression: declaration updown comma_expression  */
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 11803 "Parser/parser.cc"
    break;

  case 306: /* for_control_expression: declaration comma_expression updownS comma_expression  */
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11809 "Parser/parser.cc"
    break;

  case 307: /* for_control_expression: declaration '@' updownS comma_expression  */
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11818 "Parser/parser.cc"
    break;

  case 308: /* for_control_expression: declaration comma_expression updownS '@'  */
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 11828 "Parser/parser.cc"
    break;

  case 309: /* for_control_expression: declaration comma_expression updownEq comma_expression  */
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 11834 "Parser/parser.cc"
    break;

  case 310: /* for_control_expression: declaration comma_expression updownS comma_expression '~' comma_expression  */
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 11840 "Parser/parser.cc"
    break;

  case 311: /* for_control_expression: declaration '@' updownS comma_expression '~' comma_expression  */
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11849 "Parser/parser.cc"
    break;

  case 312: /* for_control_expression: declaration comma_expression updownS '@' '~' comma_expression  */
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 11859 "Parser/parser.cc"
    break;

  case 313: /* for_control_expression: declaration comma_expression updownS comma_expression '~' '@'  */
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 11865 "Parser/parser.cc"
    break;

  case 314: /* for_control_expression: declaration '@' updownS comma_expression '~' '@'  */
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11874 "Parser/parser.cc"
    break;

  case 315: /* for_control_expression: declaration comma_expression updownS '@' '~' '@'  */
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctrl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "illegal syntax, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctrl) = nullptr; }
			else (yyval.forctrl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 11884 "Parser/parser.cc"
    break;

  case 316: /* for_control_expression: declaration '@' updownS '@' '~' '@'  */
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, missing low/high value for ascending/descending range so index is uninitialized." ); (yyval.forctrl) = nullptr; }
#line 11890 "Parser/parser.cc"
    break;

  case 317: /* for_control_expression: comma_expression ';' type_type_specifier  */
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-2].expr), OperKinds::LEThan, new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11898 "Parser/parser.cc"
    break;

  case 318: /* for_control_expression: comma_expression ';' updown enum_key  */
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan ) {
				SemanticError( yylloc, "all enumeration ranges are equal (all values). Add an equal, e.g., ~=, -~=." ); (yyval.forctrl) = nullptr;
				(yyvsp[-1].oper) = OperKinds::GEThan;
			} // if
			(yyval.forctrl) = enumRangeCtrl( (yyvsp[-3].expr), (yyvsp[-1].oper), new ExpressionNode( new ast::TypeExpr( yylloc, (yyvsp[0].decl)->clone()->buildType() ) ), (yyvsp[0].decl) );
		}
#line 11910 "Parser/parser.cc"
    break;

  case 319: /* enum_key: type_name  */
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11919 "Parser/parser.cc"
    break;

  case 320: /* enum_key: ENUM identifier  */
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false );
		}
#line 11928 "Parser/parser.cc"
    break;

  case 321: /* enum_key: ENUM type_name  */
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 3" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false );
		}
#line 11937 "Parser/parser.cc"
    break;

  case 322: /* updown: ErangeUpLt  */
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11943 "Parser/parser.cc"
    break;

  case 323: /* updown: ErangeDownGt  */
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 11949 "Parser/parser.cc"
    break;

  case 324: /* updown: ErangeUpLe  */
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 11955 "Parser/parser.cc"
    break;

  case 325: /* updown: ErangeDownGe  */
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 11961 "Parser/parser.cc"
    break;

  case 326: /* updownS: '~'  */
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 11967 "Parser/parser.cc"
    break;

  case 328: /* updownEq: ErangeEq  */
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 11973 "Parser/parser.cc"
    break;

  case 329: /* updownEq: ErangeNe  */
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 11979 "Parser/parser.cc"
    break;

  case 330: /* updownEq: ErangeDownEq  */
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Eq; }
#line 11985 "Parser/parser.cc"
    break;

  case 331: /* updownEq: ErangeDownNe  */
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::Neq; }
#line 11991 "Parser/parser.cc"
    break;

  case 332: /* jump_statement: GOTO identifier_or_type_name ';'  */
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 11997 "Parser/parser.cc"
    break;

  case 333: /* jump_statement: GOTO '*' comma_expression ';'  */
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 12003 "Parser/parser.cc"
    break;

  case 334: /* jump_statement: FALLTHROUGH ';'  */
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 12009 "Parser/parser.cc"
    break;

  case 335: /* jump_statement: FALLTHROUGH identifier_or_type_name ';'  */
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 12015 "Parser/parser.cc"
    break;

  case 336: /* jump_statement: FALLTHROUGH DEFAULT ';'  */
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 12021 "Parser/parser.cc"
    break;

  case 337: /* jump_statement: CONTINUE ';'  */
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 12027 "Parser/parser.cc"
    break;

  case 338: /* jump_statement: CONTINUE identifier_or_type_name ';'  */
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 339: /* jump_statement: BREAK ';'  */
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 12039 "Parser/parser.cc"
    break;

  case 340: /* jump_statement: BREAK identifier_or_type_name ';'  */
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 12045 "Parser/parser.cc"
    break;

  case 341: /* jump_statement: RETURN comma_expression_opt ';'  */
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 12051 "Parser/parser.cc"
    break;

  case 342: /* jump_statement: RETURN '{' initializer_list_opt comma_opt '}' ';'  */
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 12057 "Parser/parser.cc"
    break;

  case 343: /* jump_statement: SUSPEND ';'  */
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 12063 "Parser/parser.cc"
    break;

  case 344: /* jump_statement: SUSPEND compound_statement  */
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 12069 "Parser/parser.cc"
    break;

  case 345: /* jump_statement: SUSPEND COROUTINE ';'  */
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 12075 "Parser/parser.cc"
    break;

  case 346: /* jump_statement: SUSPEND COROUTINE compound_statement  */
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 347: /* jump_statement: SUSPEND GENERATOR ';'  */
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 12087 "Parser/parser.cc"
    break;

  case 348: /* jump_statement: SUSPEND GENERATOR compound_statement  */
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 12093 "Parser/parser.cc"
    break;

  case 349: /* jump_statement: THROW assignment_expression_opt ';'  */
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 350: /* jump_statement: THROWRESUME assignment_expression_opt ';'  */
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 351: /* jump_statement: THROWRESUME assignment_expression_opt AT assignment_expression ';'  */
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 12111 "Parser/parser.cc"
    break;

  case 352: /* with_statement: WITH '(' type_list ')' statement  */
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 12117 "Parser/parser.cc"
    break;

  case 353: /* mutex_statement: MUTEX '(' argument_expression_list_opt ')' statement  */
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "illegal syntax, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 12126 "Parser/parser.cc"
    break;

  case 354: /* when_clause: WHEN '(' comma_expression ')'  */
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12132 "Parser/parser.cc"
    break;

  case 355: /* when_clause_opt: %empty  */
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12138 "Parser/parser.cc"
    break;

  case 358: /* cast_expression_list: cast_expression_list ',' cast_expression  */
#line 1769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 12144 "Parser/parser.cc"
    break;

  case 359: /* timeout: TIMEOUT '(' comma_expression ')'  */
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12150 "Parser/parser.cc"
    break;

  case 362: /* waitfor: WAITFOR '(' cast_expression ')'  */
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12156 "Parser/parser.cc"
    break;

  case 363: /* waitfor: WAITFOR '(' cast_expression_list ':' argument_expression_list_opt ')'  */
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ); }
#line 12162 "Parser/parser.cc"
    break;

  case 364: /* wor_waitfor_clause: when_clause_opt waitfor statement  */
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12168 "Parser/parser.cc"
    break;

  case 365: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt waitfor statement  */
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12174 "Parser/parser.cc"
    break;

  case 366: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt ELSE statement  */
#line 1794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12180 "Parser/parser.cc"
    break;

  case 367: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement  */
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12186 "Parser/parser.cc"
    break;

  case 368: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement wor ELSE statement  */
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "illegal syntax, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 12192 "Parser/parser.cc"
    break;

  case 369: /* wor_waitfor_clause: wor_waitfor_clause wor when_clause_opt timeout statement wor when_clause ELSE statement  */
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12198 "Parser/parser.cc"
    break;

  case 370: /* waitfor_statement: wor_waitfor_clause  */
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 12204 "Parser/parser.cc"
    break;

  case 373: /* waituntil: WAITUNTIL '(' comma_expression ')'  */
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 12210 "Parser/parser.cc"
    break;

  case 374: /* waituntil_clause: when_clause_opt waituntil statement  */
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 12216 "Parser/parser.cc"
    break;

  case 375: /* waituntil_clause: '(' wor_waituntil_clause ')'  */
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 12222 "Parser/parser.cc"
    break;

  case 376: /* wand_waituntil_clause: waituntil_clause  */
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 12228 "Parser/parser.cc"
    break;

  case 377: /* wand_waituntil_clause: waituntil_clause wand wand_waituntil_clause  */
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 12234 "Parser/parser.cc"
    break;

  case 378: /* wor_waituntil_clause: wand_waituntil_clause  */
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 12240 "Parser/parser.cc"
    break;

  case 379: /* wor_waituntil_clause: wor_waituntil_clause wor wand_waituntil_clause  */
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 12246 "Parser/parser.cc"
    break;

  case 380: /* wor_waituntil_clause: wor_waituntil_clause wor when_clause_opt ELSE statement  */
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 12252 "Parser/parser.cc"
    break;

  case 381: /* waituntil_statement: wor_waituntil_clause  */
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );	}
#line 12258 "Parser/parser.cc"
    break;

  case 382: /* corun_statement: CORUN statement  */
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_corun( yylloc, (yyvsp[0].stmt) ) ); }
#line 12264 "Parser/parser.cc"
    break;

  case 383: /* cofor_statement: COFOR '(' for_control_expression_list ')' statement  */
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_cofor( yylloc, (yyvsp[-2].forctrl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 12270 "Parser/parser.cc"
    break;

  case 384: /* exception_statement: TRY compound_statement handler_clause  */
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 12276 "Parser/parser.cc"
    break;

  case 385: /* exception_statement: TRY compound_statement finally_clause  */
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 12282 "Parser/parser.cc"
    break;

  case 386: /* exception_statement: TRY compound_statement handler_clause finally_clause  */
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 12288 "Parser/parser.cc"
    break;

  case 387: /* handler_clause: handler_key '(' exception_declaration handler_predicate_opt ')' compound_statement  */
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 12294 "Parser/parser.cc"
    break;

  case 388: /* handler_clause: handler_clause handler_key '(' exception_declaration handler_predicate_opt ')' compound_statement  */
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-6].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-5].except_kind), (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 12300 "Parser/parser.cc"
    break;

  case 389: /* handler_predicate_opt: %empty  */
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12306 "Parser/parser.cc"
    break;

  case 390: /* handler_predicate_opt: ';' conditional_expression  */
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 12312 "Parser/parser.cc"
    break;

  case 391: /* handler_key: CATCH  */
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12318 "Parser/parser.cc"
    break;

  case 392: /* handler_key: RECOVER  */
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 12324 "Parser/parser.cc"
    break;

  case 393: /* handler_key: CATCHRESUME  */
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 12330 "Parser/parser.cc"
    break;

  case 394: /* handler_key: FIXUP  */
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 12336 "Parser/parser.cc"
    break;

  case 395: /* finally_clause: FINALLY compound_statement  */
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 12342 "Parser/parser.cc"
    break;

  case 397: /* exception_declaration: type_specifier_nobody declarator  */
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 398: /* exception_declaration: type_specifier_nobody variable_abstract_declarator  */
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 399: /* exception_declaration: cfa_abstract_declarator_tuple identifier  */
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 404: /* asm_statement: ASM asm_volatile_opt '(' string_literal ')' ';'  */
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 12366 "Parser/parser.cc"
    break;

  case 405: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ')' ';'  */
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 406: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ')' ';'  */
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 407: /* asm_statement: ASM asm_volatile_opt '(' string_literal ':' asm_operands_opt ':' asm_operands_opt ':' asm_clobbers_list_opt ')' ';'  */
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 408: /* asm_statement: ASM asm_volatile_opt GOTO '(' string_literal ':' ':' asm_operands_opt ':' asm_clobbers_list_opt ':' asm_label_list ')' ';'  */
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 409: /* asm_volatile_opt: %empty  */
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 12396 "Parser/parser.cc"
    break;

  case 410: /* asm_volatile_opt: VOLATILE  */
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 12402 "Parser/parser.cc"
    break;

  case 411: /* asm_operands_opt: %empty  */
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12408 "Parser/parser.cc"
    break;

  case 414: /* asm_operands_list: asm_operands_list ',' asm_operand  */
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 415: /* asm_operand: string_literal '(' constant_expression ')'  */
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 416: /* asm_operand: '[' IDENTIFIER ']' string_literal '(' constant_expression ')'  */
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 12429 "Parser/parser.cc"
    break;

  case 417: /* asm_clobbers_list_opt: %empty  */
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 12435 "Parser/parser.cc"
    break;

  case 418: /* asm_clobbers_list_opt: string_literal  */
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 12441 "Parser/parser.cc"
    break;

  case 419: /* asm_clobbers_list_opt: asm_clobbers_list_opt ',' string_literal  */
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 12447 "Parser/parser.cc"
    break;

  case 420: /* asm_label_list: identifier_or_type_name  */
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 12453 "Parser/parser.cc"
    break;

  case 421: /* asm_label_list: asm_label_list ',' identifier_or_type_name  */
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) ); delete (yyvsp[0].tok); }
#line 12459 "Parser/parser.cc"
    break;

  case 422: /* declaration_list_opt: %empty  */
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12465 "Parser/parser.cc"
    break;

  case 424: /* declaration_list: attribute_list_opt declaration  */
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12471 "Parser/parser.cc"
    break;

  case 425: /* declaration_list: declaration_list declaration  */
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 12477 "Parser/parser.cc"
    break;

  case 426: /* KR_parameter_list_opt: %empty  */
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12483 "Parser/parser.cc"
    break;

  case 428: /* KR_parameter_list: c_declaration ';'  */
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12489 "Parser/parser.cc"
    break;

  case 429: /* KR_parameter_list: KR_parameter_list c_declaration ';'  */
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-1].decl) ); }
#line 12495 "Parser/parser.cc"
    break;

  case 439: /* static_assert: STATICASSERT '(' constant_expression ',' string_literal ')'  */
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-3].expr), maybeMoveBuild( (yyvsp[-1].expr) ) ); }
#line 12501 "Parser/parser.cc"
    break;

  case 440: /* static_assert: STATICASSERT '(' constant_expression ')'  */
#line 2022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-1].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 12507 "Parser/parser.cc"
    break;

  case 444: /* cfa_declaration: type_declaring_list  */
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12513 "Parser/parser.cc"
    break;

  case 446: /* cfa_variable_declaration: cfa_variable_specifier initializer_opt  */
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 12519 "Parser/parser.cc"
    break;

  case 447: /* cfa_variable_declaration: declaration_qualifier_list cfa_variable_specifier initializer_opt  */
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12525 "Parser/parser.cc"
    break;

  case 448: /* cfa_variable_declaration: cfa_variable_declaration pop ',' push identifier_or_type_name initializer_opt  */
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12531 "Parser/parser.cc"
    break;

  case 449: /* cfa_variable_specifier: cfa_abstract_declarator_no_tuple identifier_or_type_name asm_name_opt  */
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12537 "Parser/parser.cc"
    break;

  case 450: /* cfa_variable_specifier: cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12543 "Parser/parser.cc"
    break;

  case 451: /* cfa_variable_specifier: multi_array_dimension cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12549 "Parser/parser.cc"
    break;

  case 452: /* cfa_variable_specifier: multi_array_dimension type_qualifier_list cfa_abstract_tuple identifier_or_type_name asm_name_opt  */
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addNewArray( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 12555 "Parser/parser.cc"
    break;

  case 453: /* cfa_variable_specifier: cfa_function_return asm_name_opt  */
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple-element declarations is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12561 "Parser/parser.cc"
    break;

  case 454: /* cfa_variable_specifier: type_qualifier_list cfa_function_return asm_name_opt  */
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "tuple variable declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12567 "Parser/parser.cc"
    break;

  case 456: /* cfa_function_declaration: type_qualifier_list cfa_function_specifier  */
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12573 "Parser/parser.cc"
    break;

  case 457: /* cfa_function_declaration: declaration_qualifier_list cfa_function_specifier  */
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12579 "Parser/parser.cc"
    break;

  case 458: /* cfa_function_declaration: declaration_qualifier_list type_qualifier_list cfa_function_specifier  */
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 12585 "Parser/parser.cc"
    break;

  case 459: /* cfa_function_declaration: cfa_function_declaration ',' identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeCopy( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->set_last( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 12596 "Parser/parser.cc"
    break;

  case 460: /* cfa_function_specifier: '[' ']' identifier '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12602 "Parser/parser.cc"
    break;

  case 461: /* cfa_function_specifier: '[' ']' TYPEDEFname '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok),  DeclarationNode::newTuple( nullptr ), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12608 "Parser/parser.cc"
    break;

  case 462: /* cfa_function_specifier: cfa_abstract_tuple identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12614 "Parser/parser.cc"
    break;

  case 463: /* cfa_function_specifier: cfa_function_return identifier_or_type_name '(' push cfa_parameter_list_ellipsis_opt pop ')' attribute_list_opt  */
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 12620 "Parser/parser.cc"
    break;

  case 464: /* cfa_function_return: '[' cfa_parameter_list ']'  */
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 12626 "Parser/parser.cc"
    break;

  case 465: /* cfa_function_return: '[' cfa_parameter_list ',' cfa_abstract_parameter_list ']'  */
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-3].decl)->set_last( (yyvsp[-1].decl) ) ); }
#line 12632 "Parser/parser.cc"
    break;

  case 466: /* cfa_typedef_declaration: TYPEDEF attribute_list_opt cfa_variable_specifier  */
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12641 "Parser/parser.cc"
    break;

  case 467: /* cfa_typedef_declaration: TYPEDEF attribute_list_opt cfa_function_specifier  */
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "cfa_typedef_declaration 2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef()->addQualifiers( (yyvsp[-1].decl) );
		}
#line 12650 "Parser/parser.cc"
    break;

  case 468: /* cfa_typedef_declaration: cfa_typedef_declaration ',' attribute_list_opt identifier  */
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "cfa_typedef_declaration 3" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneType( (yyvsp[0].tok) )->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12659 "Parser/parser.cc"
    break;

  case 469: /* typedef_declaration: TYPEDEF attribute_list_opt type_specifier declarator  */
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 1" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef()->addQualifiers( (yyvsp[-2].decl) ); // watchout frees $3 and $4
		}
#line 12670 "Parser/parser.cc"
    break;

  case 470: /* typedef_declaration: typedef_declaration ',' attribute_list_opt declarator  */
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "typedef_declaration 2" );
			(yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[-3].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef()->addQualifiers( (yyvsp[-1].decl) ) );
		}
#line 12679 "Parser/parser.cc"
    break;

  case 471: /* typedef_declaration: type_qualifier_list TYPEDEF type_specifier declarator  */
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12685 "Parser/parser.cc"
    break;

  case 472: /* typedef_declaration: type_specifier TYPEDEF declarator  */
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12691 "Parser/parser.cc"
    break;

  case 473: /* typedef_declaration: type_specifier TYPEDEF type_qualifier_list declarator  */
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 12697 "Parser/parser.cc"
    break;

  case 474: /* typedef_expression: TYPEDEF identifier '=' assignment_expression  */
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12703 "Parser/parser.cc"
    break;

  case 475: /* typedef_expression: typedef_expression ',' identifier '=' assignment_expression  */
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr; }
#line 12709 "Parser/parser.cc"
    break;

  case 476: /* c_declaration: declaration_specifier declaring_list  */
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distTypeSpec( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 479: /* c_declaration: sue_declaration_specifier  */
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 12730 "Parser/parser.cc"
    break;

  case 480: /* declaring_list: variable_declarator asm_name_opt initializer_opt  */
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12736 "Parser/parser.cc"
    break;

  case 481: /* declaring_list: variable_type_redeclarator asm_name_opt initializer_opt  */
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 12742 "Parser/parser.cc"
    break;

  case 482: /* declaring_list: general_function_declarator asm_name_opt  */
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 12748 "Parser/parser.cc"
    break;

  case 483: /* declaring_list: general_function_declarator asm_name_opt '=' VOID  */
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 12754 "Parser/parser.cc"
    break;

  case 484: /* declaring_list: declaring_list ',' attribute_list_opt declarator asm_name_opt initializer_opt  */
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->set_last( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 12760 "Parser/parser.cc"
    break;

  case 490: /* declaration_specifier: sue_declaration_specifier invalid_types  */
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of \"%s\" declaration.",
						   ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ) );
			(yyval.decl) = nullptr;
		}
#line 12770 "Parser/parser.cc"
    break;

  case 503: /* type_qualifier_list_opt: %empty  */
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12776 "Parser/parser.cc"
    break;

  case 505: /* type_qualifier_list: type_qualifier attribute_list_opt  */
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12782 "Parser/parser.cc"
    break;

  case 506: /* type_qualifier_list: type_qualifier_list type_qualifier attribute_list_opt  */
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 507: /* type_qualifier: type_qualifier_name  */
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12794 "Parser/parser.cc"
    break;

  case 508: /* type_qualifier_name: CONST  */
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Const ); }
#line 12800 "Parser/parser.cc"
    break;

  case 509: /* type_qualifier_name: RESTRICT  */
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Restrict ); }
#line 12806 "Parser/parser.cc"
    break;

  case 510: /* type_qualifier_name: VOLATILE  */
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Volatile ); }
#line 12812 "Parser/parser.cc"
    break;

  case 511: /* type_qualifier_name: ATOMIC  */
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_qualifier( ast::CV::Atomic ); }
#line 12818 "Parser/parser.cc"
    break;

  case 512: /* type_qualifier_name: forall  */
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_forall( (yyvsp[0].decl) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 513: /* forall: FORALL '(' type_parameter_list ')'  */
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12830 "Parser/parser.cc"
    break;

  case 515: /* declaration_qualifier_list: type_qualifier_list storage_class_list  */
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 516: /* declaration_qualifier_list: declaration_qualifier_list type_qualifier_list storage_class_list  */
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 517: /* storage_class_list: storage_class attribute_list_opt  */
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12848 "Parser/parser.cc"
    break;

  case 518: /* storage_class_list: storage_class_list storage_class attribute_list_opt  */
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 519: /* storage_class: EXTERN  */
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Extern ); }
#line 12860 "Parser/parser.cc"
    break;

  case 520: /* storage_class: STATIC  */
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Static ); }
#line 12866 "Parser/parser.cc"
    break;

  case 521: /* storage_class: AUTO  */
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Auto ); }
#line 12872 "Parser/parser.cc"
    break;

  case 522: /* storage_class: REGISTER  */
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::Register ); }
#line 12878 "Parser/parser.cc"
    break;

  case 523: /* storage_class: THREADLOCALGCC  */
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalGcc ); }
#line 12884 "Parser/parser.cc"
    break;

  case 524: /* storage_class: THREADLOCALC11  */
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( ast::Storage::ThreadLocalC11 ); }
#line 12890 "Parser/parser.cc"
    break;

  case 525: /* storage_class: INLINE  */
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Inline ); }
#line 12896 "Parser/parser.cc"
    break;

  case 526: /* storage_class: FORTRAN  */
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Fortran ); }
#line 12902 "Parser/parser.cc"
    break;

  case 527: /* storage_class: NORETURN  */
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( ast::Function::Noreturn ); }
#line 12908 "Parser/parser.cc"
    break;

  case 528: /* basic_type_name: basic_type_name_type  */
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 529: /* basic_type_name_type: VOID  */
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Void ); }
#line 12920 "Parser/parser.cc"
    break;

  case 530: /* basic_type_name_type: BOOL  */
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Bool ); }
#line 12926 "Parser/parser.cc"
    break;

  case 531: /* basic_type_name_type: CHAR  */
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Char ); }
#line 12932 "Parser/parser.cc"
    break;

  case 532: /* basic_type_name_type: INT  */
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int ); }
#line 12938 "Parser/parser.cc"
    break;

  case 533: /* basic_type_name_type: INT128  */
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Int128 ); }
#line 12944 "Parser/parser.cc"
    break;

  case 534: /* basic_type_name_type: UINT128  */
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = addType( build_basic_type( TypeData::Int128 ), build_signedness( TypeData::Unsigned ) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 535: /* basic_type_name_type: FLOAT  */
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float ); }
#line 12956 "Parser/parser.cc"
    break;

  case 536: /* basic_type_name_type: DOUBLE  */
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Double ); }
#line 12962 "Parser/parser.cc"
    break;

  case 537: /* basic_type_name_type: FLOAT80  */
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float80 ); }
#line 12968 "Parser/parser.cc"
    break;

  case 538: /* basic_type_name_type: uuFLOAT128  */
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::uuFloat128 ); }
#line 12974 "Parser/parser.cc"
    break;

  case 539: /* basic_type_name_type: FLOAT16  */
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float16 ); }
#line 12980 "Parser/parser.cc"
    break;

  case 540: /* basic_type_name_type: FLOAT32  */
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32 ); }
#line 12986 "Parser/parser.cc"
    break;

  case 541: /* basic_type_name_type: FLOAT32X  */
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x ); }
#line 12992 "Parser/parser.cc"
    break;

  case 542: /* basic_type_name_type: FLOAT64  */
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64 ); }
#line 12998 "Parser/parser.cc"
    break;

  case 543: /* basic_type_name_type: FLOAT64X  */
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x ); }
#line 13004 "Parser/parser.cc"
    break;

  case 544: /* basic_type_name_type: FLOAT128  */
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128 ); }
#line 13010 "Parser/parser.cc"
    break;

  case 545: /* basic_type_name_type: FLOAT128X  */
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float128x ); }
#line 13016 "Parser/parser.cc"
    break;

  case 546: /* basic_type_name_type: FLOAT32X4  */
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float32x4 ); }
#line 13022 "Parser/parser.cc"
    break;

  case 547: /* basic_type_name_type: FLOAT64X2  */
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Float64x2 ); }
#line 13028 "Parser/parser.cc"
    break;

  case 548: /* basic_type_name_type: SVFLOAT32  */
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat32 ); }
#line 13034 "Parser/parser.cc"
    break;

  case 549: /* basic_type_name_type: SVFLOAT64  */
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svfloat64 ); }
#line 13040 "Parser/parser.cc"
    break;

  case 550: /* basic_type_name_type: SVBOOL  */
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_basic_type( TypeData::Svbool ); }
#line 13046 "Parser/parser.cc"
    break;

  case 551: /* basic_type_name_type: DECIMAL32  */
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13052 "Parser/parser.cc"
    break;

  case 552: /* basic_type_name_type: DECIMAL64  */
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13058 "Parser/parser.cc"
    break;

  case 553: /* basic_type_name_type: DECIMAL128  */
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13064 "Parser/parser.cc"
    break;

  case 554: /* basic_type_name_type: COMPLEX  */
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Complex ); }
#line 13070 "Parser/parser.cc"
    break;

  case 555: /* basic_type_name_type: IMAGINARY  */
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_complex_type( TypeData::Imaginary ); }
#line 13076 "Parser/parser.cc"
    break;

  case 556: /* basic_type_name_type: SIGNED  */
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Signed ); }
#line 13082 "Parser/parser.cc"
    break;

  case 557: /* basic_type_name_type: UNSIGNED  */
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_signedness( TypeData::Unsigned ); }
#line 13088 "Parser/parser.cc"
    break;

  case 558: /* basic_type_name_type: SHORT  */
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Short ); }
#line 13094 "Parser/parser.cc"
    break;

  case 559: /* basic_type_name_type: LONG  */
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_length( TypeData::Long ); }
#line 13100 "Parser/parser.cc"
    break;

  case 560: /* basic_type_name_type: VA_LIST  */
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::Valist ); }
#line 13106 "Parser/parser.cc"
    break;

  case 561: /* basic_type_name_type: AUTO_TYPE  */
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_builtin_type( TypeData::AutoType ); }
#line 13112 "Parser/parser.cc"
    break;

  case 563: /* vtable_opt: %empty  */
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 13118 "Parser/parser.cc"
    break;

  case 565: /* vtable: VTABLE '(' type_name ')' default_opt  */
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_vtable_type( (yyvsp[-2].type) ); }
#line 13124 "Parser/parser.cc"
    break;

  case 566: /* default_opt: %empty  */
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = nullptr; }
#line 13130 "Parser/parser.cc"
    break;

  case 567: /* default_opt: DEFAULT  */
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.type) = nullptr; }
#line 13136 "Parser/parser.cc"
    break;

  case 569: /* basic_declaration_specifier: declaration_qualifier_list basic_type_specifier  */
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13142 "Parser/parser.cc"
    break;

  case 570: /* basic_declaration_specifier: basic_declaration_specifier storage_class attribute_list_opt  */
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13148 "Parser/parser.cc"
    break;

  case 571: /* basic_declaration_specifier: basic_declaration_specifier storage_class type_qualifier_list  */
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13154 "Parser/parser.cc"
    break;

  case 572: /* basic_declaration_specifier: basic_declaration_specifier storage_class basic_type_specifier  */
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 13160 "Parser/parser.cc"
    break;

  case 573: /* basic_type_specifier: direct_type attribute_list_opt  */
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13166 "Parser/parser.cc"
    break;

  case 574: /* basic_type_specifier: type_qualifier_list_opt indirect_type attribute_list  */
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 575: /* basic_type_specifier: type_qualifier_list_opt indirect_type type_qualifier_list_opt  */
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 577: /* direct_type: type_qualifier_list basic_type_name  */
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 578: /* direct_type: direct_type type_qualifier  */
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 579: /* direct_type: direct_type basic_type_name  */
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 580: /* indirect_type: TYPEOF '(' type ')'  */
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13202 "Parser/parser.cc"
    break;

  case 581: /* indirect_type: TYPEOF '(' comma_expression ')'  */
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 582: /* indirect_type: BASETYPEOF '(' type ')'  */
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 13214 "Parser/parser.cc"
    break;

  case 583: /* indirect_type: BASETYPEOF '(' comma_expression ')'  */
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 13220 "Parser/parser.cc"
    break;

  case 584: /* indirect_type: ZERO_T  */
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::Zero ) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 585: /* indirect_type: ONE_T  */
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_builtin_type( TypeData::One ) ); }
#line 13232 "Parser/parser.cc"
    break;

  case 587: /* sue_declaration_specifier: declaration_qualifier_list sue_type_specifier  */
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 588: /* sue_declaration_specifier: sue_declaration_specifier storage_class  */
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13244 "Parser/parser.cc"
    break;

  case 589: /* sue_declaration_specifier: sue_declaration_specifier storage_class type_qualifier_list  */
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13250 "Parser/parser.cc"
    break;

  case 591: /* $@1: %empty  */
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 13256 "Parser/parser.cc"
    break;

  case 592: /* sue_type_specifier: type_qualifier_list $@1 elaborated_type  */
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13262 "Parser/parser.cc"
    break;

  case 593: /* sue_type_specifier: sue_type_specifier type_qualifier  */
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 13271 "Parser/parser.cc"
    break;

  case 595: /* sue_declaration_specifier_nobody: declaration_qualifier_list sue_type_specifier_nobody  */
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13277 "Parser/parser.cc"
    break;

  case 596: /* sue_declaration_specifier_nobody: sue_declaration_specifier_nobody storage_class  */
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13283 "Parser/parser.cc"
    break;

  case 597: /* sue_declaration_specifier_nobody: sue_declaration_specifier_nobody storage_class type_qualifier_list  */
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13289 "Parser/parser.cc"
    break;

  case 599: /* sue_type_specifier_nobody: type_qualifier_list elaborated_type_nobody  */
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13295 "Parser/parser.cc"
    break;

  case 600: /* sue_type_specifier_nobody: sue_type_specifier_nobody type_qualifier  */
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13301 "Parser/parser.cc"
    break;

  case 601: /* type_declaration_specifier: type_type_specifier attribute_list_opt  */
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13307 "Parser/parser.cc"
    break;

  case 602: /* type_declaration_specifier: declaration_qualifier_list type_type_specifier attribute_list_opt  */
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13313 "Parser/parser.cc"
    break;

  case 603: /* type_declaration_specifier: type_declaration_specifier storage_class attribute_list_opt  */
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13319 "Parser/parser.cc"
    break;

  case 604: /* type_declaration_specifier: type_declaration_specifier storage_class type_qualifier_list  */
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13325 "Parser/parser.cc"
    break;

  case 605: /* type_type_specifier: type_name  */
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) ); }
#line 13331 "Parser/parser.cc"
    break;

  case 606: /* type_type_specifier: type_qualifier_list type_name  */
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( (yyvsp[0].type) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 13337 "Parser/parser.cc"
    break;

  case 607: /* type_type_specifier: type_type_specifier type_qualifier  */
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13343 "Parser/parser.cc"
    break;

  case 608: /* type_name: TYPEDEFname  */
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_typedef( (yyvsp[0].tok) ); }
#line 13349 "Parser/parser.cc"
    break;

  case 609: /* type_name: '.' TYPEDEFname  */
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), build_typedef( (yyvsp[0].tok) ) ); }
#line 13355 "Parser/parser.cc"
    break;

  case 610: /* type_name: type_name '.' TYPEDEFname  */
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), build_typedef( (yyvsp[0].tok) ) ); }
#line 13361 "Parser/parser.cc"
    break;

  case 612: /* type_name: '.' typegen_name  */
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( build_global_scope(), (yyvsp[0].type) ); }
#line 13367 "Parser/parser.cc"
    break;

  case 613: /* type_name: type_name '.' typegen_name  */
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_qualified_type( (yyvsp[-2].type), (yyvsp[0].type) ); }
#line 13373 "Parser/parser.cc"
    break;

  case 614: /* typegen_name: TYPEGENname  */
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[0].tok), nullptr ); }
#line 13379 "Parser/parser.cc"
    break;

  case 615: /* typegen_name: TYPEGENname '(' ')'  */
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-2].tok), nullptr ); }
#line 13385 "Parser/parser.cc"
    break;

  case 616: /* typegen_name: TYPEGENname '(' type_list ')'  */
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.type) = build_type_gen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 13391 "Parser/parser.cc"
    break;

  case 621: /* $@2: %empty  */
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 13397 "Parser/parser.cc"
    break;

  case 622: /* aggregate_type: aggregate_key attribute_list_opt $@2 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), nullptr, (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13403 "Parser/parser.cc"
    break;

  case 623: /* $@3: %empty  */
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 1" );
			forall = false;								// reset
		}
#line 13412 "Parser/parser.cc"
    break;

  case 624: /* aggregate_type: aggregate_key attribute_list_opt identifier attribute_list_opt $@3 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13420 "Parser/parser.cc"
    break;

  case 625: /* $@4: %empty  */
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 2" );
			forall = false;								// reset
		}
#line 13429 "Parser/parser.cc"
    break;

  case 626: /* aggregate_type: aggregate_key attribute_list_opt TYPEDEFname attribute_list_opt $@4 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_typedef( (yyvsp[-7].tok) ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13438 "Parser/parser.cc"
    break;

  case 627: /* $@5: %empty  */
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[-1].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type: 3" );
			forall = false;								// reset
		}
#line 13447 "Parser/parser.cc"
    break;

  case 628: /* aggregate_type: aggregate_key attribute_list_opt TYPEGENname attribute_list_opt $@5 '{' field_declaration_list_opt '}' type_parameters_opt attribute_list_opt  */
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeData( build_type_gen( (yyvsp[-7].tok), nullptr ) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-9].aggKey), (yyvsp[-7].tok), (yyvsp[-1].expr), (yyvsp[-3].decl), true )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13456 "Parser/parser.cc"
    break;

  case 630: /* type_parameters_opt: %empty  */
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13462 "Parser/parser.cc"
    break;

  case 631: /* type_parameters_opt: '(' type_list ')'  */
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 13468 "Parser/parser.cc"
    break;

  case 632: /* aggregate_type_nobody: aggregate_key attribute_list_opt identifier  */
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname, "aggregate_type_nobody" );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13478 "Parser/parser.cc"
    break;

  case 633: /* aggregate_type_nobody: aggregate_key attribute_list_opt type_name  */
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 13497 "Parser/parser.cc"
    break;

  case 636: /* aggregate_data: STRUCT vtable_opt  */
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 13503 "Parser/parser.cc"
    break;

  case 637: /* aggregate_data: UNION  */
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 13509 "Parser/parser.cc"
    break;

  case 638: /* aggregate_data: EXCEPTION  */
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 13515 "Parser/parser.cc"
    break;

  case 639: /* aggregate_control: MONITOR  */
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13521 "Parser/parser.cc"
    break;

  case 640: /* aggregate_control: MUTEX STRUCT  */
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 13527 "Parser/parser.cc"
    break;

  case 641: /* aggregate_control: GENERATOR  */
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 13533 "Parser/parser.cc"
    break;

  case 642: /* aggregate_control: MUTEX GENERATOR  */
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13542 "Parser/parser.cc"
    break;

  case 643: /* aggregate_control: COROUTINE  */
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 13548 "Parser/parser.cc"
    break;

  case 644: /* aggregate_control: MUTEX COROUTINE  */
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13557 "Parser/parser.cc"
    break;

  case 645: /* aggregate_control: THREAD  */
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 13563 "Parser/parser.cc"
    break;

  case 646: /* aggregate_control: MUTEX THREAD  */
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 13572 "Parser/parser.cc"
    break;

  case 647: /* field_declaration_list_opt: %empty  */
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13578 "Parser/parser.cc"
    break;

  case 648: /* field_declaration_list_opt: field_declaration_list_opt attribute_list_opt field_declaration  */
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); (yyval.decl) = (yyvsp[-2].decl) ? (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 13584 "Parser/parser.cc"
    break;

  case 649: /* field_declaration: type_specifier field_declaring_list_opt ';'  */
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 13590 "Parser/parser.cc"
    break;

  case 650: /* field_declaration: type_specifier field_declaring_list_opt '}'  */
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "illegal syntax, expecting ';' at end of previous declaration." );
			(yyval.decl) = nullptr;
		}
#line 13599 "Parser/parser.cc"
    break;

  case 651: /* field_declaration: EXTENSION type_specifier field_declaring_list_opt ';'  */
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 13605 "Parser/parser.cc"
    break;

  case 652: /* field_declaration: STATIC type_specifier field_declaring_list_opt ';'  */
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13611 "Parser/parser.cc"
    break;

  case 653: /* field_declaration: INLINE attribute_list_opt type_specifier field_abstract_list_opt ';'  */
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distTypeSpec( (yyvsp[-2].decl), (yyvsp[-1].decl) );				// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 13624 "Parser/parser.cc"
    break;

  case 654: /* field_declaration: INLINE attribute_list_opt aggregate_control ';'  */
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13630 "Parser/parser.cc"
    break;

  case 657: /* field_declaration: EXTENSION cfa_field_declaring_list ';'  */
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 13636 "Parser/parser.cc"
    break;

  case 658: /* field_declaration: INLINE attribute_list_opt cfa_field_abstract_list ';'  */
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 13642 "Parser/parser.cc"
    break;

  case 661: /* field_declaring_list_opt: %empty  */
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13648 "Parser/parser.cc"
    break;

  case 664: /* field_declaring_list: field_declaring_list_opt ',' attribute_list_opt field_declarator  */
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13654 "Parser/parser.cc"
    break;

  case 665: /* field_declarator: bit_subrange_size  */
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 13660 "Parser/parser.cc"
    break;

  case 666: /* field_declarator: variable_declarator bit_subrange_size_opt  */
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13666 "Parser/parser.cc"
    break;

  case 667: /* field_declarator: variable_type_redeclarator bit_subrange_size_opt  */
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13672 "Parser/parser.cc"
    break;

  case 668: /* field_declarator: function_type_redeclarator bit_subrange_size_opt  */
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 13678 "Parser/parser.cc"
    break;

  case 669: /* field_abstract_list_opt: %empty  */
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13684 "Parser/parser.cc"
    break;

  case 671: /* field_abstract_list_opt: field_abstract_list_opt ',' attribute_list_opt field_abstract  */
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 13690 "Parser/parser.cc"
    break;

  case 673: /* cfa_field_declaring_list: cfa_abstract_declarator_tuple identifier_or_type_name  */
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 13696 "Parser/parser.cc"
    break;

  case 674: /* cfa_field_declaring_list: cfa_field_declaring_list ',' identifier_or_type_name  */
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 13702 "Parser/parser.cc"
    break;

  case 676: /* cfa_field_abstract_list: cfa_field_abstract_list ','  */
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 13708 "Parser/parser.cc"
    break;

  case 677: /* bit_subrange_size_opt: %empty  */
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13714 "Parser/parser.cc"
    break;

  case 679: /* bit_subrange_size: ':' assignment_expression  */
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13720 "Parser/parser.cc"
    break;

  case 680: /* enum_type: ENUM attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, false )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13731 "Parser/parser.cc"
    break;

  case 681: /* enum_type: ENUM enumerator_type attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-7].decl) && ((yyvsp[-7].decl)->storageClasses.val != 0 || (yyvsp[-7].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			if ( (yyvsp[-5].enum_hiding) == EnumHiding::Hide ) {
				SemanticError( yylloc, "illegal syntax, hiding ('!') the enumerator names of an anonymous enumeration means the names are inaccessible." ); (yyval.decl) = nullptr;
			} // if
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-3].decl), true, true, (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) );
		}
#line 13745 "Parser/parser.cc"
    break;

  case 682: /* $@6: %empty  */
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 1" ); }
#line 13751 "Parser/parser.cc"
    break;

  case 683: /* enum_type: ENUM attribute_list_opt identifier attribute_list_opt $@6 hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) ->addQualifiers( (yyvsp[-7].decl) ))->addQualifiers( (yyvsp[0].decl) ); }
#line 13757 "Parser/parser.cc"
    break;

  case 684: /* enum_type: ENUM attribute_list_opt typedef_name attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, false, nullptr, (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13763 "Parser/parser.cc"
    break;

  case 685: /* $@7: %empty  */
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].decl) && ((yyvsp[-3].decl)->storageClasses.any() || (yyvsp[-3].decl)->type->qualifiers.val != 0) ) {
				SemanticError( yylloc, "illegal syntax, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok), "enum_type 2" );
		}
#line 13774 "Parser/parser.cc"
    break;

  case 686: /* enum_type: ENUM enumerator_type attribute_list_opt identifier attribute_list_opt $@7 hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-8].tok), (yyvsp[-3].decl), true, true, (yyvsp[-10].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13780 "Parser/parser.cc"
    break;

  case 687: /* enum_type: ENUM enumerator_type attribute_list_opt typedef_name attribute_list_opt hide_opt '{' enumerator_list comma_opt '}' attribute_list_opt  */
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].decl)->name, (yyvsp[-3].decl), true, true, (yyvsp[-9].decl), (yyvsp[-5].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 13786 "Parser/parser.cc"
    break;

  case 689: /* enumerator_type: '(' ')'  */
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13792 "Parser/parser.cc"
    break;

  case 690: /* enumerator_type: '(' cfa_abstract_parameter_declaration ')'  */
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13798 "Parser/parser.cc"
    break;

  case 691: /* hide_opt: %empty  */
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13804 "Parser/parser.cc"
    break;

  case 692: /* hide_opt: '!'  */
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 13810 "Parser/parser.cc"
    break;

  case 693: /* enum_type_nobody: ENUM attribute_list_opt identifier  */
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), "enum_type_nobody 1" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13819 "Parser/parser.cc"
    break;

  case 694: /* enum_type_nobody: ENUM attribute_list_opt type_name  */
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].type)->symbolic.name, "enum_type_nobody 2" );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].type)->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 13828 "Parser/parser.cc"
    break;

  case 695: /* enumerator_list: %empty  */
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enumeration must have a minimum of one enumerator, empty enumerator list is meaningless." );  (yyval.decl) = nullptr; }
#line 13834 "Parser/parser.cc"
    break;

  case 696: /* enumerator_list: visible_hide_opt identifier_or_type_name enumerator_value_opt  */
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 13840 "Parser/parser.cc"
    break;

  case 697: /* enumerator_list: INLINE type_name  */
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name );
			(yyvsp[0].type)->symbolic.name = nullptr;
			delete (yyvsp[0].type);
		}
#line 13850 "Parser/parser.cc"
    break;

  case 698: /* enumerator_list: enumerator_list ',' visible_hide_opt identifier_or_type_name enumerator_value_opt  */
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 13856 "Parser/parser.cc"
    break;

  case 699: /* enumerator_list: enumerator_list ',' INLINE type_name  */
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->set_last( DeclarationNode::newEnumInLine( (yyvsp[0].type)->symbolic.name )  ); }
#line 13862 "Parser/parser.cc"
    break;

  case 701: /* visible_hide_opt: '^'  */
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 13868 "Parser/parser.cc"
    break;

  case 702: /* enumerator_value_opt: %empty  */
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 13874 "Parser/parser.cc"
    break;

  case 703: /* enumerator_value_opt: '=' constant_expression  */
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 13880 "Parser/parser.cc"
    break;

  case 704: /* enumerator_value_opt: '=' '{' initializer_list_opt comma_opt '}'  */
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 13886 "Parser/parser.cc"
    break;

  case 705: /* parameter_list_ellipsis_opt: %empty  */
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13892 "Parser/parser.cc"
    break;

  case 706: /* parameter_list_ellipsis_opt: ELLIPSIS  */
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13898 "Parser/parser.cc"
    break;

  case 708: /* parameter_list_ellipsis_opt: parameter_list ',' ELLIPSIS  */
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13904 "Parser/parser.cc"
    break;

  case 710: /* parameter_list: attribute_list parameter_declaration  */
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13910 "Parser/parser.cc"
    break;

  case 712: /* parameter_list: attribute_list abstract_parameter_declaration  */
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13916 "Parser/parser.cc"
    break;

  case 713: /* parameter_list: parameter_list ',' attribute_list_opt parameter_declaration  */
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 13922 "Parser/parser.cc"
    break;

  case 714: /* parameter_list: parameter_list ',' attribute_list_opt abstract_parameter_declaration  */
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-3].decl)->set_last( (yyvsp[0].decl) ); }
#line 13928 "Parser/parser.cc"
    break;

  case 715: /* cfa_parameter_list_ellipsis_opt: %empty  */
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeData( build_basic_type( TypeData::Void ) ); }
#line 13934 "Parser/parser.cc"
    break;

  case 716: /* cfa_parameter_list_ellipsis_opt: ELLIPSIS  */
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13940 "Parser/parser.cc"
    break;

  case 719: /* cfa_parameter_list_ellipsis_opt: cfa_parameter_list ',' cfa_abstract_parameter_list  */
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13946 "Parser/parser.cc"
    break;

  case 720: /* cfa_parameter_list_ellipsis_opt: cfa_parameter_list ',' ELLIPSIS  */
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13952 "Parser/parser.cc"
    break;

  case 721: /* cfa_parameter_list_ellipsis_opt: cfa_abstract_parameter_list ',' ELLIPSIS  */
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addVarArgs(); }
#line 13958 "Parser/parser.cc"
    break;

  case 723: /* cfa_parameter_list: cfa_abstract_parameter_list ',' cfa_parameter_declaration  */
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13964 "Parser/parser.cc"
    break;

  case 724: /* cfa_parameter_list: cfa_parameter_list ',' cfa_parameter_declaration  */
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13970 "Parser/parser.cc"
    break;

  case 725: /* cfa_parameter_list: cfa_parameter_list ',' cfa_abstract_parameter_list ',' cfa_parameter_declaration  */
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->set_last( (yyvsp[-2].decl) )->set_last( (yyvsp[0].decl) ); }
#line 13976 "Parser/parser.cc"
    break;

  case 727: /* cfa_abstract_parameter_list: cfa_abstract_parameter_list ',' cfa_abstract_parameter_declaration  */
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 13982 "Parser/parser.cc"
    break;

  case 728: /* parameter_declaration: declaration_specifier_nobody identifier_parameter_declarator default_initializer_opt  */
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13988 "Parser/parser.cc"
    break;

  case 729: /* parameter_declaration: declaration_specifier_nobody type_parameter_redeclarator default_initializer_opt  */
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 13994 "Parser/parser.cc"
    break;

  case 730: /* abstract_parameter_declaration: declaration_specifier_nobody default_initializer_opt  */
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14000 "Parser/parser.cc"
    break;

  case 731: /* abstract_parameter_declaration: declaration_specifier_nobody abstract_parameter_declarator default_initializer_opt  */
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 14006 "Parser/parser.cc"
    break;

  case 733: /* cfa_parameter_declaration: cfa_identifier_parameter_declarator_no_tuple identifier_or_type_name default_initializer_opt  */
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 14012 "Parser/parser.cc"
    break;

  case 734: /* cfa_parameter_declaration: cfa_abstract_tuple identifier_or_type_name default_initializer_opt  */
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 14018 "Parser/parser.cc"
    break;

  case 735: /* cfa_parameter_declaration: type_qualifier_list cfa_abstract_tuple identifier_or_type_name default_initializer_opt  */
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14024 "Parser/parser.cc"
    break;

  case 740: /* cfa_abstract_parameter_declaration: type_qualifier_list cfa_abstract_tuple  */
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14030 "Parser/parser.cc"
    break;

  case 742: /* identifier_list: identifier  */
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14036 "Parser/parser.cc"
    break;

  case 743: /* identifier_list: identifier_list ',' identifier  */
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 14042 "Parser/parser.cc"
    break;

  case 745: /* type_no_function: type_specifier abstract_declarator  */
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14048 "Parser/parser.cc"
    break;

  case 748: /* type: attribute_list type_no_function  */
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14054 "Parser/parser.cc"
    break;

  case 750: /* type: attribute_list cfa_abstract_function  */
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14060 "Parser/parser.cc"
    break;

  case 751: /* initializer_opt: %empty  */
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 14066 "Parser/parser.cc"
    break;

  case 752: /* initializer_opt: simple_assignment_operator initializer  */
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 14072 "Parser/parser.cc"
    break;

  case 753: /* initializer_opt: '=' VOID  */
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 14078 "Parser/parser.cc"
    break;

  case 754: /* initializer_opt: '{' initializer_list_opt comma_opt '}'  */
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 14084 "Parser/parser.cc"
    break;

  case 755: /* initializer: assignment_expression  */
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 14090 "Parser/parser.cc"
    break;

  case 756: /* initializer: '{' initializer_list_opt comma_opt '}'  */
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 14096 "Parser/parser.cc"
    break;

  case 757: /* initializer_list_opt: %empty  */
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 14102 "Parser/parser.cc"
    break;

  case 759: /* initializer_list_opt: designation initializer  */
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 14108 "Parser/parser.cc"
    break;

  case 760: /* initializer_list_opt: initializer_list_opt ',' initializer  */
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-2].init)->set_last( (yyvsp[0].init) ); }
#line 14114 "Parser/parser.cc"
    break;

  case 761: /* initializer_list_opt: initializer_list_opt ',' designation initializer  */
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) ); }
#line 14120 "Parser/parser.cc"
    break;

  case 763: /* designation: identifier_at ':'  */
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 14126 "Parser/parser.cc"
    break;

  case 765: /* designator_list: designator_list designator  */
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr)->set_last( (yyvsp[0].expr) ); }
#line 14132 "Parser/parser.cc"
    break;

  case 766: /* designator: '.' identifier_at  */
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 14138 "Parser/parser.cc"
    break;

  case 767: /* designator: '[' constant_expression ']'  */
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14144 "Parser/parser.cc"
    break;

  case 768: /* designator: '[' subrange ']'  */
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14150 "Parser/parser.cc"
    break;

  case 769: /* designator: '[' constant_expression ELLIPSIS constant_expression ']'  */
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 14156 "Parser/parser.cc"
    break;

  case 770: /* designator: '.' '[' field_name_list ']'  */
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 14162 "Parser/parser.cc"
    break;

  case 772: /* type_parameter_list: type_parameter_list ',' type_parameter  */
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl) ); }
#line 14168 "Parser/parser.cc"
    break;

  case 773: /* type_initializer_opt: %empty  */
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14174 "Parser/parser.cc"
    break;

  case 774: /* type_initializer_opt: '=' type  */
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 14180 "Parser/parser.cc"
    break;

  case 775: /* $@8: %empty  */
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "type_parameter 1" ); }
#line 14186 "Parser/parser.cc"
    break;

  case 776: /* type_parameter: type_class identifier_or_type_name $@8 type_initializer_opt assertion_list_opt  */
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 14192 "Parser/parser.cc"
    break;

  case 777: /* $@9: %empty  */
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "type_parameter 2" ); }
#line 14198 "Parser/parser.cc"
    break;

  case 778: /* type_parameter: identifier_or_type_name new_type_class $@9 type_initializer_opt assertion_list_opt  */
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 14204 "Parser/parser.cc"
    break;

  case 779: /* type_parameter: '[' identifier_or_type_name ']' assertion_list_opt  */
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-2].tok), TYPEDIMname, "type_parameter 3" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-2].tok) )->addAssertions( (yyvsp[0].decl) );
		}
#line 14213 "Parser/parser.cc"
    break;

  case 780: /* type_parameter: assertion_list  */
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( "" ) )->addAssertions( (yyvsp[0].decl) ); }
#line 14219 "Parser/parser.cc"
    break;

  case 781: /* type_parameter: ENUM '(' identifier_or_type_name ')' identifier_or_type_name new_type_class type_initializer_opt assertion_list_opt  */
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {	
			typedefTable.addToScope( *(yyvsp[-5].tok), TYPEDIMname, "type_parameter 4" );
			typedefTable.addToScope( *(yyvsp[-3].tok), TYPEDIMname, "type_parameter 5" );
			(yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-2].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) );
		}
#line 14229 "Parser/parser.cc"
    break;

  case 782: /* new_type_class: %empty  */
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 14235 "Parser/parser.cc"
    break;

  case 783: /* new_type_class: '&'  */
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 14241 "Parser/parser.cc"
    break;

  case 784: /* new_type_class: '*'  */
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 14247 "Parser/parser.cc"
    break;

  case 785: /* new_type_class: ELLIPSIS  */
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 14253 "Parser/parser.cc"
    break;

  case 786: /* type_class: OTYPE  */
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
#line 14259 "Parser/parser.cc"
    break;

  case 787: /* type_class: DTYPE  */
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
#line 14265 "Parser/parser.cc"
    break;

  case 788: /* type_class: FTYPE  */
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 14271 "Parser/parser.cc"
    break;

  case 789: /* type_class: TTYPE  */
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
#line 14277 "Parser/parser.cc"
    break;

  case 790: /* assertion_list_opt: %empty  */
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14283 "Parser/parser.cc"
    break;

  case 793: /* assertion_list: assertion_list assertion  */
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 14289 "Parser/parser.cc"
    break;

  case 794: /* assertion: '|' identifier_or_type_name '(' type_list ')'  */
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14295 "Parser/parser.cc"
    break;

  case 795: /* assertion: '|' '{' trait_declaration_list '}'  */
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14301 "Parser/parser.cc"
    break;

  case 796: /* type_list: type  */
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 14307 "Parser/parser.cc"
    break;

  case 798: /* type_list: type_list ',' type  */
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 14313 "Parser/parser.cc"
    break;

  case 799: /* type_list: type_list ',' assignment_expression  */
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ); }
#line 14319 "Parser/parser.cc"
    break;

  case 800: /* type_declaring_list: OTYPE type_declarator  */
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 14325 "Parser/parser.cc"
    break;

  case 801: /* type_declaring_list: storage_class_list OTYPE type_declarator  */
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14331 "Parser/parser.cc"
    break;

  case 802: /* type_declaring_list: type_declaring_list ',' type_declarator  */
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 14337 "Parser/parser.cc"
    break;

  case 803: /* type_declarator: type_declarator_name assertion_list_opt  */
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 14343 "Parser/parser.cc"
    break;

  case 804: /* type_declarator: type_declarator_name assertion_list_opt '=' type  */
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 14349 "Parser/parser.cc"
    break;

  case 805: /* type_declarator_name: identifier_or_type_name  */
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "type_declarator_name 1" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 14358 "Parser/parser.cc"
    break;

  case 806: /* type_declarator_name: identifier_or_type_name '(' type_parameter_list ')'  */
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "type_declarator_name 2" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 14367 "Parser/parser.cc"
    break;

  case 807: /* trait_specifier: TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' '}'  */
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 14376 "Parser/parser.cc"
    break;

  case 808: /* trait_specifier: forall TRAIT identifier_or_type_name '{' '}'  */
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 14382 "Parser/parser.cc"
    break;

  case 809: /* trait_specifier: TRAIT identifier_or_type_name '(' type_parameter_list ')' '{' trait_declaration_list '}'  */
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-6].tok), (yyvsp[-4].decl), (yyvsp[-1].decl) );
		}
#line 14391 "Parser/parser.cc"
    break;

  case 810: /* trait_specifier: forall TRAIT identifier_or_type_name '{' trait_declaration_list '}'  */
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-3].tok), (yyvsp[-5].decl), (yyvsp[-1].decl) ); }
#line 14397 "Parser/parser.cc"
    break;

  case 812: /* trait_declaration_list: trait_declaration_list trait_declaration  */
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->set_last( (yyvsp[0].decl) ); }
#line 14403 "Parser/parser.cc"
    break;

  case 817: /* cfa_trait_declaring_list: cfa_trait_declaring_list ',' identifier_or_type_name  */
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 14409 "Parser/parser.cc"
    break;

  case 818: /* trait_declaring_list: type_specifier_nobody declarator  */
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 14415 "Parser/parser.cc"
    break;

  case 819: /* trait_declaring_list: trait_declaring_list ',' declarator  */
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->set_last( (yyvsp[-2].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 14421 "Parser/parser.cc"
    break;

  case 820: /* trait_declaring_list: error  */
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Possible cause is declaring an aggregate or enumeration type in a trait." ); (yyval.decl) = nullptr; }
#line 14427 "Parser/parser.cc"
    break;

  case 822: /* translation_unit: external_definition_list  */
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->set_last( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 14433 "Parser/parser.cc"
    break;

  case 823: /* external_definition_list_opt: %empty  */
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14439 "Parser/parser.cc"
    break;

  case 825: /* external_definition_list: attribute_list_opt push external_definition pop  */
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 14445 "Parser/parser.cc"
    break;

  case 826: /* external_definition_list: external_definition_list attribute_list_opt push external_definition pop  */
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distAttr( (yyvsp[-3].decl), (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-4].decl) ? (yyvsp[-4].decl)->set_last( (yyvsp[-1].decl) ) : (yyvsp[-1].decl)->addQualifiers( (yyvsp[-3].decl) ); }
#line 14451 "Parser/parser.cc"
    break;

  case 827: /* up: %empty  */
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 14457 "Parser/parser.cc"
    break;

  case 828: /* down: %empty  */
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 14463 "Parser/parser.cc"
    break;

  case 829: /* external_definition: DIRECTIVE  */
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 14469 "Parser/parser.cc"
    break;

  case 830: /* external_definition: declaration  */
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 14485 "Parser/parser.cc"
    break;

  case 831: /* external_definition: IDENTIFIER IDENTIFIER  */
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 14491 "Parser/parser.cc"
    break;

  case 832: /* external_definition: IDENTIFIER type_qualifier  */
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 14497 "Parser/parser.cc"
    break;

  case 833: /* external_definition: IDENTIFIER storage_class  */
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 14503 "Parser/parser.cc"
    break;

  case 834: /* external_definition: IDENTIFIER basic_type_name  */
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14509 "Parser/parser.cc"
    break;

  case 835: /* external_definition: IDENTIFIER TYPEDEFname  */
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14515 "Parser/parser.cc"
    break;

  case 836: /* external_definition: IDENTIFIER TYPEGENname  */
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 14521 "Parser/parser.cc"
    break;

  case 838: /* external_definition: EXTENSION external_definition  */
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 14530 "Parser/parser.cc"
    break;

  case 839: /* external_definition: ASM '(' string_literal ')' ';'  */
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 14536 "Parser/parser.cc"
    break;

  case 840: /* $@10: %empty  */
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14545 "Parser/parser.cc"
    break;

  case 841: /* external_definition: EXTERN STRINGliteral $@10 up external_definition down  */
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 14555 "Parser/parser.cc"
    break;

  case 842: /* $@11: %empty  */
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 14564 "Parser/parser.cc"
    break;

  case 843: /* external_definition: EXTERN STRINGliteral $@11 '{' up external_definition_list_opt down '}'  */
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14574 "Parser/parser.cc"
    break;

  case 844: /* $@12: %empty  */
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 14585 "Parser/parser.cc"
    break;

  case 845: /* external_definition: type_qualifier_list $@12 '{' up external_definition_list_opt down '}'  */
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14595 "Parser/parser.cc"
    break;

  case 846: /* $@13: %empty  */
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 14606 "Parser/parser.cc"
    break;

  case 847: /* external_definition: declaration_qualifier_list $@13 '{' up external_definition_list_opt down '}'  */
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14616 "Parser/parser.cc"
    break;

  case 848: /* $@14: %empty  */
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "illegal syntax, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 14627 "Parser/parser.cc"
    break;

  case 849: /* external_definition: declaration_qualifier_list type_qualifier_list $@14 '{' up external_definition_list_opt down '}'  */
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14637 "Parser/parser.cc"
    break;

  case 850: /* external_definition: ';'  */
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14643 "Parser/parser.cc"
    break;

  case 852: /* external_function_definition: function_declarator compound_statement  */
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14649 "Parser/parser.cc"
    break;

  case 853: /* external_function_definition: KR_function_declarator KR_parameter_list_opt compound_statement  */
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 14655 "Parser/parser.cc"
    break;

  case 854: /* with_clause_opt: %empty  */
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 14661 "Parser/parser.cc"
    break;

  case 855: /* with_clause_opt: WITH '(' type_list ')' attribute_list_opt  */
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "illegal syntax, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 14673 "Parser/parser.cc"
    break;

  case 856: /* function_definition: cfa_function_declaration with_clause_opt compound_statement  */
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 14684 "Parser/parser.cc"
    break;

  case 857: /* function_definition: declaration_specifier function_declarator with_clause_opt compound_statement  */
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14693 "Parser/parser.cc"
    break;

  case 858: /* function_definition: declaration_specifier function_type_redeclarator with_clause_opt compound_statement  */
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 14702 "Parser/parser.cc"
    break;

  case 859: /* function_definition: type_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14708 "Parser/parser.cc"
    break;

  case 860: /* function_definition: declaration_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 14714 "Parser/parser.cc"
    break;

  case 861: /* function_definition: declaration_qualifier_list type_qualifier_list function_declarator with_clause_opt compound_statement  */
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14720 "Parser/parser.cc"
    break;

  case 862: /* function_definition: declaration_specifier KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 14729 "Parser/parser.cc"
    break;

  case 863: /* function_definition: type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14735 "Parser/parser.cc"
    break;

  case 864: /* function_definition: declaration_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 14741 "Parser/parser.cc"
    break;

  case 865: /* function_definition: declaration_qualifier_list type_qualifier_list KR_function_declarator KR_parameter_list_opt with_clause_opt compound_statement  */
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 14747 "Parser/parser.cc"
    break;

  case 870: /* subrange: constant_expression '~' constant_expression  */
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 14753 "Parser/parser.cc"
    break;

  case 871: /* asm_name_opt: %empty  */
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14759 "Parser/parser.cc"
    break;

  case 872: /* asm_name_opt: ASM '(' string_literal ')' attribute_list_opt  */
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 14769 "Parser/parser.cc"
    break;

  case 873: /* attribute_list_opt: %empty  */
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14775 "Parser/parser.cc"
    break;

  case 876: /* attribute_list: attribute_list attribute  */
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 14781 "Parser/parser.cc"
    break;

  case 877: /* attribute: ATTRIBUTE '(' '(' attribute_name_list ')' ')'  */
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 14787 "Parser/parser.cc"
    break;

  case 878: /* attribute: ATTRIBUTE '(' attribute_name_list ')'  */
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14793 "Parser/parser.cc"
    break;

  case 879: /* attribute: ATTR attribute_name_list ']'  */
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14799 "Parser/parser.cc"
    break;

  case 880: /* attribute: C23_ATTRIBUTE  */
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14805 "Parser/parser.cc"
    break;

  case 882: /* attribute_name_list: attribute_name_list ',' attribute_name  */
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14811 "Parser/parser.cc"
    break;

  case 883: /* attribute_name: %empty  */
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 14817 "Parser/parser.cc"
    break;

  case 884: /* attribute_name: attr_name  */
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 14823 "Parser/parser.cc"
    break;

  case 885: /* attribute_name: attr_name '(' argument_expression_list_opt ')'  */
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 14829 "Parser/parser.cc"
    break;

  case 887: /* attr_name: FALLTHROUGH  */
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 14835 "Parser/parser.cc"
    break;

  case 888: /* attr_name: CONST  */
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 14841 "Parser/parser.cc"
    break;

  case 889: /* paren_identifier: identifier_at  */
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14847 "Parser/parser.cc"
    break;

  case 890: /* paren_identifier: '?' identifier  */
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 14853 "Parser/parser.cc"
    break;

  case 891: /* paren_identifier: '(' paren_identifier ')'  */
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14859 "Parser/parser.cc"
    break;

  case 892: /* variable_declarator: paren_identifier attribute_list_opt  */
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14865 "Parser/parser.cc"
    break;

  case 894: /* variable_declarator: variable_array attribute_list_opt  */
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14871 "Parser/parser.cc"
    break;

  case 895: /* variable_declarator: variable_function attribute_list_opt  */
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14877 "Parser/parser.cc"
    break;

  case 896: /* variable_ptr: ptrref_operator variable_declarator  */
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 14883 "Parser/parser.cc"
    break;

  case 897: /* variable_ptr: ptrref_operator attribute_list variable_declarator  */
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 14889 "Parser/parser.cc"
    break;

  case 898: /* variable_ptr: ptrref_operator type_qualifier_list variable_declarator  */
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 14895 "Parser/parser.cc"
    break;

  case 899: /* variable_ptr: '(' variable_ptr ')' attribute_list_opt  */
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14901 "Parser/parser.cc"
    break;

  case 900: /* variable_ptr: '(' attribute_list variable_ptr ')' attribute_list_opt  */
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 14907 "Parser/parser.cc"
    break;

  case 901: /* variable_array: paren_identifier array_dimension  */
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 14913 "Parser/parser.cc"
    break;

  case 902: /* variable_array: '(' variable_ptr ')' array_dimension  */
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14919 "Parser/parser.cc"
    break;

  case 903: /* variable_array: '(' attribute_list variable_ptr ')' array_dimension  */
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14925 "Parser/parser.cc"
    break;

  case 904: /* variable_array: '(' variable_array ')' multi_array_dimension  */
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 14931 "Parser/parser.cc"
    break;

  case 905: /* variable_array: '(' attribute_list variable_array ')' multi_array_dimension  */
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 14937 "Parser/parser.cc"
    break;

  case 906: /* variable_array: '(' variable_array ')'  */
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14943 "Parser/parser.cc"
    break;

  case 907: /* variable_array: '(' attribute_list variable_array ')'  */
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14949 "Parser/parser.cc"
    break;

  case 908: /* variable_function: '(' variable_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14955 "Parser/parser.cc"
    break;

  case 909: /* variable_function: '(' attribute_list variable_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 14961 "Parser/parser.cc"
    break;

  case 910: /* variable_function: '(' variable_function ')'  */
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 14967 "Parser/parser.cc"
    break;

  case 911: /* variable_function: '(' attribute_list variable_function ')'  */
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 14973 "Parser/parser.cc"
    break;

  case 912: /* function_declarator: function_no_ptr attribute_list_opt  */
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14979 "Parser/parser.cc"
    break;

  case 914: /* function_declarator: function_array attribute_list_opt  */
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 14985 "Parser/parser.cc"
    break;

  case 915: /* function_no_ptr: paren_identifier '(' parameter_list_ellipsis_opt ')'  */
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14991 "Parser/parser.cc"
    break;

  case 916: /* function_no_ptr: '(' function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 14997 "Parser/parser.cc"
    break;

  case 917: /* function_no_ptr: '(' attribute_list function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15003 "Parser/parser.cc"
    break;

  case 918: /* function_no_ptr: '(' function_no_ptr ')'  */
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15009 "Parser/parser.cc"
    break;

  case 919: /* function_no_ptr: '(' attribute_list function_no_ptr ')'  */
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15015 "Parser/parser.cc"
    break;

  case 920: /* function_ptr: ptrref_operator function_declarator  */
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15021 "Parser/parser.cc"
    break;

  case 921: /* function_ptr: ptrref_operator attribute_list function_declarator  */
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15027 "Parser/parser.cc"
    break;

  case 922: /* function_ptr: ptrref_operator type_qualifier_list function_declarator  */
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15033 "Parser/parser.cc"
    break;

  case 923: /* function_ptr: '(' function_ptr ')' attribute_list_opt  */
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15039 "Parser/parser.cc"
    break;

  case 924: /* function_ptr: '(' attribute_list function_ptr ')' attribute_list_opt  */
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15045 "Parser/parser.cc"
    break;

  case 925: /* function_array: '(' function_ptr ')' array_dimension  */
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15051 "Parser/parser.cc"
    break;

  case 926: /* function_array: '(' attribute_list function_ptr ')' array_dimension  */
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15057 "Parser/parser.cc"
    break;

  case 927: /* function_array: '(' function_array ')' multi_array_dimension  */
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15063 "Parser/parser.cc"
    break;

  case 928: /* function_array: '(' attribute_list function_array ')' multi_array_dimension  */
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15069 "Parser/parser.cc"
    break;

  case 929: /* function_array: '(' function_array ')'  */
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15075 "Parser/parser.cc"
    break;

  case 930: /* function_array: '(' attribute_list function_array ')'  */
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15081 "Parser/parser.cc"
    break;

  case 934: /* KR_function_no_ptr: paren_identifier '(' identifier_list ')'  */
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 15087 "Parser/parser.cc"
    break;

  case 935: /* KR_function_no_ptr: '(' KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15093 "Parser/parser.cc"
    break;

  case 936: /* KR_function_no_ptr: '(' attribute_list KR_function_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15099 "Parser/parser.cc"
    break;

  case 937: /* KR_function_no_ptr: '(' KR_function_no_ptr ')'  */
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15105 "Parser/parser.cc"
    break;

  case 938: /* KR_function_no_ptr: '(' attribute_list KR_function_no_ptr ')'  */
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15111 "Parser/parser.cc"
    break;

  case 939: /* KR_function_ptr: ptrref_operator KR_function_declarator  */
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15117 "Parser/parser.cc"
    break;

  case 940: /* KR_function_ptr: ptrref_operator attribute_list KR_function_declarator  */
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15123 "Parser/parser.cc"
    break;

  case 941: /* KR_function_ptr: ptrref_operator type_qualifier_list KR_function_declarator  */
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15129 "Parser/parser.cc"
    break;

  case 942: /* KR_function_ptr: '(' KR_function_ptr ')'  */
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15135 "Parser/parser.cc"
    break;

  case 943: /* KR_function_ptr: '(' attribute_list KR_function_ptr ')'  */
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15141 "Parser/parser.cc"
    break;

  case 944: /* KR_function_array: '(' KR_function_ptr ')' array_dimension  */
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15147 "Parser/parser.cc"
    break;

  case 945: /* KR_function_array: '(' attribute_list KR_function_ptr ')' array_dimension  */
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15153 "Parser/parser.cc"
    break;

  case 946: /* KR_function_array: '(' KR_function_array ')' multi_array_dimension  */
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15159 "Parser/parser.cc"
    break;

  case 947: /* KR_function_array: '(' attribute_list KR_function_array ')' multi_array_dimension  */
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15165 "Parser/parser.cc"
    break;

  case 948: /* KR_function_array: '(' KR_function_array ')'  */
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15171 "Parser/parser.cc"
    break;

  case 949: /* KR_function_array: '(' attribute_list KR_function_array ')'  */
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15177 "Parser/parser.cc"
    break;

  case 950: /* paren_type: typedef_name  */
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "paren_type" );
		}
#line 15186 "Parser/parser.cc"
    break;

  case 951: /* paren_type: '(' paren_type ')'  */
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15192 "Parser/parser.cc"
    break;

  case 952: /* variable_type_redeclarator: paren_type attribute_list_opt  */
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15198 "Parser/parser.cc"
    break;

  case 954: /* variable_type_redeclarator: variable_type_array attribute_list_opt  */
#line 3768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15204 "Parser/parser.cc"
    break;

  case 955: /* variable_type_redeclarator: variable_type_function attribute_list_opt  */
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15210 "Parser/parser.cc"
    break;

  case 956: /* variable_type_ptr: ptrref_operator variable_type_redeclarator  */
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15216 "Parser/parser.cc"
    break;

  case 957: /* variable_type_ptr: ptrref_operator attribute_list variable_type_redeclarator  */
#line 3777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15222 "Parser/parser.cc"
    break;

  case 958: /* variable_type_ptr: ptrref_operator type_qualifier_list variable_type_redeclarator  */
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15228 "Parser/parser.cc"
    break;

  case 959: /* variable_type_ptr: '(' variable_type_ptr ')' attribute_list_opt  */
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15234 "Parser/parser.cc"
    break;

  case 960: /* variable_type_ptr: '(' attribute_list variable_type_ptr ')' attribute_list_opt  */
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15240 "Parser/parser.cc"
    break;

  case 961: /* variable_type_array: paren_type array_dimension  */
#line 3788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15246 "Parser/parser.cc"
    break;

  case 962: /* variable_type_array: '(' variable_type_ptr ')' array_dimension  */
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15252 "Parser/parser.cc"
    break;

  case 963: /* variable_type_array: '(' attribute_list variable_type_ptr ')' array_dimension  */
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15258 "Parser/parser.cc"
    break;

  case 964: /* variable_type_array: '(' variable_type_array ')' multi_array_dimension  */
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15264 "Parser/parser.cc"
    break;

  case 965: /* variable_type_array: '(' attribute_list variable_type_array ')' multi_array_dimension  */
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15270 "Parser/parser.cc"
    break;

  case 966: /* variable_type_array: '(' variable_type_array ')'  */
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15276 "Parser/parser.cc"
    break;

  case 967: /* variable_type_array: '(' attribute_list variable_type_array ')'  */
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15282 "Parser/parser.cc"
    break;

  case 968: /* variable_type_function: '(' variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15288 "Parser/parser.cc"
    break;

  case 969: /* variable_type_function: '(' attribute_list variable_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15294 "Parser/parser.cc"
    break;

  case 970: /* variable_type_function: '(' variable_type_function ')'  */
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15300 "Parser/parser.cc"
    break;

  case 971: /* variable_type_function: '(' attribute_list variable_type_function ')'  */
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15306 "Parser/parser.cc"
    break;

  case 972: /* function_type_redeclarator: function_type_no_ptr attribute_list_opt  */
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15312 "Parser/parser.cc"
    break;

  case 974: /* function_type_redeclarator: function_type_array attribute_list_opt  */
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15318 "Parser/parser.cc"
    break;

  case 975: /* function_type_no_ptr: paren_type '(' parameter_list_ellipsis_opt ')'  */
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15324 "Parser/parser.cc"
    break;

  case 976: /* function_type_no_ptr: '(' function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15330 "Parser/parser.cc"
    break;

  case 977: /* function_type_no_ptr: '(' attribute_list function_type_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addQualifiers( (yyvsp[-5].decl) )->addParamList( (yyvsp[-1].decl) ); }
#line 15336 "Parser/parser.cc"
    break;

  case 978: /* function_type_no_ptr: '(' function_type_no_ptr ')'  */
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15342 "Parser/parser.cc"
    break;

  case 979: /* function_type_no_ptr: '(' attribute_list function_type_no_ptr ')'  */
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15348 "Parser/parser.cc"
    break;

  case 980: /* function_type_ptr: ptrref_operator function_type_redeclarator  */
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15354 "Parser/parser.cc"
    break;

  case 981: /* function_type_ptr: ptrref_operator attribute_list function_type_redeclarator  */
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15360 "Parser/parser.cc"
    break;

  case 982: /* function_type_ptr: ptrref_operator type_qualifier_list function_type_redeclarator  */
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15366 "Parser/parser.cc"
    break;

  case 983: /* function_type_ptr: '(' function_type_ptr ')' attribute_list_opt  */
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15372 "Parser/parser.cc"
    break;

  case 984: /* function_type_ptr: '(' attribute_list function_type_ptr ')' attribute_list_opt  */
#line 3849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15378 "Parser/parser.cc"
    break;

  case 985: /* function_type_array: '(' function_type_ptr ')' array_dimension  */
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15384 "Parser/parser.cc"
    break;

  case 986: /* function_type_array: '(' attribute_list function_type_ptr ')' array_dimension  */
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15390 "Parser/parser.cc"
    break;

  case 987: /* function_type_array: '(' function_type_array ')' multi_array_dimension  */
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15396 "Parser/parser.cc"
    break;

  case 988: /* function_type_array: '(' attribute_list function_type_array ')' multi_array_dimension  */
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 15402 "Parser/parser.cc"
    break;

  case 989: /* function_type_array: '(' function_type_array ')'  */
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15408 "Parser/parser.cc"
    break;

  case 990: /* function_type_array: '(' attribute_list function_type_array ')'  */
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 15414 "Parser/parser.cc"
    break;

  case 991: /* identifier_parameter_declarator: paren_identifier attribute_list_opt  */
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15420 "Parser/parser.cc"
    break;

  case 992: /* identifier_parameter_declarator: '&' MUTEX paren_identifier attribute_list_opt  */
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15427 "Parser/parser.cc"
    break;

  case 994: /* identifier_parameter_declarator: identifier_parameter_array attribute_list_opt  */
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15433 "Parser/parser.cc"
    break;

  case 995: /* identifier_parameter_declarator: identifier_parameter_function attribute_list_opt  */
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15439 "Parser/parser.cc"
    break;

  case 996: /* identifier_parameter_ptr: ptrref_operator identifier_parameter_declarator  */
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15445 "Parser/parser.cc"
    break;

  case 997: /* identifier_parameter_ptr: ptrref_operator attribute_list identifier_parameter_declarator  */
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15451 "Parser/parser.cc"
    break;

  case 998: /* identifier_parameter_ptr: ptrref_operator type_qualifier_list identifier_parameter_declarator  */
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15457 "Parser/parser.cc"
    break;

  case 999: /* identifier_parameter_ptr: '(' identifier_parameter_ptr ')' attribute_list_opt  */
#line 3893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15463 "Parser/parser.cc"
    break;

  case 1000: /* identifier_parameter_array: paren_identifier array_parameter_dimension  */
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15469 "Parser/parser.cc"
    break;

  case 1001: /* identifier_parameter_array: '(' identifier_parameter_ptr ')' array_dimension  */
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15475 "Parser/parser.cc"
    break;

  case 1002: /* identifier_parameter_array: '(' identifier_parameter_array ')' multi_array_dimension  */
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15481 "Parser/parser.cc"
    break;

  case 1003: /* identifier_parameter_array: '(' identifier_parameter_array ')'  */
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15487 "Parser/parser.cc"
    break;

  case 1004: /* identifier_parameter_function: paren_identifier '(' parameter_list_ellipsis_opt ')'  */
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15493 "Parser/parser.cc"
    break;

  case 1005: /* identifier_parameter_function: '(' identifier_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15499 "Parser/parser.cc"
    break;

  case 1006: /* identifier_parameter_function: '(' identifier_parameter_function ')'  */
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15505 "Parser/parser.cc"
    break;

  case 1007: /* type_parameter_redeclarator: typedef_name attribute_list_opt  */
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15511 "Parser/parser.cc"
    break;

  case 1008: /* type_parameter_redeclarator: '&' MUTEX typedef_name attribute_list_opt  */
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
															OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15518 "Parser/parser.cc"
    break;

  case 1010: /* type_parameter_redeclarator: type_parameter_array attribute_list_opt  */
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15524 "Parser/parser.cc"
    break;

  case 1011: /* type_parameter_redeclarator: type_parameter_function attribute_list_opt  */
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15530 "Parser/parser.cc"
    break;

  case 1012: /* typedef_name: TYPEDEFname  */
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15536 "Parser/parser.cc"
    break;

  case 1013: /* typedef_name: TYPEGENname  */
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 15542 "Parser/parser.cc"
    break;

  case 1014: /* type_parameter_ptr: ptrref_operator type_parameter_redeclarator  */
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15548 "Parser/parser.cc"
    break;

  case 1015: /* type_parameter_ptr: ptrref_operator attribute_list type_parameter_redeclarator  */
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15554 "Parser/parser.cc"
    break;

  case 1016: /* type_parameter_ptr: ptrref_operator type_qualifier_list type_parameter_redeclarator  */
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15560 "Parser/parser.cc"
    break;

  case 1017: /* type_parameter_ptr: '(' type_parameter_ptr ')' attribute_list_opt  */
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15566 "Parser/parser.cc"
    break;

  case 1018: /* type_parameter_array: typedef_name array_parameter_dimension  */
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15572 "Parser/parser.cc"
    break;

  case 1019: /* type_parameter_array: '(' type_parameter_ptr ')' array_parameter_dimension  */
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15578 "Parser/parser.cc"
    break;

  case 1020: /* type_parameter_function: typedef_name '(' parameter_list_ellipsis_opt ')'  */
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15584 "Parser/parser.cc"
    break;

  case 1021: /* type_parameter_function: '(' type_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15590 "Parser/parser.cc"
    break;

  case 1023: /* abstract_declarator: abstract_array attribute_list_opt  */
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15596 "Parser/parser.cc"
    break;

  case 1024: /* abstract_declarator: abstract_function attribute_list_opt  */
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15602 "Parser/parser.cc"
    break;

  case 1025: /* abstract_ptr: ptrref_operator attribute_list_opt  */
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15608 "Parser/parser.cc"
    break;

  case 1026: /* abstract_ptr: ptrref_operator type_qualifier_list  */
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15614 "Parser/parser.cc"
    break;

  case 1027: /* abstract_ptr: ptrref_operator abstract_declarator  */
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15620 "Parser/parser.cc"
    break;

  case 1028: /* abstract_ptr: ptrref_operator attribute_list abstract_declarator  */
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) )->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 15626 "Parser/parser.cc"
    break;

  case 1029: /* abstract_ptr: ptrref_operator type_qualifier_list abstract_declarator  */
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15632 "Parser/parser.cc"
    break;

  case 1030: /* abstract_ptr: '(' abstract_ptr ')' attribute_list_opt  */
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15638 "Parser/parser.cc"
    break;

  case 1032: /* abstract_array: '(' abstract_ptr ')' array_dimension  */
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15644 "Parser/parser.cc"
    break;

  case 1033: /* abstract_array: '(' abstract_array ')' multi_array_dimension  */
#line 4010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15650 "Parser/parser.cc"
    break;

  case 1034: /* abstract_array: '(' abstract_array ')'  */
#line 4012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15656 "Parser/parser.cc"
    break;

  case 1035: /* abstract_function: '(' parameter_list_ellipsis_opt ')'  */
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15662 "Parser/parser.cc"
    break;

  case 1036: /* abstract_function: '(' abstract_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15668 "Parser/parser.cc"
    break;

  case 1037: /* abstract_function: '(' abstract_function ')'  */
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15674 "Parser/parser.cc"
    break;

  case 1038: /* array_dimension: '[' ']'  */
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15680 "Parser/parser.cc"
    break;

  case 1039: /* array_dimension: '[' ']' multi_array_dimension  */
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 15686 "Parser/parser.cc"
    break;

  case 1040: /* array_dimension: '[' assignment_expression ',' ']'  */
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15692 "Parser/parser.cc"
    break;

  case 1041: /* array_dimension: '[' assignment_expression ',' comma_expression ']'  */
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 15698 "Parser/parser.cc"
    break;

  case 1042: /* array_dimension: '[' array_type_list ']'  */
#line 4042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15704 "Parser/parser.cc"
    break;

  case 1044: /* array_type_list: basic_type_name  */
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 15710 "Parser/parser.cc"
    break;

  case 1045: /* array_type_list: type_name  */
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ); }
#line 15716 "Parser/parser.cc"
    break;

  case 1047: /* array_type_list: array_type_list ',' basic_type_name  */
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) ); }
#line 15722 "Parser/parser.cc"
    break;

  case 1048: /* array_type_list: array_type_list ',' type_name  */
#line 4060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].type) ) ) ) ); }
#line 15728 "Parser/parser.cc"
    break;

  case 1050: /* upupeq: '~'  */
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 15734 "Parser/parser.cc"
    break;

  case 1051: /* upupeq: ErangeUpLe  */
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 15740 "Parser/parser.cc"
    break;

  case 1052: /* multi_array_dimension: '[' assignment_expression ']'  */
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ); }
#line 15746 "Parser/parser.cc"
    break;

  case 1053: /* multi_array_dimension: '[' '*' ']'  */
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 15752 "Parser/parser.cc"
    break;

  case 1054: /* multi_array_dimension: multi_array_dimension '[' assignment_expression ']'  */
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 15758 "Parser/parser.cc"
    break;

  case 1055: /* multi_array_dimension: multi_array_dimension '[' '*' ']'  */
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 15764 "Parser/parser.cc"
    break;

  case 1056: /* abstract_parameter_declarator_opt: %empty  */
#line 4113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 15770 "Parser/parser.cc"
    break;

  case 1059: /* abstract_parameter_declarator: '&' MUTEX attribute_list_opt  */
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newFromTypeData( build_type_qualifier( ast::CV::Mutex ) ),
											OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 15777 "Parser/parser.cc"
    break;

  case 1060: /* abstract_parameter_declarator: abstract_parameter_array attribute_list_opt  */
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15783 "Parser/parser.cc"
    break;

  case 1061: /* abstract_parameter_declarator: abstract_parameter_function attribute_list_opt  */
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15789 "Parser/parser.cc"
    break;

  case 1062: /* abstract_parameter_ptr: ptrref_operator attribute_list_opt  */
#line 4130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15795 "Parser/parser.cc"
    break;

  case 1063: /* abstract_parameter_ptr: ptrref_operator type_qualifier_list  */
#line 4132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15801 "Parser/parser.cc"
    break;

  case 1064: /* abstract_parameter_ptr: ptrref_operator abstract_parameter_declarator  */
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15807 "Parser/parser.cc"
    break;

  case 1065: /* abstract_parameter_ptr: ptrref_operator type_qualifier_list abstract_parameter_declarator  */
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15813 "Parser/parser.cc"
    break;

  case 1066: /* abstract_parameter_ptr: '(' abstract_parameter_ptr ')' attribute_list_opt  */
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15819 "Parser/parser.cc"
    break;

  case 1068: /* abstract_parameter_array: '(' abstract_parameter_ptr ')' array_parameter_dimension  */
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15825 "Parser/parser.cc"
    break;

  case 1069: /* abstract_parameter_array: '(' abstract_parameter_array ')' multi_array_dimension  */
#line 4146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15831 "Parser/parser.cc"
    break;

  case 1070: /* abstract_parameter_array: '(' abstract_parameter_array ')'  */
#line 4148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15837 "Parser/parser.cc"
    break;

  case 1071: /* abstract_parameter_function: '(' parameter_list_ellipsis_opt ')'  */
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-1].decl), nullptr ); }
#line 15843 "Parser/parser.cc"
    break;

  case 1072: /* abstract_parameter_function: '(' abstract_parameter_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15849 "Parser/parser.cc"
    break;

  case 1073: /* abstract_parameter_function: '(' abstract_parameter_function ')'  */
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15855 "Parser/parser.cc"
    break;

  case 1075: /* array_parameter_dimension: array_parameter_1st_dimension multi_array_dimension  */
#line 4164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 15861 "Parser/parser.cc"
    break;

  case 1077: /* array_parameter_1st_dimension: '[' ']'  */
#line 4175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 15867 "Parser/parser.cc"
    break;

  case 1078: /* array_parameter_1st_dimension: '[' push type_qualifier_list '*' pop ']'  */
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 15873 "Parser/parser.cc"
    break;

  case 1079: /* array_parameter_1st_dimension: '[' push type_qualifier_list pop ']'  */
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 15879 "Parser/parser.cc"
    break;

  case 1080: /* array_parameter_1st_dimension: '[' push type_qualifier_list assignment_expression pop ']'  */
#line 4183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 15885 "Parser/parser.cc"
    break;

  case 1081: /* array_parameter_1st_dimension: '[' push STATIC type_qualifier_list_opt assignment_expression pop ']'  */
#line 4185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 15891 "Parser/parser.cc"
    break;

  case 1082: /* array_parameter_1st_dimension: '[' push type_qualifier_list STATIC assignment_expression pop ']'  */
#line 4187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 15897 "Parser/parser.cc"
    break;

  case 1084: /* variable_abstract_declarator: variable_abstract_array attribute_list_opt  */
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15903 "Parser/parser.cc"
    break;

  case 1085: /* variable_abstract_declarator: variable_abstract_function attribute_list_opt  */
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15909 "Parser/parser.cc"
    break;

  case 1086: /* variable_abstract_ptr: ptrref_operator attribute_list_opt  */
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) )->addQualifiers( (yyvsp[0].decl) ); }
#line 15915 "Parser/parser.cc"
    break;

  case 1087: /* variable_abstract_ptr: ptrref_operator type_qualifier_list  */
#line 4211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 15921 "Parser/parser.cc"
    break;

  case 1088: /* variable_abstract_ptr: ptrref_operator variable_abstract_declarator  */
#line 4213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15927 "Parser/parser.cc"
    break;

  case 1089: /* variable_abstract_ptr: ptrref_operator type_qualifier_list variable_abstract_declarator  */
#line 4215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 15933 "Parser/parser.cc"
    break;

  case 1090: /* variable_abstract_ptr: '(' variable_abstract_ptr ')' attribute_list_opt  */
#line 4217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 15939 "Parser/parser.cc"
    break;

  case 1092: /* variable_abstract_array: '(' variable_abstract_ptr ')' array_dimension  */
#line 4223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15945 "Parser/parser.cc"
    break;

  case 1093: /* variable_abstract_array: '(' variable_abstract_array ')' multi_array_dimension  */
#line 4225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 15951 "Parser/parser.cc"
    break;

  case 1094: /* variable_abstract_array: '(' variable_abstract_array ')'  */
#line 4227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15957 "Parser/parser.cc"
    break;

  case 1095: /* variable_abstract_function: '(' variable_abstract_ptr ')' '(' parameter_list_ellipsis_opt ')'  */
#line 4232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addParamList( (yyvsp[-1].decl) ); }
#line 15963 "Parser/parser.cc"
    break;

  case 1096: /* variable_abstract_function: '(' variable_abstract_function ')'  */
#line 4234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 15969 "Parser/parser.cc"
    break;

  case 1099: /* cfa_identifier_parameter_declarator_tuple: type_qualifier_list cfa_abstract_tuple  */
#line 4244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15975 "Parser/parser.cc"
    break;

  case 1102: /* cfa_identifier_parameter_declarator_no_tuple: type_qualifier_list cfa_identifier_parameter_array  */
#line 4251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 15981 "Parser/parser.cc"
    break;

  case 1103: /* cfa_identifier_parameter_ptr: ptrref_operator type_specifier_nobody  */
#line 4257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 15987 "Parser/parser.cc"
    break;

  case 1104: /* cfa_identifier_parameter_ptr: ptrref_operator attribute_list type_specifier_nobody  */
#line 4259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 15993 "Parser/parser.cc"
    break;

  case 1105: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator type_specifier_nobody  */
#line 4261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 15999 "Parser/parser.cc"
    break;

  case 1106: /* cfa_identifier_parameter_ptr: ptrref_operator cfa_abstract_function  */
#line 4263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16005 "Parser/parser.cc"
    break;

  case 1107: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator cfa_abstract_function  */
#line 4265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16011 "Parser/parser.cc"
    break;

  case 1108: /* cfa_identifier_parameter_ptr: ptrref_operator cfa_identifier_parameter_declarator_tuple  */
#line 4267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16017 "Parser/parser.cc"
    break;

  case 1109: /* cfa_identifier_parameter_ptr: type_qualifier_list ptrref_operator cfa_identifier_parameter_declarator_tuple  */
#line 4269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16023 "Parser/parser.cc"
    break;

  case 1110: /* cfa_identifier_parameter_array: '[' ']' type_specifier_nobody  */
#line 4276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16029 "Parser/parser.cc"
    break;

  case 1111: /* cfa_identifier_parameter_array: '[' ']' cfa_abstract_tuple  */
#line 4278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16035 "Parser/parser.cc"
    break;

  case 1112: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension type_specifier_nobody  */
#line 4280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16041 "Parser/parser.cc"
    break;

  case 1113: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension cfa_abstract_tuple  */
#line 4282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16047 "Parser/parser.cc"
    break;

  case 1114: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension type_specifier_nobody  */
#line 4284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16053 "Parser/parser.cc"
    break;

  case 1115: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension cfa_abstract_tuple  */
#line 4286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16059 "Parser/parser.cc"
    break;

  case 1116: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension type_specifier_nobody  */
#line 4288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16065 "Parser/parser.cc"
    break;

  case 1117: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension cfa_abstract_tuple  */
#line 4290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16071 "Parser/parser.cc"
    break;

  case 1118: /* cfa_identifier_parameter_array: multi_array_dimension type_specifier_nobody  */
#line 4292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16077 "Parser/parser.cc"
    break;

  case 1119: /* cfa_identifier_parameter_array: multi_array_dimension cfa_abstract_tuple  */
#line 4294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16083 "Parser/parser.cc"
    break;

  case 1120: /* cfa_identifier_parameter_array: '[' ']' cfa_identifier_parameter_ptr  */
#line 4297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16089 "Parser/parser.cc"
    break;

  case 1121: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension cfa_identifier_parameter_ptr  */
#line 4299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16095 "Parser/parser.cc"
    break;

  case 1122: /* cfa_identifier_parameter_array: '[' ']' multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16101 "Parser/parser.cc"
    break;

  case 1123: /* cfa_identifier_parameter_array: cfa_array_parameter_1st_dimension multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 16107 "Parser/parser.cc"
    break;

  case 1124: /* cfa_identifier_parameter_array: multi_array_dimension cfa_identifier_parameter_ptr  */
#line 4305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16113 "Parser/parser.cc"
    break;

  case 1125: /* cfa_array_parameter_1st_dimension: '[' type_qualifier_list '*' ']'  */
#line 4310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-2].decl) ); }
#line 16119 "Parser/parser.cc"
    break;

  case 1126: /* cfa_array_parameter_1st_dimension: '[' type_qualifier_list assignment_expression ']'  */
#line 4312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), false ); }
#line 16125 "Parser/parser.cc"
    break;

  case 1127: /* cfa_array_parameter_1st_dimension: '[' declaration_qualifier_list assignment_expression ']'  */
#line 4317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl), true ); }
#line 16131 "Parser/parser.cc"
    break;

  case 1128: /* cfa_array_parameter_1st_dimension: '[' declaration_qualifier_list type_qualifier_list assignment_expression ']'  */
#line 4319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-1].expr), (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) ), true ); }
#line 16137 "Parser/parser.cc"
    break;

  case 1130: /* cfa_abstract_declarator_tuple: type_qualifier_list cfa_abstract_tuple  */
#line 4346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 16143 "Parser/parser.cc"
    break;

  case 1134: /* cfa_abstract_ptr: ptrref_operator type_specifier  */
#line 4357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16149 "Parser/parser.cc"
    break;

  case 1135: /* cfa_abstract_ptr: ptrref_operator attribute_list type_specifier  */
#line 4359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-2].oper) ) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 16155 "Parser/parser.cc"
    break;

  case 1136: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator type_specifier  */
#line 4361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16161 "Parser/parser.cc"
    break;

  case 1137: /* cfa_abstract_ptr: ptrref_operator cfa_abstract_function  */
#line 4363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16167 "Parser/parser.cc"
    break;

  case 1138: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator cfa_abstract_function  */
#line 4365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16173 "Parser/parser.cc"
    break;

  case 1139: /* cfa_abstract_ptr: ptrref_operator cfa_abstract_declarator_tuple  */
#line 4367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 16179 "Parser/parser.cc"
    break;

  case 1140: /* cfa_abstract_ptr: type_qualifier_list ptrref_operator cfa_abstract_declarator_tuple  */
#line 4369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 16185 "Parser/parser.cc"
    break;

  case 1141: /* cfa_abstract_array: '[' ']' type_specifier  */
#line 4376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16191 "Parser/parser.cc"
    break;

  case 1142: /* cfa_abstract_array: '[' ']' multi_array_dimension type_specifier  */
#line 4378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16197 "Parser/parser.cc"
    break;

  case 1143: /* cfa_abstract_array: multi_array_dimension type_specifier  */
#line 4380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16203 "Parser/parser.cc"
    break;

  case 1144: /* cfa_abstract_array: '[' ']' cfa_abstract_ptr  */
#line 4382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16209 "Parser/parser.cc"
    break;

  case 1145: /* cfa_abstract_array: '[' ']' multi_array_dimension cfa_abstract_ptr  */
#line 4384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 16215 "Parser/parser.cc"
    break;

  case 1146: /* cfa_abstract_array: multi_array_dimension cfa_abstract_ptr  */
#line 4386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 16221 "Parser/parser.cc"
    break;

  case 1147: /* cfa_abstract_tuple: '[' cfa_abstract_parameter_list ']'  */
#line 4391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-1].decl) ); }
#line 16227 "Parser/parser.cc"
    break;

  case 1148: /* cfa_abstract_tuple: '[' type_specifier_nobody ELLIPSIS ']'  */
#line 4393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 16233 "Parser/parser.cc"
    break;

  case 1149: /* cfa_abstract_tuple: '[' type_specifier_nobody ELLIPSIS constant_expression ']'  */
#line 4395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 16239 "Parser/parser.cc"
    break;

  case 1150: /* cfa_abstract_function: '[' ']' '(' cfa_parameter_list_ellipsis_opt ')'  */
#line 4400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, DeclarationNode::newTuple( nullptr ), (yyvsp[-1].decl), nullptr ); }
#line 16245 "Parser/parser.cc"
    break;

  case 1151: /* cfa_abstract_function: cfa_abstract_tuple '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 4402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 16251 "Parser/parser.cc"
    break;

  case 1152: /* cfa_abstract_function: cfa_function_return '(' push cfa_parameter_list_ellipsis_opt pop ')'  */
#line 4404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 16257 "Parser/parser.cc"
    break;

  case 1155: /* default_initializer_opt: %empty  */
#line 4428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 16263 "Parser/parser.cc"
    break;

  case 1156: /* default_initializer_opt: '=' assignment_expression  */
#line 4430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 16269 "Parser/parser.cc"
    break;


#line 16273 "Parser/parser.cc"

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

#line 4433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "bison -Wcounterexamples parser.yy" //
// End: //
