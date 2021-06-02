/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

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
    THREADLOCAL = 263,
    INLINE = 264,
    FORTRAN = 265,
    NORETURN = 266,
    CONST = 267,
    VOLATILE = 268,
    RESTRICT = 269,
    ATOMIC = 270,
    FORALL = 271,
    MUTEX = 272,
    VIRTUAL = 273,
    VTABLE = 274,
    COERCE = 275,
    VOID = 276,
    CHAR = 277,
    SHORT = 278,
    INT = 279,
    LONG = 280,
    FLOAT = 281,
    DOUBLE = 282,
    SIGNED = 283,
    UNSIGNED = 284,
    BOOL = 285,
    COMPLEX = 286,
    IMAGINARY = 287,
    INT128 = 288,
    UINT128 = 289,
    uuFLOAT80 = 290,
    uuFLOAT128 = 291,
    uFLOAT16 = 292,
    uFLOAT32 = 293,
    uFLOAT32X = 294,
    uFLOAT64 = 295,
    uFLOAT64X = 296,
    uFLOAT128 = 297,
    ZERO_T = 298,
    ONE_T = 299,
    SIZEOF = 300,
    TYPEOF = 301,
    VALIST = 302,
    AUTO_TYPE = 303,
    OFFSETOF = 304,
    BASETYPEOF = 305,
    TYPEID = 306,
    ENUM = 307,
    STRUCT = 308,
    UNION = 309,
    EXCEPTION = 310,
    GENERATOR = 311,
    COROUTINE = 312,
    MONITOR = 313,
    THREAD = 314,
    OTYPE = 315,
    FTYPE = 316,
    DTYPE = 317,
    TTYPE = 318,
    TRAIT = 319,
    LABEL = 320,
    SUSPEND = 321,
    ATTRIBUTE = 322,
    EXTENSION = 323,
    IF = 324,
    ELSE = 325,
    SWITCH = 326,
    CASE = 327,
    DEFAULT = 328,
    DO = 329,
    WHILE = 330,
    FOR = 331,
    BREAK = 332,
    CONTINUE = 333,
    GOTO = 334,
    RETURN = 335,
    CHOOSE = 336,
    FALLTHRU = 337,
    FALLTHROUGH = 338,
    WITH = 339,
    WHEN = 340,
    WAITFOR = 341,
    DISABLE = 342,
    ENABLE = 343,
    TRY = 344,
    THROW = 345,
    THROWRESUME = 346,
    AT = 347,
    ASM = 348,
    ALIGNAS = 349,
    ALIGNOF = 350,
    GENERIC = 351,
    STATICASSERT = 352,
    IDENTIFIER = 353,
    QUOTED_IDENTIFIER = 354,
    TYPEDEFname = 355,
    TYPEGENname = 356,
    TIMEOUT = 357,
    WOR = 358,
    CATCH = 359,
    RECOVER = 360,
    CATCHRESUME = 361,
    FIXUP = 362,
    FINALLY = 363,
    INTEGERconstant = 364,
    CHARACTERconstant = 365,
    STRINGliteral = 366,
    DIRECTIVE = 367,
    FLOATING_DECIMALconstant = 368,
    FLOATING_FRACTIONconstant = 369,
    FLOATINGconstant = 370,
    ARROW = 371,
    ICR = 372,
    DECR = 373,
    LS = 374,
    RS = 375,
    LE = 376,
    GE = 377,
    EQ = 378,
    NE = 379,
    ANDAND = 380,
    OROR = 381,
    ELLIPSIS = 382,
    EXPassign = 383,
    MULTassign = 384,
    DIVassign = 385,
    MODassign = 386,
    PLUSassign = 387,
    MINUSassign = 388,
    LSassign = 389,
    RSassign = 390,
    ANDassign = 391,
    ERassign = 392,
    ORassign = 393,
    ErangeUpEq = 394,
    ErangeDown = 395,
    ErangeDownEq = 396,
    ATassign = 397,
    THEN = 398
  };
#endif
/* Tokens.  */
#define TYPEDEF 258
#define EXTERN 259
#define STATIC 260
#define AUTO 261
#define REGISTER 262
#define THREADLOCAL 263
#define INLINE 264
#define FORTRAN 265
#define NORETURN 266
#define CONST 267
#define VOLATILE 268
#define RESTRICT 269
#define ATOMIC 270
#define FORALL 271
#define MUTEX 272
#define VIRTUAL 273
#define VTABLE 274
#define COERCE 275
#define VOID 276
#define CHAR 277
#define SHORT 278
#define INT 279
#define LONG 280
#define FLOAT 281
#define DOUBLE 282
#define SIGNED 283
#define UNSIGNED 284
#define BOOL 285
#define COMPLEX 286
#define IMAGINARY 287
#define INT128 288
#define UINT128 289
#define uuFLOAT80 290
#define uuFLOAT128 291
#define uFLOAT16 292
#define uFLOAT32 293
#define uFLOAT32X 294
#define uFLOAT64 295
#define uFLOAT64X 296
#define uFLOAT128 297
#define ZERO_T 298
#define ONE_T 299
#define SIZEOF 300
#define TYPEOF 301
#define VALIST 302
#define AUTO_TYPE 303
#define OFFSETOF 304
#define BASETYPEOF 305
#define TYPEID 306
#define ENUM 307
#define STRUCT 308
#define UNION 309
#define EXCEPTION 310
#define GENERATOR 311
#define COROUTINE 312
#define MONITOR 313
#define THREAD 314
#define OTYPE 315
#define FTYPE 316
#define DTYPE 317
#define TTYPE 318
#define TRAIT 319
#define LABEL 320
#define SUSPEND 321
#define ATTRIBUTE 322
#define EXTENSION 323
#define IF 324
#define ELSE 325
#define SWITCH 326
#define CASE 327
#define DEFAULT 328
#define DO 329
#define WHILE 330
#define FOR 331
#define BREAK 332
#define CONTINUE 333
#define GOTO 334
#define RETURN 335
#define CHOOSE 336
#define FALLTHRU 337
#define FALLTHROUGH 338
#define WITH 339
#define WHEN 340
#define WAITFOR 341
#define DISABLE 342
#define ENABLE 343
#define TRY 344
#define THROW 345
#define THROWRESUME 346
#define AT 347
#define ASM 348
#define ALIGNAS 349
#define ALIGNOF 350
#define GENERIC 351
#define STATICASSERT 352
#define IDENTIFIER 353
#define QUOTED_IDENTIFIER 354
#define TYPEDEFname 355
#define TYPEGENname 356
#define TIMEOUT 357
#define WOR 358
#define CATCH 359
#define RECOVER 360
#define CATCHRESUME 361
#define FIXUP 362
#define FINALLY 363
#define INTEGERconstant 364
#define CHARACTERconstant 365
#define STRINGliteral 366
#define DIRECTIVE 367
#define FLOATING_DECIMALconstant 368
#define FLOATING_FRACTIONconstant 369
#define FLOATINGconstant 370
#define ARROW 371
#define ICR 372
#define DECR 373
#define LS 374
#define RS 375
#define LE 376
#define GE 377
#define EQ 378
#define NE 379
#define ANDAND 380
#define OROR 381
#define ELLIPSIS 382
#define EXPassign 383
#define MULTassign 384
#define DIVassign 385
#define MODassign 386
#define PLUSassign 387
#define MINUSassign 388
#define LSassign 389
#define RSassign 390
#define ANDassign 391
#define ERassign 392
#define ORassign 393
#define ErangeUpEq 394
#define ErangeDown 395
#define ErangeDownEq 396
#define ATassign 397
#define THEN 398

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	Token tok;
	ParseNode * pn;
	ExpressionNode * en;
	DeclarationNode * decl;
	AggregateDecl::Aggregate aggKey;
	TypeDecl::Kind tclass;
	StatementNode * sn;
	WaitForStmt * wfs;
	Expression * constant;
	IfCtrl * ifctl;
	ForCtrl * fctl;
	enum OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;

#line 365 "Parser/parser.hh"

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
