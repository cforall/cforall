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
#line 270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 375 "Parser/parser.hh"

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
