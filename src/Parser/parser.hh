/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

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

#line 432 "Parser/parser.hh"

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
