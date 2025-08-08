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

#line 430 "Parser/parser.hh"

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
