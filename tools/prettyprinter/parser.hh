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

#ifndef YY_YY_PARSER_HH_INCLUDED
# define YY_YY_PARSER_HH_INCLUDED
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
    MARK = 258,                    /* MARK  */
    LCURL = 259,                   /* LCURL  */
    RCURL = 260,                   /* RCURL  */
    INTEGER = 261,                 /* INTEGER  */
    CHARACTER = 262,               /* CHARACTER  */
    IDENTIFIER = 263,              /* IDENTIFIER  */
    CODE = 264,                    /* CODE  */
    DEFINE = 265,                  /* DEFINE  */
    EXPECT = 266,                  /* EXPECT  */
    LEFT = 267,                    /* LEFT  */
    LOCATIONS = 268,               /* LOCATIONS  */
    NONASSOC = 269,                /* NONASSOC  */
    PRECEDENCE = 270,              /* PRECEDENCE  */
    PURE_PARSER = 271,             /* PURE_PARSER  */
    RIGHT = 272,                   /* RIGHT  */
    SEMANTIC_PARSER = 273,         /* SEMANTIC_PARSER  */
    START = 274,                   /* START  */
    THONG = 275,                   /* THONG  */
    TOKEN = 276,                   /* TOKEN  */
    TYPE = 277,                    /* TYPE  */
    UNION = 278,                   /* UNION  */
    PREC = 279,                    /* PREC  */
    END_TERMINALS = 280,           /* END_TERMINALS  */
    _SECTIONS = 281,               /* _SECTIONS  */
    _DEFSECTION_OPT = 282,         /* _DEFSECTION_OPT  */
    _LITERALBLOCK = 283,           /* _LITERALBLOCK  */
    _DECLARATION = 284,            /* _DECLARATION  */
    _TAG_OPT = 285,                /* _TAG_OPT  */
    _NAMENOLIST = 286,             /* _NAMENOLIST  */
    _NAMENO = 287,                 /* _NAMENO  */
    _NAMELIST = 288,               /* _NAMELIST  */
    _RULESECTION = 289,            /* _RULESECTION  */
    _RULE = 290,                   /* _RULE  */
    _LHS = 291,                    /* _LHS  */
    _RHS = 292,                    /* _RHS  */
    _PREC = 293,                   /* _PREC  */
    _ACTION = 294,                 /* _ACTION  */
    _USERSECTION_OPT = 295         /* _USERSECTION_OPT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define MARK 258
#define LCURL 259
#define RCURL 260
#define INTEGER 261
#define CHARACTER 262
#define IDENTIFIER 263
#define CODE 264
#define DEFINE 265
#define EXPECT 266
#define LEFT 267
#define LOCATIONS 268
#define NONASSOC 269
#define PRECEDENCE 270
#define PURE_PARSER 271
#define RIGHT 272
#define SEMANTIC_PARSER 273
#define START 274
#define THONG 275
#define TOKEN 276
#define TYPE 277
#define UNION 278
#define PREC 279
#define END_TERMINALS 280
#define _SECTIONS 281
#define _DEFSECTION_OPT 282
#define _LITERALBLOCK 283
#define _DECLARATION 284
#define _TAG_OPT 285
#define _NAMENOLIST 286
#define _NAMENO 287
#define _NAMELIST 288
#define _RULESECTION 289
#define _RULE 290
#define _LHS 291
#define _RHS 292
#define _PREC 293
#define _ACTION 294
#define _USERSECTION_OPT 295

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 42 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/tools/prettyprinter/parser.yy"

	Token *tokenp;

#line 151 "parser.hh"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_HH_INCLUDED  */
