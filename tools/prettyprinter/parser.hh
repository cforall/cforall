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

#ifndef YY_YY_PARSER_HH_INCLUDED
# define YY_YY_PARSER_HH_INCLUDED
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
    MARK = 258,
    LCURL = 259,
    RCURL = 260,
    INTEGER = 261,
    CHARACTER = 262,
    IDENTIFIER = 263,
    CODE = 264,
    DEFINE = 265,
    EXPECT = 266,
    LEFT = 267,
    LOCATIONS = 268,
    NONASSOC = 269,
    PRECEDENCE = 270,
    PURE_PARSER = 271,
    RIGHT = 272,
    SEMANTIC_PARSER = 273,
    START = 274,
    THONG = 275,
    TOKEN = 276,
    TYPE = 277,
    UNION = 278,
    PREC = 279,
    END_TERMINALS = 280,
    _SECTIONS = 281,
    _DEFSECTION_OPT = 282,
    _LITERALBLOCK = 283,
    _DECLARATION = 284,
    _TAG_OPT = 285,
    _NAMENOLIST = 286,
    _NAMENO = 287,
    _NAMELIST = 288,
    _RULESECTION = 289,
    _RULE = 290,
    _LHS = 291,
    _RHS = 292,
    _PREC = 293,
    _ACTION = 294,
    _USERSECTION_OPT = 295
  };
#endif
/* Tokens.  */
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

#line 141 "parser.hh"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_HH_INCLUDED  */
