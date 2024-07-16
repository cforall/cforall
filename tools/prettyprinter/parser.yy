//  
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//  
// parser.yy --
//  
// Author           : Rodolfo G. Esteves
// Created On       : Sat Dec 15 13:44:21 2001
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Jan 26 22:50:03 2021
// Update Count     : 1053
// 

%{
#define YYDEBUG_LEXER_TEXT( yylval )					// lexer loads this up each time
#define YYDEBUG 1										// get the pretty debugging code to compile
#define YYERROR_VERBOSE									// more information in syntax errors

#include <iostream>
using namespace std;
#include "ParserTypes.h"
#include "filter.h"

extern list<string> ws_list;							// lex variable containing accumulated whitespace
void lexC( void );
string lexYacc( void );

void yyerror( string s ) {
	extern int yylineno;

	cerr << "Error in line: " << yylineno << ": " << s << endl;
	return;
}

Token *declstart;
Token *rulestart;
Token *nameliststart;
%}

%union {
	Token *tokenp;
}

%token<tokenp>	','
%token<tokenp>	'<'
%token<tokenp>	'>'
%token<tokenp>	'{'
%token<tokenp>	'}'
%token<tokenp>	':'
%token<tokenp>	';'
%token<tokenp>	'|'

%token<tokenp>	MARK									// %%
%token<tokenp>	LCURL									// %{
%token<tokenp>	RCURL									// %}

%token<tokenp>	INTEGER									// integer constant
%token<tokenp>	CHARACTER								// character constant
%token<tokenp>	IDENTIFIER								// identifier
%token<tokenp>	CODE									// C code

%token<tokenp>	DEFINE									// %define
%token<tokenp>	EXPECT									// %expect
%token<tokenp>	LEFT									// %left
%token<tokenp>	LOCATIONS								// %locations
%token<tokenp>	NONASSOC								// %nonassoc
%token<tokenp>	PRECEDENCE								// %precedence
%token<tokenp>	PURE_PARSER								// %pure_parser
%token<tokenp>	RIGHT									// %right
%token<tokenp>	SEMANTIC_PARSER							// %semantic_parser
%token<tokenp>	START									// %start
%token<tokenp>	THONG									// %thong
%token<tokenp>	TOKEN									// %token
%token<tokenp>	TYPE									// %type
%token<tokenp>	UNION									// %union

%token<tokenp>	PREC									// %prec

%token			END_TERMINALS							// ALL TERMINAL TOKEN NAMES MUST APPEAR BEFORE THIS

%type<tokenp>	sections
%token			_SECTIONS
%type<tokenp>	mark
%type<tokenp>	defsection_opt
%token			_DEFSECTION_OPT
%type<tokenp>	declarations
%type<tokenp>	literalblock
%token			_LITERALBLOCK
%type<tokenp>	declaration
%token			_DECLARATION
%type<tokenp>	union
%type<tokenp>	rword
%type<tokenp>	tag_opt
%token			_TAG_OPT
%type<tokenp>	namenolist
%token			_NAMENOLIST
%type<tokenp>	nameno
%token			_NAMENO
%type<tokenp>	namelist
%token			_NAMELIST
%type<tokenp>	name
%type<tokenp>	rulesection
%token			_RULESECTION
%type<tokenp>	rules
%token			_RULE
%type<tokenp>	lhs
%token			_LHS
%type<tokenp>	rhs
%token			_RHS
%type<tokenp>	prod
%type<tokenp>	prec
%token			_PREC
%type<tokenp>	action
%token			_ACTION
%type<tokenp>	usersection_opt
%token			_USERSECTION_OPT
%type<tokenp>	ccode_opt
%type<tokenp>	blocks

%start grammar

%%
grammar :
	sections
		{
			filter( $1 );								// filter parse tree
			freeTree( $1 );								// free parse-tree storage (optional: used with purify)
		}
	;

sections :
	defsection_opt mark rulesection usersection_opt
		{
			$$ = new Token( "sections", _SECTIONS );
			$1->left = $2;
			$2->left = $3;
			$3->left = $4;
			$$->down = $1;
		}
	;

mark :
	MARK
	| error												// missing %%
		{
			cerr << "no input grammar, missing %% mark" << endl;
			exit( -1 );
		}
	;

defsection_opt :
	// empty
		{
			//cerr << "defsection_opt1: " << endl;
			$$ = new Token( "declaration_opt", _DEFSECTION_OPT );
		}
	| declarations
		{
			//cerr << "defsection_opt2: " << $1->text << "(" << $1 << ")" << endl;
			$$ = new Token( "declaration_opt", _DEFSECTION_OPT );
			$$->down = declstart;
		}
	;

declarations :
	literalblock
		{
			//cerr << "declarations1: " << $1->text << "(" << $1 << ")" << endl;
			$$ = declstart = $1;
		}
	| declaration
		{
			//cerr << "declarations2: " << $1->text << "(" << $1 << ")" << endl;
			$$ = declstart = new Token( "declaration", _DECLARATION );
			$$->down = $1;
		}
	| declarations literalblock
		{
			//cerr << "declarations3: "<< $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
			$1->left = $2;
			$$ = $2;
		}
	| declarations declaration
		{
			//cerr << "declarations4: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
			$$ = new Token( "declaration", _DECLARATION );
			$1->left = $$;
			$$->down = $2;
		}
	;

literalblock :
	LCURL
		{ lexC(); }
	  ccode_opt
		{ $<tokenp>$ = new Token( lexYacc(), CODE ); }
	  RCURL
		{
			//cerr << "literalblock: " << $1->text << "(" << $1 << ") " << $<tokenp>4->text << " " << $5->text << "(" << $5 << ")" << endl;
			$1->left = $<tokenp>4;
			$<tokenp>4->left = $5;
			$$ = new Token( "literalblock", _LITERALBLOCK );
			$$->down = $1;
		}
	;

declaration :
	union
	| START IDENTIFIER
		{
			$1->left = $2;
			$$ = $1;
		}
	| rword tag_opt namenolist
		{
		    Token *n = new Token( "namenolist", _NAMENOLIST );
		    n->down = nameliststart;
		    $1->left = $2;
		    $2->left = n;
		    $$ = $1;
		}
	| TYPE tag_opt namelist
		{
		    Token *n = new Token( "namelist", _NAMELIST );
		    n->down = nameliststart;
		    $1->left = $2;
		    $2->left = n;
		    $$ = $1;
		}
	| PURE_PARSER
	| SEMANTIC_PARSER
	| EXPECT INTEGER									// bison
		{
		    $1->left = $2;
		    $$ = $1;
		}
	| DEFINE											// bison
	| LOCATIONS
	| THONG												// bison
	;

union :
	UNION '{'
		{ lexC(); }
	  ccode_opt
		{
		    // Remove the trailing '}' which is added in lex.
		    string temp( lexYacc() );
		    $<tokenp>$ = new Token( temp.substr( 0, temp.length() - 1 ), CODE );
		}
	  '}'
		{
		    $1->left = $2;
		    $2->left = $<tokenp>5;
		    $<tokenp>5->left = $6;
		    $$ = $1;
		}
	;

rword :
	TOKEN
	| LEFT
	| RIGHT
	| NONASSOC
	| PRECEDENCE
	;

tag_opt :
	// empty
		{
		    //cerr << "tag_opt" << endl;
		    $$ = new Token( "tag_opt", _TAG_OPT );
		}
	| '<' IDENTIFIER '>'
		{
		    $1->left = $2;
		    $2->left = $3;
		    $$ = new Token( "tag_opt", _TAG_OPT );
		    $$->down = $1;
		}
	;

namenolist :
	nameno
		{
			//cerr << "namenolist1: " << $1->text << "(" << $1 << ")" << endl;
			$$ = nameliststart = $1;
		}
	| namenolist nameno
		{
		    //cerr << "namenolist2: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $1->left = $2;
		    $$ = $2;
		}
	| namenolist ',' nameno
		{
		    //cerr << "namenolist3: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ") " << $3->text << "(" << $3 << ")" << endl;
		    $1->left = $2;
		    $2->left = $3;
		    $$ = $3;
		}
	;

nameno :
	name
		{
		    $$ = new Token( "nameno", _NAMENO );
		    $$->down = $1;
		}
	| name INTEGER
		{
		    $$ = new Token( "nameno", _NAMENO );
		    $1->left = $2;
		    $$->down = $1;
		}
	;

namelist :
	name
		{
		    //cerr << "namelist1: " << $1->text << "(" << $1 << ")" << endl;
		    $$ = nameliststart = $1;
		}
	| namelist name
		{
		    //cerr << "namelist2: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $1->left = $2;
		    $$ = $2;
		}
	| namelist ',' name
		{
		    //cerr << "namelist3: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ") " << $3->text << "(" << $3 << ")" << endl;
		    $1->left = $2;
		    $2->left = $3;
		    $$ = $3;
		}
	;

name :
	IDENTIFIER
	| CHARACTER
	;

rulesection :
	rules
		{
		    //cerr << "rulesection1: " << $1->text << "(" << $1 << ")" << endl;
		    $$ = new Token( "rulesection", _RULESECTION );
		    $$->down = $1;
		}
	| error												// no rules
		{
			cerr << "no rules in the input grammar" << endl;
			exit( -1 );
		}
	;

// These grammar rules are complex because the Yacc language is LR(2) due to the optional ';' at the end of rules. The
// following rules convert the LR(2) grammar into LR(1) by lengthening the rules to allow sufficient look
// ahead. Unfortunately, this change makes handling the semantic actions more complex because there are two lists
// (rules, rhs) being built but only one list tail can be returned through $$ for chaining.

rules :
	lhs rhs
		{
		    //cerr << "rules1: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $$ = rulestart;
		}
	| lhs rhs ';'
		{
		    //cerr << "rules2: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ") " << $3->text << "(" << $3 << ")" << endl;
		    $2->addDownLeftTail( $3 );
		    $$ = rulestart;
		}
	;

lhs :
	IDENTIFIER ':'
		{
		    //cerr << "lhs: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $$ = new Token( "lhs", _LHS );
		    //cerr << " lhs: "  << $$->text << "(" << $$ << ")" << endl;
		    $1->left = $2;
		    $$->down = $1;
		}
	;

rhs	:
	// empty
		{
		    //cerr << "rhs1: " << $<tokenp>0->text << "(" << $<tokenp>0 << ")"  << endl;
		    rulestart = new Token( "rule", _RULE );
		    rulestart->down = $<tokenp>0; // initial lhs is already on the stack from "rules"
		    $$ = new Token( "rhs", _RHS );
		    //cerr << "  rhs: " << $$->text << "(" << $$ << ")" << endl;
		    $<tokenp>0->left = $$;
		}
	| rhs lhs
		{
		    //cerr << "rhs2: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    Token *temp = new Token( "rule", _RULE );
		    rulestart->addLeftTail( temp );
		    temp->down = $2;
		    $$ = new Token( "rhs", _RHS );
		    //cerr << "  rhs: "  << $$->text << "(" << $$ << ")" << endl;
		    $2->left = $$;
		}
	| rhs ';' lhs
		{
		    //cerr << "rhs3: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ") " << $3->text << "(" << $3 << ")" << endl;
		    $1->addDownLeftTail( $2 );
		    Token *temp = new Token( "rule", _RULE );
		    rulestart->addLeftTail( temp );
		    temp->down = $3;
		    $$ = new Token( "rhs", _RHS );
		    //cerr << "  rhs: "  << $$->text << "(" << $$ << ")" << endl;
		    $3->left = $$;
		}
	| rhs prod
		{
		    //cerr << "rhs4: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $1->addDownLeftTail( $2 );
		    $$ = $1;
		}
	| rhs '|'
		{
		    //cerr << "rhs5: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $1->addDownLeftTail( $2 );
		    $$ = new Token( "rhs", _RHS );
		    $1->left = $$;
		    //cerr << "  rhs: "  << $$->text << "(" << $$ << ")" << endl;
		}
	;

prod :
	action
	| IDENTIFIER
	| CHARACTER
	| prec
	;

prec :
	PREC name
		{
		    //cerr << "prec: " << $1->text << "(" << $1 << ") " << $2->text << "(" << $2 << ")" << endl;
		    $1->left = $2;
		    $$ = new Token( "prec", _PREC );
		    $$->down = $1;
		}
	;

action :
	'{'
		{ lexC(); }
	  ccode_opt
		{
		    // Remove the trailing '}' added in lex.
		    string temp( lexYacc() );
		    $<tokenp>$ = new Token( temp.substr( 0, temp.length() - 1 ), CODE );
		}
	  '}'
		{
		    $1->left = $<tokenp>4;
		    $<tokenp>4->left = $5;
		    $$ = new Token( "action", _ACTION );
		    $$->down = $1;
		}
	;

usersection_opt :
	// empty
		{
		    //cerr << "usersection_opt" << endl;
		    // attach remaining WS to fictitious code
		    Token *temp = new Token( "", ws_list, CODE );
		    $$ = new Token( "usersection_opt", _USERSECTION_OPT );
		    $$->down = temp;
		}
	| MARK
		{ lexC(); }
	  ccode_opt
		{
		    Token *temp = new Token( lexYacc(), CODE );
		    //cerr << "usersection_opt: " << $1->text << " " << temp->text << endl;
		    $1->left = temp;
		    $$ = new Token( "usersection_opt", _USERSECTION_OPT );
		    $$->down = $1;
		}
	;

ccode_opt :
	// empty
		{}
	| blocks
	;

// This rule matches internal braces "{}" in C code to the level of the braces of a union/action.  These internal braces
// are returned as Tokens from the lexer but are unused because the braces are already concatenated into the code string
// built by the lexer. Therefore, the tokens for the braces are immediately deleted.

blocks :
	'{'
		{ delete $1; }
	  ccode_opt '}'
		{ delete $4; }
	| blocks '{'
		{ delete $2; }
	  ccode_opt '}'
		{ delete $5; }
	;
%%

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
