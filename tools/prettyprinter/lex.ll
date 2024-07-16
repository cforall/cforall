/* 
 * Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
 *
 * The contents of this file are covered under the licence agreement in the
 * file "LICENCE" distributed with Cforall.
 * 
 * lex.ll --
 * 
 * Author           : Peter A. Buhr
 * Created On       : Sat Dec 15 11:45:59 2001
 * Last Modified By : Peter A. Buhr
 * Last Modified On : Thu May 31 08:49:58 2018
 * Update Count     : 274
 */

%option stack
%option yylineno
%option nounput

%{
#include <list>
#include <string>
#include <iostream>
using namespace std;
#include "ParserTypes.h" 
#include "parser.hh" 

#define RETURN_TOKEN( kind ) yylval.tokenp = new Token( yytext, ws_list, kind ); return kind;

list<string> ws_list;
string comment_str;
string code_str;

// Stop warning due to incorrectly generated flex code.
#pragma GCC diagnostic ignored "-Wsign-compare"
%}

integer [0-9]+
identifier [a-zA-Z_$][0-9a-zA-Z_$]*

simple_escape ['"?\\]
escape_sequence [\\]{simple_escape}
c_char [^'\\\n]|{escape_sequence}
s_char [^"\\\n]|{escape_sequence}

%x C_COMMENT STR C_CODE

/* ---------------------------- Token Section ---------------------------- */
%%
<INITIAL,C_CODE>"/*" {									// C style comments */
#if defined(DEBUG_ALL) | defined(DEBUG_COMMENT)
		cerr << "\"/*\" : " << yytext << endl;
#endif
		if ( YYSTATE == C_CODE ) code_str += yytext;
		else comment_str += yytext;
		yy_push_state(C_COMMENT);
}
<C_COMMENT>(.|"\n")	{									// C style comments
#if defined(DEBUG_ALL) | defined(DEBUG_COMMENT)
		cerr << "<C_COMMENT>(.|\\n) : " << yytext << endl;
#endif
		if ( yy_top_state() == C_CODE ) code_str += yytext;
		else comment_str += yytext;
}
<C_COMMENT>"*/"	{										// C style comments
#if defined(DEBUG_ALL) | defined(DEBUG_COMMENT)
	cerr << "<C_COMMENT>\"*/\" : " << yytext << endl;
#endif
	if ( yy_top_state() == C_CODE ) code_str += yytext;
	else {
		comment_str += yytext;
		//cerr << "C COMMENT : " << endl << comment_str << endl;
		ws_list.push_back( comment_str );
		comment_str = "";
	}
	yy_pop_state();
}

<INITIAL,C_CODE>"//"[^\n]* {							// C++ style comments
#if defined(DEBUG_ALL) | defined(DEBUG_COMMENT)
	cerr << "\"//\"[^\\n]*\"\n\" : " << yytext << endl;
#endif
	if ( YYSTATE == C_CODE ) code_str += yytext;
	else {
		comment_str += yytext;
		//cerr << "C++ COMMENT : " << endl << comment_str << endl;
		ws_list.push_back( comment_str );
		comment_str = "";
	}
}

";"				{ RETURN_TOKEN( ';' ) }
":"				{ RETURN_TOKEN( ':' ) }
"|"				{ RETURN_TOKEN( '|' ) }
","				{ RETURN_TOKEN( ',' ) }
"<"				{ RETURN_TOKEN( '<' ) }
">"				{ RETURN_TOKEN( '>' ) }

[[:space:]]+ {											// [ \t\n]+
	ws_list.push_back( yytext );
	//cerr << "WS : " << "\"" << yytext << "\"" << endl;
}

<INITIAL>"{"	{ RETURN_TOKEN( '{' ) }
<INITIAL>"}"	{ RETURN_TOKEN( '}' ) }
<C_CODE>"{"	{
#if defined(DEBUG_ALL) | defined(DEBUG_C)
	cerr << "<C_CODE>. : " << yytext << endl;
#endif
	code_str += yytext;
	RETURN_TOKEN( '{' )
}
<C_CODE>"}"	{
#if defined(DEBUG_ALL) | defined(DEBUG_C)
	cerr << "<C_CODE>. : " << yytext << endl;
#endif
	code_str += yytext;
	RETURN_TOKEN( '}' )
}

"%%"			{ RETURN_TOKEN( MARK ) }
"%{"			{ RETURN_TOKEN( LCURL ) }
<C_CODE>"%}"	{ RETURN_TOKEN( RCURL ) }

^"%define"[^\n]*"\n" { RETURN_TOKEN( DEFINE ) }
^"%expect"		{ RETURN_TOKEN( EXPECT ) }
^"%left"		{ RETURN_TOKEN( LEFT ) }
^"%locations"	{ RETURN_TOKEN( LOCATIONS ) }
^"%nonassoc"	{ RETURN_TOKEN( NONASSOC ) }
^"%precedence"	{ RETURN_TOKEN( PRECEDENCE ) }
^"%pure_parser" { RETURN_TOKEN( PURE_PARSER ) }
^"%right"		{ RETURN_TOKEN( RIGHT ) }
^"%semantic_parser"	{ RETURN_TOKEN( SEMANTIC_PARSER ) }
^"%start"		{ RETURN_TOKEN( START ) }
^"%thong" 		{ RETURN_TOKEN( THONG ) }
^"%token"		{ RETURN_TOKEN( TOKEN ) }
^"%type"		{ RETURN_TOKEN( TYPE ) }
^"%union"		{ RETURN_TOKEN( UNION ) }

"%prec" 		{ RETURN_TOKEN( PREC ) }

{integer}		{ RETURN_TOKEN( INTEGER ); }
[']{c_char}[']	{ RETURN_TOKEN( CHARACTER ); }
{identifier}	{ RETURN_TOKEN( IDENTIFIER ); }

<C_CODE>["]{s_char}*["]	{								// hide braces "{}" in strings
#if defined(DEBUG_ALL) | defined(DEBUG_C)
	cerr << "<C_CODE>. : " << yytext << endl;
#endif
	code_str += yytext;
}

<C_CODE>(.|\n) {										// must be last rule of C_CODE
#if defined(DEBUG_ALL) | defined(DEBUG_C)
	cerr << "<C_CODE>. : " << yytext << endl;
#endif
	code_str += yytext;
}

				/* unknown characters */
.				{ printf("unknown character(s):\"%s\" on line %d\n", yytext, yylineno); }
%%
void lexC(void) {
	BEGIN(C_CODE);
}

string lexYacc(void) {
	BEGIN(INITIAL);
	//cerr << "CODE: " << endl << code_str << endl;
	string temp( code_str );
	code_str = "";
	return temp;
}

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
