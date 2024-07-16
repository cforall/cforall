// 
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// Pretty Printer, Copyright (C) Richard C. Bilson and Rodolfo G. Esteves 2001
// 
// token.cc -- 
// 
// Author           : Peter A. Buhr
// Created On       : Wed Jun 28 22:46:23 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 23:07:04 2017
// Update Count     : 10
// 

#include <string>
using namespace std;
#include "ParserTypes.h" 
#include "parser.hh"
#include "filter.h"

Token::Token( const string & text, int kind ) : text(text), kind(kind) {
	left = down = NULL;
} // Token::Token

Token::Token( const string & text, list<string> & ws_list, int kind ) : text(text), kind(kind) {
//    cerr << "Token3 : text \"" << text << "\"";
//    for ( list<string>::iterator i = ws_list.begin(); i != ws_list.end(); i ++ ) {
//	cerr << " WSt: \"" << *i << "\"";
//    }
//    cerr << endl;
	left = down = NULL;
	// O(1) transfer of the current lex whitespace list to the token's whitespace list, and clearing the lex whitespace
	// list.
	Token::ws_list.splice( Token::ws_list.end(), ws_list );
} // Token::Token

void Token::addLeftTail( Token * n ) {
	Token * p = this;
	while ( p->left != 0 ) {
		p = p->left;
	} // while
	p->left = n;
} // Token::addLeftTail

void Token::addDownLeftTail( Token * n ) {
	if ( down == 0 ) {
		down = n;
	} else {
		down->addLeftTail( n );
	} // if
} // Token::addDownLeftTail

bool Token::isTerminal() {
	return kind < END_TERMINALS;
} // Token::isTerminal

int Token::getKind() const {
	return kind;
} // Token::getKind()

string Token::getText() const {
	return text;
} // Token::getText()

string Token::getWS() {
	string ret;
	// concatenate whitespace and comment text
	for ( list<string>::iterator i = ws_list.begin(); i != ws_list.end(); i ++ ) {
		ret += *i;
	} // for
	return ret;
} // Token::getWS

bool Token::isComment() {
	return ws_list.size() > 1 || ( ws_list.size() == 1 && (*ws_list.begin())[0] == '/' );
} // Token::isComment

string Token::getComment() {
	string ret;
	// concatenate whitespace and comment text up to the last comment
	if ( ws_list.size() > 1 ) {
		list<string>::iterator end = -- ws_list.end();
		if ( (*end)[0] == '/' ) end ++;
		for ( list<string>::iterator i = ws_list.begin(); i != end; i ++ ) {
			ret += *i;
		} // for
	} else if ( ws_list.size() == 1 ) {
		if ( (*ws_list.begin())[0] == '/' ) ret = *ws_list.begin();
	} // if
	return ret;
} // Token::getComment

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
