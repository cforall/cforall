// 
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// token.h -- 
// 
// Author           : Peter A. Buhr
// Created On       : Wed Jun 28 22:47:58 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 10:12:42 2017
// Update Count     : 6
// 

#pragma once

struct Token {
	std::string text;									// text of terminal or non-terminal token
	int kind;											// kind of terminal or non-terminal token
	std::list<std::string> ws_list;						// list of whitespace and comments before terminal token
	Token *left, *down;									// binary parse tree links

	Token( const std::string &, int );
	Token( const std::string &, std::list<std::string> &, int );
	void addLeftTail( Token * );
	void addDownLeftTail( Token * );
	bool isTerminal();
	int getKind() const;
	std::string getText() const;
	std::string getWS();
	bool isComment();
	std::string getComment();
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
