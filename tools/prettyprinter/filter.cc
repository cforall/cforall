// 
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
// 
// filter.cc -- 
// 
// Author           : Peter A. Buhr
// Created On       : Tue Apr  9 22:33:44 2002
// Last Modified By : Peter A. Buhr
// Last Modified On : Thu Jun 29 08:56:46 2017
// Update Count     : 73
// 

#include <iostream>
#include <string>
#include <list>
using namespace std;
#include "filter.h"
#include "parser.hh" 


void (* filter)( Token * tree ) = 0;


void freeTree( Token * tree ) {							// postfix tree traversal
	if ( tree == NULL ) return;
	if ( tree->down != NULL ) freeTree( tree->down );
	if ( tree->left != NULL ) freeTree( tree->left );
	//cerr << "free token: \"" << tree->getText() << "\" : " << tree->getKind() << endl;
	delete tree;
} // freeTree


void Identity( Token * tree ) {							// prefix tree traversal
	if ( tree == NULL ) return;
	// print only the terminals
	if ( tree->isTerminal() ) cout << tree->getWS() << tree->getText();
	if ( tree->down != NULL ) Identity( tree->down );
	if ( tree->left != NULL ) Identity( tree->left );
} // Identity


static void Parse_Tree1( Token * tree, int indent ) {	// prefix tree traversal
	cout << string( indent, ' ' );
	if ( tree->isTerminal() ) {							// terminals
		cout << "\"" << tree->getText() << "\"";
	} else {											// non-terminals
		cout << tree->getText();
	} // if
	cout << " : " << tree->getKind()
		//<< " \"" << tree->getWS() << " \""
		 << endl;
	if ( tree->down != NULL ) Parse_Tree1( tree->down, indent + 2 );
	if ( tree->left != NULL ) Parse_Tree1( tree->left, indent );
} // Parse_Tree1

void Parse_Tree( Token * tree ) {
	if ( tree == NULL ) return;
	Parse_Tree1( tree, 0 );
} // Parse_Tree


void Nocode( Token * tree ) {							// prefix tree traversal
	static bool declprt = true;
	if ( tree == NULL ) return;

	if ( tree->isTerminal() ) {							// terminals
		cout << tree->getWS() << tree->getText();
	} else {											// non-terminals
		switch ( tree->getKind() ) {
		  case _RHS: {
			  int first = 0;							// first RHS after ':' or '|' treated specially
			  int push = 0, pop = 0;
			  for ( Token * i = tree->down; i != 0; i = i->left ) {
				  switch ( i->getKind() ) {
					case _ACTION:
					  cout << string( (push * 4 + pop * 3) / 7, '\t' );
					  push = pop = 0;
					  cout << i->down->getComment();	// ignore code but print its comment, if any
					  break;
					case _PREC:
					  Nocode( i->down );				// print verbatim
					  break;
					case '|':							// start of alternative and special case
					  first = 0;
					case ';':							// print with whitespace
					  cout << string( (push * 4 + pop * 3) / 7, '\t' );
					  push = pop = 0;
					  cout << i->getWS() << i->getText();
					  break;
					default:
					  if ( i->getKind() == IDENTIFIER ) {
						  if ( i->getText() == "push" ) {
							  push += 1;
							  if ( first == 0 ) {		   // first RHS after ':' or '|' ?
								  cout << i->getComment(); // ignore rhs but print its comment, if any
							  } // if 
							  break;
						  } else if ( i->getText() == "pop" ) {
							  pop += 1;
							  if ( first == 0 ) {		   // first RHS after ':' or '|' ?
								  cout << i->getComment(); // ignore rhs but print its comment, if any
							  } // if 
							  break;
						  } // if
					  } // if
					  // If there is a comment or this is the first RHS after ':' or '|', then include the whitespace
					  // before the token. Otherwise, fold the token onto the same line separated with a blank.
					  string t1( i->getText() );
					  if ( i->isComment() || first == 0 ) {
						  first = t1.length();
						  cout << i->getWS() << t1;
					  } else {
						  if ( first + t1.length() <= 100 ) { // check for long lines during folding
							  first += t1.length();
							  cout << " " << t1;
						  } else {
							  first = t1.length();
							  cout << endl << "\t\t\t\t" << t1;
						  } // if
					  } // if
				  } // switch
			  } // for
			  break;
		  }
		  case _LITERALBLOCK:							// ignore code but print its comment, if any
			cout << tree->down->getComment();
			break;
		  case _DECLARATION: {							// ignore certain declarations
			  int kind = tree->down->getKind();			// get kind of declaration
			  if ( kind != UNION && kind != TYPE ) {
				  declprt = true;
				  Nocode( tree->down );					// print verbatim
			  } else if ( declprt ) {					// ignore declaration but print its comment, if any
				  declprt = false;
				  cout << tree->down->getComment();
			  } // if
			  break;
		  }
		  case _USERSECTION_OPT:						// ignore but add newline at the end
			cout << endl;
			break;
		  default:
			if ( tree->down != NULL ) Nocode( tree->down );
		} // switch
	} // if
	if ( tree->left != NULL ) Nocode( tree->left );
} // Nocode


void LaTeX( Token * tree ) {							// prefix tree traversal
	if ( tree == NULL ) return;

	if ( tree->isTerminal() ) {							// terminals
		cout << tree->getWS() << tree->getText();
		if ( tree->getKind() == IDENTIFIER ) {
			string id( tree->getText() );
			cout << "\\(\\index{" << id << "@\\protect\\LGbegin\\protect\\lgrinde\\)" << id << "\\(\\protect\\endlgrinde\\protect\\LGend{}}\\)";
		} // if
	} else {											// non-terminals
		switch ( tree->getKind() ) {
		  case _RHS: {
			  int first = 0;							// first RHS after ':' or '|' treated specially
			  int push = 0, pop = 0;
			  for ( Token * i = tree->down; i != 0; i = i->left ) {
				  switch ( i->getKind() ) {
					case _ACTION:
					  cout << i->down->getComment();	// ignore code but print its comment, if any
					  break;
					case _PREC:
					  LaTeX( i->down );					// print verbatim
					  break;
					case '|':							// start of alternative and special case
					  first = 0;
					  push = pop = 0;
					case ';':							// print with whitespace
					  cout << i->getWS() << i->getText();
					  break;
					default:
					  if ( i->getKind() == IDENTIFIER ) {
						  if ( i->getText() == "push" ) {
							  push += 1;
							  break;
						  } else if ( i->getText() == "pop" ) {
							  pop += 1;
							  break;
						  } // if
					  } // if
					  // If there is a comment or this is the first RHS after ':' or '|', then include the whitespace
					  // before the token. Otherwise, fold the token onto the same line separated with a blank.
					  string t1( i->getText() );
					  if ( i->isComment() || first == 0 ) {
						  first = t1.length();
						  cout << i->getWS() << t1;
						  if ( i->getKind() == IDENTIFIER ) {
							  string id( tree->getText() );
							  cout << "\\(\\index{" << id << "@\"\\verb=" << id << "=}\\)";
						  } // if
					  } else {
						  if ( first + t1.length() <= 100 ) { // check for long lines during folding
							  first += t1.length();
							  cout << " " << t1;
						  } else {
							  first = t1.length();
							  cout << endl << "\t\t\t\t" << t1;
						  } // if
					  } // if
				  } // switch
			  } // for
			  break;
		  }
		  case _LITERALBLOCK:							// ignore code but print its comment, if any
			cout << tree->down->getComment();
			break;
		  case _DECLARATION: {							// ignore certain declarations
			  int kind = tree->down->getKind();			// get kind of declaration
			  if ( kind != UNION && kind != TYPE ) {
				  LaTeX( tree->down );					// print verbatim
			  } // if
			  break;
		  }
		  case _USERSECTION_OPT:						// ignore but add newline at the end
			cout << endl;
			break;
		  default:
			if ( tree->down != NULL ) LaTeX( tree->down );
		} // switch
	} // if
	if ( tree->left != NULL ) LaTeX( tree->left );
} // LaTeX


void HTML( Token * tree ) {								// prefix tree traversal
	cerr << "ERROR: html style not implemented" << endl;
} // HTML

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
