//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypedefTable.cc --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 15:20:13 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb 15 08:27:24 2022
// Update Count     : 275
//


#include "TypedefTable.h"
#include <cassert>										// for assert
#include <iostream>
using namespace std;

#if 0
#define debugPrint( code ) code
#else
#define debugPrint( code )
#endif

using namespace std;									// string, iostream

debugPrint(
	static const char *kindName( int kind ) {
		switch ( kind ) {
		case IDENTIFIER: return "identifier";
		case TYPEDIMname: return "typedim";
		case TYPEDEFname: return "typedef";
		case TYPEGENname: return "typegen";
		default:
			cerr << "Error: cfa-cpp internal error, invalid kind of identifier" << endl;
			abort();
		} // switch
	} // kindName
);

TypedefTable::~TypedefTable() {
	if ( ! SemanticErrorThrow && kindTable.currentScope() != 0 ) {
		cerr << "Error: cfa-cpp internal error, scope failure " << kindTable.currentScope() << endl;
		abort();
	} // if
} // TypedefTable::~TypedefTable

bool TypedefTable::exists( const string & identifier ) const {
	return kindTable.find( identifier ) != kindTable.end();
} // TypedefTable::exists

bool TypedefTable::existsCurr( const string & identifier ) const {
	return kindTable.findAt( kindTable.currentScope() - 1, identifier ) != kindTable.end();
} // TypedefTable::exists

int TypedefTable::isKind( const string & identifier ) const {
	KindTable::const_iterator posn = kindTable.find( identifier );
	// Name lookup defaults to identifier, and then the identifier's kind is set by the parser.
	if ( posn == kindTable.end() ) return IDENTIFIER;
	return posn->second;
} // TypedefTable::isKind

// SKULLDUGGERY: Generate a typedef for the aggregate name so the aggregate does not have to be qualified by
// "struct". Only generate the typedef, if the name is not in use. The typedef is implicitly (silently) removed if the
// name is explicitly used.
void TypedefTable::makeTypedef( const string & name, int kind ) {
//    Check for existence is necessary to handle:
//        struct Fred {};
//        void Fred();
//        void fred() {
//           struct Fred act; // do not add as type in this scope
//           Fred();
//        }
	if ( ! typedefTable.exists( name ) ) {
		typedefTable.addToEnclosingScope( name, kind, "MTD" );
	} // if
} // TypedefTable::makeTypedef

void TypedefTable::addToScope( const string & identifier, int kind, const char * locn __attribute__((unused)) ) {
	KindTable::size_type scope = kindTable.currentScope();
	debugPrint( cerr << "Adding current at " << locn << " " << identifier << " as " << kindName( kind ) << " scope " << scope << endl );
	kindTable.insertAt( scope, identifier, kind );
} // TypedefTable::addToScope

void TypedefTable::addToEnclosingScope( const string & identifier, int kind, const char * locn __attribute__((unused)) ) {
	KindTable::size_type scope = kindTable.currentScope() - 1 - kindTable.getNote( kindTable.currentScope() - 1 ).level;
//	size_type scope = level - kindTable.getNote( kindTable.currentScope() - 1 ).level;
	debugPrint( cerr << "Adding enclosing at " << locn << " " << identifier << " as " << kindName( kind ) << " scope " << scope << " level " << level << " note " << kindTable.getNote( kindTable.currentScope() - 1 ).level << endl );
	pair< KindTable::iterator, bool > ret = kindTable.insertAt( scope, identifier, kind );
	if ( ! ret.second ) ret.first->second = kind;		// exists => update
} // TypedefTable::addToEnclosingScope

void TypedefTable::enterScope() {
	kindTable.beginScope( (Note){ 0, false } );
	debugPrint( cerr << "Entering scope " << kindTable.currentScope() << " level " << level << endl; print() );
} // TypedefTable::enterScope

void TypedefTable::leaveScope() {
	debugPrint( cerr << "Leaving scope " << kindTable.currentScope() << endl; print() );
	kindTable.endScope();
} // TypedefTable::leaveScope

void TypedefTable::up( bool forall ) {
	level += 1;
	kindTable.getNote( kindTable.currentScope() ) = (Note){ level, forall || getEnclForall() };
	debugPrint( cerr << "Up " << " level " << level << " note " << kindTable.getNote( level ).level << ", " << kindTable.getNote( level ).forall << endl; );
} // TypedefTable::up

void TypedefTable::down() {
	level -= 1;
	debugPrint( cerr << "Down " << " level " << level << " note " << kindTable.getNote( level ).level << endl; );
} // TypedefTable::down

void TypedefTable::print( void ) const {
	KindTable::size_type scope = kindTable.currentScope();
	debugPrint( cerr << "[" << scope << "] " << kindTable.getNote( scope ).level << ", " << kindTable.getNote( scope ).forall << ":" );
	for ( KindTable::const_iterator i = kindTable.begin(); i != kindTable.end(); i++ ) {
		while ( i.get_level() != scope ) {
			--scope;
			debugPrint( cerr << endl << "[" << scope << "] " << kindTable.getNote( scope ).level << ", " << kindTable.getNote( scope ).forall << ":" );
		} // while
		debugPrint( cerr << " " << (*i).first << ":" << kindName( (*i).second ) );
	} // for
	while ( scope > 0 ) {
		--scope;
		debugPrint( cerr << endl << "[" << scope << "] " << kindTable.getNote( scope ).level << ", " << kindTable.getNote( scope ).forall << ":" );
	} // while
	debugPrint( cerr << endl );
} // TypedefTable::print

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
