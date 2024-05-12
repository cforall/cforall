//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// TypedefTable.hpp --
//
// Author           : Peter A. Buhr
// Created On       : Sat May 16 15:24:36 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Jul 12 06:09:37 2023
// Update Count     : 118
//

#pragma once

#include <string>										// for string

#include "Common/ScopedMap.hpp"							// for ScopedMap

class TypedefTable {
	struct Note {
		size_t level;
		bool forall;
	};
	typedef ScopedMap< std::string, int, Note > KindTable;
	KindTable kindTable;
	unsigned int level = 0;
  public:
	~TypedefTable();

	bool exists( const std::string & identifier ) const;
	bool existsCurr( const std::string & identifier ) const;
	int isKind( const std::string & identifier ) const;
	void makeTypedef( const std::string & name, int kind, const char * );
	void makeTypedef( const std::string & name, const char * );
	void addToScope( const std::string & identifier, int kind, const char * );
	void addToEnclosingScope( const std::string & identifier, int kind, const char * );
	bool getEnclForall() { return kindTable.getNote( kindTable.currentScope() -  1 ).forall; }

	void enterScope();
	void leaveScope();

	void up( bool );
	void down();

	void print( void ) const;
}; // TypedefTable

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
