//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Indenter.h --
//
// Author           : Rob Schluntz
// Created On       : Fri Jun 30 16:55:23 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Aug 11 11:15:00 2017
// Update Count     : 1
//

#ifndef INDENTER_H
#define INDENTER_H

struct Indenter {
	static unsigned tabsize;  ///< default number of spaces in one level of indentation

	unsigned int indent;      ///< number of spaces to indent
	unsigned int amt;         ///< spaces in one level of indentation

	Indenter( unsigned int indent = 0, unsigned int amt = tabsize )
	: indent( indent ), amt( amt ) {}

	Indenter & operator+=(int nlevels) { indent += nlevels; return *this; }
	Indenter & operator-=(int nlevels) { indent -= nlevels; return *this; }
	Indenter operator+(int nlevels) { Indenter indenter = *this; return indenter += nlevels; }
	Indenter operator-(int nlevels) { Indenter indenter = *this; return indenter -= nlevels; }
	Indenter & operator++() { return *this += 1; }
	Indenter & operator--() { return *this -= 1; }
};

inline std::ostream & operator<<( std::ostream & out, const Indenter & indent ) {
	return out << std::string(indent.indent * indent.amt, ' ');
}

#endif // INDENTER_H
