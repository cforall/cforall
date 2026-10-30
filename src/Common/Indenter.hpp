//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Indenter.hpp --
//
// Author           : Rob Schluntz
// Created On       : Fri Jun 30 16:55:23 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri May 13 14:10:00 2022
// Update Count     : 2
//

#pragma once

#include <ostream>

/// An indentation amount (count of steps), with a printing operator that applies a fixed number of spaces-per-step.
//  See also, "A note about Indenter, passed by reference and by value," in Print.hpp.
struct Indenter {
	static unsigned tabsize;        ///< default number of spaces in one level of indentation

	unsigned int level;             ///< current level of indentation
	const unsigned int spaces_per;  ///< spaces in one level of indentation

	Indenter( unsigned int level = 0, unsigned int spaces_per = tabsize )
	: level( level ), spaces_per( spaces_per ) {}

	Indenter & operator+=(int nlevels) { level += nlevels; return *this; }
	Indenter & operator-=(int nlevels) { level -= nlevels; return *this; }
	Indenter operator+(int nlevels) { Indenter indenter = *this; return indenter += nlevels; }
	Indenter operator-(int nlevels) { Indenter indenter = *this; return indenter -= nlevels; }
	Indenter & operator++() { return *this += 1; }
	Indenter & operator--() { return *this -= 1; }
};

inline std::ostream & operator<<( std::ostream & out, const Indenter & indent ) {
	return out << std::string(indent.level * indent.spaces_per, ' ');
}
