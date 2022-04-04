//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeLocationTools.hpp -- Additional tools for code locations.
//
// Author           : Andrew Beach
// Created On       : Fri Dec  4 15:35:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Mon Mar 14 15:14:00 2022
// Update Count     : 2
//

#pragma once

struct CodeLocation;
namespace ast {
	class Node;
	class TranslationUnit;
}

// Search the translation unit for unset code locations and print information
// on them, and where the check was run if provided. Abort if any unset code
// locations are found.
void checkAllCodeLocations( ast::TranslationUnit const & unit );
void checkAllCodeLocations( char const *, ast::TranslationUnit const & );

// Assign a nearby code-location to any unset code locations in the forest.
void forceFillCodeLocations( ast::TranslationUnit & unit );

// Fill in code-locations with a parent code location,
// using the provided CodeLocation as the base.
ast::Node const *
	localFillCodeLocations( CodeLocation const &, ast::Node const * );
