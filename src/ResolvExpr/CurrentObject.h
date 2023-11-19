//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CurrentObject.h --
//
// Author           : Rob Schluntz
// Created On       : Thu Jun  8 11:07:25 2017
// Last Modified By : Andrew Beach
// Last Modified On : Thu Apr  6 16:14:00 2023
// Update Count     : 4
//

#pragma once

#include <deque>
#include <memory> // for unique_ptr
#include <vector>

#include "AST/Node.hpp"  // for ptr
#include "Common/CodeLocation.h"

namespace ast {

// AST class types:
class Designation;
class Type;
struct InitAlternative;

/// Iterates members of a type by initializer
class MemberIterator;

/// Builds initializer lists in resolution
class CurrentObject final {
	std::vector<std::shared_ptr<MemberIterator>> objStack;

public:
	CurrentObject() = default;
	CurrentObject( const CodeLocation & loc, const Type * type );

	/// Resolves unresolved designation.
	const Designation * findNext( const Designation * designation );
	/// Sets current position using the resolved designation.
	void setNext( const Designation * designation );
	/// Steps to next sub-object of current object.
	void increment();
	/// Sets new current object for the duration of this brace-enclosed intializer-list.
	void enterListInit( const CodeLocation & loc );
	/// Restores previous current object.
	void exitListInit();
	/// Produces a list of alternatives (Type *, Designation *)
	/// for the current sub-object's initializer.
	std::deque< InitAlternative > getOptions();
	/// Produces the type of the current object but no subobjects.
	const Type * getCurrentType();
};

} // namespace ast

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //

