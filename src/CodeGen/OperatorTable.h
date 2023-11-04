//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// OperatorTable.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov  3 14:53:00 2023
// Update Count     : 27
//

#pragma once

#include <string>

namespace CodeGen {

enum OperatorType {
	OT_CTOR,
	OT_DTOR,
	OT_CONSTRUCTOR = OT_DTOR,
	OT_PREFIXASSIGN,
	OT_POSTFIXASSIGN,
	OT_INFIXASSIGN,
	OT_ASSIGNMENT = OT_INFIXASSIGN,
	OT_CALL,
	OT_PREFIX,
	OT_INFIX,
	OT_POSTFIX,
	OT_INDEX,
	OT_LABELADDRESS,
	OT_CONSTANT
};

struct OperatorInfo {
	// The Cforall special function name.
	std::string inputName;
	// The string used when the operator is used as an operator.
	std::string symbol;
	// The base name used in the mangled name.
	std::string outputName;
	// Human-readable name of the operator.
	std::string friendlyName;
	// The type of operator shows how it is used as an operator.
	OperatorType type;
};

// Look up the operator (by inputName), return nullptr if no such operator.
const OperatorInfo * operatorLookup( const std::string & inputName );
// Is there an operator with this name?
bool isOperator( const std::string & inputName );
// Get the friendlyName of the operator with the inputName
std::string operatorFriendlyName( const std::string & inputName );
// Get the OperatorInfo with the given outputName, if one exists.
const OperatorInfo * operatorLookupByOutput( const std::string & outputName );

// Is the operator a constructor, destructor or any form of assignment.
// (Last two are "or" combinations of the first three.)
bool isConstructor( const std::string & );
bool isDestructor( const std::string & );
bool isAssignment( const std::string & );
bool isCtorDtor( const std::string & );
bool isCtorDtorAssign( const std::string & );

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
