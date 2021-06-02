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
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 08:13:34 2020
// Update Count     : 26
//

#pragma once

#include <string>
#include <map>

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
		std::string inputName;
		std::string symbol;
		std::string outputName;
		std::string friendlyName;
		OperatorType type;
	};

	class CodeGen {
		friend const OperatorInfo * operatorLookup( const std::string & funcName );

		static const OperatorInfo tableValues[];
		static std::map< std::string, OperatorInfo > table;
	  public:
		CodeGen();
	}; // CodeGen

	bool isOperator( const std::string & funcName );
	const OperatorInfo * operatorLookup( const std::string & funcName );
	std::string operatorFriendlyName( const std::string & funcName );

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
