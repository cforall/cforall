//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// OperatorTable.cpp --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Nov  3 16:00:00 2023
// Update Count     : 56
//

#include "OperatorTable.hpp"

#include <cassert>         // for assert
#include <unordered_map>   // for unordered_map

namespace CodeGen {

static const OperatorInfo tableValues[] = {
	//  inputName symbol  outputName                     friendlyName                  type
	{	"?[?]",   "",     "_operator_index",             "Index",                      OT_INDEX          },
	{	"?{}",    "=",    "_constructor",                "Constructor",                OT_CTOR           },
	{	"^?{}",   "",     "_destructor",                 "Destructor",                 OT_DTOR           },
	{	"?()",    "",     "_operator_call",              "Call Operator",              OT_CALL           },
	{	"?++",    "++",   "_operator_postincr",          "Postfix Increment",          OT_POSTFIXASSIGN  },
	{	"?--",    "--",   "_operator_postdecr",          "Postfix Decrement",          OT_POSTFIXASSIGN  },
	{	"*?",     "*",    "_operator_deref",             "Dereference",                OT_PREFIX         },
	{	"+?",     "+",    "_operator_unaryplus",         "Plus",                       OT_PREFIX         },
	{	"-?",     "-",    "_operator_unaryminus",        "Minus",                      OT_PREFIX         },
	{	"~?",     "~",    "_operator_bitnot",            "Bitwise Not",                OT_PREFIX         },
	{	"!?",     "!",    "_operator_lognot",            "Logical Not",                OT_PREFIX         },
	{	"++?",    "++",   "_operator_preincr",           "Prefix Increment",           OT_PREFIXASSIGN   },
	{	"--?",    "--",   "_operator_predecr",           "Prefix Decrement",           OT_PREFIXASSIGN   },
	{	"?\\?",   "\\",   "_operator_exponential",       "Exponentiation",             OT_INFIX          },
	{	"?*?",    "*",    "_operator_multiply",          "Multiplication",             OT_INFIX          },
	{	"?/?",    "/",    "_operator_divide",            "Division",                   OT_INFIX          },
	{	"?%?",    "%",    "_operator_modulus",           "Modulo",                     OT_INFIX          },
	{	"?+?",    "+",    "_operator_add",               "Addition",                   OT_INFIX          },
	{	"?-?",    "-",    "_operator_subtract",          "Substraction",               OT_INFIX          },
	{	"?<<?",   "<<",   "_operator_shiftleft",         "Shift Left",                 OT_INFIX          },
	{	"?>>?",   ">>",   "_operator_shiftright",        "Shift Right",                OT_INFIX          },
	{	"?<?",    "<",    "_operator_less",              "Less-than",                  OT_INFIX          },
	{	"?>?",    ">",    "_operator_greater",           "Greater-than",               OT_INFIX          },
	{	"?<=?",   "<=",   "_operator_lessequal",         "Less-than-or-Equal",         OT_INFIX          },
	{	"?>=?",   ">=",   "_operator_greaterequal",      "Greater-than-or-Equal",      OT_INFIX          },
	{	"?==?",   "==",   "_operator_equal",             "Equality",                   OT_INFIX          },
	{	"?!=?",   "!=",   "_operator_notequal",          "Not-Equal",                  OT_INFIX          },
	{	"?&?",    "&",    "_operator_bitand",            "Bitwise And",                OT_INFIX          },
	{	"?^?",    "^",    "_operator_bitxor",            "Bitwise Xor",                OT_INFIX          },
	{	"?|?",    "|",    "_operator_bitor",             "Bitwise Or",                 OT_INFIX          },
	{	"?=?",    "=",    "_operator_assign",            "Assignment",                 OT_INFIXASSIGN    },
	{	"?\\=?",  "\\=",  "_operator_expassign",         "Exponentiation Assignment",  OT_INFIXASSIGN    },
	{	"?*=?",   "*=",   "_operator_multassign",        "Multiplication Assignment",  OT_INFIXASSIGN    },
	{	"?/=?",   "/=",   "_operator_divassign",         "Division Assignment",        OT_INFIXASSIGN    },
	{	"?%=?",   "%=",   "_operator_modassign",         "Modulo Assignment",          OT_INFIXASSIGN    },
	{	"?+=?",   "+=",   "_operator_addassign",         "Addition Assignment",        OT_INFIXASSIGN    },
	{	"?-=?",   "-=",   "_operator_subassign",         "Substrction Assignment",     OT_INFIXASSIGN    },
	{	"?<<=?",  "<<=",  "_operator_shiftleftassign",   "Shift Left Assignment",      OT_INFIXASSIGN    },
	{	"?>>=?",  ">>=",  "_operator_shiftrightassign",  "Shift Right Assignment",     OT_INFIXASSIGN    },
	{	"?&=?",   "&=",   "_operator_bitandassign",      "Bitwise And Assignment",     OT_INFIXASSIGN    },
	{	"?^=?",   "^=",   "_operator_bitxorassign",      "Bitwise Xor Assignment",     OT_INFIXASSIGN    },
	{	"?|=?",   "|=",   "_operator_bitorassign",       "Bitwise Or Assignment",      OT_INFIXASSIGN    },
}; // tableValues

enum { numOps = sizeof( tableValues ) / sizeof( OperatorInfo ) };

const OperatorInfo * operatorLookup( const std::string & inputName ) {
	// Static information set up:
	static std::unordered_map<std::string, const OperatorInfo *> inputTable;
	if ( inputTable.empty() ) for ( const OperatorInfo & op : tableValues ) {
		inputTable[ op.inputName ] = &op;
	}

	// Filter out non-operator names:
	if ( inputName.find_first_of( "?^*+-!", 0, 1 ) == std::string::npos ) return nullptr;
	auto entry = inputTable.find( inputName );
	// This can only happen if an invalid identifier name has been used.
	assertf( entry != inputTable.end(), "Not a real operator: %s\n", inputName.c_str() );
	const OperatorInfo * ret = entry->second;
	assert( ret );
	return ret;
}

bool isOperator( const std::string & inputName ) {
	return operatorLookup( inputName ) != nullptr;
}

std::string operatorFriendlyName( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->friendlyName;
	return "";
}

// This is only used in the demangler, so it is smaller (and only maybe slow).
const OperatorInfo * operatorLookupByOutput( const std::string & outputName ) {
	if ( '_' != outputName[0] ) return nullptr;
	for ( const OperatorInfo & op : tableValues ) {
		if ( outputName == op.outputName ) {
			return &op;
		}
	}
	return nullptr;
}

bool isConstructor( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->type == OT_CTOR;
	return false;
}

bool isDestructor( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->type == OT_DTOR;
	return false;
}

bool isCtorDtor( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->type <= OT_CONSTRUCTOR;
	return false;
}

bool isAssignment( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->type > OT_CONSTRUCTOR && info->type <= OT_ASSIGNMENT;
	return false;
}

bool isCtorDtorAssign( const std::string & inputName ) {
	const OperatorInfo * info = operatorLookup( inputName );
	if ( info ) return info->type <= OT_ASSIGNMENT;
	return false;
}

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// End: //
