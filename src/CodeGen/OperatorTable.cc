//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// OperatorTable.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb 18 15:55:01 2020
// Update Count     : 55
//

#include <algorithm>  // for any_of
#include <map>        // for map, _Rb_tree_const_iterator, map<>::const_iterator
#include <utility>    // for pair
using namespace std;

#include "OperatorTable.h"
#include "Common/utility.h"

namespace CodeGen {
	const OperatorInfo CodeGen::tableValues[] = {
		// inputName symbol   outputName                     friendlyName                  type
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

	std::map< std::string, OperatorInfo > CodeGen::table;

	CodeGen::CodeGen() {
		enum { numOps = sizeof( tableValues ) / sizeof( OperatorInfo ) };
		for ( int i = 0; i < numOps; i += 1 ) {
			table[ tableValues[i].inputName ] = tableValues[i];
		} // for
	}

	const OperatorInfo * operatorLookup( const string & funcName ) {
		if ( funcName.find_first_of( "?^*+-!", 0, 1 ) == string::npos ) return nullptr; // prefilter
		const OperatorInfo * ret = &CodeGen::table.find( funcName )->second; // must be in the table
		assert( ret );
		return ret;
	}

	bool isOperator( const string & funcName ) {
		return operatorLookup( funcName ) != nullptr;
	}

	string operatorFriendlyName( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->friendlyName;
		return "";
	}

	bool isConstructor( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->type == OT_CTOR;
		return false;
	}

	bool isDestructor( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->type == OT_DTOR;
		return false;
	}

	bool isCtorDtor( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->type <= OT_CONSTRUCTOR;
		return false;
	}

	bool isAssignment( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->type > OT_CONSTRUCTOR && info->type <= OT_ASSIGNMENT;
		return false;
	}

	bool isCtorDtorAssign( const string & funcName ) {
		const OperatorInfo * info = operatorLookup( funcName );
		if ( info ) return info->type <= OT_ASSIGNMENT;
		return false;
	}

	CodeGen codegen;									// initialize singleton package
} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// End: //
