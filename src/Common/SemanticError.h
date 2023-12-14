//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SemanticError.h --
//
// Author           : Thierry Delisle
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Dec 11 21:54:22 2023
// Update Count     : 54
//

#pragma once

#include "ErrorObjects.h"
#include "AST/Node.hpp"
#include "AST/ParseNode.hpp"
#include <cstring>

//-----------------------------------------------------------------------------
// Errors

extern bool SemanticErrorThrow;

__attribute__((noreturn, format(printf, 2, 3))) void SemanticError( CodeLocation location, const char fmt[], ... );

__attribute__((noreturn)) void SemanticError( CodeLocation location, std::string error );

__attribute__((noreturn)) static inline void SemanticError( const ast::ParseNode * obj, const std::string & error ) {
	SemanticError( obj->location, toString( error, obj ) );
}

__attribute__((noreturn)) static inline void SemanticError( CodeLocation location, const ast::Node * obj, const std::string & error ) {
	SemanticError( location, toString( error, obj ) );
}

//-----------------------------------------------------------------------------
// Warnings

enum class Severity {
	Suppress,
	Warn,
	Error,
	Critical
};

struct WarningData {
	const char * const name;
	const Severity default_severity;
	const char * const message;
};

constexpr WarningData WarningFormats[] = {
	{"self-assign"              , Severity::Warn, "self assignment of expression: %s"                          },
	{"reference-conversion"     , Severity::Warn, "rvalue to reference conversion of rvalue: %s"               },
	{"qualifiers-zero_t-one_t"  , Severity::Warn, "questionable use of type qualifier(s) with %s"              },
	{"aggregate-forward-decl"   , Severity::Warn, "forward declaration of nested aggregate: %s"                },
	{"superfluous-decl"         , Severity::Warn, "declaration does not allocate storage: %s"                  },
	{"superfluous-else"         , Severity::Warn, "else clause never executed for empty loop conditional"      },
	{"gcc-attributes"           , Severity::Warn, "invalid attribute: %s"                                      },
	{"c++-like-copy"            , Severity::Warn, "Constructor from reference is not a valid copy constructor" },
	{"depreciated-trait-syntax" , Severity::Warn, "trait type-parameters are now specified using the forall clause" },
};

enum class Warning {
	SelfAssignment,
	RvalueToReferenceConversion,
	BadQualifiersZeroOne,
	AggrForwardDecl,
	SuperfluousDecl,
	SuperfluousElse,
	GccAttributes,
	CppCopy,
	DeprecTraitSyntax,
	NUMBER_OF_WARNINGS, // MUST be the last warning
};

static_assert(
	(sizeof(WarningFormats) / sizeof(WarningFormats[0])) == ((unsigned long)Warning::NUMBER_OF_WARNINGS),
	"Each warning format should have a corresponding warning enum value"
);

#define SemanticWarning(loc, id, ...) SemanticWarningImpl(loc, id, WarningFormats[(int)id].message, ##__VA_ARGS__)

void SemanticWarningImpl (CodeLocation loc, Warning warn, const char * const fmt, ...) __attribute__((format(printf, 3, 4)));

void SemanticWarning_SuppressAll   ();
void SemanticWarning_EnableAll     ();
void SemanticWarning_WarningAsError();
void SemanticWarning_Set           (const char * const name, Severity s);

// SKULLDUGGERY: cfa.cc is built before SemanticError.cc but needs this routine.
static inline bool SemanticWarning_Exist(const char * const name) {
	for ( const auto & w : WarningFormats ) {
		if ( std::strcmp( name, w.name ) == 0 ) return true;
	}
	return false;
}

//-----------------------------------------------------------------------------
// Helpers
namespace ErrorHelpers {
	enum class Colors {
		Never = false,
		Always = true,
		Auto,
	};

	extern Colors colors;

	const std::string & error_str();
	const std::string & warning_str();
	const std::string & bold_ttycode();
	const std::string & reset_font_ttycode();

	std::string make_bold( const std::string & str );

	struct bold {};
	std::ostream & operator<<(std::ostream & os, bold);

	struct reset_font {};
	std::ostream & operator<<(std::ostream & os, reset_font);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
