//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenType.cpp --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri May 20 11:18:00 2022
// Update Count     : 24
//

#include "GenType.hpp"

#include <cassert>                // for assert, assertf
#include <list>                   // for _List_iterator, _List_const_iterator
#include <sstream>                // for operator<<, ostringstream, basic_os...

#include "AST/Print.hpp"          // for print
#include "AST/Vector.hpp"         // for vector
#include "CodeGenerator.hpp"      // for CodeGenerator
#include "Common/UniqueName.hpp"  // for UniqueName

namespace CodeGen {

namespace {

struct GenType final :
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<GenType> {
	std::string result;
	GenType( const std::string &typeString, const Options &options );

	void previsit( ast::Node const * );
	void postvisit( ast::Node const * );

	void postvisit( ast::FunctionType const * type );
	void postvisit( ast::VoidType const * type );
	void postvisit( ast::BasicType const * type );
	void postvisit( ast::PointerType const * type );
	void postvisit( ast::ArrayType const * type );
	void postvisit( ast::ReferenceType const * type );
	void postvisit( ast::StructInstType const * type );
	void postvisit( ast::UnionInstType const * type );
	void postvisit( ast::EnumInstType const * type );
	void postvisit( ast::TypeInstType const * type );
	void postvisit( ast::TupleType const * type );
	void postvisit( ast::VarArgsType const * type );
	void postvisit( ast::ZeroType const * type );
	void postvisit( ast::OneType const * type );
	void postvisit( ast::GlobalScopeType const * type );
	void postvisit( ast::TraitInstType const * type );
	void postvisit( ast::TypeofType const * type );
	void postvisit( ast::VTableType const * type );
	void postvisit( ast::QualifiedType const * type );

private:
	void handleQualifiers( ast::Type const *type );
	std::string handleGeneric( ast::BaseInstType const * type );
	void genArray( const ast::CV::Qualifiers &qualifiers, ast::Type const *base, ast::Expr const *dimension, bool isVarLen, bool isStatic );
	std::string genParamList( const ast::vector<ast::Type> & );

	Options options;
};

GenType::GenType( const std::string &typeString, const Options &options ) : result( typeString ), options( options ) {}

void GenType::previsit( ast::Node const * ) {
	// Turn off automatic recursion for all nodes, to allow each visitor to
	// precisely control the order in which its children are visited.
	visit_children = false;
}

void GenType::postvisit( ast::Node const * node ) {
	std::stringstream ss;
	ast::print( ss, node );
	assertf( false, "Unhandled node reached in GenType: %s", ss.str().c_str() );
}

void GenType::postvisit( ast::VoidType const * type ) {
	result = "void " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::BasicType const * type ) {
	ast::BasicKind kind = type->kind;
	assert( 0 <= kind && kind < ast::BasicKind::NUMBER_OF_BASIC_TYPES );
	result = std::string( ast::BasicType::typeNames[kind] ) + " " + result;
	handleQualifiers( type );
}

void GenType::genArray( const ast::CV::Qualifiers & qualifiers, ast::Type const * base, ast::Expr const *dimension, bool isVarLen, bool isStatic ) {
	std::ostringstream os;
	if ( result != "" ) {
		if ( result[ 0 ] == '*' ) {
			os << "(" << result << ")";
		} else {
			os << result;
		}
	}
	os << "[";
	if ( isStatic ) {
		os << "static ";
	}
	if ( qualifiers.is_const ) {
		os << "const ";
	}
	if ( qualifiers.is_volatile ) {
		os << "volatile ";
	}
	if ( qualifiers.is_restrict ) {
		os << "__restrict ";
	}
	if ( qualifiers.is_atomic ) {
		os << "_Atomic ";
	}
	if ( dimension != 0 ) {
		ast::Pass<CodeGenerator>::read( dimension, os, options );
	} else if ( isVarLen ) {
		// no dimension expression on a VLA means it came in with the * token
		os << "*";
	}
	os << "]";

	result = os.str();

	base->accept( *visitor );
}

void GenType::postvisit( ast::PointerType const * type ) {
	if ( type->isStatic || type->isVarLen || type->dimension ) {
		genArray( type->qualifiers, type->base, type->dimension, type->isVarLen, type->isStatic );
	} else {
		handleQualifiers( type );
		if ( result[ 0 ] == '?' ) {
			result = "* " + result;
		} else {
			result = "*" + result;
		}
		type->base->accept( *visitor );
	}
}

void GenType::postvisit( ast::ArrayType const * type ) {
	genArray( type->qualifiers, type->base, type->dimension, type->isVarLen, type->isStatic );
}

void GenType::postvisit( ast::ReferenceType const * type ) {
	assertf( !options.genC, "Reference types should not reach code generation." );
	handleQualifiers( type );
	result = "&" + result;
	type->base->accept( *visitor );
}

void GenType::postvisit( ast::FunctionType const * type ) {
	std::ostringstream os;

	if ( result != "" ) {
		if ( result[ 0 ] == '*' ) {
			os << "(" << result << ")";
		} else {
			os << result;
		}
	}

	if ( type->params.empty() ) {
		if ( type->isVarArgs ) {
			os << "()";
		} else {
			os << "(void)";
		}
	} else {
		os << "(" ;

		os << genParamList( type->params );

		if ( type->isVarArgs ) {
			os << ", ...";
		}
		os << ")";
	}

	result = os.str();

	if ( type->returns.size() == 0 ) {
		result = "void " + result;
	} else {
		type->returns.front()->accept( *visitor );
	}

	// Add forall clause.
	if( !type->forall.empty() && !options.genC ) {
		//assertf( !options.genC, "FunctionDecl type parameters should not reach code generation." );
		std::ostringstream os;
		ast::Pass<CodeGenerator> cg( os, options );
		os << "forall(";
		cg.core.genCommaList( type->forall );
		os << ")" << std::endl;
		result = os.str() + result;
	}
}

std::string GenType::handleGeneric( ast::BaseInstType const * type ) {
	if ( !type->params.empty() ) {
		std::ostringstream os;
		ast::Pass<CodeGenerator> cg( os, options );
		os << "(";
		cg.core.genCommaList( type->params );
		os << ") ";
		return os.str();
	}
	return "";
}

void GenType::postvisit( ast::StructInstType const * type )  {
	result = type->name + handleGeneric( type ) + " " + result;
	if ( options.genC ) result = "struct " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::UnionInstType const * type ) {
	result = type->name + handleGeneric( type ) + " " + result;
	if ( options.genC ) result = "union " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::EnumInstType const * type ) {
	// if ( type->base && type->base->base ) {
	// 	result = genType( type->base->base, result, options );
	// } else {
		result = type->name + " " + result;
		if ( options.genC ) {
			result = "enum " + result;
		}
	// }
	handleQualifiers( type );
}

void GenType::postvisit( ast::TypeInstType const * type ) {
	assertf( !options.genC, "TypeInstType should not reach code generation." );
	result = type->name + " " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::TupleType const * type ) {
	assertf( !options.genC, "TupleType should not reach code generation." );
	unsigned int i = 0;
	std::ostringstream os;
	os << "[";
	for ( ast::ptr<ast::Type> const & t : type->types ) {
		i++;
		os << genType( t, "", options ) << (i == type->size() ? "" : ", ");
	}
	os << "] ";
	result = os.str() + result;
}

void GenType::postvisit( ast::VarArgsType const * type ) {
	result = "__builtin_va_list " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::ZeroType const * type ) {
	// Ideally these wouldn't hit codegen at all, but should be safe to make them ints.
	result = (options.pretty ? "zero_t " : "long int ") + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::OneType const * type ) {
	// Ideally these wouldn't hit codegen at all, but should be safe to make them ints.
	result = (options.pretty ? "one_t " : "long int ") + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::GlobalScopeType const * type ) {
	assertf( !options.genC, "GlobalScopeType should not reach code generation." );
	handleQualifiers( type );
}

void GenType::postvisit( ast::TraitInstType const * type ) {
	assertf( !options.genC, "TraitInstType should not reach code generation." );
	result = type->name + " " + result;
	handleQualifiers( type );
}

void GenType::postvisit( ast::TypeofType const * type ) {
	std::ostringstream os;
	os << "typeof(";
	ast::Pass<CodeGenerator>::read( type->expr.get(), os, options );
	os << ") " << result;
	result = os.str();
	handleQualifiers( type );
}

void GenType::postvisit( ast::VTableType const * type ) {
	assertf( !options.genC, "Virtual table types should not reach code generation." );
	std::ostringstream os;
	os << "vtable(" << genType( type->base, "", options ) << ") " << result;
	result = os.str();
	handleQualifiers( type );
}

void GenType::postvisit( ast::QualifiedType const * type ) {
	assertf( !options.genC, "QualifiedType should not reach code generation." );
	std::ostringstream os;
	os << genType( type->parent, "", options ) << "." << genType( type->child, "", options ) << result;
	result = os.str();
	handleQualifiers( type );
}

void GenType::handleQualifiers( ast::Type const * type ) {
	if ( type->is_const() ) {
		result = "const " + result;
	}
	if ( type->is_volatile() ) {
		result = "volatile " + result;
	}
	if ( type->is_restrict() ) {
		result = "__restrict " + result;
	}
	if ( type->is_atomic() ) {
		result = "_Atomic " + result;
	}
}

std::string GenType::genParamList( const ast::vector<ast::Type> & range ) {
	auto cur = range.begin();
	auto end = range.end();
	if ( cur == end ) return "";
	std::ostringstream oss;
	UniqueName param( "__param_" );
	while ( true ) {
		oss << genType( *cur++, options.genC ? param.newName() : "", options );
		if ( cur == end ) break;
		oss << ", ";
	}
	return oss.str();
}

} // namespace

std::string genType( ast::Type const * type, const std::string & base, const Options & options ) {
	std::ostringstream os;
	if ( !type->attributes.empty() ) {
		ast::Pass<CodeGenerator> cg( os, options );
		cg.core.genAttributes( type->attributes );
	}

	return os.str() + ast::Pass<GenType>::read( type, base, options );
}

std::string genTypeNoAttr( ast::Type const * type, const std::string & base, const Options & options ) {
	return ast::Pass<GenType>::read( type, base, options );
}

} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
