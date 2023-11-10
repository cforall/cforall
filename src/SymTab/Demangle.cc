//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Demangle.cc -- Convert a mangled name into a human readable name.
//
// Author           : Rob Schluntz
// Created On       : Thu Jul 19 12:52:41 2018
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  6 15:59:00 2023
// Update Count     : 12
//

#include <algorithm>
#include <sstream>

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "CodeGen/GenType.h"
#include "CodeGen/OperatorTable.h"
#include "Common/utility.h"								// isPrefix
#include "Mangler.h"

#define DEBUG
#ifdef DEBUG
#define PRINT(x) x
#else
#define PRINT(x) {}
#endif

namespace Mangle {

namespace {

struct Demangler {
private:
	std::string str;
	size_t index = 0;
	using Parser = std::function<ast::Type * ( ast::CV::Qualifiers )>;
	std::vector<std::pair<std::string, Parser>> parsers;
public:
	Demangler( const std::string & str );

	bool done() const { return str.size() <= index; }
	char cur() const { assert( !done() ); return str[index]; }
	bool expect( char ch ) { return str[index++] == ch; }

	bool isPrefix( const std::string & pref );
	bool extractNumber( size_t & out );
	bool extractName( std::string & out );
	bool stripMangleName( std::string & name );

	ast::Type * parseFunction( ast::CV::Qualifiers tq );
	ast::Type * parseTuple( ast::CV::Qualifiers tq );
	ast::Type * parsePointer( ast::CV::Qualifiers tq );
	ast::Type * parseArray( ast::CV::Qualifiers tq );
	ast::Type * parseStruct( ast::CV::Qualifiers tq );
	ast::Type * parseUnion( ast::CV::Qualifiers tq );
	ast::Type * parseEnum( ast::CV::Qualifiers tq );
	ast::Type * parseType( ast::CV::Qualifiers tq );
	ast::Type * parseZero( ast::CV::Qualifiers tq );
	ast::Type * parseOne( ast::CV::Qualifiers tq );

	ast::Type * parseType();
	bool parse( std::string & name, ast::Type *& type );
};

Demangler::Demangler(const std::string & str) : str(str) {
	for (size_t k = 0; k < ast::BasicType::NUMBER_OF_BASIC_TYPES; ++k) {
		parsers.emplace_back(Encoding::basicTypes[k], [k]( ast::CV::Qualifiers tq ) {
			PRINT( std::cerr << "basic type: " << k << std::endl; )
			return new ast::BasicType( (ast::BasicType::Kind)k, tq );
		});
	}

	for (size_t k = 0; k < ast::TypeDecl::NUMBER_OF_KINDS; ++k) {
		static const std::string typeVariableNames[] = { "DT", "DST", "OT", "FT", "TT", "ALT", };
		static_assert(
			sizeof(typeVariableNames)/sizeof(typeVariableNames[0]) == ast::TypeDecl::NUMBER_OF_KINDS,
			"Each type variable kind should have a demangle name prefix"
		);
		parsers.emplace_back(Encoding::typeVariables[k], [k, this]( ast::CV::Qualifiers tq ) -> ast::TypeInstType * {
			PRINT( std::cerr << "type variable type: " << k << std::endl; )
			size_t N;
			if (!extractNumber(N)) return nullptr;
			return new ast::TypeInstType(
				toString(typeVariableNames[k], N),
				(ast::TypeDecl::Kind)k,
				tq );
		});
	}

	parsers.emplace_back(Encoding::void_t, [this]( ast::CV::Qualifiers tq ) { return new ast::VoidType(tq); });
	parsers.emplace_back(Encoding::function, [this]( ast::CV::Qualifiers tq ) { return parseFunction(tq); });
	parsers.emplace_back(Encoding::pointer, [this]( ast::CV::Qualifiers tq ) { return parsePointer(tq); });
	parsers.emplace_back(Encoding::array, [this]( ast::CV::Qualifiers tq ) { return parseArray(tq); });
	parsers.emplace_back(Encoding::tuple, [this]( ast::CV::Qualifiers tq ) { return parseTuple(tq); });
	parsers.emplace_back(Encoding::struct_t, [this]( ast::CV::Qualifiers tq ) { return parseStruct(tq); });
	parsers.emplace_back(Encoding::union_t, [this]( ast::CV::Qualifiers tq ) { return parseUnion(tq); });
	parsers.emplace_back(Encoding::enum_t, [this]( ast::CV::Qualifiers tq ) { return parseEnum(tq); });
	parsers.emplace_back(Encoding::type, [this]( ast::CV::Qualifiers tq ) { return parseType(tq); });
	parsers.emplace_back(Encoding::zero, []( ast::CV::Qualifiers tq ) { return new ast::ZeroType(tq); });
	parsers.emplace_back(Encoding::one, []( ast::CV::Qualifiers tq ) { return new ast::OneType(tq); });
}

bool Demangler::extractNumber( size_t & out ) {
	std::stringstream numss;
	if ( str.size() <= index ) return false;
	while ( isdigit( str[index] ) ) {
		numss << str[index];
		++index;
		if ( str.size() == index ) break;
	}
	if ( !(numss >> out) ) return false;
	PRINT( std::cerr << "extractNumber success: " << out << std::endl; )
	return true;
}

bool Demangler::extractName( std::string & out ) {
	size_t len;
	if ( !extractNumber(len) ) return false;
	if ( str.size() < index + len ) return false;
	out = str.substr( index, len );
	index += len;
	PRINT( std::cerr << "extractName success: " << out << std::endl; )
	return true;
}

bool Demangler::isPrefix( const std::string & pref ) {
	// Wraps the utility isPrefix function.
	if ( ::isPrefix( str, pref, index ) ) {
		index += pref.size();
		return true;
	}
	return false;
}

// strips __NAME__cfa__TYPE_N, where N is [0-9]+: returns str is a match is found, returns empty string otherwise
bool Demangler::stripMangleName( std::string & name ) {
	PRINT( std::cerr << "====== " << str.size() << " " << str << std::endl; )
	if (str.size() < 2+Encoding::manglePrefix.size()) return false; // +2 for at least _1 suffix
	if ( !isPrefix(Encoding::manglePrefix) || !isdigit(str.back() ) ) return false;

	if (!extractName(name)) return false;

	// Find bounds for type.
	PRINT( std::cerr << index << " " << str.size() << std::endl; )
	PRINT( std::cerr << "[");
	while (isdigit(str.back())) {
		PRINT(std::cerr << ".");
		str.pop_back();
		if (str.size() <= index) return false;
	}
	PRINT( std::cerr << "]" << std::endl );
	if (str.back() != '_') return false;
	str.pop_back();
	PRINT( std::cerr << str.size() << " " << name << " " << str.substr(index) << std::endl; )
	return index < str.size();
}

ast::Type * Demangler::parseFunction( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "function..." << std::endl; )
	if ( done() ) return nullptr;
	ast::FunctionType * ftype = new ast::FunctionType( ast::FixedArgs, tq );
	std::unique_ptr<ast::Type> manager( ftype );
	ast::Type * retVal = parseType();
	if ( !retVal ) return nullptr;
	PRINT( std::cerr << "with return type: " << retVal << std::endl; )
	ftype->returns.emplace_back( retVal );
	if ( done() || !expect('_') ) return nullptr;
	while ( !done() ) {
		PRINT( std::cerr << "got ch: " << cur() << std::endl; )
		if ( cur() == '_' ) return manager.release();
		ast::Type * param = parseType();
		if ( !param ) return nullptr;
		PRINT( std::cerr << "with parameter : " << param << std::endl; )
		ftype->params.emplace_back( param );
	}
	return nullptr;
}

ast::Type * Demangler::parseTuple( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "tuple..." << std::endl; )
	std::vector<ast::ptr<ast::Type>> types;
	size_t ncomponents;
	if ( !extractNumber(ncomponents) ) return nullptr;
	for ( size_t i = 0; i < ncomponents; ++i ) {
		if ( done() ) return nullptr;
		PRINT( std::cerr << "got ch: " << cur() << std::endl; )
		ast::Type * t = parseType();
		if ( !t ) return nullptr;
		PRINT( std::cerr << "with type : " << t << std::endl; )
		types.push_back( t );
	}
	return new ast::TupleType( std::move( types ), tq );
}

ast::Type * Demangler::parsePointer( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "pointer..." << std::endl; )
	ast::Type * t = parseType();
	if ( !t ) return nullptr;
	return new ast::PointerType( t, tq );
}

ast::Type * Demangler::parseArray( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "array..." << std::endl; )
	size_t length;
	if ( !extractNumber(length) ) return nullptr;
	ast::Type * t = parseType();
	if ( !t ) return nullptr;
	return new ast::ArrayType(
		t,
		ast::ConstantExpr::from_ulong( CodeLocation(), length ),
		ast::FixedLen,
		ast::DynamicDim,
		tq );
}

ast::Type * Demangler::parseStruct( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "struct..." << std::endl; )
	std::string name;
	if ( !extractName(name) ) return nullptr;
	return new ast::StructInstType( name, tq );
}

ast::Type * Demangler::parseUnion( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "union..." << std::endl; )
	std::string name;
	if ( !extractName(name) ) return nullptr;
	return new ast::UnionInstType( name, tq );
}

ast::Type * Demangler::parseEnum( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "enum..." << std::endl; )
	std::string name;
	if ( !extractName(name) ) return nullptr;
	return new ast::EnumInstType( name, tq );
}

ast::Type * Demangler::parseType( ast::CV::Qualifiers tq ) {
	PRINT( std::cerr << "type..." << std::endl; )
	std::string name;
	if ( !extractName(name) ) return nullptr;
	PRINT( std::cerr << "typename..." << name << std::endl; )
	return new ast::TypeInstType( name, ast::TypeDecl::Dtype, tq );
}

ast::Type * Demangler::parseType() {
	if (done()) return nullptr;

	if (isPrefix(Encoding::forall)) {
		PRINT( std::cerr << "polymorphic with..." << std::endl; )
		size_t dcount, fcount, vcount, acount;
		if ( !extractNumber(dcount) ) return nullptr;
		PRINT( std::cerr << dcount << " dtypes" << std::endl; )
		if ( !expect('_') ) return nullptr;
		if ( !extractNumber(fcount) ) return nullptr;
		PRINT( std::cerr << fcount << " ftypes" << std::endl; )
		if ( !expect('_')) return nullptr;
		if ( !extractNumber(vcount)) return nullptr;
		PRINT( std::cerr << vcount << " ttypes" << std::endl; )
		if ( !expect('_') ) return nullptr;
		if ( !extractNumber(acount) ) return nullptr;
		PRINT( std::cerr << acount << " assertions" << std::endl; )
		if ( !expect('_') ) return nullptr;
		for ( size_t i = 0 ; i < acount ; ++i ) {
			// TODO: need to recursively parse assertions, but for now just return nullptr so that
			// demangler does not crash if there are assertions
			return nullptr;
		}
		if ( !expect('_') ) return nullptr;
	}

	ast::CV::Qualifiers tq;
	while (true) {
		auto qual = std::find_if(Encoding::qualifiers.begin(), Encoding::qualifiers.end(), [this](decltype(Encoding::qualifiers)::value_type val) {
			return isPrefix(val.second);
		});
		if (qual == Encoding::qualifiers.end()) break;
		tq |= qual->first;
	}

	// Find the correct type parser and then apply it.
	auto iter = std::find_if(parsers.begin(), parsers.end(), [this](std::pair<std::string, Parser> & p) {
		return isPrefix(p.first);
	});
	assertf(iter != parsers.end(), "Unhandled type letter: %c at index: %zd", cur(), index);
	ast::Type * ret = iter->second(tq);
	if ( !ret ) return nullptr;
	return ret;
}

bool Demangler::parse( std::string & name, ast::Type *& type) {
	if ( !stripMangleName(name) ) return false;
	PRINT( std::cerr << "stripped name: " << name << std::endl; )
	ast::Type * t = parseType();
	if ( !t ) return false;
	type = t;
	return true;
}

std::string demangle( const std::string & mangleName ) {
	using namespace CodeGen;
	Demangler demangler( mangleName );
	std::string name;
	ast::Type * type = nullptr;
	if ( !demangler.parse( name, type ) ) return mangleName;
	ast::readonly<ast::Type> roType = type;
	if ( auto info = operatorLookupByOutput( name ) ) name = info->inputName;
	return genType( type, name, Options( false, false, false, false ) );
}

} // namespace

} // namespace Mangle

extern "C" {
	char * cforall_demangle(const char * mangleName, int option __attribute__((unused))) {
		const std::string & demangleName = Mangle::demangle(mangleName);
		return strdup(demangleName.c_str());
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
