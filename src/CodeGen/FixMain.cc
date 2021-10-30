//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixMain.cc --
//
// Author           : Thierry Delisle
// Created On       : Thr Jan 12 14:11:09 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 0
//


#include "FixMain.h"

#include <cassert>                 // for assert, assertf
#include <fstream>                 // for operator<<, basic_ostream::operator<<
#include <list>                    // for list
#include <string>                  // for operator<<

#include "AST/Decl.hpp"
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "CodeGen/GenType.h"       // for GenType
#include "SynTree/Declaration.h"   // for FunctionDecl, operator<<
#include "SynTree/Type.h"          // for FunctionType
#include "SymTab/Mangler.h"

namespace CodeGen {

namespace {

struct FindMainCore {
	FunctionDecl * main_signature = nullptr;

	void previsit( FunctionDecl * decl ) {
		if ( FixMain::isMain( decl ) ) {
			if ( main_signature ) {
				SemanticError( decl, "Multiple definition of main routine\n" );
			}
			main_signature = decl;
		}
	}
};

}

	bool FixMain::replace_main = false;

	template<typename container>
	std::string genTypeAt(const container& p, size_t idx) {
		return genType((*std::next(p.begin(), idx))->get_type(), "");
	}

	void FixMain::fix( std::list< Declaration * > & translationUnit,
			std::ostream &os, const char* bootloader_filename ) {
		PassVisitor< FindMainCore > main_finder;
		acceptAll( translationUnit, main_finder );
		FunctionDecl * main_signature = main_finder.pass.main_signature;

		if( main_signature ) {
			os << "static inline int invoke_main(int argc, char* argv[], char* envp[]) { (void)argc; (void)argv; (void)envp; return ";
			main_signature->mangleName = SymTab::Mangler::mangle(main_signature);

			os << main_signature->get_scopedMangleName() << "(";
			const auto& params = main_signature->get_functionType()->get_parameters();
			switch(params.size()) {
				case 3: os << "(" << genTypeAt(params, 0) << ")argc, (" << genTypeAt(params, 1) << ")argv, (" << genTypeAt(params, 2) << ")envp"; break;
				case 2: os << "(" << genTypeAt(params, 0) << ")argc, (" << genTypeAt(params, 1) << ")argv"; break;
				case 0: break;
				default : assert(false);
			}
			os << "); }\n";

			std::ifstream bootloader(bootloader_filename, std::ios::in);
			assertf( bootloader.is_open(), "cannot open bootloader.c\n" );
			os << bootloader.rdbuf();
		}
	}

namespace {

ObjectDecl * signedIntObj() {
	return new ObjectDecl(
		"", Type::StorageClasses(), LinkageSpec::Cforall, 0,
		new BasicType( Type::Qualifiers(), BasicType::SignedInt ), nullptr );
}

ObjectDecl * charStarObj() {
	return new ObjectDecl(
		"", Type::StorageClasses(), LinkageSpec::Cforall, 0,
		new PointerType( Type::Qualifiers(),
			new PointerType( Type::Qualifiers(),
				new BasicType( Type::Qualifiers(), BasicType::Char ) ) ),
		nullptr );
}

std::string create_mangled_main_function_name( FunctionType * function_type ) {
	std::unique_ptr<FunctionDecl> decl( new FunctionDecl(
		"main", Type::StorageClasses(), LinkageSpec::Cforall,
		function_type, nullptr ) );
	return SymTab::Mangler::mangle( decl.get() );
}

std::string mangled_0_argument_main() {
	FunctionType* main_type = new FunctionType( Type::Qualifiers(), true );
	main_type->get_returnVals().push_back( signedIntObj() );
	return create_mangled_main_function_name( main_type );
}

std::string mangled_2_argument_main() {
	FunctionType* main_type = new FunctionType( Type::Qualifiers(), false );
	main_type->get_returnVals().push_back( signedIntObj() );
	main_type->get_parameters().push_back( signedIntObj() );
	main_type->get_parameters().push_back( charStarObj() );
	return create_mangled_main_function_name( main_type );
}

bool is_main( const std::string & mangled_name ) {
	// This breaks if you move it out of the function.
	static const std::string mangled_mains[] = {
		mangled_0_argument_main(),
		mangled_2_argument_main(),
		//mangled_3_argument_main(),
	};

	for ( auto main_name : mangled_mains ) {
		if ( main_name == mangled_name ) return true;
	}
	return false;
}

} // namespace

bool FixMain::isMain( FunctionDecl * decl ) {
	if ( std::string("main") != decl->name ) {
		return false;
	}
	return is_main( SymTab::Mangler::mangle( decl, true, true ) );
}

bool FixMain::isMain( const ast::FunctionDecl * decl ) {
	if ( std::string("main") != decl->name ) {
		return false;
	}
	return is_main( Mangle::mangle( decl, Mangle::Type ) );
}

};
