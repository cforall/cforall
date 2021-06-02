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

#include "Common/SemanticError.h"  // for SemanticError
#include "CodeGen/GenType.h"       // for GenType
#include "SynTree/Declaration.h"   // for FunctionDecl, operator<<
#include "SynTree/Type.h"          // for FunctionType
#include "SymTab/Mangler.h"

namespace CodeGen {
	bool FixMain::replace_main = false;
	std::unique_ptr<FunctionDecl> FixMain::main_signature = nullptr;

	template<typename container>
	std::string genTypeAt(const container& p, size_t idx) {
		return genType((*std::next(p.begin(), idx))->get_type(), "");
	}

	void FixMain::registerMain(FunctionDecl* functionDecl)
	{
		if(main_signature) {
			SemanticError(functionDecl, "Multiple definition of main routine\n");
		}
		main_signature.reset( functionDecl->clone() );
	}

	void FixMain::fix(std::ostream &os, const char* bootloader_filename) {
		if( main_signature ) {
			os << "static inline int invoke_main(int argc, char* argv[], char* envp[]) { (void)argc; (void)argv; (void)envp; return ";
			main_signature->mangleName = SymTab::Mangler::mangle(main_signature.get());

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
};
