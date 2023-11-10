//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixMain.h -- 
//
// Author           : Thierry Delisle
// Created On       : Thr Jan 12 14:11:09 2017
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct 29 16:20:00 2021
// Update Count     : 8
//

#pragma once

#include <iosfwd>
#include <memory>
#include <list>

#include "AST/LinkageSpec.hpp"

namespace ast {
	class FunctionDecl;
	class TranslationUnit;
}

namespace CodeGen {

class FixMain {
public :
	static inline ast::Linkage::Spec getMainLinkage() {
		return replace_main ? ast::Linkage::Cforall : ast::Linkage::C;
	}

	static inline void setReplaceMain(bool val) {
		replace_main = val;
	}

	static bool isMain(const ast::FunctionDecl * decl);

	static void fix( ast::TranslationUnit & translationUnit,
			std::ostream &os, const char * bootloader_filename );

private:
	static bool replace_main;
};

} // namespace CodeGen
