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
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 03:24:32 2020
// Update Count     : 5
//

#pragma once

#include <iosfwd>
#include <memory>

#include "SynTree/LinkageSpec.h"

class FunctionDecl;

namespace CodeGen {
	class FixMain {
	  public :
		static inline LinkageSpec::Spec mainLinkage() {
			return replace_main ? LinkageSpec::Cforall : LinkageSpec::C;
		}
		
		static inline void setReplaceMain(bool val) {
			replace_main = val;
		}

		static void registerMain(FunctionDecl* val);

		static void fix(std::ostream &os, const char* bootloader_filename);

	  private:
  		static bool replace_main;
		static std::unique_ptr<FunctionDecl> main_signature;
	};
} // namespace CodeGen
