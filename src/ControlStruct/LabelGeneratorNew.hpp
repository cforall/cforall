//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelGenerator.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fir Mar 25 15:40:00 2022
// Update Count     : 28
//

#pragma once

#include <string>										// for string

struct CodeLocation;

namespace ast {
	class Label;
	class Stmt;
} // namespace ast

namespace ControlStruct {
	ast::Label newLabel( const std::string &, const ast::Stmt * );
	ast::Label newLabel( const std::string &, const CodeLocation & );
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
