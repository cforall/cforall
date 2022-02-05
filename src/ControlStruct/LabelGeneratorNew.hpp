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
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 31 18:03:09 2022
// Update Count     : 27
//

#pragma once

#include <string>										// for string

class Statement;

namespace ast {
	class Stmt;
	class Label;
} // namespace ast

namespace ControlStruct {
	ast::Label newLabel( const std::string &, const ast::Stmt * );
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
