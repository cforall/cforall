//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ForExprMutator.h --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:18:50 2022
// Update Count     : 7
//

#pragma once

class IfStmt;
class ForStmt;
class WhileDoStmt;
class Statement;

namespace ControlStruct {
	class ForExprMutator {
	  public:
		Statement * postmutate( IfStmt * );
		Statement * postmutate( ForStmt * );
		Statement * postmutate( WhileDoStmt * );
	};
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
