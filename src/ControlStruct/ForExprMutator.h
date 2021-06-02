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
// Last Modified On : Thu Aug 17 15:32:48 2017
// Update Count     : 5
//

#pragma once

class IfStmt;
class ForStmt;
class WhileStmt;
class Statement;

namespace ControlStruct {
	class ForExprMutator {
	  public:
		Statement *postmutate( IfStmt * );
		Statement *postmutate( ForStmt * );
		Statement *postmutate( WhileStmt * );
	};
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
