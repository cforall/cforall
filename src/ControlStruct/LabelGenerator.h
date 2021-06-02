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
// Last Modified On : Sat Jul 22 09:20:14 2017
// Update Count     : 6
//

#pragma once

#include <string>           // for string

#include "SynTree/Label.h"  // for Label

class Statement;

namespace ControlStruct {
	class LabelGenerator {
	  public:
		static LabelGenerator *getGenerator();
		Label newLabel(std::string suffix, Statement * stmt = nullptr);
		void reset() { current = 0; }
		void rewind() { current--; }
	  protected:
		LabelGenerator(): current(0) {}
	  private:
		int current;
		static LabelGenerator *labelGenerator;
	};
} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
