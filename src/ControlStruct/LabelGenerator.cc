//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// LabelGenerator.cc --
//
// Author           : Rodolfo G. Esteves
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Nov  8 10:18:00 2021
// Update Count     : 17
//

#include <iostream>             // for operator<<, basic_ostream
#include <sstream>              // for ostringstream
#include <list>                 // for list

#include "LabelGenerator.h"

#include "AST/Attribute.hpp"
#include "AST/Label.hpp"
#include "AST/Stmt.hpp"
#include "SynTree/Attribute.h"  // for Attribute
#include "SynTree/Label.h"      // for Label, operator<<
#include "SynTree/Statement.h"  // for Statement

namespace ControlStruct {

int LabelGenerator::current = 0;
LabelGenerator * LabelGenerator::labelGenerator = nullptr;

	LabelGenerator * LabelGenerator::getGenerator() {
		if ( LabelGenerator::labelGenerator == 0 )
			LabelGenerator::labelGenerator = new LabelGenerator();
		return labelGenerator;
	}

	Label LabelGenerator::newLabel( std::string suffix, Statement * stmt ) {
		std::ostringstream os;
		os << "__L" << current++ << "__" << suffix;
		if ( stmt && ! stmt->get_labels().empty() ) {
			os << "_" << stmt->get_labels().front() << "__";
		} // if
		std::string ret = os.str();
		Label l( ret );
		l.get_attributes().push_back( new Attribute("unused") );
		return l;
	}

ast::Label LabelGenerator::newLabel(
		const std::string & suffix, const ast::Stmt * stmt ) {
	assert( stmt );

	std::ostringstream os;
	os << "__L" << current++ << "__" << suffix;
	if ( stmt && !stmt->labels.empty() ) {
		os << "_" << stmt->labels.front() << "__";
	}
	ast::Label ret_label( stmt->location, os.str() );
	ret_label.attributes.push_back( new ast::Attribute( "unused" ) );
	return ret_label;
}

} // namespace ControlStruct

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
