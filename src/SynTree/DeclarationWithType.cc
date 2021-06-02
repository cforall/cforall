//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclarationWithType.cc --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:45:16 2019
// Update Count     : 26
//

#include <list>                  // for list
#include <string>                // for string

#include "Attribute.h"           // for Attribute
#include "Common/utility.h"      // for cloneAll, deleteAll, maybeClone
#include "Declaration.h"         // for DeclarationWithType, Declaration
#include "LinkageSpec.h"         // for Spec
#include "Expression.h"          // for ConstantExpr
#include "Type.h"                // for Type, Type::FuncSpecifiers, Type::St...

DeclarationWithType::DeclarationWithType( const std::string &name, Type::StorageClasses scs, LinkageSpec::Spec linkage, const std::list< Attribute * > & attributes, Type::FuncSpecifiers fs )
	: Declaration( name, scs, linkage ), asmName( nullptr ), attributes( attributes ), fs( fs ) {
}

DeclarationWithType::DeclarationWithType( const DeclarationWithType &other )
		: Declaration( other ), mangleName( other.mangleName ), scopeLevel( other.scopeLevel ), fs( other.fs ) {
	cloneAll( other.attributes, attributes );
	asmName = maybeClone( other.asmName );
}

DeclarationWithType::~DeclarationWithType() {
	deleteAll( attributes );
	delete asmName;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
