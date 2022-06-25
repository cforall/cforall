//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Node.hpp --
//
// Author           : Thierry Delisle
// Created On       : Fri Jun 24 13:16:31 2022
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "AST/Decl.hpp"
#include "AST/Type.hpp"

#include <iostream>
#include <AST/Print.hpp>

namespace ast {
	bool structHasFlexibleArray( const ast::StructDecl * decl ) {
		if(decl->members.size() == 0) return false;
		const auto & last = *decl->members.rbegin();
		auto lastd = last.as<ast::DeclWithType>();
		if(!lastd) return false; // I don't know what this is possible, but it might be.
		auto atype = dynamic_cast<const ast::ArrayType *>(lastd->get_type());
		if(!atype) return false;
		return !atype->isVarLen && !atype->dimension;
	}
};