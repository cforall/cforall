//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclReplacer.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 8 13:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 8 13:00:00 2019
// Update Count     : 1
//

#pragma once

#include <unordered_map>

namespace ast {
	class DeclWithType;
	class Expr;
	class Node;
	class TypeDecl;
}

namespace ast {

namespace DeclReplacer {

using DeclMap = std::unordered_map< const DeclWithType *, const DeclWithType * >;
using TypeMap = std::unordered_map< const TypeDecl *, const TypeDecl * >;
using ExprMap = std::unordered_map< const DeclWithType *, const Expr * >;

const Node * replace( const Node * node, const DeclMap & declMap, bool debug = false );
const Node * replace( const Node * node, const TypeMap & typeMap, bool debug = false );
const Node * replace( const Node * node, const DeclMap & declMap, const TypeMap & typeMap, bool debug = false );
const Node * replace( const Node * node, const ExprMap & exprMap);

}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
