//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VarExprReplacer.h --
//
// Author           : Rob Schluntz
// Created On       : Wed Jan 13 16:29:30 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:53:41 2017
// Update Count     : 6
//

#pragma once

#include <map>                // for map, map<>::value_compare

#include "SynTree/Visitor.h"  // for Visitor

class DeclarationWithType;
class VariableExpr;

namespace DeclReplacer {
	typedef std::map< DeclarationWithType *, DeclarationWithType * > DeclMap;
	typedef std::map< TypeDecl *, TypeDecl * > TypeMap;
	typedef std::map< DeclarationWithType *, Expression * > ExprMap;

	size_t replace( BaseSyntaxNode * node, const DeclMap & declMap, bool debug = false );
	size_t replace( BaseSyntaxNode * node, const TypeMap & typeMap, bool debug = false );
	size_t replace( BaseSyntaxNode * node, const DeclMap & declMap, const TypeMap & typeMap, bool debug = false );

	size_t replace( BaseSyntaxNode *& node, const ExprMap & exprMap, bool debug = false);

	template<typename T>
	size_t replace( T *& node, const ExprMap & exprMap, bool debug = false ) {
		if ( ! node ) return 0ul;
		BaseSyntaxNode * arg = node;
		size_t replaced = replace( arg, exprMap, debug );
		node = dynamic_cast<T *>( arg );
		assertf( node, "DeclReplacer fundamentally changed the type of its argument." );
		return replaced;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
