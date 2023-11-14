//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixFunction.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 16:19:49 2015
// Last Modified By : Andrew Beach
// Last Modified On : Tue Jul 12 14:28:00 2022
// Update Count     : 7
//

#include "FixFunction.h"

#include <list>                   // for list

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "Common/utility.h"       // for copy

namespace SymTab {

namespace {
	struct FixFunction final : public ast::WithShortCircuiting {
		bool isVoid = false;

		void previsit( const ast::FunctionDecl * ) { visit_children = false; }

		const ast::DeclWithType * postvisit( const ast::FunctionDecl * func ) {
			// Cannot handle cases with asserions.
			assert( func->assertions.empty() );
			return new ast::ObjectDecl{
				func->location, func->name, new ast::PointerType( func->type ), nullptr,
				func->storage, func->linkage, nullptr, copy( func->attributes ) };
		}

		void previsit( const ast::ArrayType * ) { visit_children = false; }

		const ast::Type * postvisit( const ast::ArrayType * array ) {
			return new ast::PointerType{
				array->base, array->dimension, array->isVarLen, array->isStatic,
				array->qualifiers };
		}

		void previsit( const ast::FunctionType * ) { visit_children = false; }

		const ast::Type * postvisit( const ast::FunctionType * type ) {
			return new ast::PointerType( type );
		}

		void previsit( const ast::VoidType * ) { isVoid = true; }

		void previsit( const ast::BasicType * ) { visit_children = false; }
		void previsit( const ast::PointerType * ) { visit_children = false; }
		void previsit( const ast::StructInstType * ) { visit_children = false; }
		void previsit( const ast::UnionInstType * ) { visit_children = false; }
		void previsit( const ast::EnumInstType * ) { visit_children = false; }
		void previsit( const ast::TraitInstType * ) { visit_children = false; }
		void previsit( const ast::TypeInstType * ) { visit_children = false; }
		void previsit( const ast::TupleType * ) { visit_children = false; }
		void previsit( const ast::VarArgsType * ) { visit_children = false; }
		void previsit( const ast::ZeroType * ) { visit_children = false; }
		void previsit( const ast::OneType * ) { visit_children = false; }
	};
} // anonymous namespace

const ast::DeclWithType * fixFunction( const ast::DeclWithType * dwt, bool & isVoid ) {
	ast::Pass< FixFunction > fixer;
	dwt = dwt->accept( fixer );
	isVoid |= fixer.core.isVoid;
	return dwt;
}

const ast::Type * fixFunction( const ast::Type * type, bool & isVoid ) {
	ast::Pass< FixFunction > fixer;
	type = type->accept( fixer );
	isVoid |= fixer.core.isVoid;
	return type;
}

} // namespace SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
