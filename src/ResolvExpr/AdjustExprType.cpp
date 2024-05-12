//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AdjustExprType.cpp --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 23:41:42 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 21:43:56 2019
// Update Count     : 6
//

#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/SymbolTable.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"

namespace ResolvExpr {

namespace {

class AdjustExprType final : public ast::WithShortCircuiting {
	const ast::SymbolTable & symtab;
public:
	const ast::TypeEnvironment & tenv;

	AdjustExprType( const ast::TypeEnvironment & e, const ast::SymbolTable & syms )
	: symtab( syms ), tenv( e ) {}

	void previsit( const ast::VoidType * ) { visit_children = false; }
	void previsit( const ast::BasicType * ) { visit_children = false; }
	void previsit( const ast::PointerType * ) { visit_children = false; }
	void previsit( const ast::ArrayType * ) { visit_children = false; }
	void previsit( const ast::FunctionType * ) { visit_children = false; }
	void previsit( const ast::StructInstType * ) { visit_children = false; }
	void previsit( const ast::UnionInstType * ) { visit_children = false; }
	void previsit( const ast::EnumInstType * ) { visit_children = false; }
	void previsit( const ast::TraitInstType * ) { visit_children = false; }
	void previsit( const ast::TypeInstType * ) { visit_children = false; }
	void previsit( const ast::TupleType * ) { visit_children = false; }
	void previsit( const ast::VarArgsType * ) { visit_children = false; }
	void previsit( const ast::ZeroType * ) { visit_children = false; }
	void previsit( const ast::OneType * ) { visit_children = false; }

	const ast::Type * postvisit( const ast::ArrayType * at ) {
		return new ast::PointerType( at->base, at->qualifiers );
	}

	const ast::Type * postvisit( const ast::FunctionType * ft ) {
		return new ast::PointerType( ft );
	}

	const ast::Type * postvisit( const ast::TypeInstType * inst ) {
		// replace known function-type-variables with pointer-to-function
		if ( const ast::EqvClass * eqvClass = tenv.lookup( *inst ) ) {
			if ( eqvClass->data.kind == ast::TypeDecl::Ftype ) {
				return new ast::PointerType( inst );
			}
		} else if ( const ast::NamedTypeDecl * ntDecl = symtab.lookupType( inst->name ) ) {
			if ( auto tyDecl = dynamic_cast< const ast::TypeDecl * >( ntDecl ) ) {
				if ( tyDecl->kind == ast::TypeDecl::Ftype ) {
					return new ast::PointerType( inst );
				}
			}
		}
		return inst;
	}
};

} // anonymous namespace

const ast::Type * adjustExprType(
	const ast::Type * type, const ast::TypeEnvironment & env, const ast::SymbolTable & symtab
) {
	ast::Pass<AdjustExprType> adjuster{ env, symtab };
	return type->accept( adjuster );
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
