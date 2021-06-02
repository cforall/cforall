//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// AdjustExprType_old.cc --
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
#include "Common/PassVisitor.h"
#include "SymTab/Indexer.h"       // for Indexer
#include "SynTree/Declaration.h"  // for TypeDecl, TypeDecl::Kind::Ftype
#include "SynTree/Mutator.h"      // for Mutator
#include "SynTree/Type.h"         // for PointerType, TypeInstType, Type
#include "TypeEnvironment.h"      // for EqvClass, TypeEnvironment

namespace ResolvExpr {

namespace {
	class AdjustExprType_old final : public WithShortCircuiting {
		public:
		AdjustExprType_old( const TypeEnvironment & env, const SymTab::Indexer & indexer );
		void premutate( VoidType * ) { visit_children = false; }
		void premutate( BasicType * ) { visit_children = false; }
		void premutate( PointerType * ) { visit_children = false; }
		void premutate( ArrayType * ) { visit_children = false; }
		void premutate( FunctionType * ) { visit_children = false; }
		void premutate( StructInstType * ) { visit_children = false; }
		void premutate( UnionInstType * ) { visit_children = false; }
		void premutate( EnumInstType * ) { visit_children = false; }
		void premutate( TraitInstType * ) { visit_children = false; }
		void premutate( TypeInstType * ) { visit_children = false; }
		void premutate( TupleType * ) { visit_children = false; }
		void premutate( VarArgsType * ) { visit_children = false; }
		void premutate( ZeroType * ) { visit_children = false; }
		void premutate( OneType * ) { visit_children = false; }

		Type * postmutate( ArrayType * arrayType );
		Type * postmutate( FunctionType * functionType );
		Type * postmutate( TypeInstType * aggregateUseType );

		private:
		const TypeEnvironment & env;
		const SymTab::Indexer & indexer;
	};

	AdjustExprType_old::AdjustExprType_old( const TypeEnvironment &env, const SymTab::Indexer &indexer )
		: env( env ), indexer( indexer ) {
	}

	Type * AdjustExprType_old::postmutate( ArrayType * arrayType ) {
		PointerType * pointerType = new PointerType{ arrayType->get_qualifiers(), arrayType->base };
		arrayType->base = nullptr;
		delete arrayType;
		return pointerType;
	}

	Type * AdjustExprType_old::postmutate( FunctionType * functionType ) {
		return new PointerType{ Type::Qualifiers(), functionType };
	}

	Type * AdjustExprType_old::postmutate( TypeInstType * typeInst ) {
		if ( const EqvClass * eqvClass = env.lookup( typeInst->get_name() ) ) {
			if ( eqvClass->data.kind == TypeDecl::Ftype ) {
				return new PointerType{ Type::Qualifiers(), typeInst };
			}
		} else if ( const NamedTypeDecl * ntDecl = indexer.lookupType( typeInst->get_name() ) ) {
			if ( const TypeDecl * tyDecl = dynamic_cast< const TypeDecl * >( ntDecl ) ) {
				if ( tyDecl->get_kind() == TypeDecl::Ftype ) {
					return new PointerType{ Type::Qualifiers(), typeInst };
				} // if
			} // if
		} // if
		return typeInst;
	}
} // anonymous namespace

void adjustExprType( Type *&type, const TypeEnvironment &env, const SymTab::Indexer &indexer ) {
	PassVisitor<AdjustExprType_old> adjuster( env, indexer );
	Type * newType = type->acceptMutator( adjuster );
	type = newType;
}

void adjustExprType( Type *& type ) {
	TypeEnvironment env;
	SymTab::Indexer indexer;
	adjustExprType( type, env, indexer );
}

namespace {
	class AdjustExprType_new final : public ast::WithShortCircuiting {
		const ast::SymbolTable & symtab;
	public:
		const ast::TypeEnvironment & tenv;

		AdjustExprType_new( const ast::TypeEnvironment & e, const ast::SymbolTable & syms )
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
			return new ast::PointerType{ at->base, at->qualifiers };
		}

		const ast::Type * postvisit( const ast::FunctionType * ft ) {
			return new ast::PointerType{ ft };
		}

		const ast::Type * postvisit( const ast::TypeInstType * inst ) {
			// replace known function-type-variables with pointer-to-function
			if ( const ast::EqvClass * eqvClass = tenv.lookup( *inst ) ) {
				if ( eqvClass->data.kind == ast::TypeDecl::Ftype ) {
					return new ast::PointerType{ inst };
				}
			} else if ( const ast::NamedTypeDecl * ntDecl = symtab.lookupType( inst->name ) ) {
				if ( auto tyDecl = dynamic_cast< const ast::TypeDecl * >( ntDecl ) ) {
					if ( tyDecl->kind == ast::TypeDecl::Ftype ) {
						return new ast::PointerType{ inst };
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
	ast::Pass<AdjustExprType_new> adjuster{ env, symtab };
	return type->accept( adjuster );
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
