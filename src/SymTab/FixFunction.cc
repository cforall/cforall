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
#include "Common/utility.h"       // for maybeClone, copy
#include "SynTree/Declaration.h"  // for FunctionDecl, ObjectDecl, Declarati...
#include "SynTree/Expression.h"   // for Expression
#include "SynTree/Type.h"         // for ArrayType, PointerType, Type, Basic...

namespace SymTab {
	class FixFunction_old : public WithShortCircuiting {
		typedef Mutator Parent;
	  public:
		FixFunction_old() : isVoid( false ) {}

		void premutate(FunctionDecl *functionDecl);
		DeclarationWithType* postmutate(FunctionDecl *functionDecl);

		Type * postmutate(ArrayType * arrayType);

		void premutate(ArrayType * arrayType);
		void premutate(VoidType * voidType);
		void premutate(BasicType * basicType);
		void premutate(PointerType * pointerType);
		void premutate(StructInstType * aggregateUseType);
		void premutate(UnionInstType * aggregateUseType);
		void premutate(EnumInstType * aggregateUseType);
		void premutate(TraitInstType * aggregateUseType);
		void premutate(TypeInstType * aggregateUseType);
		void premutate(TupleType * tupleType);
		void premutate(VarArgsType * varArgsType);
		void premutate(ZeroType * zeroType);
		void premutate(OneType * oneType);

		bool isVoid;
	};

	DeclarationWithType * FixFunction_old::postmutate(FunctionDecl *functionDecl) {
		// can't delete function type because it may contain assertions, so transfer ownership to new object
		ObjectDecl *pointer = new ObjectDecl( functionDecl->name, functionDecl->get_storageClasses(), functionDecl->linkage, nullptr, new PointerType( Type::Qualifiers(), functionDecl->type ), nullptr, functionDecl->attributes );
		pointer->location = functionDecl->location;
		functionDecl->attributes.clear();
		functionDecl->type = nullptr;
		delete functionDecl;
		return pointer;
	}

	// xxx - this passes on void[], e.g.
	//   void foo(void [10]);
	// does not cause an error

	Type * FixFunction_old::postmutate(ArrayType *arrayType) {
		// need to recursively mutate the base type in order for multi-dimensional arrays to work.
		PointerType *pointerType = new PointerType( arrayType->get_qualifiers(), arrayType->base, arrayType->dimension, arrayType->isVarLen, arrayType->isStatic );
		pointerType->location = arrayType->location;
		arrayType->base = nullptr;
		arrayType->dimension = nullptr;
		delete arrayType;
		return pointerType;
	}

	void FixFunction_old::premutate(VoidType *) {
		isVoid = true;
	}

	void FixFunction_old::premutate(FunctionDecl *) { visit_children = false; }
	void FixFunction_old::premutate(ArrayType *) { visit_children = false; }
	void FixFunction_old::premutate(BasicType *) { visit_children = false; }
	void FixFunction_old::premutate(PointerType *) { visit_children = false; }
	void FixFunction_old::premutate(StructInstType *) { visit_children = false; }
	void FixFunction_old::premutate(UnionInstType *) { visit_children = false; }
	void FixFunction_old::premutate(EnumInstType *) { visit_children = false; }
	void FixFunction_old::premutate(TraitInstType *) { visit_children = false; }
	void FixFunction_old::premutate(TypeInstType *) { visit_children = false; }
	void FixFunction_old::premutate(TupleType *) { visit_children = false; }
	void FixFunction_old::premutate(VarArgsType *) { visit_children = false; }
	void FixFunction_old::premutate(ZeroType *) { visit_children = false; }
	void FixFunction_old::premutate(OneType *) { visit_children = false; }

	bool fixFunction( DeclarationWithType *& dwt ) {
		PassVisitor<FixFunction_old> fixer;
		dwt = dwt->acceptMutator( fixer );
		return fixer.pass.isVoid;
	}

namespace {
	struct FixFunction_new final : public ast::WithShortCircuiting {
		bool isVoid = false;

		void previsit( const ast::FunctionDecl * ) { visit_children = false; }

		const ast::DeclWithType * postvisit( const ast::FunctionDecl * func ) {
			return new ast::ObjectDecl{ 
				func->location, func->name, new ast::PointerType{ func->type }, nullptr, 
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
	ast::Pass< FixFunction_new > fixer;
	dwt = dwt->accept( fixer );
	isVoid |= fixer.core.isVoid;
	return dwt;
}

const ast::Type * fixFunction( const ast::Type * type, bool & isVoid ) {
	ast::Pass< FixFunction_new > fixer;
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
