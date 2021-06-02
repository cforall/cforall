//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PtrsAssignable.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 11:44:11 2015
// Last Modified By : Andrew
// Last Modified On : Mon Jun 24 15:29:00 2019
// Update Count     : 9
//

#include "typeops.h"

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/PassVisitor.h"
#include "ResolvExpr/TypeEnvironment.h"  // for EqvClass, TypeEnvironment
#include "SynTree/Type.h"                // for TypeInstType, Type, BasicType
#include "SynTree/Visitor.h"             // for Visitor


namespace ResolvExpr {
	struct PtrsAssignable : public WithShortCircuiting {
		PtrsAssignable( const Type * dest, const TypeEnvironment &env );

		int get_result() const { return result; }

		void previsit( const Type * ) { visit_children = false; }

		void postvisit( const VoidType * voidType );
		void postvisit( const BasicType * basicType );
		void postvisit( const PointerType * pointerType );
		void postvisit( const ArrayType * arrayType );
		void postvisit( const FunctionType * functionType );
		void postvisit( const StructInstType * inst );
		void postvisit( const UnionInstType * inst );
		void postvisit( const EnumInstType * inst );
		void postvisit( const TraitInstType * inst );
		void postvisit( const TypeInstType * inst );
		void postvisit( const TupleType * tupleType );
		void postvisit( const VarArgsType * varArgsType );
		void postvisit( const ZeroType * zeroType );
		void postvisit( const OneType * oneType );
	  private:
		const Type * dest;
		int result;
		const TypeEnvironment &env;
	};

	int ptrsAssignable( const Type *src, const Type * dest, const TypeEnvironment &env ) {
		// std::cerr << "assignable: " << src << " | " << dest << std::endl;
		if ( const TypeInstType * destAsTypeInst = dynamic_cast< const TypeInstType* >( dest ) ) {
			if ( const EqvClass * eqvClass = env.lookup( destAsTypeInst->get_name() ) ) {
				return ptrsAssignable( src, eqvClass->type, env );
			} // if
		} // if
		if ( dynamic_cast< const VoidType* >( dest ) ) {
			// void * = T * for any T is unsafe
			// xxx - this should be safe, but that currently breaks the build
			return -1;
		} else {
			PassVisitor<PtrsAssignable> ptrs( dest, env );
			src->accept( ptrs );
			return ptrs.pass.get_result();
		} // if
	}

	PtrsAssignable::PtrsAssignable( const Type * dest, const TypeEnvironment &env ) : dest( dest ), result( 0 ), env( env ) {}

	void PtrsAssignable::postvisit( const VoidType * ) {
		// T * = void * is disallowed - this is a change from C, where any
		// void * can be assigned or passed to a non-void pointer without a cast.
	}

	void PtrsAssignable::postvisit( const BasicType * ) {}
	void PtrsAssignable::postvisit( const PointerType * ) {}
	void PtrsAssignable::postvisit( const ArrayType * ) {}
	void PtrsAssignable::postvisit( const FunctionType * ) {}

	void PtrsAssignable::postvisit( const StructInstType * ) {}
	void PtrsAssignable::postvisit( const UnionInstType * ) {}

	void PtrsAssignable::postvisit( const EnumInstType * ) {
		if ( dynamic_cast< const BasicType* >( dest ) ) {
			// int * = E *, etc. is safe. This isn't technically correct, as each
			// enum has one basic type that it is compatible with, an that type can
			// differ from enum to enum. Without replicating GCC's internal logic,
			// there is no way to know which type this particular enum is compatible
			// with, so punt on this for now.
			result = 1;
		}
	}

	void PtrsAssignable::postvisit(  const TraitInstType * ) {}
	void PtrsAssignable::postvisit( const TypeInstType * inst ) {
		if ( const EqvClass * eqvClass = env.lookup( inst->name ) ) {
			if ( eqvClass->type ) {
				// T * = S * for any S depends on the type bound to T
				result = ptrsAssignable( eqvClass->type, dest, env );
			}
		} // if
	}

	void PtrsAssignable::postvisit( const TupleType * ) {}
	void PtrsAssignable::postvisit( const VarArgsType * ) {}
	void PtrsAssignable::postvisit( const ZeroType * ) {}
	void PtrsAssignable::postvisit( const OneType * ) {}

// TODO: Get rid of the `_new` suffix when the old version is removed.
struct PtrsAssignable_new : public ast::WithShortCircuiting {
	const ast::Type * dst;
	const ast::TypeEnvironment & typeEnv;
	int result;

	PtrsAssignable_new( const ast::Type * dst, const ast::TypeEnvironment & env ) :
		dst( dst ), typeEnv( env ), result( 0 ) {}

	void previsit( Type * ) { visit_children = false; }

	void postvisit( const ast::EnumInstType * ) {
		if ( dynamic_cast< const ast::BasicType * >( dst ) ) {
			// int * = E *, etc. is safe. This isn't technically correct, as each
			// enum has one basic type that it is compatible with, an that type can
			// differ from enum to enum. Without replicating GCC's internal logic,
			// there is no way to know which type this particular enum is compatible
			// with, so punt on this for now.
			result = 1;
		}
	}
	void postvisit( const ast::TypeInstType * inst ) {
		if ( const ast::EqvClass * eqv = typeEnv.lookup( *inst ) ) {
			if ( eqv->bound ) {
				// T * = S * for any S depends on the type bound to T
				result = ptrsAssignable( eqv->bound, dst, typeEnv );
			}
		}
	}
};

int ptrsAssignable( const ast::Type * src, const ast::Type * dst,
		const ast::TypeEnvironment & env ) {
	if ( const ast::TypeInstType * dstAsInst = dynamic_cast< const ast::TypeInstType * >( dst ) ) {
		if ( const ast::EqvClass * eqv = env.lookup( *dstAsInst ) ) {
			return ptrsAssignable( src, eqv->bound, env );
		}
	}
	if ( dynamic_cast< const ast::VoidType * >( dst ) ) {
		return -1;
	} else {
		ast::Pass<PtrsAssignable_new> visitor( dst, env );
		src->accept( visitor );
		return visitor.core.result;
	}

// see ticket #136 (this should be able to replace the visitor).
#if 0
	if ( const ast::TypeInstType * dstAsTypeInst =
			dynamic_cast< const ast::TypeInstType* >( dst ) ) {
		if ( const ast::EqvClass * eqv = env.lookup( dstAsTypeInst->get_name() ) ) {
			return ptrsAssignable( src, eqv->type, env );
		} // if
	} // if
	if ( dynamic_cast< VoidType* >( dst ) ) {
		// void * = T * for any T is unsafe
		// xxx - this should be safe, but that currently breaks the build
		return -1;
	} else if ( dynamic_cast< EnumInstType * >( src ) ) {
		if ( dynamic_cast< BasicType * >( dst ) ) {
			// int * = E *, etc. is safe. This isn't technically correct, as each
			// enum has one basic type that it is compatible with, an that type can
			// differ from enum to enum. Without replicating GCC's internal logic,
			// there is no way to know which type this particular enum is compatible
			// with, so punt on this for now.
			return 1;
		}
	} else if ( const ast::TypeInstType * typeInstType =
			dynamic_cast< const ast::TypeInstType * >( src ) ) {
		if ( const ast::EqvClass * eqv = env.lookup( typeInstType->name ) ) {
			if ( eqv->bound ) {
				// T * = S * for any S depends on the type bound to T
				return ptrsAssignable( eqv->bound, dst, env );
			}
		}
	}
	return 0;
#endif
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
