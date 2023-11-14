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

#include "PtrsAssignable.hpp"

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"

namespace ResolvExpr {

struct PtrsAssignable : public ast::WithShortCircuiting {
	const ast::Type * dst;
	const ast::TypeEnvironment & typeEnv;
	int result;

	PtrsAssignable( const ast::Type * dst, const ast::TypeEnvironment & env ) :
		dst( dst ), typeEnv( env ), result( 0 ) {}

	void previsit( ast::Type * ) { visit_children = false; }

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
		ast::Pass<PtrsAssignable> visitor( dst, env );
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
