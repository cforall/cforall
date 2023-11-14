//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PtrsCastable.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 11:48:00 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Wed Dec 11 21:48:33 2019
// Update Count     : 9
//

#include "PtrsCastable.hpp"

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "ResolvExpr/PtrsAssignable.hpp" // for ptrsAssignable

namespace ResolvExpr {

namespace {
	// can this type be cast to an object (1 for yes, -1 for no)
	int objectCast(
		const ast::Type * src, const ast::TypeEnvironment & env, const ast::SymbolTable & symtab
	) {
		if ( dynamic_cast< const ast::FunctionType * >( src ) ) {
			return -1;
		} else if ( auto inst = dynamic_cast< const ast::TypeInstType * >( src ) ) {
			if ( const ast::NamedTypeDecl * named = symtab.lookupType( inst->name ) ) {
				if ( auto tyDecl = dynamic_cast< const ast::TypeDecl * >( named ) ) {
					if ( tyDecl->kind == ast::TypeDecl::Ftype ) {
						return -1;
					}
				}
			} else if ( const ast::EqvClass * eqvClass = env.lookup( *inst ) ) {
				if ( eqvClass->data.kind == ast::TypeDecl::Ftype ) {
					return -1;
				}
			}
		}

		return 1;
	}

	// can this type be cast to a function (inverse of objectCast)
	int functionCast(
		const ast::Type * src, const ast::TypeEnvironment & env, const ast::SymbolTable & symtab
	) {
		return -1 * objectCast( src, env, symtab );
	}

	class PtrsCastable : public ast::WithShortCircuiting {
		const ast::Type * dst;
		const ast::TypeEnvironment & env;
		const ast::SymbolTable & symtab;
	public:
		int result;

		PtrsCastable(
			const ast::Type * d, const ast::TypeEnvironment & e, const ast::SymbolTable & syms )
		: dst( d ), env( e ), symtab( syms ), result( 0 ) {}

		void previsit( const ast::Type * ) { visit_children = false; }

		void postvisit( const ast::VoidType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::BasicType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::PointerType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::ArrayType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::FunctionType * ) {
			result = functionCast( dst, env, symtab );
		}

		void postvisit( const ast::StructInstType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::UnionInstType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::EnumInstType * ) {
			if ( dynamic_cast< const ast::EnumInstType * >( dst ) ) {
				result = 1;
			} else if ( auto bt = dynamic_cast< const ast::BasicType * >( dst ) ) {
				if ( bt->kind == ast::BasicType::SignedInt ) {
					result = 0;
				} else {
					result = 1;
				}
			} else {
				result = objectCast( dst, env, symtab );
			}
		}

		void postvisit( const ast::TraitInstType * ) {}

		void postvisit( const ast::TypeInstType * inst ) {
			// check trait and destination type are both object or both function
			result = objectCast( inst, env, symtab ) == objectCast( dst, env, symtab ) ? 1 : -1;
		}

		void postvisit( const ast::TupleType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::VarArgsType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::ZeroType * ) {
			result = objectCast( dst, env, symtab );
		}

		void postvisit( const ast::OneType * ) {
			result = objectCast( dst, env, symtab );
		}

	};
} // anonymous namespace

int ptrsCastable(
	const ast::Type * src, const ast::Type * dst, const ast::SymbolTable & symtab,
	const ast::TypeEnvironment & env
) {
	if ( auto inst = dynamic_cast< const ast::TypeInstType * >( dst ) ) {
		if ( const ast::EqvClass * eqvClass = env.lookup( *inst ) ) {
			return ptrsAssignable( src, eqvClass->bound, env );
		}
	}

	if ( dynamic_cast< const ast::VoidType * >( dst ) ) {
		return objectCast( src, env, symtab );
	} else {
		return ast::Pass<PtrsCastable>::read( src, dst, env, symtab );
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
