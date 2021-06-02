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

#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/PassVisitor.h"
#include "ResolvExpr/TypeEnvironment.h"  // for EqvClass, TypeEnvironment
#include "SymTab/Indexer.h"              // for Indexer
#include "SynTree/Declaration.h"         // for TypeDecl, TypeDecl::Kind::Ftype
#include "SynTree/Type.h"                // for TypeInstType, Type, BasicType
#include "SynTree/Visitor.h"             // for Visitor
#include "typeops.h"                     // for ptrsAssignable

namespace ResolvExpr {
	struct PtrsCastable_old : public WithShortCircuiting  {
	  public:
		PtrsCastable_old( const Type * dest, const TypeEnvironment &env, const SymTab::Indexer &indexer );

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
		const SymTab::Indexer &indexer;
	};

	namespace {
		int objectCast( const Type * src, const TypeEnvironment &env, const SymTab::Indexer &indexer ) {
			if ( dynamic_cast< const FunctionType* >( src ) ) {
				return -1;
			} else if ( const TypeInstType * typeInst = dynamic_cast< const TypeInstType* >( src ) ) {
				if ( const NamedTypeDecl * ntDecl = indexer.lookupType( typeInst->name ) ) {
					if ( const TypeDecl * tyDecl = dynamic_cast< const TypeDecl* >( ntDecl ) ) {
						if ( tyDecl->kind == TypeDecl::Ftype ) {
							return -1;
						} // if
					} //if
				} else if ( const EqvClass * eqvClass = env.lookup( typeInst->get_name() ) ) {
					if ( eqvClass->data.kind == TypeDecl::Ftype ) {
						return -1;
					} // if
				} // if
			} //if
			return 1;
		}
		int functionCast( const Type * src, const TypeEnvironment &env, const SymTab::Indexer &indexer ) {
			return -1 * objectCast( src, env, indexer );  // reverse the sense of objectCast
		}
	}

	int ptrsCastable( const Type * src, const Type * dest, const TypeEnvironment &env, const SymTab::Indexer &indexer ) {
		if ( const TypeInstType * destAsTypeInst = dynamic_cast< const TypeInstType* >( dest ) ) {
			if ( const EqvClass * eqvClass = env.lookup( destAsTypeInst->get_name() ) ) {
				// xxx - should this be ptrsCastable?
				return ptrsAssignable( src, eqvClass->type, env );
			} // if
		} // if
		if ( dynamic_cast< const VoidType* >( dest ) ) {
			return objectCast( src, env, indexer );
		} else {
			PassVisitor<PtrsCastable_old> ptrs( dest, env, indexer );
			src->accept( ptrs );
			return ptrs.pass.get_result();
		} // if
	}

	PtrsCastable_old::PtrsCastable_old( const Type * dest, const TypeEnvironment &env, const SymTab::Indexer &indexer )
		: dest( dest ), result( 0 ), env( env ), indexer( indexer )	{
	}

	void PtrsCastable_old::postvisit( const VoidType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const BasicType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const PointerType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const ArrayType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const FunctionType * ) {
		// result = -1;
		result = functionCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const StructInstType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const UnionInstType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const EnumInstType * ) {
		if ( dynamic_cast< const EnumInstType * >( dest ) ) {
			result = 1;
		} else if ( const BasicType * bt = dynamic_cast< const BasicType * >( dest ) ) {
			if ( bt->kind == BasicType::SignedInt ) {
				result = 0;
			} else {
				result = 1;
			}
		} else {
			result = objectCast( dest, env, indexer );
		}
	}

	void PtrsCastable_old::postvisit( const TraitInstType * ) {}

	void PtrsCastable_old::postvisit( const TypeInstType *inst ) {
		//result = objectCast( inst, env, indexer ) > 0 && objectCast( dest, env, indexer ) > 0 ? 1 : -1;
		result = objectCast( inst, env, indexer ) == objectCast( dest, env, indexer ) ? 1 : -1;
	}

	void PtrsCastable_old::postvisit( const TupleType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const VarArgsType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const ZeroType * ) {
		result = objectCast( dest, env, indexer );
	}

	void PtrsCastable_old::postvisit( const OneType * ) {
		result = objectCast( dest, env, indexer );
	}

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

	class PtrsCastable_new : public ast::WithShortCircuiting {
		const ast::Type * dst;
		const ast::TypeEnvironment & env;
		const ast::SymbolTable & symtab;
	public:
		int result;

		PtrsCastable_new(
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
		ast::Pass< PtrsCastable_new > ptrs{ dst, env, symtab };
		src->accept( ptrs );
		return ptrs.core.result;
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
