//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindSpecialDecls.cc --
//
// Author           : Rob Schluntz
// Created On       : Thu Aug 30 09:49:43 2018
// Last Modified By : Rob Schluntz
// Last Modified On : Thu Aug 30 09:55:25 2018
// Update Count     : 2
//

#include "FindSpecialDecls.h"

#include "Common/PassVisitor.h"
#include "SynTree/Declaration.h"
#include "SynTree/Type.h"

// NOTE: currently, it is assumed that every special declaration occurs at the top-level,
// so function bodies, aggregate bodies, object initializers, etc. are not visited.
// If this assumption changes, e.g., with the introduction of namespaces, remove the visit_children assignments.

namespace Validate {
	Type * SizeType = nullptr;
	FunctionDecl * dereferenceOperator = nullptr;
	StructDecl * dtorStruct = nullptr;
	FunctionDecl * dtorStructDestroy = nullptr;

	namespace {
		struct FindSpecialDecls final : public WithShortCircuiting {
			void previsit( ObjectDecl * objDecl );
			void previsit( FunctionDecl * funcDecl );
			void previsit( StructDecl * structDecl );
			void previsit( UnionDecl * unionDecl );
			void previsit( EnumDecl * enumDecl );
			void previsit( TraitDecl * traitDecl );
		};
	} // namespace

	void findSpecialDecls( std::list< Declaration * > &translationUnit ) {
		PassVisitor<FindSpecialDecls> finder;
		acceptAll( translationUnit, finder );
		// TODO: conditionally generate 'fake' declarations for missing features, so that
		// translation can proceed in the event that builtins, prelude, etc. are missing.
	}

	namespace {
		void FindSpecialDecls::previsit( ObjectDecl * ) {
			visit_children = false;
		}

		void FindSpecialDecls::previsit( FunctionDecl * funcDecl ) {
			visit_children = false;
			if ( ! dereferenceOperator && funcDecl->name == "*?" && funcDecl->linkage == LinkageSpec::Intrinsic ) {
				// find and remember the intrinsic dereference operator for object pointers
				FunctionType * ftype = funcDecl->type;
				if ( ftype->parameters.size() == 1 ) {
					PointerType * ptrType = strict_dynamic_cast<PointerType *>( ftype->parameters.front()->get_type() );
					if ( ptrType->base->get_qualifiers() == Type::Qualifiers() ) {
						TypeInstType * inst = dynamic_cast<TypeInstType *>( ptrType->base );
						if ( inst && ! inst->get_isFtype() ) {
							dereferenceOperator = funcDecl;
						}
					}
				}
			} else if ( ! dtorStructDestroy && funcDecl->name == "__destroy_Destructor" ) {
				dtorStructDestroy = funcDecl;
			}
		}

		void FindSpecialDecls::previsit( StructDecl * structDecl ) {
			visit_children = false;
			if ( ! dtorStruct && structDecl->name == "__Destructor" ) {
				dtorStruct = structDecl;
			}
		}

		void FindSpecialDecls::previsit( UnionDecl * ) {
			visit_children = false;
		}

		void FindSpecialDecls::previsit( EnumDecl * ) {
			visit_children = false;
		}

		void FindSpecialDecls::previsit( TraitDecl * ) {
			visit_children = false;
		}

	} // namespace
} // namespace Validate

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
