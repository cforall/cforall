//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MakeLibCfa.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sat May 16 10:33:33 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Feb 16 03:49:49 2020
// Update Count     : 45
//

#include "MakeLibCfa.h"

#include <cassert>                  // for assert
#include <string>                   // for operator==, string

#include "CodeGen/OperatorTable.h"  // for OperatorInfo, operatorLookup, Ope...
#include "Common/PassVisitor.h"     // for PassVisitor
#include "Common/SemanticError.h"   // for SemanticError
#include "Common/UniqueName.h"      // for UniqueName
#include "SynTree/LinkageSpec.h"    // for Spec, Intrinsic, C
#include "SynTree/Declaration.h"    // for FunctionDecl, ObjectDecl, Declara...
#include "SynTree/Expression.h"     // for NameExpr, UntypedExpr, VariableExpr
#include "SynTree/Initializer.h"    // for SingleInit
#include "SynTree/Label.h"          // for Label
#include "SynTree/Statement.h"      // for CompoundStmt, ReturnStmt
#include "SynTree/Type.h"           // for FunctionType
#include "SynTree/Visitor.h"        // for acceptAll, Visitor

namespace LibCfa {
	namespace {
		struct MakeLibCfa {
		  public:
			void postvisit( FunctionDecl* funcDecl );

			std::list< Declaration* > newDecls;
		};
	}

	void makeLibCfa( std::list< Declaration* > &prelude ) {
		PassVisitor<MakeLibCfa> maker;
		acceptAll( prelude, maker );
		prelude.splice( prelude.end(), maker.pass.newDecls );
	}

	namespace {
		struct TypeFinder	{
			void postvisit( TypeInstType * inst ) {
				// if a type variable is seen, assume all zero_t/one_t in the parameter list
				//  can be replaced with the equivalent 'general' pointer.
				if ( type ) return;
				if ( inst->isFtype ) {
					type = new PointerType( Type::Qualifiers(), new FunctionType( Type::Qualifiers(), false ) );
				} else {
					type = new PointerType( Type::Qualifiers(), new VoidType( Type::Qualifiers() ) );
				}
			}
			Type * type = nullptr;
		};

		struct ZeroOneReplacer {
			ZeroOneReplacer( Type * t ) : type( t ) {}
			~ZeroOneReplacer() { delete type; }
			Type * type = nullptr;

			Type * common( Type * t ) {
				if ( ! type ) return t;
				delete t;
				return type->clone();
			}

			Type * postmutate( OneType * t ) { return common( t ); }
			Type * postmutate( ZeroType * t ) { return common( t ); }
		};

		void fixZeroOneType( FunctionDecl * origFuncDecl ) {
			// find appropriate type to replace zero_t/one_t with
			PassVisitor<TypeFinder> finder;
			origFuncDecl->type->accept( finder );
			// replace zero_t/one_t in function type
			PassVisitor<ZeroOneReplacer> replacer( finder.pass.type );
			origFuncDecl->type->acceptMutator( replacer );
		}

		void MakeLibCfa::postvisit( FunctionDecl* origFuncDecl ) {
			// don't change non-intrinsic functions
			if ( origFuncDecl->get_linkage() != LinkageSpec::Intrinsic ) return;
			// replace zero_t/one_t with void */void (*)(void)
			fixZeroOneType( origFuncDecl );
			// skip functions already defined
			if ( origFuncDecl->get_statements() ) return;

			FunctionDecl *funcDecl = origFuncDecl->clone();
			const CodeGen::OperatorInfo * opInfo;
			opInfo = CodeGen::operatorLookup( funcDecl->get_name() );
			assert( opInfo );
			assert( ! funcDecl->get_statements() );
			// build a recursive call - this is okay, as the call will actually be codegen'd using operator syntax
			UntypedExpr *newExpr = new UntypedExpr( new NameExpr( funcDecl->get_name() ) );
			UniqueName paramNamer( "_p" );
			std::list< DeclarationWithType* >::iterator param = funcDecl->get_functionType()->get_parameters().begin();
			assert( param != funcDecl->get_functionType()->get_parameters().end() );

			for ( ; param != funcDecl->get_functionType()->get_parameters().end(); ++param ) {
				// name each unnamed parameter
				if ( (*param)->get_name() == "" ) {
					(*param)->set_name( paramNamer.newName() );
					(*param)->set_linkage( LinkageSpec::C );
				}
				// add parameter to the expression
				newExpr->get_args().push_back( new VariableExpr( *param ) );
			} // for

			funcDecl->set_statements( new CompoundStmt() );
			newDecls.push_back( funcDecl );

			Statement * stmt = nullptr;
			switch ( opInfo->type ) {
			  case CodeGen::OT_INDEX:
			  case CodeGen::OT_CALL:
			  case CodeGen::OT_PREFIX:
			  case CodeGen::OT_POSTFIX:
			  case CodeGen::OT_INFIX:
			  case CodeGen::OT_PREFIXASSIGN:
			  case CodeGen::OT_POSTFIXASSIGN:
			  case CodeGen::OT_INFIXASSIGN:
					// return the recursive call
					stmt = new ReturnStmt( newExpr );
					break;
			  case CodeGen::OT_CTOR:
			  case CodeGen::OT_DTOR:
					// execute the recursive call
					stmt = new ExprStmt( newExpr );
					break;
			  case CodeGen::OT_CONSTANT:
			  case CodeGen::OT_LABELADDRESS:
				// there are no intrinsic definitions of 0/1 or label addresses as functions
				assert( false );
			} // switch
			funcDecl->get_statements()->push_back( stmt );
		}
	} // namespace
} // namespace LibCfa

// Local Variables: //
// tab-width: 4 //
// End: //
