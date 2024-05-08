//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// MakeLibCfa.cpp --
//
// Author           : Henry Xue
// Created On       : Fri Aug 27 15:50:14 2021
// Last Modified By : Henry Xue
// Last Modified On : Fri Aug 27 15:50:14 2021
// Update Count     : 1
//

#include "MakeLibCfa.hpp"

#include "AST/Copy.hpp"
#include "AST/Fwd.hpp"
#include "AST/Pass.hpp"
#include "CodeGen/OperatorTable.h"
#include "Common/UniqueName.h"

namespace LibCfa {
namespace {
	struct MakeLibCfa {
		const ast::DeclWithType * postvisit( const ast::FunctionDecl * funcDecl );
		std::list< ast::ptr< ast::Decl > > newDecls;
	};
}

void makeLibCfa( ast::TranslationUnit & prelude ) {
	ast::Pass< MakeLibCfa > maker;
	accept_all( prelude, maker );
	prelude.decls.splice( prelude.decls.end(), maker.core.newDecls );
}

namespace {
	struct TypeFinder {
		void postvisit( const ast::TypeInstType * inst ) {
			// if a type variable is seen, assume all zero_t/one_t in the parameter list
			//  can be replaced with the equivalent 'general' pointer.
			if ( type ) return;
			if ( inst->kind == ast::TypeDecl::Ftype ) {
				type = new ast::PointerType( new ast::FunctionType() );
			} else {
				type = new ast::PointerType( new ast::VoidType() );
			}
		}
		ast::ptr< ast::Type > type;
	};

	struct ZeroOneReplacer {
		ZeroOneReplacer( const ast::Type * t ) : type( t ) {}
		ast::ptr< ast::Type > type;

		const ast::Type * common( const ast::Type * t ) {
			if ( ! type ) return t;
			return type;
		}

		const ast::Type * postvisit( const ast::OneType * t ) { return common( t ); }
		const ast::Type * postvisit( const ast::ZeroType * t ) { return common( t ); }
	};

	// const ast::Type * fixZeroOneType( ast::FunctionDecl * origFuncDecl ) {
	// 	// find appropriate type to replace zero_t/one_t with
	// 	ast::Pass< TypeFinder > finder;
	// 	origFuncDecl->type->accept( finder );
	// 	// replace zero_t/one_t in function type
	// 	ast::Pass< ZeroOneReplacer > replacer( finder.core.type );
	// 	//auto funcDecl = mutate( origFuncDecl );
	// 	return origFuncDecl->type->accept( replacer );
	// }

	const ast::DeclWithType * MakeLibCfa::postvisit( const ast::FunctionDecl * origFuncDecl ) {
		// don't change non-intrinsic functions
		if ( origFuncDecl->linkage != ast::Linkage::Intrinsic ) return origFuncDecl;
		// replace zero_t/one_t with void */void (*)(void)
		auto mutDecl = mutate( origFuncDecl );
		//mutDecl->type = fixZeroOneType( mutDecl );

		// find appropriate type to replace zero_t/one_t with
		ast::Pass< TypeFinder > finder;
		mutDecl->type->accept( finder );
		// replace zero_t/one_t in function type
		ast::Pass< ZeroOneReplacer > replacer( finder.core.type );
		mutDecl->type = mutDecl->type->accept( replacer );

		// skip functions already defined
		if ( mutDecl->has_body() ) return mutDecl;

		const CodeLocation loc = mutDecl->location;
		auto funcDecl = dynamic_cast<ast::FunctionDecl *>(ast::deepCopy( (ast::DeclWithType*)mutDecl ));
		const CodeGen::OperatorInfo * opInfo;
		opInfo = CodeGen::operatorLookup( funcDecl->name );
		assert( opInfo );
		assert( ! funcDecl->has_body() );
		// build a recursive call - this is okay, as the call will actually be codegen'd using operator syntax
		auto newExpr = new ast::UntypedExpr( loc, new ast::NameExpr( loc, funcDecl->name ) );
		UniqueName paramNamer( "_p" );
		auto param = funcDecl->params.begin();
		assert( param != funcDecl->params.end() );

		for ( ; param != funcDecl->params.end(); ++param ) {
			// name each unnamed parameter
			if ( (*param)->name == "" ) {
				auto _param = param->get_and_mutate();
				_param->name = paramNamer.newName() ;
				_param->linkage = ast::Linkage::C;
			}
			// add parameter to the expression
			newExpr->args.push_back( new ast::VariableExpr( loc, *param ) );
		} // for

		auto stmts = new ast::CompoundStmt( loc );;
		newDecls.push_back( funcDecl );

		ast::ptr< ast::Stmt > stmt;
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
				stmt = new ast::ReturnStmt( loc, newExpr );
				break;
			case CodeGen::OT_CTOR:
			case CodeGen::OT_DTOR:
				// execute the recursive call
				stmt = new ast::ExprStmt( loc, newExpr );
				break;
			case CodeGen::OT_CONSTANT:
			case CodeGen::OT_LABELADDRESS:
			// there are no intrinsic definitions of 0/1 or label addresses as functions
			assert( false );
		} // switch
		stmts->push_back( stmt );
		funcDecl->stmts = stmts;
		return mutDecl;
	}
} // namespace
} // namespace LibCfa
