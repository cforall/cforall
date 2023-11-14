//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixGlobalInit.cc --
//
// Author           : Rob Schluntz
// Created On       : Mon May 04 15:14:56 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:41:10 2019
// Update Count     : 19
//

#include "FixGlobalInit.h"

#include <cassert>                 // for assert
#include <stddef.h>                // for NULL
#include <algorithm>               // for replace_if

#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "Common/UniqueName.h"     // for UniqueName
#include "InitTweak.h"             // for isIntrinsicSingleArgCallStmt

namespace InitTweak {
	class GlobalFixer : public ast::WithShortCircuiting {
	public:
		void previsit (const ast::ObjectDecl *);
		void previsit (const ast::FunctionDecl *) { visit_children = false; }
		void previsit (const ast::StructDecl *) { visit_children = false; }
		void previsit (const ast::UnionDecl *) { visit_children = false; }
		void previsit (const ast::EnumDecl *) { visit_children = false; }
		void previsit (const ast::TraitDecl *) { visit_children = false; }
		void previsit (const ast::TypeDecl *) { visit_children = false; }

		std::list< ast::ptr<ast::Stmt> > initStmts;
		std::list< ast::ptr<ast::Stmt> > destroyStmts;
	};

	void fixGlobalInit(ast::TranslationUnit & translationUnit, bool inLibrary) {
		ast::Pass<GlobalFixer> fixer;
		accept_all(translationUnit, fixer);

		// Say these magic declarations come at the end of the file.
		CodeLocation const & location = translationUnit.decls.back()->location;

		if ( !fixer.core.initStmts.empty() ) {
			std::vector<ast::ptr<ast::Expr>> ctorParams;
			if (inLibrary) ctorParams.emplace_back(ast::ConstantExpr::from_int(location, 200));
			auto initFunction = new ast::FunctionDecl(location,
				"__global_init__", {}, {}, {},
				new ast::CompoundStmt(location, std::move(fixer.core.initStmts)),
				ast::Storage::Static, ast::Linkage::C,
				{new ast::Attribute("constructor", std::move(ctorParams))});

			translationUnit.decls.emplace_back( initFunction );
		} // if

		if ( !fixer.core.destroyStmts.empty() ) {
			std::vector<ast::ptr<ast::Expr>> dtorParams;
			if (inLibrary) dtorParams.emplace_back(ast::ConstantExpr::from_int(location, 200));
			auto destroyFunction = new ast::FunctionDecl( location,
				"__global_destroy__", {}, {}, {},
				new ast::CompoundStmt(location, std::move(fixer.core.destroyStmts)),
				ast::Storage::Static, ast::Linkage::C,
				{new ast::Attribute("destructor", std::move(dtorParams))});

			translationUnit.decls.emplace_back(destroyFunction);
		} // if
	}

	void GlobalFixer::previsit(const ast::ObjectDecl * objDecl) {
		auto mutDecl = mutate(objDecl);
		assertf(mutDecl == objDecl, "Global object decl must be unique");
		if ( auto ctorInit = objDecl->init.as<ast::ConstructorInit>() ) {
			// a decision should have been made by the resolver, so ctor and init are not both non-NULL
			assert( ! ctorInit->ctor || ! ctorInit->init );

			const ast::Stmt * dtor = ctorInit->dtor;
			if ( dtor && ! isIntrinsicSingleArgCallStmt( dtor ) ) {
				// don't need to call intrinsic dtor, because it does nothing, but
				// non-intrinsic dtors must be called
				destroyStmts.push_front( dtor );
				// ctorInit->dtor = nullptr;
			} // if
			if ( const ast::Stmt * ctor = ctorInit->ctor ) {
				addDataSectionAttribute(mutDecl);
				initStmts.push_back( ctor );
				mutDecl->init = nullptr;
				// ctorInit->ctor = nullptr;
			} else if ( const ast::Init * init = ctorInit->init ) {
				mutDecl->init = init;
				// ctorInit->init = nullptr;
			} else {
				// no constructor and no initializer, which is okay
				mutDecl->init = nullptr;
			} // if
			// delete ctorInit;
		} // if
	}

} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
