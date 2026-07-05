//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FixGlobalInit.cpp --
//
// Author           : Rob Schluntz
// Created On       : Mon May 04 15:14:56 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul  3 08:41:54 2026
// Update Count     : 37
//

#include "FixGlobalInit.hpp"

#include <set>
#include <cassert>                 // for assert
#include <stddef.h>                // for NULL
#include <algorithm>               // for replace_if

#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "Common/UniqueName.hpp"   // for UniqueName
#include "InitTweak.hpp"           // for isIntrinsicSingleArgCallStmt

namespace InitTweak {

namespace {

class GlobalFixer : public ast::WithShortCircuiting {
public:
	void previsit(const ast::ObjectDecl *);
	void previsit(const ast::FunctionDecl *) { visit_children = false; }
	void previsit(const ast::StructDecl *) { visit_children = false; }
	void previsit(const ast::UnionDecl *) { visit_children = false; }
	void previsit(const ast::EnumDecl *) { visit_children = false; }
	void previsit(const ast::TraitDecl *) { visit_children = false; }
	void previsit(const ast::TypeDecl *) { visit_children = false; }

	bool pass = false;
	std::list< ast::ptr<ast::Stmt> > initStmts;
	std::list< ast::ptr<ast::Stmt> > destroyStmts;
	std::set< std::string > constDeclsMnames;
};

void GlobalFixer::previsit(const ast::ObjectDecl * objDecl) {
	auto mutDecl = mutate(objDecl);
	assertf(mutDecl == objDecl, "Global object decl must be unique");
	if ( ! pass ) {										// pass 1
		auto ctorInit = objDecl->init.as<ast::ConstructorInit>();
		if ( nullptr == ctorInit ) return;

		// a decision should have been made by the resolver, so ctor and init are not both non-NULL
		assert( !ctorInit->ctor || !ctorInit->init );

		const ast::Stmt * dtor = ctorInit->dtor;
		if ( dtor && !isIntrinsicSingleArgCallStmt( dtor ) ) {
			// don't need to call intrinsic dtor, because it does nothing, but
			// non-intrinsic dtors must be called
			destroyStmts.push_front( dtor );
		} // if
		if ( const ast::Stmt * ctor = ctorInit->ctor ) {
			if ( mutDecl->type->is_const() ) {
				// Store names of all declarations with const qualifier and initializer for second pass.
				constDeclsMnames.emplace( mutDecl->mangleName );
			}
			initStmts.push_back( ctor );
			mutDecl->init = nullptr;
		} else if ( const ast::Init * init = ctorInit->init ) {
			mutDecl->init = init;
		} else {
			// no constructor and no initializer, which is okay
			mutDecl->init = nullptr;
		}
	} else {										// pass 2
		// Remove const qualifier from matching names, covering all forward declaration(s) and definition.
		if ( constDeclsMnames.find( objDecl->mangleName ) != constDeclsMnames.end() ) {
			// SKULLDUGGERY: change value through const pointer.
			const_cast<ast::Type *>(objDecl->get_type())->set_const( false );
		} // if
	}
}

} // namespace

void fixGlobalInit(ast::TranslationUnit & translationUnit, bool inLibrary) {
	ast::Pass<GlobalFixer> fixer;

	// First pass fixes global initialization.
	accept_all( translationUnit, fixer );

	// Say these magic declarations come at the end of the file.
	CodeLocation const & location = translationUnit.decls.back()->location;

	if ( !fixer.core.initStmts.empty() ) {
		std::vector<ast::ptr<ast::Expr>> ctorParams;
		if (inLibrary) ctorParams.emplace_back(ast::ConstantExpr::from_int(location, 200));
		auto initFunction = new ast::FunctionDecl(location,
			"__global_init__", {}, {}, {}, {},
			new ast::CompoundStmt(location, std::move(fixer.core.initStmts)),
			ast::Storage::Static, ast::Linkage::C,
			{new ast::Attribute("constructor", std::move(ctorParams))});

		translationUnit.decls.emplace_back( initFunction );
	} // if

	if ( !fixer.core.destroyStmts.empty() ) {
		std::vector<ast::ptr<ast::Expr>> dtorParams;
		if (inLibrary) dtorParams.emplace_back(ast::ConstantExpr::from_int(location, 200));
		auto destroyFunction = new ast::FunctionDecl( location,
			"__global_destroy__", {}, {}, {}, {},
			new ast::CompoundStmt(location, std::move(fixer.core.destroyStmts)),
			ast::Storage::Static, ast::Linkage::C,
			{new ast::Attribute("destructor", std::move(dtorParams))});

		translationUnit.decls.emplace_back(destroyFunction);
	} // if

	fixer.core.pass = ! fixer.core.pass;
	// Second pass, removes const qualifiers so declarations appear in mutable .data section.
	// Note, the resolver has already checked for const-ness, so C only has to get the initialization correct.
	accept_all(translationUnit, fixer);
}

} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
