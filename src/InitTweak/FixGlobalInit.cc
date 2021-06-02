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

#include "Common/PassVisitor.h"
#include "Common/UniqueName.h"     // for UniqueName
#include "InitTweak.h"             // for isIntrinsicSingleArgCallStmt
#include "SynTree/LinkageSpec.h"   // for C
#include "SynTree/Attribute.h"     // for Attribute
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for FunctionDecl, ObjectDecl, Declaration
#include "SynTree/Expression.h"    // for ConstantExpr, Expression (ptr only)
#include "SynTree/Initializer.h"   // for ConstructorInit, Initializer
#include "SynTree/Label.h"         // for Label
#include "SynTree/Statement.h"     // for CompoundStmt, Statement (ptr only)
#include "SynTree/Type.h"          // for Type, Type::StorageClasses, Functi...
#include "SynTree/Visitor.h"       // for acceptAll, Visitor

#include "AST/Expr.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"

namespace InitTweak {
	class GlobalFixer : public WithShortCircuiting {
	  public:
		GlobalFixer( bool inLibrary );

		void previsit( ObjectDecl *objDecl );
		void previsit( FunctionDecl *functionDecl );
		void previsit( StructDecl *aggregateDecl );
		void previsit( UnionDecl *aggregateDecl );
		void previsit( EnumDecl *aggregateDecl );
		void previsit( TraitDecl *aggregateDecl );
		void previsit( TypeDecl *typeDecl );

		UniqueName tempNamer;
		FunctionDecl * initFunction;
		FunctionDecl * destroyFunction;
	};

	class GlobalFixer_new : public ast::WithShortCircuiting {
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

	void fixGlobalInit( std::list< Declaration * > & translationUnit, bool inLibrary ) {
		PassVisitor<GlobalFixer> visitor( inLibrary );
		acceptAll( translationUnit, visitor );
		GlobalFixer & fixer = visitor.pass;
		// don't need to include function if it's empty
		if ( fixer.initFunction->get_statements()->get_kids().empty() ) {
			delete fixer.initFunction;
		} else {
			translationUnit.push_back( fixer.initFunction );
		} // if

		if ( fixer.destroyFunction->get_statements()->get_kids().empty() ) {
			delete fixer.destroyFunction;
		} else {
			translationUnit.push_back( fixer.destroyFunction );
		} // if
	}

	GlobalFixer::GlobalFixer( bool inLibrary ) : tempNamer( "_global_init" ) {
		std::list< Expression * > ctorParameters;
		std::list< Expression * > dtorParameters;
		if ( inLibrary ) {
			// Constructor/destructor attributes take a single parameter which
			// is the priority, with lower numbers meaning higher priority.
			// Functions specified with priority are guaranteed to run before
			// functions without a priority. To ensure that constructors and destructors
			// for library code are run before constructors and destructors for user code,
			// specify a priority when building the library. Priorities 0-100 are reserved by gcc.
			// Priorities 101-200 are reserved by cfa, so use priority 200 for CFA library globals,
			// allowing room for overriding with a higher priority.
			ctorParameters.push_back( new ConstantExpr( Constant::from_int( 200 ) ) );
			dtorParameters.push_back( new ConstantExpr( Constant::from_int( 200 ) ) );
		}
		initFunction = new FunctionDecl( "__global_init__", Type::StorageClasses( Type::Static ), LinkageSpec::C, new FunctionType( Type::Qualifiers(), false ), new CompoundStmt() );
		initFunction->get_attributes().push_back( new Attribute( "constructor", ctorParameters ) );
		destroyFunction = new FunctionDecl( "__global_destroy__", Type::StorageClasses( Type::Static ), LinkageSpec::C, new FunctionType( Type::Qualifiers(), false ), new CompoundStmt() );
		destroyFunction->get_attributes().push_back( new Attribute( "destructor", dtorParameters ) );
	}

	void fixGlobalInit(ast::TranslationUnit & translationUnit, bool inLibrary) {
		ast::Pass<GlobalFixer_new> fixer;
		accept_all(translationUnit, fixer);

		if ( !fixer.core.initStmts.empty() ) {
			std::vector<ast::ptr<ast::Expr>> ctorParams;
			if (inLibrary) ctorParams.emplace_back(ast::ConstantExpr::from_int({}, 200));
			auto initFunction = new ast::FunctionDecl({}, "__global_init__", {}, {}, {}, new ast::CompoundStmt({}, std::move(fixer.core.initStmts)), 
				ast::Storage::Static, ast::Linkage::C, {new ast::Attribute("constructor", std::move(ctorParams))});

			translationUnit.decls.emplace_back( initFunction );
		} // if

		if ( !fixer.core.destroyStmts.empty() ) {
			std::vector<ast::ptr<ast::Expr>> dtorParams;
			if (inLibrary) dtorParams.emplace_back(ast::ConstantExpr::from_int({}, 200));
			auto destroyFunction = new ast::FunctionDecl({}, "__global_destroy__", {}, {}, {}, new ast::CompoundStmt({}, std::move(fixer.core.destroyStmts)), 
				ast::Storage::Static, ast::Linkage::C, {new ast::Attribute("destructor", std::move(dtorParams))});

			translationUnit.decls.emplace_back(destroyFunction);
		} // if
	}

	void GlobalFixer::previsit( ObjectDecl *objDecl ) {
		std::list< Statement * > & initStatements = initFunction->get_statements()->get_kids();
		std::list< Statement * > & destroyStatements = destroyFunction->get_statements()->get_kids();

		// C allows you to initialize objects with constant expressions
		// xxx - this is an optimization. Need to first resolve constructors before we decide
		// to keep C-style initializer.
		// if ( isConstExpr( objDecl->get_init() ) ) return;

		if ( ConstructorInit * ctorInit = dynamic_cast< ConstructorInit * >( objDecl->get_init() ) ) {
			// a decision should have been made by the resolver, so ctor and init are not both non-NULL
			assert( ! ctorInit->ctor || ! ctorInit->init );

			Statement * dtor = ctorInit->dtor;
			if ( dtor && ! isIntrinsicSingleArgCallStmt( dtor ) ) {
				// don't need to call intrinsic dtor, because it does nothing, but
				// non-intrinsic dtors must be called
				destroyStatements.push_front( dtor );
				ctorInit->dtor = nullptr;
			} // if
			if ( Statement * ctor = ctorInit->ctor ) {
				addDataSectonAttribute( objDecl );
				initStatements.push_back( ctor );
				objDecl->init = nullptr;
				ctorInit->ctor = nullptr;
			} else if ( Initializer * init = ctorInit->init ) {
				objDecl->init = init;
				ctorInit->init = nullptr;
			} else {
				// no constructor and no initializer, which is okay
				objDecl->init = nullptr;
			} // if
			delete ctorInit;
		} // if
	}

	void GlobalFixer_new::previsit(const ast::ObjectDecl * objDecl) {
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

	// only modify global variables
	void GlobalFixer::previsit( FunctionDecl * ) { visit_children = false; }
	void GlobalFixer::previsit( StructDecl * ) { visit_children = false; }
	void GlobalFixer::previsit( UnionDecl * ) { visit_children = false; }
	void GlobalFixer::previsit( EnumDecl * ) { visit_children = false; }
	void GlobalFixer::previsit( TraitDecl * ) { visit_children = false; }
	void GlobalFixer::previsit( TypeDecl * ) { visit_children = false; }

} // namespace InitTweak

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
