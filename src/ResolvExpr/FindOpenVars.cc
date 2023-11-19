//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindOpenVars.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:42:48 2015
// Last Modified By : Andrew
// Last Modified On : Fri Jul 12 14:18:00 2019
// Update Count     : 4
//

#include "FindOpenVars.h"

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"

#include <iostream>

namespace ResolvExpr {

namespace {

struct FindOpenVars final : public ast::WithGuards {
	ast::OpenVarSet & open;
	ast::OpenVarSet & closed;
	ast::AssertionSet & need;
	ast::AssertionSet & have;
	ast::TypeEnvironment & env;
	bool nextIsOpen;

	FindOpenVars(
		ast::OpenVarSet & o, ast::OpenVarSet & c, ast::AssertionSet & n,
		ast::AssertionSet & h, ast::TypeEnvironment & env, FirstMode firstIsOpen )
	: open( o ), closed( c ), need( n ), have( h ), env (env), nextIsOpen( firstIsOpen ) {}

	void previsit( const ast::FunctionType * type ) {
		// mark open/closed variables
		if ( nextIsOpen ) {
			// trying to remove this from resolver.
			// occasionally used in other parts so not deleting right now.

			// insert open variables unbound to environment.
			env.add(type->forall);

			for ( auto & decl : type->forall ) {
				open[ *decl ] = ast::TypeData{ decl->base };
			}
			for ( auto & assert : type->assertions ) {
				need[ assert ].isUsed = false;
			}
		} else {
			for ( auto & decl : type->forall ) {
				closed[ *decl ] = ast::TypeData{ decl->base };
			}
			for ( auto & assert : type->assertions ) {
				have[ assert ].isUsed = false;
			}
		}

		// flip open variables for contained function types
	//	nextIsOpen = ! nextIsOpen;
	//	GuardAction( [this](){ nextIsOpen = ! nextIsOpen; } );
		GuardValue( nextIsOpen ) = !nextIsOpen;
	}
};

} // namespace

void findOpenVars(
		const ast::Type * type, ast::OpenVarSet & open, ast::OpenVarSet & closed,
		ast::AssertionSet & need, ast::AssertionSet & have, ast::TypeEnvironment & env, FirstMode firstIsOpen ) {
	ast::Pass< FindOpenVars > finder{ open, closed, need, have, env, firstIsOpen };
	type->accept( finder );

	if (!closed.empty()) {
		std::cerr << "closed: ";
		for (auto& i : closed) {
			std::cerr << i.first.base->location << ":" << i.first.base->name << ' ';
		}
		std::cerr << std::endl;
	}
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
