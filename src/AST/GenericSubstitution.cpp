//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// GenericSubstitution.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 22 14:15:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Wed May 22 14:15:00 2019
// Update Count     : 1
//

#include "GenericSubstitution.hpp"

#include <cassert>
#include <utility>               // for move

#include "Decl.hpp"
#include "Node.hpp"              // for maybe_accept
#include "Pass.hpp"
#include "Type.hpp"
#include "TypeSubstitution.hpp"

namespace ast {

namespace {
	struct GenericSubstitutionBuilder : public WithShortCircuiting {
		TypeSubstitution sub;

		void previsit( const Type * ) {
			// allow empty substitution for non-generic type
			visit_children = false;
		}

		void previsit( const ReferenceType * ) {
			// do nothing; allows substitution from base type
		}

	private:
		// make substitution for generic type
		void makeSub( const BaseInstType * ty ) {
			visit_children = false;
			const AggregateDecl * aggr = ty->aggr();
			sub = TypeSubstitution{ aggr->params.begin(), aggr->params.end(), ty->params.begin() };
		}

	public:
		void previsit( const StructInstType * ty ) {
			makeSub( ty );
		}

		void previsit( const UnionInstType * ty ) {
			makeSub( ty );
		}
	};
}

TypeSubstitution genericSubstitution( const Type * ty ) {
	Pass<GenericSubstitutionBuilder> builder;
	maybe_accept( ty, builder );
	return std::move(builder.core.sub);
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
