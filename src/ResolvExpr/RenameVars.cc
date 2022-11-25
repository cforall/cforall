//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// RenameVars.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:05:18 2015
// Last Modified By : Andrew Beach
// Last Modified On : Thr Jun 20 17:39:00 2019
// Update Count     : 8
//

#include <ext/alloc_traits.h>      // for __alloc_traits<>::value_type
#include <memory>                  // for allocator_traits<>::value_type
#include <sstream>                 // for operator<<, basic_ostream, ostring...
#include <utility>                 // for pair

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"
#include "Common/ScopedMap.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "RenameVars.h"
#include "SynTree/Declaration.h"   // for DeclarationWithType, TypeDecl, Dec...
#include "SynTree/Expression.h"    // for Expression
#include "SynTree/Type.h"          // for Type, TypeInstType, TraitInstType
#include "SynTree/Visitor.h"       // for acceptAll, maybeAccept

#include "AST/Copy.hpp"

namespace ResolvExpr {

namespace {
	class RenamingData {
		int level = 0;
		int resetCount = 0;

		int next_expr_id = 1;
		int next_usage_id = 1;
		ScopedMap< std::string, std::string > nameMap;
		ScopedMap< std::string, ast::TypeEnvKey > idMap;
	public:
		void reset() {
			level = 0;
			++resetCount;
		}

		void rename( TypeInstType * type ) {
			auto it = nameMap.find( type->name );
			if ( it != nameMap.end() ) {
				type->name = it->second;
			}
		}

		void nextUsage() {
			++next_usage_id;
		}

		void openLevel( Type * type ) {
			if ( ! type->forall.empty() ) {
				nameMap.beginScope();
				// renames all "forall" type names to `_${level}_${name}'
				for ( auto td : type->forall ) {
					std::ostringstream output;
					output << "_" << resetCount << "_" << level << "_" << td->name;
					std::string newname( output.str() );
					nameMap[ td->get_name() ] = newname;
					td->name = newname;
					// ditto for assertion names, the next level in
					level++;
				}
			}
		}

		void closeLevel( Type * type ) {
			if ( !type->forall.empty() ) {
				nameMap.endScope();
			}
		}

		const ast::TypeInstType * rename( const ast::TypeInstType * type ) {
			// rename
			auto it = idMap.find( type->name );
			if ( it != idMap.end() ) {
				// unconditionally mutate because map will *always* have different name
				ast::TypeInstType * mut = ast::shallowCopy( type );
				// reconcile base node since some copies might have been made
				mut->base = it->second.base;
				mut->formal_usage = it->second.formal_usage;
				mut->expr_id = it->second.expr_id;
	            type = mut;
			}

			return type;
		}

		const ast::FunctionType * openLevel( const ast::FunctionType * type, RenameMode mode ) {
			if ( type->forall.empty() ) return type;
			idMap.beginScope();

			// Load new names from this forall clause and perform renaming.
			auto mutType = ast::shallowCopy( type );
			// assert( type == mutType && "mutated type must be unique from ForallSubstitutor" );
			for ( auto & td : mutType->forall ) {
				auto mut = ast::shallowCopy( td.get() );
				// assert( td == mutDecl && "mutated decl must be unique from ForallSubstitutor" );

				if (mode == GEN_EXPR_ID) {
					mut->expr_id = next_expr_id;
					mut->formal_usage = -1;
					++next_expr_id;
				}
				else if (mode == GEN_USAGE) {
					assertf(mut->expr_id, "unfilled expression id in generating candidate type");
					mut->formal_usage = next_usage_id;
				}
				else {
					assert(false);
				}
				idMap[ td->name ] = ast::TypeEnvKey( *mut );

				td = mut;
			}

			return mutType;
		}

		void closeLevel( const ast::FunctionType * type ) {
			if ( type->forall.empty() ) return;
			idMap.endScope();
		}
	};

	// Global State:
	RenamingData renaming;

	struct RenameVars_old {
		void previsit( TypeInstType * instType ) {
			renaming.openLevel( (Type*)instType );
			renaming.rename( instType );
		}
		void previsit( Type * type ) {
			renaming.openLevel( type );
		}
		void postvisit( Type * type ) {
			renaming.closeLevel( type );
		}
	};

	struct RenameVars_new : public ast::PureVisitor /*: public ast::WithForallSubstitutor*/ {
		RenameMode mode;

		const ast::FunctionType * previsit( const ast::FunctionType * type ) {
			return renaming.openLevel( type, mode );
		}

		/*
		const ast::StructInstType * previsit( const ast::StructInstType * type ) {
			return renaming.openLevel( type );
		}
		const ast::UnionInstType * previsit( const ast::UnionInstType * type ) {
			return renaming.openLevel( type );
		}
		const ast::TraitInstType * previsit( const ast::TraitInstType * type ) {
			return renaming.openLevel( type );
		}
		*/

		const ast::TypeInstType * previsit( const ast::TypeInstType * type ) {
			if (mode == GEN_USAGE && !type->formal_usage) return type; // do not rename an actual type
			return renaming.rename( type );
		}
		void postvisit( const ast::FunctionType * type ) {
			renaming.closeLevel( type );
		}
	};

} // namespace

void renameTyVars( Type * t ) {
	PassVisitor<RenameVars_old> renamer;
	t->accept( renamer );
}

const ast::Type * renameTyVars( const ast::Type * t, RenameMode mode, bool reset ) {
	// ast::Type *tc = ast::deepCopy(t);
	ast::Pass<RenameVars_new> renamer;
	renamer.core.mode = mode;
	if (mode == GEN_USAGE && reset) {
		renaming.nextUsage();
	}
	return t->accept( renamer );
}

void resetTyVarRenaming() {
	renaming.reset();
	renaming.nextUsage();
}

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
