//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclReplacer.cpp --
//
// Author           : Aaron B. Moss
// Created On       : Wed May 8 13:00:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Wed May 8 13:00:00 2019
// Update Count     : 1
//

#include "DeclReplacer.hpp"
#include "Expr.hpp"
#include "Type.hpp"

#include "Pass.hpp"

namespace ast {

namespace DeclReplacer {
	namespace {
		struct DeclReplacer {
		private:
			const DeclMap & declMap;
			const TypeMap & typeMap;
			bool debug;

		public:
			DeclReplacer(const DeclMap & declMap, const TypeMap & typeMap, bool debug)
				: declMap( declMap ), typeMap( typeMap ), debug( debug )
			{}

			const ast::VariableExpr * previsit( const ast::VariableExpr * );
			const ast::TypeInstType * previsit( const ast::TypeInstType * );
		};

		struct VarExprReplacer {
		private:
			const ExprMap & exprMap;
			
		public:
			VarExprReplacer(const ExprMap & exprMap): exprMap (exprMap) {}

			const Expr * postvisit (const VariableExpr *);
		};
	}

	const ast::Node * replace( const ast::Node * node, const DeclMap & declMap, const TypeMap & typeMap, bool debug ) {
		if(!node) return nullptr;
		Pass<DeclReplacer> replacer = { declMap, typeMap, debug };
		return node->accept( replacer );
	}

	const ast::Node * replace( const ast::Node * node, const DeclMap & declMap, bool debug ) {
		TypeMap typeMap;
		return replace( node, declMap, typeMap, debug );
	}

	const ast::Node * replace( const ast::Node * node, const TypeMap & typeMap, bool debug ) {
		DeclMap declMap;
		return replace( node, declMap, typeMap, debug );
	}

	const ast::Node * replace( const ast::Node * node, const ExprMap & exprMap) {
		Pass<VarExprReplacer> replacer = {exprMap};
		return node->accept( replacer );
	}

	namespace {
		// replace variable with new node from decl map
		const ast::VariableExpr * DeclReplacer::previsit( const VariableExpr * varExpr ) {
			// xxx - assertions and parameters aren't accounted for in this... (i.e. they aren't inserted into the map when it's made, only DeclStmts are)
			if ( !declMap.count( varExpr->var ) ) return varExpr;

			auto replacement = declMap.at( varExpr->var );
			if ( debug ) {
				std::cerr << "replacing variable reference: "
					<< (void*)varExpr->var.get() << " " << varExpr->var
					<< " with " << (void*)replacement << " " << replacement
					<< std::endl;
			}
			auto nexpr = mutate(varExpr);
			nexpr->var = replacement;
			return nexpr;
		}

		const TypeInstType * DeclReplacer::previsit( const TypeInstType * inst ) {
			if ( !typeMap.count( inst->base ) ) return inst;

			auto replacement = typeMap.at( inst->base );
			if ( debug ) {
				std::cerr << "replacing type reference: "
					<< (void*)inst->base.get() << " " << inst->base
					<< " with " << (void*)replacement << " " << replacement
					<< std::endl;
			}
			auto ninst = mutate(inst);
			ninst->base = replacement;
			return ninst;
		}

		const Expr * VarExprReplacer::postvisit( const VariableExpr * expr ) {
			if (!exprMap.count(expr->var)) return expr;

			return exprMap.at(expr->var);
		}

	}
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
