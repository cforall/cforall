//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VarExprReplacer.h --
//
// Author           : Rob Schluntz
// Created On       : Wed Jan 13 16:29:30 2016
// Last Modified By : Rob Schluntz
// Last Modified On : Fri May 13 11:27:52 2016
// Update Count     : 5
//

#include <iostream>       // for operator<<, basic_ostream, ostream, basic_o...

#include "Common/PassVisitor.h"
#include "Declaration.h"  // for operator<<, DeclarationWithType
#include "Expression.h"   // for VariableExpr
#include "DeclReplacer.h"

namespace DeclReplacer {
	namespace {
		/// Visitor that replaces the declarations that VariableExprs refer to, according to the supplied mapping
		struct DeclReplacer {
		private:
			const DeclMap & declMap;
			const TypeMap & typeMap;
			bool debug;
		public:
			size_t replaced;

		public:
			DeclReplacer( const DeclMap & declMap, const TypeMap & typeMap, bool debug = false );

			// replace variable with new node from decl map
			void previsit( VariableExpr * varExpr );

			// replace type inst with new node from type map
			void previsit( TypeInstType * inst );
		};

		/// Mutator that replaces uses of declarations with arbitrary expressions, according to the supplied mapping
		struct ExprDeclReplacer {
		private:
			const ExprMap & exprMap;
			bool debug;
		public:
			size_t replaced;

		public:
			ExprDeclReplacer( const ExprMap & exprMap, bool debug = false );

			// replace variable with new node from expr map
			Expression * postmutate( VariableExpr * varExpr );
		};
	}

	size_t replace( BaseSyntaxNode * node, const DeclMap & declMap, const TypeMap & typeMap, bool debug ) {
		PassVisitor<DeclReplacer> replacer( declMap, typeMap, debug );
		maybeAccept( node, replacer );
		return replacer.pass.replaced;
	}

	size_t replace( BaseSyntaxNode * node, const DeclMap & declMap, bool debug ) {
		TypeMap typeMap;
		return replace( node, declMap, typeMap, debug );
	}

	size_t replace( BaseSyntaxNode * node, const TypeMap & typeMap, bool debug ) {
		DeclMap declMap;
		return replace( node, declMap, typeMap, debug );
	}

	size_t replace( BaseSyntaxNode *& node, const ExprMap & exprMap, bool debug ) {
		PassVisitor<ExprDeclReplacer> replacer( exprMap, debug );
		node = maybeMutate( node, replacer );
		return replacer.pass.replaced;
	}

	namespace {
		DeclReplacer::DeclReplacer( const DeclMap & declMap, const TypeMap & typeMap, bool debug ) : declMap( declMap ), typeMap( typeMap ) , debug( debug ), replaced( 0 ) {}

		// replace variable with new node from decl map
		void DeclReplacer::previsit( VariableExpr * varExpr ) {
			// xxx - assertions and parameters aren't accounted for in this... (i.e. they aren't inserted into the map when it's made, only DeclStmts are)
			if ( declMap.count( varExpr->var ) ) {
				replaced++;
				auto replacement = declMap.at( varExpr->var );
				if ( debug ) {
					std::cerr << "replacing variable reference: " << (void*)varExpr->var << " " << varExpr->var << " with " << (void*)replacement << " " << replacement << std::endl;
				}
				varExpr->var = replacement;
			}
		}

		void DeclReplacer::previsit( TypeInstType * inst ) {
			if ( typeMap.count( inst->baseType ) ) {
				replaced++;
				auto replacement = typeMap.at( inst->baseType );
				if ( debug ) {
					std::cerr << "replacing type reference: " << (void*)inst->baseType << " " << inst->baseType << " with " << (void*)replacement << " " << replacement << std::endl;
				}
				inst->baseType = replacement;
			}
		}

		ExprDeclReplacer::ExprDeclReplacer( const ExprMap & exprMap, bool debug ) : exprMap( exprMap ), debug( debug ), replaced( 0 ) {}

		Expression * ExprDeclReplacer::postmutate( VariableExpr * varExpr ) {
			if ( exprMap.count( varExpr->var ) ) {
				replaced++;
				Expression * replacement = exprMap.at( varExpr->var )->clone();
				if ( debug ) {
					std::cerr << "replacing variable reference: " << (void*)varExpr->var << " " << varExpr->var << " with " << (void*)replacement << " " << replacement << std::endl;
				}
				std::swap( varExpr->env, replacement->env );
				delete varExpr;
				return replacement;
			}
			return varExpr;
		}
	}
} // namespace VarExprReplacer
