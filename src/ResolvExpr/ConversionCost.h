//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ConversionCost.h --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:37:28 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jul 29 16:12:00 2020
// Update Count     : 7
//

#pragma once

#include <functional>         // for function

#include "Cost.h"             // for Cost

#include "AST/Fwd.hpp"
#include "AST/Pass.hpp"       // for WithShortCircuiting
#include "Common/PassVisitor.h"
#include "SynTree/Visitor.h"  // for Visitor
#include "SynTree/SynTree.h"  // for Visitor Nodes

namespace SymTab {
	class Indexer;
}  // namespace SymTab

namespace ResolvExpr {
	class TypeEnvironment;

	typedef std::function<Cost(const Type *, const Type *, bool,
		const SymTab::Indexer &, const TypeEnvironment &)> CostFunction;

	struct ConversionCost : public WithShortCircuiting {
	  public:
		ConversionCost( const Type * dest, bool srcIsLvalue,
			const SymTab::Indexer &indexer, const TypeEnvironment &env, CostFunction );

		Cost get_cost() const { return cost; }

		void previsit( const BaseSyntaxNode * ) { visit_children = false; }

		void postvisit( const VoidType * voidType );
		void postvisit( const BasicType * basicType );
		void postvisit( const PointerType * pointerType );
		void postvisit( const ArrayType * arrayType );
		void postvisit( const ReferenceType * refType );
		void postvisit( const FunctionType * functionType );
		void postvisit( const EnumInstType * aggregateUseType );
		void postvisit( const TraitInstType * aggregateUseType );
		void postvisit( const TypeInstType * aggregateUseType );
		void postvisit( const TupleType * tupleType );
		void postvisit( const VarArgsType * varArgsType );
		void postvisit( const ZeroType * zeroType );
		void postvisit( const OneType * oneType );
	  protected:
		const Type * dest;
		bool srcIsLvalue;
		const SymTab::Indexer &indexer;
		Cost cost;
		const TypeEnvironment &env;
		CostFunction costFunc;
	  private:
	  	// refactor for code resue
	  	void conversionCostFromBasicToBasic( const BasicType * src, const BasicType* dest );
	};

	typedef std::function<int(const Type *, const Type *, const SymTab::Indexer &, const TypeEnvironment &)> PtrsFunction;
	Cost convertToReferenceCost( const Type * src, const ReferenceType * dest, bool srcIsLvalue,
		const SymTab::Indexer & indexer, const TypeEnvironment & env, PtrsFunction func );

// Some function pointer types, differ in return type.
using CostCalculation = std::function<Cost(const ast::Type *, const ast::Type *, bool,
	const ast::SymbolTable &, const ast::TypeEnvironment &)>;
using PtrsCalculation = std::function<int(const ast::Type *, const ast::Type *,
	const ast::SymbolTable &, const ast::TypeEnvironment &)>;

#warning when the old ConversionCost is removed, get ride of the _new suffix.
class ConversionCost_new : public ast::WithShortCircuiting {
protected:
	const ast::Type * dst;
	bool srcIsLvalue;
	const ast::SymbolTable & symtab;
	const ast::TypeEnvironment & env;
	CostCalculation costCalc;
public:
	static size_t traceId;
	Cost cost;
	Cost result() { return cost; }

	ConversionCost_new( const ast::Type * dst, bool srcIsLvalue, const ast::SymbolTable & symtab,
			const ast::TypeEnvironment & env, CostCalculation costCalc ) :
		dst( dst ), srcIsLvalue( srcIsLvalue ), symtab( symtab ), env( env ),
		costCalc( costCalc ), cost( Cost::infinity )
	{}

	void previsit( const ast::Node * ) { visit_children = false; }

	void postvisit( const ast::VoidType * voidType );
	void postvisit( const ast::BasicType * basicType );
	void postvisit( const ast::PointerType * pointerType );
	void postvisit( const ast::ArrayType * arrayType );
	void postvisit( const ast::ReferenceType * refType );
	void postvisit( const ast::FunctionType * functionType );
	void postvisit( const ast::EnumInstType * enumInstType );
	void postvisit( const ast::TraitInstType * traitInstType );
	void postvisit( const ast::TypeInstType * typeInstType );
	void postvisit( const ast::TupleType * tupleType );
	void postvisit( const ast::VarArgsType * varArgsType );
	void postvisit( const ast::ZeroType * zeroType );
	void postvisit( const ast::OneType * oneType );
private:
	// refactor for code resue
	void conversionCostFromBasicToBasic( const ast::BasicType * src, const ast::BasicType* dest );
};

Cost convertToReferenceCost( const ast::Type * src, const ast::ReferenceType * dest,
	bool srcIsLvalue, const ast::SymbolTable & indexer, const ast::TypeEnvironment & env,
	PtrsCalculation func );

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
