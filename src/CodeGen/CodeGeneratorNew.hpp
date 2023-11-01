//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeGeneratorNew.hpp --
//
// Author           : Andrew Beach
// Created On       : Tue Oct 17 15:54:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct 25 17:56:00 2023
// Update Count     : 0
//

#pragma once

#include <ostream>               // for ostream, operator<<

#include "AST/Fwd.hpp"
#include "AST/Pass.hpp"          // for WithGuards, WithShortCircuiting, ...
#include "CodeGen/Options.h"     // for Options


namespace CodeGen {

#warning Remove the _new when old version is removed.
struct CodeGenerator_new :
		public ast::WithGuards,
		public ast::WithShortCircuiting,
		public ast::WithVisitorRef<CodeGenerator_new> {
	CodeGenerator_new( std::ostream & out, Options const & options );

	// Turn off visit_children for all nodes.
	void previsit( ast::Node const * );
	void previsit( ast::ParseNode const * );

	// Error for unhandled node types.
	void postvisit( ast::Node const * );

	// Print type for all expressions.
	void previsit( ast::Expr const * );

	void postvisit( ast::StructDecl const * );
	void postvisit( ast::FunctionDecl const * );
	// Yes, there is one visit that does modify the ast.
	ast::ObjectDecl const * postvisit( ast::ObjectDecl const * );
	void postvisit( ast::UnionDecl const * );
	void postvisit( ast::EnumDecl const * );
	void postvisit( ast::TraitDecl const * );
	void postvisit( ast::TypedefDecl const * );
	void postvisit( ast::TypeDecl const * );
	void postvisit( ast::StaticAssertDecl const * );

	void postvisit( ast::Designation const * );
	void postvisit( ast::SingleInit const * );
	void postvisit( ast::ListInit const * );
	void postvisit( ast::ConstructorInit const * );

	void postvisit( ast::ApplicationExpr const * );
	void postvisit( ast::UntypedExpr const * );
	void postvisit( ast::RangeExpr const * );
	void postvisit( ast::NameExpr const * );
	void postvisit( ast::AddressExpr const * );
	void postvisit( ast::LabelAddressExpr const * );
	void postvisit( ast::CastExpr const * );
	void postvisit( ast::KeywordCastExpr const * );
	void postvisit( ast::VirtualCastExpr const * );
	void postvisit( ast::UntypedMemberExpr const * );
	void postvisit( ast::MemberExpr const * );
	void postvisit( ast::VariableExpr const * );
	void postvisit( ast::ConstantExpr const * );
	void postvisit( ast::SizeofExpr const * );
	void postvisit( ast::AlignofExpr const * );
	void postvisit( ast::UntypedOffsetofExpr const * );
	void postvisit( ast::OffsetofExpr const * );
	void postvisit( ast::OffsetPackExpr const * );
	void postvisit( ast::LogicalExpr const * );
	void postvisit( ast::ConditionalExpr const * );
	void postvisit( ast::CommaExpr const * );
	void postvisit( ast::CompoundLiteralExpr const * );
	void postvisit( ast::UniqueExpr const * );
	void postvisit( ast::TupleAssignExpr const * );
	void postvisit( ast::UntypedTupleExpr const * );
	void postvisit( ast::TupleExpr const * );
	void postvisit( ast::TupleIndexExpr const * );
	void postvisit( ast::TypeExpr const * );
	void postvisit( ast::DimensionExpr const * );
	void postvisit( ast::AsmExpr const * );
	void postvisit( ast::StmtExpr const * );
	void postvisit( ast::ConstructorExpr const * );
	void postvisit( ast::DeletedExpr const * );
	void postvisit( ast::DefaultArgExpr const * );
	void postvisit( ast::GenericExpr const * );

	void postvisit( ast::CompoundStmt const * );
	void postvisit( ast::ExprStmt const * );
	void postvisit( ast::AsmStmt const * );
	void postvisit( ast::DirectiveStmt const* );
	void postvisit( ast::AsmDecl const * );
	void postvisit( ast::DirectiveDecl const * );
	void postvisit( ast::IfStmt const * );
	void postvisit( ast::SwitchStmt const * );
	void postvisit( ast::CaseClause const * );
	void postvisit( ast::BranchStmt const * );
	void postvisit( ast::ReturnStmt const * );
	void postvisit( ast::ThrowStmt const * );
	void postvisit( ast::CatchClause const * );
	void postvisit( ast::WaitForStmt const * );
	void postvisit( ast::WithStmt const * );
	void postvisit( ast::WhileDoStmt const * );
	void postvisit( ast::ForStmt const * );
	void postvisit( ast::NullStmt const * );
	void postvisit( ast::DeclStmt const * );
	void postvisit( ast::ImplicitCtorDtorStmt const * );
	void postvisit( ast::MutexStmt const * stmt );

private:
	/// Custom local implementation of endl that updates print location.
	struct LineEnder {
		CodeGenerator_new & cg;
		LineEnder( CodeGenerator_new & cg ) : cg( cg ) {}
		std::ostream & operator()( std::ostream & ) const;
	};
	friend std::ostream & operator<<( std::ostream & os, const LineEnder & e ) {
		return e( os );
	}

	/// Wrapper class to help print vectors of Labels.
	struct LabelPrinter {
		LabelPrinter( CodeGenerator_new & cg ) : cg( cg ), labels( nullptr ) {}
		LabelPrinter & operator()( std::vector<ast::Label> const & l );
		std::ostream & operator()( std::ostream & ) const;
		CodeGenerator_new & cg;
		std::vector<ast::Label> const * labels;
	};
	friend std::ostream & operator<<( std::ostream & os, const LabelPrinter & p ) {
		return p( os );
	}

	static int tabsize;

	Indenter indent;
	std::ostream & output;
	Options options;
	LabelPrinter printLabels;

	CodeLocation currentLocation;
	void updateLocation( CodeLocation const & to );

	template<typename Iterator>
	void genCommaList( Iterator begin, Iterator end ) {
		if ( begin == end ) return;
		while (true) {
			(*begin++)->accept( *visitor );
			if ( begin == end ) break;
			output << ", ";
		}
	}

public:
	LineEnder endl;
	void updateLocation( ast::ParseNode const * to );

	template<typename T>
	void genCommaList( std::vector<ast::ptr<T>> const & range ) {
		genCommaList( range.begin(), range.end() );
	}

	void genAttributes( std::vector<ast::ptr<ast::Attribute>> const & );

private:
	void asmName( ast::DeclWithType const * decl );
	void extension( ast::Decl const * );
	void extension( ast::Expr const * );

	void handleStorageClass( ast::DeclWithType const * decl );
	void handleAggregate( ast::AggregateDecl const *, const std::string & );
	void handleTypedef( ast::NamedTypeDecl const * type );
	std::string mangleName( ast::DeclWithType const * decl );
};

inline bool doSemicolon( ast::Decl const * decl ) {
	if ( auto func = dynamic_cast<ast::FunctionDecl const *>( decl ) ) {
		return !func->stmts;
	}
	return true;
}

/// Returns the C-compatible name of the declaration.
std::string genName( ast::DeclWithType const * decl );

} // namespace CodeGen
