//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeGenerator.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 29 14:32:00 2022
// Update Count     : 65
//

#pragma once

#include <list>                   // for list
#include <ostream>                // for ostream, operator<<
#include <string>                 // for string

#include "CodeGen/Options.h"      // for Options
#include "Common/Indenter.h"      // for Indenter
#include "Common/PassVisitor.h"   // for PassVisitor
#include "SynTree/Declaration.h"  // for DeclarationWithType (ptr only), Fun...
#include "SynTree/Visitor.h"      // for Visitor
#include "SynTree/SynTree.h"      // for Visitor Nodes

namespace ast {
	class DeclWithType;
}

namespace CodeGen {
	struct CodeGenerator : public WithShortCircuiting, public WithGuards, public WithVisitorRef<CodeGenerator> {
		static int tabsize;

		CodeGenerator( std::ostream &os, bool pretty = false, bool genC = false, bool lineMarks = false, bool printExprTypes = false );
		CodeGenerator( std::ostream &os, const Options &options );

		//*** Turn off visit_children for all nodes
		void previsit( BaseSyntaxNode * );

		//*** Error for unhandled node types
		void postvisit( BaseSyntaxNode * );

		//*** print type for all expressions
		void previsit( Expression * node );

		//*** Declaration
		void postvisit( StructDecl * );
		void postvisit( FunctionDecl * );
		void postvisit( ObjectDecl * );
		void postvisit( UnionDecl * aggregateDecl );
		void postvisit( EnumDecl * aggregateDecl );
		void postvisit( TraitDecl * aggregateDecl );
		void postvisit( TypedefDecl * typeDecl );
		void postvisit( TypeDecl * typeDecl );
		void postvisit( StaticAssertDecl * assertDecl );

		//*** Initializer
		void postvisit( Designation * );
		void postvisit( SingleInit * );
		void postvisit( ListInit * );
		void postvisit( ConstructorInit * );

		//*** Constant
		void postvisit( Constant * );

		//*** Expression
		void postvisit( ApplicationExpr *applicationExpr );
		void postvisit( UntypedExpr *untypedExpr );
		void postvisit( RangeExpr * rangeExpr );
		void postvisit( NameExpr *nameExpr );
		void postvisit( AddressExpr *addressExpr );
		void postvisit( LabelAddressExpr *addressExpr );
		void postvisit( CastExpr *castExpr );
		void postvisit( KeywordCastExpr * castExpr );
		void postvisit( VirtualCastExpr *castExpr );
		void postvisit( UntypedMemberExpr *memberExpr );
		void postvisit( MemberExpr *memberExpr );
		void postvisit( VariableExpr *variableExpr );
		void postvisit( ConstantExpr *constantExpr );
		void postvisit( SizeofExpr *sizeofExpr );
		void postvisit( AlignofExpr *alignofExpr );
		void postvisit( UntypedOffsetofExpr *offsetofExpr );
		void postvisit( OffsetofExpr *offsetofExpr );
		void postvisit( OffsetPackExpr *offsetPackExpr );
		void postvisit( LogicalExpr *logicalExpr );
		void postvisit( ConditionalExpr *conditionalExpr );
		void postvisit( CommaExpr *commaExpr );
		void postvisit( CompoundLiteralExpr *compLitExpr );
		void postvisit( UniqueExpr * );
		void postvisit( TupleAssignExpr * tupleExpr );
		void postvisit( UntypedTupleExpr *tupleExpr );
		void postvisit( TupleExpr *tupleExpr );
		void postvisit( TupleIndexExpr * tupleExpr );
		void postvisit( TypeExpr *typeExpr );
		void postvisit( DimensionExpr *dimensionExpr );
		void postvisit( AsmExpr * );
		void postvisit( StmtExpr * );
		void postvisit( ConstructorExpr * );
		void postvisit( DeletedExpr * );
		void postvisit( DefaultArgExpr * );
		void postvisit( GenericExpr * );
		void postvisit( QualifiedNameExpr *);

		//*** Statements
		void postvisit( CompoundStmt * );
		void postvisit( ExprStmt * );
		void postvisit( AsmStmt * );
		void postvisit( DirectiveStmt * );
		void postvisit( AsmDecl * );					// special: statement in declaration context
		void postvisit( DirectiveDecl * );				// special: statement in declaration context
		void postvisit( IfStmt * );
		void postvisit( SwitchStmt * );
		void postvisit( CaseStmt * );
		void postvisit( BranchStmt * );
		void postvisit( ReturnStmt * );
		void postvisit( ThrowStmt * );
		void postvisit( CatchStmt * );
		void postvisit( WaitForStmt * );
		void postvisit( WithStmt * );
		void postvisit( WhileDoStmt * );
		void postvisit( ForStmt * );
		void postvisit( NullStmt * );
		void postvisit( DeclStmt * );
		void postvisit( ImplicitCtorDtorStmt * );
		void postvisit( MutexStmt * stmt );

		void genAttributes( std::list< Attribute * > & attributes );

		template< class Iterator > void genCommaList( Iterator begin, Iterator end );

		struct LabelPrinter {
			LabelPrinter(CodeGenerator &cg) : cg(cg), labels( 0 ) {}
			LabelPrinter & operator()( std::list< Label > & l );
			CodeGenerator & cg;
			std::list< Label > * labels;
		};

		void asmName( DeclarationWithType *decl );

		void extension( Expression *expr );
		void extension( Declaration *decl );

		void updateLocation( BaseSyntaxNode const * to );
		struct LineEnder {
			CodeGenerator & cg;
			LineEnder( CodeGenerator & cg ) : cg( cg ) {}
			std::ostream & operator()(std::ostream &) const;
		};
	  private:
		Indenter indent;
		std::ostream & output;
		LabelPrinter printLabels;
		Options options;
	  public:
		LineEnder endl;
	  private:

		CodeLocation currentLocation;
		void updateLocation( CodeLocation const & to );

		void handleStorageClass( DeclarationWithType *decl );
		void handleAggregate( AggregateDecl *aggDecl, const std::string & kind );
		void handleTypedef( NamedTypeDecl *namedType );
		std::string mangleName( DeclarationWithType * decl );
	}; // CodeGenerator

	template< class Iterator >
	void CodeGenerator::genCommaList( Iterator begin, Iterator end ) {
		if ( begin == end ) return;
		for ( ;; ) {
			(*begin++)->accept( *visitor );
			if ( begin == end ) break;
			output << ", ";								// separator
		} // for
	} // genCommaList

	inline bool doSemicolon( Declaration* decl ) {
		if ( FunctionDecl* func = dynamic_cast< FunctionDecl* >( decl ) ) {
			return ! func->get_statements();
		} // if
		return true;
	} // doSemicolon

	/// returns C-compatible name of declaration
	std::string genName( DeclarationWithType * decl );
	std::string genName( ast::DeclWithType const * decl );

	inline std::ostream & operator<<( std::ostream & os, const CodeGenerator::LineEnder & endl ) {
		return endl( os );
	}
} // namespace CodeGen

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
