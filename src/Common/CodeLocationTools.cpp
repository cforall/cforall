//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// CodeLocationTools.cpp -- Additional tools for code locations.
//
// Author           : Andrew Beach
// Created On       : Fri Dec  4 15:42:00 2020
// Last Modified By : Andrew Beach
// Last Modified On : Wed May 11 16:16:00 2022
// Update Count     : 5
//

#include "CodeLocationTools.hpp"

#include <type_traits>

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "Common/CodeLocation.h"

namespace {

// Fill every location with a nearby (parent) location.
class FillCore : public ast::WithGuards {
	CodeLocation const * parent;

	template<typename node_t>
	node_t const * parse_visit( node_t const * node ) {
		if ( node->location.isUnset() ) {
			assert( parent );
			node_t * newNode = ast::mutate( node );
			newNode->location = *parent;
			return newNode;
		}
		GuardValue( parent ) = &node->location;
		return node;
	}

	bool hasUnsetLabels( const ast::Stmt * stmt ) {
		for ( const ast::Label& label : stmt->labels ) {
			if ( label.location.isUnset() ) {
				return true;
			}
		}
		return false;
	}

	template<typename node_t>
	node_t const * stmt_visit( node_t const * node ) {
		assert( node->location.isSet() );

		if ( hasUnsetLabels( node ) ) {
			node_t * newNode = ast::mutate( node );
			for ( ast::Label& label : newNode->labels ) {
				if ( label.location.isUnset() ) {
					label.location = newNode->location;
				}
			}
			return newNode;
		}
		return node;
	}

	template<typename node_t>
	auto visit( node_t const * node, long ) {
		return node;
	}

	template<typename node_t>
	auto visit( node_t const * node, int ) -> typename
			std::remove_reference< decltype( node->location, node ) >::type {
		return parse_visit( node );
	}

	template<typename node_t>
	auto visit( node_t const * node, char ) -> typename
			std::remove_reference< decltype( node->labels, node ) >::type {
		return stmt_visit( parse_visit( node ) );
	}

public:
	FillCore() : parent( nullptr ) {}
	FillCore( const CodeLocation& location ) : parent( &location ) {
		assert( location.isSet() );
	}

	template<typename node_t>
	node_t const * previsit( node_t const * node ) {
		return visit( node, '\0' );
	}
};

// ALL_VISITS(macro)
// Expands `macro(node_type, return_type)` for every visit method of the
// ast::Visitor class where node_type is the name of the parameter and
// return_type is the name of the return type; not including the namespace,
// pointer or const qualifiers.
#define ALL_VISITS(macro) \
    macro(ObjectDecl, DeclWithType) \
    macro(FunctionDecl, DeclWithType) \
    macro(StructDecl, Decl) \
    macro(UnionDecl, Decl) \
    macro(EnumDecl, Decl) \
    macro(TraitDecl, Decl) \
    macro(TypeDecl, Decl) \
    macro(TypedefDecl, Decl) \
    macro(AsmDecl, AsmDecl) \
    macro(DirectiveDecl, DirectiveDecl) \
    macro(StaticAssertDecl, StaticAssertDecl) \
    macro(CompoundStmt, CompoundStmt) \
    macro(ExprStmt, Stmt) \
    macro(AsmStmt, Stmt) \
    macro(DirectiveStmt, Stmt) \
    macro(IfStmt, Stmt) \
    macro(WhileDoStmt, Stmt) \
    macro(ForStmt, Stmt) \
    macro(SwitchStmt, Stmt) \
    macro(CaseClause, CaseClause) \
    macro(BranchStmt, Stmt) \
    macro(ReturnStmt, Stmt) \
    macro(ThrowStmt, Stmt) \
    macro(TryStmt, Stmt) \
    macro(CatchClause, CatchClause) \
    macro(FinallyClause, FinallyClause) \
    macro(SuspendStmt, Stmt) \
    macro(WaitForStmt, Stmt) \
    macro(WaitForClause, WaitForClause) \
    macro(WithStmt, Decl) \
    macro(NullStmt, NullStmt) \
    macro(DeclStmt, Stmt) \
    macro(ImplicitCtorDtorStmt, Stmt) \
    macro(MutexStmt, Stmt) \
    macro(ApplicationExpr, Expr) \
    macro(UntypedExpr, Expr) \
    macro(NameExpr, Expr) \
	macro(QualifiedNameExpr, Expr) \
    macro(AddressExpr, Expr) \
    macro(LabelAddressExpr, Expr) \
    macro(CastExpr, Expr) \
    macro(KeywordCastExpr, Expr) \
    macro(VirtualCastExpr, Expr) \
    macro(UntypedMemberExpr, Expr) \
    macro(MemberExpr, Expr) \
    macro(VariableExpr, Expr) \
    macro(ConstantExpr, Expr) \
    macro(SizeofExpr, Expr) \
    macro(AlignofExpr, Expr) \
    macro(UntypedOffsetofExpr, Expr) \
    macro(OffsetofExpr, Expr) \
    macro(OffsetPackExpr, Expr) \
    macro(LogicalExpr, Expr) \
    macro(ConditionalExpr, Expr) \
    macro(CommaExpr, Expr) \
    macro(TypeExpr, Expr) \
    macro(DimensionExpr, Expr) \
    macro(AsmExpr, Expr) \
    macro(ImplicitCopyCtorExpr, Expr) \
    macro(ConstructorExpr, Expr) \
    macro(CompoundLiteralExpr, Expr) \
    macro(RangeExpr, Expr) \
    macro(UntypedTupleExpr, Expr) \
    macro(TupleExpr, Expr) \
    macro(TupleIndexExpr, Expr) \
    macro(TupleAssignExpr, Expr) \
    macro(StmtExpr, Expr) \
    macro(UniqueExpr, Expr) \
    macro(UntypedInitExpr, Expr) \
    macro(InitExpr, Expr) \
    macro(DeletedExpr, Expr) \
    macro(DefaultArgExpr, Expr) \
    macro(GenericExpr, Expr) \
    macro(VoidType, Type) \
    macro(BasicType, Type) \
    macro(PointerType, Type) \
    macro(ArrayType, Type) \
    macro(ReferenceType, Type) \
    macro(QualifiedType, Type) \
    macro(FunctionType, Type) \
    macro(StructInstType, Type) \
    macro(UnionInstType, Type) \
    macro(EnumInstType, Type) \
    macro(TraitInstType, Type) \
    macro(TypeInstType, Type) \
    macro(TupleType, Type) \
    macro(TypeofType, Type) \
    macro(VTableType, Type) \
    macro(VarArgsType, Type) \
    macro(ZeroType, Type) \
    macro(OneType, Type) \
    macro(GlobalScopeType, Type) \
    macro(Designation, Designation) \
    macro(SingleInit, Init) \
    macro(ListInit, Init) \
    macro(ConstructorInit, Init) \
    macro(Attribute, Attribute) \
    macro(TypeSubstitution, TypeSubstitution)

// These could even go into the ast namespace.
enum class LeafKind {
#define VISIT(node_type, return_type) node_type,
	ALL_VISITS(VISIT)
#undef VISIT
};

struct LeafKindVisitor : public ast::Visitor {
	LeafKind kind;

#define VISIT(node_type, return_type) \
	const ast::return_type * visit( const ast::node_type * ) final { \
		kind = LeafKind::node_type; \
		return nullptr; \
	}
	ALL_VISITS(VISIT)
#undef VISIT
};

constexpr size_t leaf_kind_count = (1 + (size_t)LeafKind::TypeSubstitution);

LeafKind get_leaf_kind( ast::Node const * node ) {
	LeafKindVisitor visitor;
	node->accept( visitor );
	return visitor.kind;
}

const char * leaf_kind_names[leaf_kind_count] = {
#define VISIT(node_type, return_type) #node_type,
	ALL_VISITS(VISIT)
#undef VISIT
};

// Collect pointers to all the nodes with unset code locations.
class CollectCore {
	std::list< ast::ptr< ast::Node > > & unset;
public:
	CollectCore( std::list< ast::ptr< ast::Node > > & unset ) :
		unset( unset )
	{}

	template<typename node_t>
	auto previsit( node_t const * node ) -> decltype( node->location, void() ) {
		if ( node->location.isUnset() ) {
			unset.push_back( node );
		}
	}
};

} // namespace

void checkAllCodeLocations( ast::TranslationUnit const & unit ) {
	checkAllCodeLocations( "unknown location", unit );
}

void checkAllCodeLocations( char const * label, ast::TranslationUnit const & unit ) {
	std::list< ast::ptr< ast::Node > > unsetNodes;
	{
		ast::Pass<CollectCore> collector( unsetNodes );
		for ( auto node : unit.decls ) {
			node->accept( collector );
		}
	}
	if ( unsetNodes.empty() ) {
		return;
	}

	std::cerr << "Code Location check at " << label << " failed." << std::endl;
	std::cerr << "Total nodes without a set code location: "
		<< unsetNodes.size() << std::endl;

	size_t node_counts[leaf_kind_count] = {0};
	for ( auto unset : unsetNodes ) {
		node_counts[(size_t)get_leaf_kind(unset)] += 1;
	}
	for ( size_t i = 0 ; i < leaf_kind_count ; ++i ) {
		if ( node_counts[i] ) {
			std::cerr << '\t' << node_counts[i]
				<< " of type " << leaf_kind_names[i] << std::endl;
		}
	}

	assert( unsetNodes.empty() );
}

void forceFillCodeLocations( ast::TranslationUnit & unit ) {
	ast::Pass<FillCore>::run( unit );
}

ast::Node const * localFillCodeLocations(
		CodeLocation const & location , ast::Node const * node ) {
	ast::Pass<FillCore> visitor( location );
	return node->accept( visitor );
}
