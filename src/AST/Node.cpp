//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Node.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May 16 14:16:00 2019
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 25 10:30:00 2022
// Update Count     : 4
//

#include "Node.hpp"
#include "Fwd.hpp"

#include <csignal>  // MEMORY DEBUG -- for raise
#include <iostream>
#include <utility>

#include "Attribute.hpp"
#include "Decl.hpp"
#include "Expr.hpp"
#include "Init.hpp"
#include "Stmt.hpp"
#include "Type.hpp"
#include "TypeSubstitution.hpp"

#include "Print.hpp"

/// MEMORY DEBUG -- allows breaking on ref-count changes of dynamically chosen object.
/// Process to use in GDB:
///   break ast::Node::_trap()
///   run
///   set variable MEM_TRAP_OBJ = <target>
///   disable <first breakpoint>
///   continue
void * MEM_TRAP_OBJ = nullptr;

void _trap( const void * node ) {
	if ( node == MEM_TRAP_OBJ ) std::raise(SIGTRAP);
}

[[noreturn]] static inline void strict_fail(const ast::Node * node) {
	assertf(node, "strict_as had nullptr input.");
	const ast::ParseNode * parse = dynamic_cast<const ast::ParseNode *>( node );
	if ( nullptr == parse ) {
		assertf(false, "%s (no location)", toString(node).c_str());
	} else if ( parse->location.isUnset() ) {
		assertf(false, "%s (unset location)", toString(node).c_str());
	} else {
		assertf(false, "%s (at %s:%d)", toString(node).c_str(),
			parse->location.filename.c_str(), parse->location.first_line);
	}
}

template< typename node_t, enum ast::Node::ref_type ref_t >
void ast::ptr_base<node_t, ref_t>::_strict_fail() const {
	strict_fail(node);
}

template< typename node_t, enum ast::Node::ref_type ref_t >
void ast::ptr_base<node_t, ref_t>::_inc( const node_t * node ) {
	node->increment(ref_t);
	_trap( node );
}

template< typename node_t, enum ast::Node::ref_type ref_t >
void ast::ptr_base<node_t, ref_t>::_dec( const node_t * node, bool do_delete ) {
	_trap( node );
	node->decrement( ref_t, do_delete );
}

template< typename node_t, enum ast::Node::ref_type ref_t >
void ast::ptr_base<node_t, ref_t>::_check() const {
	// if(node) assert(node->was_ever_strong == false || node->strong_count > 0);
}

template< typename node_t, enum ast::Node::ref_type ref_t >
void ast::ptr_base<node_t, ref_t>::swap( ptr_base & other ) noexcept {
	std::swap( this->node, other.node );
	_trap( this->node );
	_trap( other.node );
}

template< typename node_t, enum ast::Node::ref_type ref_t >
node_t * ast::ptr_base<node_t, ref_t>::get_and_mutate() {
	// get mutable version of `n`
	auto r = mutate( node );
	// re-assign mutable version in case `mutate()` produced a new pointer
	assign( r );
	return r;
}

template< typename node_t, enum ast::Node::ref_type ref_t >
node_t * ast::ptr_base<node_t, ref_t>::set_and_mutate( const node_t * n ) {
	// ensure ownership of `n` by this node to avoid spurious single-owner mutates
	assign( n );
	// return mutable version
	return get_and_mutate();
}

std::ostream & ast::operator<< ( std::ostream & out, const ast::Node * node ) {
	print(out, node);
	return out;
}

template class ast::ptr_base< ast::Node, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Node, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ParseNode, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ParseNode, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Decl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Decl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DeclWithType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DeclWithType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ObjectDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ObjectDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::FunctionDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::FunctionDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AggregateDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AggregateDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::StructDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::StructDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UnionDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UnionDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::EnumDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::EnumDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TraitDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TraitDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::NamedTypeDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::NamedTypeDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypeDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypeDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypedefDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypedefDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AsmDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AsmDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DirectiveDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DirectiveDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::StaticAssertDecl, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::StaticAssertDecl, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Stmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Stmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CompoundStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CompoundStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ExprStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ExprStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AsmStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AsmStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DirectiveStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DirectiveStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::IfStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::IfStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::WhileDoStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::WhileDoStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ForStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ForStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::SwitchStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::SwitchStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CaseClause, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CaseClause, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::BranchStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::BranchStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ReturnStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ReturnStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ThrowStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ThrowStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TryStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TryStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CatchClause, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CatchClause, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::FinallyClause, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::FinallyClause, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::WaitForStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::WaitForStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::WithStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::WithStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DeclStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DeclStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::NullStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::NullStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ImplicitCtorDtorStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ImplicitCtorDtorStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::MutexStmt, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::MutexStmt, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Expr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Expr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ApplicationExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ApplicationExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UntypedExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UntypedExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::NameExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::NameExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AddressExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AddressExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::LabelAddressExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::LabelAddressExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CastExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CastExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::KeywordCastExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::KeywordCastExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::VirtualCastExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::VirtualCastExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::MemberExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::MemberExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UntypedMemberExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UntypedMemberExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::VariableExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::VariableExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ConstantExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ConstantExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::SizeofExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::SizeofExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AlignofExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AlignofExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UntypedOffsetofExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UntypedOffsetofExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::OffsetofExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::OffsetofExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::OffsetPackExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::OffsetPackExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::LogicalExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::LogicalExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ConditionalExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ConditionalExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CommaExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CommaExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypeExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypeExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::AsmExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::AsmExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ImplicitCopyCtorExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ImplicitCopyCtorExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ConstructorExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ConstructorExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::CompoundLiteralExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::CompoundLiteralExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::RangeExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::RangeExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UntypedTupleExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UntypedTupleExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TupleExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TupleExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TupleIndexExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TupleIndexExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TupleAssignExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TupleAssignExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::StmtExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::StmtExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UniqueExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UniqueExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UntypedInitExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UntypedInitExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::InitExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::InitExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DeletedExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DeletedExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::DefaultArgExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::DefaultArgExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::GenericExpr, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::GenericExpr, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Type, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Type, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::VoidType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::VoidType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::BasicType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::BasicType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::PointerType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::PointerType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ArrayType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ArrayType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ReferenceType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ReferenceType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::QualifiedType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::QualifiedType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::FunctionType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::FunctionType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::BaseInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::BaseInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::StructInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::StructInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::UnionInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::UnionInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::EnumInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::EnumInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TraitInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TraitInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypeInstType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypeInstType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TupleType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TupleType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypeofType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypeofType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::VarArgsType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::VarArgsType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ZeroType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ZeroType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::OneType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::OneType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::GlobalScopeType, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::GlobalScopeType, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Designation, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Designation, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Init, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Init, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::SingleInit, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::SingleInit, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ListInit, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ListInit, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::ConstructorInit, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::ConstructorInit, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::Attribute, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::Attribute, ast::Node::ref_type::strong >;
template class ast::ptr_base< ast::TypeSubstitution, ast::Node::ref_type::weak >;
template class ast::ptr_base< ast::TypeSubstitution, ast::Node::ref_type::strong >;
