//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Visitor.hpp -- Interface of a class that visits ast nodes.
//
// Author           : Andrew Beach
// Created On       : Thr May 9 15:28:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 18:25:07 2021
// Update Count     : 1
//

#pragma once

#include "Fwd.hpp"

namespace ast {

class Visitor {
public:
    virtual const ast::DeclWithType *     visit( const ast::ObjectDecl           * ) = 0;
    virtual const ast::DeclWithType *     visit( const ast::FunctionDecl         * ) = 0;
    virtual const ast::Decl *             visit( const ast::StructDecl           * ) = 0;
    virtual const ast::Decl *             visit( const ast::UnionDecl            * ) = 0;
    virtual const ast::Decl *             visit( const ast::EnumDecl             * ) = 0;
    virtual const ast::Decl *             visit( const ast::TraitDecl            * ) = 0;
    virtual const ast::Decl *             visit( const ast::TypeDecl             * ) = 0;
    virtual const ast::Decl *             visit( const ast::TypedefDecl          * ) = 0;
    virtual const ast::AsmDecl *          visit( const ast::AsmDecl              * ) = 0;
    virtual const ast::DirectiveDecl *    visit( const ast::DirectiveDecl        * ) = 0;
    virtual const ast::StaticAssertDecl * visit( const ast::StaticAssertDecl     * ) = 0;
    virtual const ast::CompoundStmt *     visit( const ast::CompoundStmt         * ) = 0;
    virtual const ast::Stmt *             visit( const ast::ExprStmt             * ) = 0;
    virtual const ast::Stmt *             visit( const ast::AsmStmt              * ) = 0;
    virtual const ast::Stmt *             visit( const ast::DirectiveStmt        * ) = 0;
    virtual const ast::Stmt *             visit( const ast::IfStmt               * ) = 0;
    virtual const ast::Stmt *             visit( const ast::WhileStmt            * ) = 0;
    virtual const ast::Stmt *             visit( const ast::ForStmt              * ) = 0;
    virtual const ast::Stmt *             visit( const ast::SwitchStmt           * ) = 0;
    virtual const ast::Stmt *             visit( const ast::CaseStmt             * ) = 0;
    virtual const ast::Stmt *             visit( const ast::BranchStmt           * ) = 0;
    virtual const ast::Stmt *             visit( const ast::ReturnStmt           * ) = 0;
    virtual const ast::Stmt *             visit( const ast::ThrowStmt            * ) = 0;
    virtual const ast::Stmt *             visit( const ast::TryStmt              * ) = 0;
    virtual const ast::Stmt *             visit( const ast::CatchStmt            * ) = 0;
    virtual const ast::Stmt *             visit( const ast::FinallyStmt          * ) = 0;
    virtual const ast::Stmt *             visit( const ast::SuspendStmt          * ) = 0;
    virtual const ast::Stmt *             visit( const ast::WaitForStmt          * ) = 0;
    virtual const ast::Decl *             visit( const ast::WithStmt             * ) = 0;
    virtual const ast::NullStmt *         visit( const ast::NullStmt             * ) = 0;
    virtual const ast::Stmt *             visit( const ast::DeclStmt             * ) = 0;
    virtual const ast::Stmt *             visit( const ast::ImplicitCtorDtorStmt * ) = 0;
    virtual const ast::Expr *             visit( const ast::ApplicationExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::UntypedExpr          * ) = 0;
    virtual const ast::Expr *             visit( const ast::NameExpr             * ) = 0;
    virtual const ast::Expr *             visit( const ast::AddressExpr          * ) = 0;
    virtual const ast::Expr *             visit( const ast::LabelAddressExpr     * ) = 0;
    virtual const ast::Expr *             visit( const ast::CastExpr             * ) = 0;
    virtual const ast::Expr *             visit( const ast::KeywordCastExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::VirtualCastExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::UntypedMemberExpr    * ) = 0;
    virtual const ast::Expr *             visit( const ast::MemberExpr           * ) = 0;
    virtual const ast::Expr *             visit( const ast::VariableExpr         * ) = 0;
    virtual const ast::Expr *             visit( const ast::ConstantExpr         * ) = 0;
    virtual const ast::Expr *             visit( const ast::SizeofExpr           * ) = 0;
    virtual const ast::Expr *             visit( const ast::AlignofExpr          * ) = 0;
    virtual const ast::Expr *             visit( const ast::UntypedOffsetofExpr  * ) = 0;
    virtual const ast::Expr *             visit( const ast::OffsetofExpr         * ) = 0;
    virtual const ast::Expr *             visit( const ast::OffsetPackExpr       * ) = 0;
    virtual const ast::Expr *             visit( const ast::LogicalExpr          * ) = 0;
    virtual const ast::Expr *             visit( const ast::ConditionalExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::CommaExpr            * ) = 0;
    virtual const ast::Expr *             visit( const ast::TypeExpr             * ) = 0;
    virtual const ast::Expr *             visit( const ast::AsmExpr              * ) = 0;
    virtual const ast::Expr *             visit( const ast::ImplicitCopyCtorExpr * ) = 0;
    virtual const ast::Expr *             visit( const ast::ConstructorExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::CompoundLiteralExpr  * ) = 0;
    virtual const ast::Expr *             visit( const ast::RangeExpr            * ) = 0;
    virtual const ast::Expr *             visit( const ast::UntypedTupleExpr     * ) = 0;
    virtual const ast::Expr *             visit( const ast::TupleExpr            * ) = 0;
    virtual const ast::Expr *             visit( const ast::TupleIndexExpr       * ) = 0;
    virtual const ast::Expr *             visit( const ast::TupleAssignExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::StmtExpr             * ) = 0;
    virtual const ast::Expr *             visit( const ast::UniqueExpr           * ) = 0;
    virtual const ast::Expr *             visit( const ast::UntypedInitExpr      * ) = 0;
    virtual const ast::Expr *             visit( const ast::InitExpr             * ) = 0;
    virtual const ast::Expr *             visit( const ast::DeletedExpr          * ) = 0;
    virtual const ast::Expr *             visit( const ast::DefaultArgExpr       * ) = 0;
    virtual const ast::Expr *             visit( const ast::GenericExpr          * ) = 0;
    virtual const ast::Type *             visit( const ast::VoidType             * ) = 0;
    virtual const ast::Type *             visit( const ast::BasicType            * ) = 0;
    virtual const ast::Type *             visit( const ast::PointerType          * ) = 0;
    virtual const ast::Type *             visit( const ast::ArrayType            * ) = 0;
    virtual const ast::Type *             visit( const ast::ReferenceType        * ) = 0;
    virtual const ast::Type *             visit( const ast::QualifiedType        * ) = 0;
    virtual const ast::Type *             visit( const ast::FunctionType         * ) = 0;
    virtual const ast::Type *             visit( const ast::StructInstType       * ) = 0;
    virtual const ast::Type *             visit( const ast::UnionInstType        * ) = 0;
    virtual const ast::Type *             visit( const ast::EnumInstType         * ) = 0;
    virtual const ast::Type *             visit( const ast::TraitInstType        * ) = 0;
    virtual const ast::Type *             visit( const ast::TypeInstType         * ) = 0;
    virtual const ast::Type *             visit( const ast::TupleType            * ) = 0;
    virtual const ast::Type *             visit( const ast::TypeofType           * ) = 0;
    virtual const ast::Type *             visit( const ast::VTableType           * ) = 0;
    virtual const ast::Type *             visit( const ast::VarArgsType          * ) = 0;
    virtual const ast::Type *             visit( const ast::ZeroType             * ) = 0;
    virtual const ast::Type *             visit( const ast::OneType              * ) = 0;
    virtual const ast::Type *             visit( const ast::GlobalScopeType      * ) = 0;
    virtual const ast::Designation *      visit( const ast::Designation          * ) = 0;
    virtual const ast::Init *             visit( const ast::SingleInit           * ) = 0;
    virtual const ast::Init *             visit( const ast::ListInit             * ) = 0;
    virtual const ast::Init *             visit( const ast::ConstructorInit      * ) = 0;
    virtual const ast::Attribute *        visit( const ast::Attribute            * ) = 0;
    virtual const ast::TypeSubstitution * visit( const ast::TypeSubstitution     * ) = 0;
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
