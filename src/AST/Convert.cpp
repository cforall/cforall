//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Convert.cpp -- Convert between the new and old syntax trees.
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15::37::05 2019
// Last Modified By : Andrew Beach
// Last Modified On : Wed Apr 20 13:58:00 2022
// Update Count     : 43
//

#include "Convert.hpp"

#include <deque>
#include <unordered_map>

#include "AST/Attribute.hpp"
#include "AST/Copy.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Init.hpp"
#include "AST/Stmt.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/TypeSubstitution.hpp"

#include "SymTab/Autogen.h"
#include "SynTree/Attribute.h"
#include "SynTree/Declaration.h"
#include "SynTree/TypeSubstitution.h"

#include "Validate/FindSpecialDecls.h"

//================================================================================================
// Utilities
template<template <class...> class C>
struct to {
	template<typename T>
	static auto from( T && v ) -> C< typename T::value_type > {
		C< typename T::value_type > l;
		std::move(std::begin(v), std::end(v), std::back_inserter(l));
		return l;
	}
};

//================================================================================================
namespace ast {
// These are the shared local information used by ConverterNewToOld and
// ConverterOldToNew to update the global information in the two versions.

static ast::ptr<ast::Type> sizeType = nullptr;
static const ast::FunctionDecl * dereferenceOperator = nullptr;
static const ast::StructDecl   * dtorStruct = nullptr;
static const ast::FunctionDecl * dtorStructDestroy = nullptr;

}

//================================================================================================
class ConverterNewToOld : public ast::Visitor {
	BaseSyntaxNode * node = nullptr;
	using Cache = std::unordered_map< const ast::Node *, BaseSyntaxNode * >;
	Cache cache;

	// Statements can no longer be shared.
	// however, since StmtExprResult is now implemented, need to still maintain
	// readonly references.
	Cache readonlyCache;

	template<typename T>
	struct Getter {
		ConverterNewToOld & visitor;

		template<typename U, enum ast::Node::ref_type R>
		T * accept1( const ast::ptr_base<U, R> & ptr ) {
			if ( ! ptr ) return nullptr;
			ptr->accept( visitor );
			T * ret = strict_dynamic_cast< T * >( visitor.node );
			visitor.node = nullptr;
			return ret;
		}

		template<typename U>
		std::list< T * > acceptL( const U & container ) {
			std::list< T * > ret;
			for ( auto ptr : container ) {
				ret.emplace_back( accept1( ptr ) );
			}
			return ret;
		}
	};

	template<typename T>
	Getter<T> get() {
		return Getter<T>{ *this };
	}

	Label makeLabel(Statement * labelled, const ast::Label& label) {
		// This probably will leak memory, but only until we get rid of the old tree.
		if ( nullptr == labelled && label.location.isSet() ) {
			labelled = new NullStmt();
			labelled->location = label.location;
		}
		return Label(
			label.name,
			labelled,
			get<Attribute>().acceptL(label.attributes)
		);
	}

	template<template <class...> class C>
	std::list<Label> makeLabelL(Statement * labelled, const C<ast::Label>& labels) {
		std::list<Label> ret;
		for (auto label : labels) {
			ret.push_back( makeLabel(labelled, label) );
		}
		return ret;
	}

	/// get new qualifiers from old type
	Type::Qualifiers cv( const ast::Type * ty ) { return { ty->qualifiers.val }; }

	/// returns true and sets `node` if in cache
	bool inCache( const ast::Node * node ) {
		auto it = cache.find( node );
		if ( it == cache.end() ) return false;
		this->node = it->second;
		return true;
	}

public:
	Declaration * decl( const ast::Decl * declNode ) {
		return get<Declaration>().accept1( ast::ptr<ast::Decl>( declNode ) );
	}

private:
	void declPostamble( Declaration * decl, const ast::Decl * node ) {
		decl->location = node->location;
		// name comes from constructor
		// linkage comes from constructor
		decl->extension = node->extension;
		decl->uniqueId = node->uniqueId;
		// storageClasses comes from constructor
		this->node = decl;
	}

	const ast::DeclWithType * declWithTypePostamble (
			DeclarationWithType * decl, const ast::DeclWithType * node ) {
		cache.emplace( node, decl );
		decl->mangleName = node->mangleName;
		decl->scopeLevel = node->scopeLevel;
		decl->asmName = get<Expression>().accept1( node->asmName );
		// attributes comes from constructor
		decl->isDeleted = node->isDeleted;
		// fs comes from constructor
		declPostamble( decl, node );
		return nullptr;
	}

	const ast::DeclWithType * visit( const ast::ObjectDecl * node ) override final {	
		if ( inCache( node ) ) {
			return nullptr;
		}
		auto bfwd = get<Expression>().accept1( node->bitfieldWidth );
		auto type = get<Type>().accept1( node->type );
		auto attr = get<Attribute>().acceptL( node->attributes );

		// This field can be unset very early on (Pre-FixReturnTypes).
		auto newType = (type) ? type->clone() : nullptr;

		auto decl = new ObjectDecl(
			node->name,
			Type::StorageClasses( node->storage.val ),
			LinkageSpec::Spec( node->linkage.val ),
			bfwd,
			newType,
			nullptr, // prevent infinite loop
			attr,
			Type::FuncSpecifiers( node->funcSpec.val )
		);

		// handles the case where node->init references itself
		// xxx - does it really happen?
		declWithTypePostamble(decl, node);
		auto init = get<Initializer>().accept1( node->init );
		decl->init = init;

		this->node = decl;
		return nullptr;
	}

	const ast::DeclWithType * visit( const ast::FunctionDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;

		// function decl contains real variables that the type must use.
		// the structural change means function type in and out of decl
		// must be handled **differently** on convert back to old.
		auto ftype = new FunctionType(
			cv(node->type),
			(bool)node->type->isVarArgs
		);
		ftype->returnVals = get<DeclarationWithType>().acceptL(node->returns);
		ftype->parameters = get<DeclarationWithType>().acceptL(node->params);

		ftype->forall = get<TypeDecl>().acceptL( node->type_params );
		if (!node->assertions.empty()) {
			assert(!ftype->forall.empty());
			// find somewhere to place assertions back, for convenience it is the last slot
			ftype->forall.back()->assertions = get<DeclarationWithType>().acceptL(node->assertions);
		}

		visitType(node->type, ftype);

		auto decl = new FunctionDecl(
			node->name,
			Type::StorageClasses( node->storage.val ),
			LinkageSpec::Spec( node->linkage.val ),
			ftype,
			//get<FunctionType>().accept1( node->type ),
			{},
			get<Attribute>().acceptL( node->attributes ),
			Type::FuncSpecifiers( node->funcSpec.val )
		);
		cache.emplace( node, decl );
		decl->statements = get<CompoundStmt>().accept1( node->stmts );
		decl->withExprs = get<Expression>().acceptL( node->withExprs );
		if ( ast::dereferenceOperator == node ) {
			Validate::dereferenceOperator = decl;
		}
		if ( ast::dtorStructDestroy == node ) {
			Validate::dtorStructDestroy = decl;
		}
		return declWithTypePostamble( decl, node );
	}

	// InlineMemberDecl vanish after EnumAndPointerDecay pass so no necessary to implement NewToOld
	const ast::DeclWithType * visit( const ast::InlineMemberDecl * node ) override final {	
		assert( false );
		(void) node;
		return nullptr;
	}

	const ast::Decl * namedTypePostamble( NamedTypeDecl * decl, const ast::NamedTypeDecl * node ) {
		// base comes from constructor
		decl->assertions = get<DeclarationWithType>().acceptL( node->assertions );
		declPostamble( decl, node );
		return nullptr;
	}

	const ast::Decl * visit( const ast::TypeDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto decl = new TypeDecl(
			node->name,
			Type::StorageClasses( node->storage.val ),
			get<Type>().accept1( node->base ),
			(TypeDecl::Kind)(unsigned)node->kind,
			node->sized,
			get<Type>().accept1( node->init )
		);
		cache.emplace( node, decl );
		return namedTypePostamble( decl, node );
	}

	const ast::Decl * visit( const ast::TypedefDecl * node ) override final {
		auto decl = new TypedefDecl(
			node->name,
			node->location,
			Type::StorageClasses( node->storage.val ),
            get<Type>().accept1( node->base ),
			LinkageSpec::Spec( node->linkage.val )
		);
		return namedTypePostamble( decl, node );
	}

	const ast::Decl * aggregatePostamble( AggregateDecl * decl, const ast::AggregateDecl * node ) {
		cache.emplace( node, decl );
		decl->members = get<Declaration>().acceptL( node->members );
		decl->parameters = get<TypeDecl>().acceptL( node->params );
		decl->body = node->body;
		// attributes come from constructor
		decl->parent = get<AggregateDecl>().accept1( node->parent );
		declPostamble( decl, node );
		return nullptr; // ??
	}

	const ast::Decl * visit( const ast::StructDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto decl = new StructDecl(
			node->name,
			(AggregateDecl::Aggregate)node->kind,
			get<Attribute>().acceptL( node->attributes ),
			LinkageSpec::Spec( node->linkage.val )
		);

		if ( ast::dtorStruct == node ) {
			Validate::dtorStruct = decl;
		}

		return aggregatePostamble( decl, node );
	}

	const ast::Decl * visit( const ast::UnionDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto decl = new UnionDecl(
			node->name,
			get<Attribute>().acceptL( node->attributes ),
			LinkageSpec::Spec( node->linkage.val )
		);
		return aggregatePostamble( decl, node );
	}

	const ast::Decl * visit( const ast::EnumDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto decl = new EnumDecl(
			node->name,
			get<Attribute>().acceptL( node->attributes ),
			node->isTyped,
			LinkageSpec::Spec( node->linkage.val ),
			get<Type>().accept1(node->base)
		);
		return aggregatePostamble( decl, node );
	}

	const ast::Decl * visit( const ast::TraitDecl * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto decl = new TraitDecl(
			node->name,
			{},
			LinkageSpec::Spec( node->linkage.val )
		);
		return aggregatePostamble( decl, node );
	}

	const ast::AsmDecl * visit( const ast::AsmDecl * node ) override final {
		auto decl = new AsmDecl( get<AsmStmt>().accept1( node->stmt ) );
		declPostamble( decl, node );
		return nullptr;
	}

	const ast::DirectiveDecl * visit( const ast::DirectiveDecl * node ) override final {
		auto decl = new DirectiveDecl( get<DirectiveStmt>().accept1( node->stmt ) );
		declPostamble( decl, node );
		return nullptr;
	}

	const ast::StaticAssertDecl * visit( const ast::StaticAssertDecl * node ) override final {
		auto decl = new StaticAssertDecl(
			get<Expression>().accept1( node->cond ),
			get<ConstantExpr>().accept1( node->msg )
		);
		declPostamble( decl, node );
		return nullptr;
	}

	const ast::Stmt * stmtPostamble( Statement * stmt, const ast::Stmt * node ) {
		// force statements in old tree to be unique.
		// cache.emplace( node, stmt );
		readonlyCache.emplace( node, stmt );
		stmt->location = node->location;
		stmt->labels = makeLabelL( stmt, node->labels );
		this->node = stmt;
		return nullptr;
	}

	void clausePostamble( Statement * stmt, const ast::StmtClause * node ) {
		stmt->location = node->location;
		this->node = stmt;
	}

	const ast::CompoundStmt * visit( const ast::CompoundStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new CompoundStmt( get<Statement>().acceptL( node->kids ) );
		stmtPostamble( stmt, node );
		return nullptr;
	}

	const ast::Stmt * visit( const ast::ExprStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new ExprStmt( nullptr );
		stmt->expr = get<Expression>().accept1( node->expr );
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::AsmStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new AsmStmt(
			node->isVolatile,
			get<Expression>().accept1( node->instruction ),
			get<Expression>().acceptL( node->output ),
			get<Expression>().acceptL( node->input ),
			get<ConstantExpr>().acceptL( node->clobber ),
			makeLabelL( nullptr, node->gotoLabels ) // What are these labelling?
		);
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::DirectiveStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new DirectiveStmt( node->directive );
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::IfStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new IfStmt(
			get<Expression>().accept1( node->cond ),
			get<Statement>().accept1( node->then ),
			get<Statement>().accept1( node->else_ ),
			get<Statement>().acceptL( node->inits )
		);
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::SwitchStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new SwitchStmt(
			get<Expression>().accept1( node->cond ),
			get<Statement>().acceptL( node->cases )
		);
		return stmtPostamble( stmt, node );
	}

	const ast::CaseClause * visit( const ast::CaseClause * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new CaseStmt(
			get<Expression>().accept1( node->cond ),
			get<Statement>().acceptL( node->stmts ),
			node->isDefault()
		);
		clausePostamble( stmt, node );
		return nullptr;
	}

	const ast::Stmt * visit( const ast::WhileDoStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto inits = get<Statement>().acceptL( node->inits );
		auto stmt = new WhileDoStmt(
			get<Expression>().accept1( node->cond ),
			get<Statement>().accept1( node->body ),
			get<Statement>().accept1( node->else_ ),
			inits,
			node->isDoWhile
		);
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::ForStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new ForStmt(
			get<Statement>().acceptL( node->inits ),
			get<Expression>().accept1( node->cond ),
			get<Expression>().accept1( node->inc ),
			get<Statement>().accept1( node->body ),
			get<Statement>().accept1( node->else_ )
		);
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::BranchStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		BranchStmt * stmt;
		if (node->computedTarget) {
			stmt = new BranchStmt( get<Expression>().accept1( node->computedTarget ),
				BranchStmt::Goto );
		} else {
			BranchStmt::Type type;
			switch (node->kind) {
			#define CASE(n) \
			case ast::BranchStmt::n: \
				type = BranchStmt::n; \
				break
			CASE(Goto);
			CASE(Break);
			CASE(Continue);
			CASE(FallThrough);
			CASE(FallThroughDefault);
			#undef CASE
			default:
				assertf(false, "Invalid ast::BranchStmt::Kind: %d\n", node->kind);
			}

			// The labels here are also weird.
			stmt = new BranchStmt( makeLabel( nullptr, node->originalTarget ), type );
			stmt->target = makeLabel( stmt, node->target );
		}
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::ReturnStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new ReturnStmt( get<Expression>().accept1( node->expr ) );
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::ThrowStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		ThrowStmt::Kind kind;
		switch (node->kind) {
		case ast::ExceptionKind::Terminate:
			kind = ThrowStmt::Terminate;
			break;
		case ast::ExceptionKind::Resume:
			kind = ThrowStmt::Resume;
			break;
		default:
			assertf(false, "Invalid ast::ThrowStmt::Kind: %d\n", node->kind);
		}
		auto stmt = new ThrowStmt(
			kind,
			get<Expression>().accept1( node->expr ),
			get<Expression>().accept1( node->target )
		);
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::TryStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto handlers = get<CatchStmt>().acceptL( node->handlers );
		auto stmt = new TryStmt(
			get<CompoundStmt>().accept1( node->body ),
			handlers,
			get<FinallyStmt>().accept1( node->finally )
		);
		return stmtPostamble( stmt, node );
	}

	const ast::CatchClause * visit( const ast::CatchClause * node ) override final {
		if ( inCache( node ) ) return nullptr;
		CatchStmt::Kind kind;
		switch (node->kind) {
		case ast::ExceptionKind::Terminate:
			kind = CatchStmt::Terminate;
			break;
		case ast::ExceptionKind::Resume:
			kind = CatchStmt::Resume;
			break;
		default:
			assertf(false, "Invalid ast::ExceptionKind: %d\n", node->kind);
		}
		auto stmt = new CatchStmt(
			kind,
			get<Declaration>().accept1( node->decl ),
			get<Expression>().accept1( node->cond ),
			get<Statement>().accept1( node->body )
		);
		return clausePostamble( stmt, node ), nullptr;
	}

	const ast::FinallyClause * visit( const ast::FinallyClause * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new FinallyStmt( get<CompoundStmt>().accept1( node->body ) );
		return clausePostamble( stmt, node ), nullptr;
	}

	const ast::Stmt * visit(const ast::SuspendStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new SuspendStmt();
		stmt->then   = get<CompoundStmt>().accept1( node->then   );
		switch (node->kind) {
			case ast::SuspendStmt::None     : stmt->type = SuspendStmt::None     ; break;
			case ast::SuspendStmt::Coroutine: stmt->type = SuspendStmt::Coroutine; break;
			case ast::SuspendStmt::Generator: stmt->type = SuspendStmt::Generator; break;
		}
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::WaitForStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new WaitForStmt;
		stmt->clauses.reserve( node->clauses.size() );
		for ( auto clause : node->clauses ) {
			stmt->clauses.push_back({{
					get<Expression>().accept1( clause->target_func ),
					get<Expression>().acceptL( clause->target_args ),
				},
				get<Statement>().accept1( clause->stmt ),
				get<Expression>().accept1( clause->cond ),
			});
		}
		stmt->timeout = {
			get<Expression>().accept1( node->timeout_time ),
			get<Statement>().accept1( node->timeout_stmt ),
			get<Expression>().accept1( node->timeout_cond ),
		};
		stmt->orelse = {
			get<Statement>().accept1( node->else_stmt ),
			get<Expression>().accept1( node->else_cond ),
		};
		return stmtPostamble( stmt, node );
	}

	const ast::WaitForClause * visit( const ast::WaitForClause * node ) override final {
		// There is no old-AST WaitForClause, so this should never be called.
		assert( !node );
		return nullptr;
	}

	const ast::Decl * visit( const ast::WithStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new WithStmt(
			get<Expression>().acceptL( node->exprs ),
			get<Statement>().accept1( node->stmt )
		);
		declPostamble( stmt, node );
		return nullptr;
	}

	const ast::NullStmt * visit( const ast::NullStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new NullStmt();
		stmtPostamble( stmt, node );
		return nullptr;
	}

	const ast::Stmt * visit( const ast::DeclStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new DeclStmt( get<Declaration>().accept1( node->decl ) );
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::ImplicitCtorDtorStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		auto stmt = new ImplicitCtorDtorStmt{
			get<Statement>().accept1( node->callStmt )
		};
		return stmtPostamble( stmt, node );
	}

	const ast::Stmt * visit( const ast::MutexStmt * node ) override final {
		if ( inCache( node ) ) return nullptr;
		 auto stmt = new MutexStmt(
			get<Statement>().accept1( node->stmt ),
		 	get<Expression>().acceptL( node->mutexObjs )
		);
		return stmtPostamble( stmt, node );
	}

	TypeSubstitution * convertTypeSubstitution(const ast::TypeSubstitution * src) {

		if (!src) return nullptr;

		TypeSubstitution *rslt = new TypeSubstitution();

		for (decltype(src->begin()) src_i = src->begin(); src_i != src->end(); src_i++) {
			rslt->add( src_i->first.typeString(),
			           get<Type>().accept1(src_i->second) );
		}

		return rslt;
	}

	void convertInferUnion(std::map<UniqueId,ParamEntry> &tgtInferParams,
						   std::vector<UniqueId>         &tgtResnSlots,
						   const ast::Expr::InferUnion   &srcInferred ) {

		assert( tgtInferParams.empty() );
		assert( tgtResnSlots.empty() );

		if ( srcInferred.data.inferParams ) {
			const ast::InferredParams &srcParams = srcInferred.inferParams();
			for (auto & srcParam : srcParams) {
				auto res = tgtInferParams.emplace(srcParam.first, ParamEntry(
					srcParam.second.decl,
					get<Declaration>().accept1(srcParam.second.declptr),
					get<Type>().accept1(srcParam.second.actualType)->clone(),
					get<Type>().accept1(srcParam.second.formalType)->clone(),
					get<Expression>().accept1(srcParam.second.expr)->clone()
				));
				assert(res.second);
			}
		}
		if ( srcInferred.data.resnSlots ) {
			const ast::ResnSlots &srcSlots = srcInferred.resnSlots();
			for (auto srcSlot : srcSlots) {
				tgtResnSlots.push_back(srcSlot);
			}
		}
	}

	Expression * visitBaseExpr_skipResultType(const ast::Expr * src, Expression * tgt) {

		tgt->location  = src->location;
		tgt->env       = convertTypeSubstitution(src->env);
		tgt->extension = src->extension;

		convertInferUnion(tgt->inferParams, tgt->resnSlots, src->inferred);
		return tgt;
	}

	Expression * visitBaseExpr(const ast::Expr * src, Expression * tgt) {

		tgt->result = get<Type>().accept1(src->result);
		// Unconditionally use a clone of the result type.
		// We know this will leak some objects: much of the immediate conversion result.
		// In some cases, using the conversion result directly gives unintended object sharing.
		// A parameter (ObjectDecl, a child of a FunctionType) is shared by the weak-ref cache.
		// But tgt->result must be fully owned privately by tgt.
		// Applying these conservative copies here means
		// - weak references point at the declaration's copy, not these expr.result copies (good)
		// - we copy more objects than really needed (bad, tolerated)
		if (tgt->result) {
			tgt->result = tgt->result->clone();
		}
		return visitBaseExpr_skipResultType(src, tgt);
	}

	const ast::Expr * visit( const ast::ApplicationExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new ApplicationExpr(
				get<Expression>().accept1(node->func),
				get<Expression>().acceptL(node->args)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::UntypedExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new UntypedExpr(
				get<Expression>().accept1(node->func),
				get<Expression>().acceptL(node->args)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::NameExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new NameExpr(
				node->name
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::QualifiedNameExpr * node ) override final {
		auto temp = new QualifiedNameExpr(
				get<Declaration>().accept1(node->type_decl),
				node->name
		);
		auto expr = visitBaseExpr( node,
			temp
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::AddressExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new AddressExpr(
				get<Expression>().accept1(node->arg)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::LabelAddressExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new LabelAddressExpr(
				makeLabel(nullptr, node->arg)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::CastExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new CastExpr(
				get<Expression>().accept1(node->arg),
				(node->isGenerated == ast::GeneratedCast)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::KeywordCastExpr * node ) override final {
		AggregateDecl::Aggregate castTarget = (AggregateDecl::Aggregate)node->target;
		assert( AggregateDecl::Generator <= castTarget && castTarget <= AggregateDecl::Thread );
		auto expr = visitBaseExpr( node,
			new KeywordCastExpr(
				get<Expression>().accept1(node->arg),
				castTarget,
				{node->concrete_target.field, node->concrete_target.getter}
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::VirtualCastExpr * node ) override final {
		auto expr = visitBaseExpr_skipResultType( node,
			new VirtualCastExpr(
				get<Expression>().accept1(node->arg),
				get<Type>().accept1(node->result)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::UntypedMemberExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new UntypedMemberExpr(
				get<Expression>().accept1(node->member),
				get<Expression>().accept1(node->aggregate)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::MemberExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new MemberExpr(
				get<DeclarationWithType>().accept1(node->member),
				get<Expression>().accept1(node->aggregate)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::VariableExpr * node ) override final {
		auto expr = new VariableExpr();
		expr->var = get<DeclarationWithType>().accept1(node->var);
		visitBaseExpr( node, expr );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::ConstantExpr * node ) override final {
		// Old world:   two types: rslt->constant.type, rslt->result
		// New workd:   one public type: node->result, plus node->underlyer only to support roundtrip conversion
		//              preserving underlyer because the correct type for string literals is complicated to construct,
	    //              and distinguishing a string from other literals using the type is hard to do accurately
		// Both worlds: the outer, expression-level type can change during resolution
		//              for a string, that's char[k] before-resolve and char * after
		// Old world:   the inner Constant type stays what it was built with
		//              for a string, that's char[k] always
		// Both worlds: the "rep" field of a constant is the C source file fragment that compiles to the desired value
        //              for a string, that includes outer quotes, backslashes, et al cases from the Literals test
		ConstantExpr *rslt = new ConstantExpr(Constant(
			get<Type>().accept1(node->underlyer),
			node->rep,
			node->ival));
		auto expr = visitBaseExpr( node, rslt );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::SizeofExpr * node ) override final {
		assert (node->expr || node->type);
		assert (! (node->expr && node->type));
		SizeofExpr *rslt;
		if (node->expr) {
			rslt = new SizeofExpr(
				get<Expression>().accept1(node->expr)
			);
			assert (!rslt->isType);
		}
		else {
			assert(node->type);
			rslt = new SizeofExpr(
				get<Type>().accept1(node->type)
			);
			assert (rslt->isType);
		}
		auto expr = visitBaseExpr( node, rslt );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::AlignofExpr * node ) override final {
		assert (node->expr || node->type);
		assert (! (node->expr && node->type));
		AlignofExpr *rslt;
		if (node->expr) {
			rslt = new AlignofExpr(
				get<Expression>().accept1(node->expr)
			);
			assert (!rslt->isType);
		}
		else {
			assert(node->type);
			rslt = new AlignofExpr(
				get<Type>().accept1(node->type)
			);
			assert (rslt->isType);
		}
		auto expr = visitBaseExpr( node, rslt );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::UntypedOffsetofExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new UntypedOffsetofExpr(
				get<Type>().accept1(node->type),
				node->member
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::OffsetofExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new OffsetofExpr(
				get<Type>().accept1(node->type),
				get<DeclarationWithType>().accept1(node->member)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::OffsetPackExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new OffsetPackExpr(
				get<StructInstType>().accept1(node->type)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::LogicalExpr * node ) override final {
		assert (node->isAnd == ast::LogicalFlag::AndExpr ||
				node->isAnd == ast::LogicalFlag::OrExpr	);
		auto expr = visitBaseExpr( node,
			new LogicalExpr(
				get<Expression>().accept1(node->arg1),
				get<Expression>().accept1(node->arg2),
				(node->isAnd == ast::LogicalFlag::AndExpr)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::ConditionalExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new ConditionalExpr(
				get<Expression>().accept1(node->arg1),
				get<Expression>().accept1(node->arg2),
				get<Expression>().accept1(node->arg3)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::CommaExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new CommaExpr(
				get<Expression>().accept1(node->arg1),
				get<Expression>().accept1(node->arg2)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::TypeExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new TypeExpr(
				get<Type>().accept1(node->type)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::DimensionExpr * node ) override final {
		auto expr = visitBaseExpr( node, new DimensionExpr( node->name ) );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::AsmExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new AsmExpr(
				new std::string(node->inout),
				get<Expression>().accept1(node->constraint),
				get<Expression>().accept1(node->operand)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::ImplicitCopyCtorExpr * node ) override final {
		auto rslt = new ImplicitCopyCtorExpr(
			get<ApplicationExpr>().accept1(node->callExpr)
		);

		auto expr = visitBaseExpr( node, rslt );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::ConstructorExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new ConstructorExpr(
				get<Expression>().accept1(node->callExpr)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::CompoundLiteralExpr * node ) override final {
		auto expr = visitBaseExpr_skipResultType( node,
			new CompoundLiteralExpr(
				get<Type>().accept1(node->result),
				get<Initializer>().accept1(node->init)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::RangeExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new RangeExpr(
				get<Expression>().accept1(node->low),
				get<Expression>().accept1(node->high)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::UntypedTupleExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new UntypedTupleExpr(
				get<Expression>().acceptL(node->exprs)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::TupleExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new TupleExpr(
				get<Expression>().acceptL(node->exprs)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::TupleIndexExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new TupleIndexExpr(
				get<Expression>().accept1(node->tuple),
				node->index
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::TupleAssignExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new TupleAssignExpr(
				get<StmtExpr>().accept1(node->stmtExpr)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::StmtExpr * node ) override final {
		auto rslt = new StmtExpr(
			get<CompoundStmt>().accept1(node->stmts)
		);

		rslt->returnDecls = get<ObjectDecl>().acceptL(node->returnDecls);
		rslt->dtors       = get<Expression>().acceptL(node->dtors);

		// is this even used after convert?
		//if (tmp->resultExpr) {
		//	// this MUST be found by children visit
		//	rslt->resultExpr  = strict_dynamic_cast<ExprStmt *>(readonlyCache.at(tmp->resultExpr));
		//}

		auto expr = visitBaseExpr( node, rslt );
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::UniqueExpr * node ) override final {
		auto rslt = new UniqueExpr(
			get<Expression>().accept1(node->expr),
			node->id
		);

		rslt->object = get<ObjectDecl>  ().accept1(node->object);
		rslt->var    = get<VariableExpr>().accept1(node->var);

		auto expr = visitBaseExpr( node, rslt );
		this->node = expr->clone();
		return nullptr;
	}

	const ast::Expr * visit( const ast::UntypedInitExpr * node ) override final {
		std::list<InitAlternative> initAlts;
		for (auto ia : node->initAlts) {
			initAlts.push_back(InitAlternative(
				get<Type>       ().accept1(ia.type),
				get<Designation>().accept1(ia.designation)
			));
		}
		auto expr = visitBaseExpr( node,
			new UntypedInitExpr(
				get<Expression>().accept1(node->expr),
				initAlts
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::InitExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new InitExpr(
				get<Expression>().accept1(node->expr),
				get<Designation>().accept1(node->designation)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::DeletedExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new DeletedExpr(
				get<Expression>().accept1(node->expr),
				inCache(node->deleteStmt) ?
					strict_dynamic_cast<Declaration*>(this->node) :
					get<Declaration>().accept1(node->deleteStmt)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::DefaultArgExpr * node ) override final {
		auto expr = visitBaseExpr( node,
			new DefaultArgExpr(
				get<Expression>().accept1(node->expr)
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Expr * visit( const ast::GenericExpr * node ) override final {
		std::list<GenericExpr::Association> associations;
		for (auto association : node->associations) {
			associations.push_back(GenericExpr::Association(
				get<Type>      ().accept1(association.type),
				get<Expression>().accept1(association.expr)
			));
		}
		auto expr = visitBaseExpr( node,
			new GenericExpr(
				get<Expression>().accept1(node->control),
				associations
			)
		);
		this->node = expr;
		return nullptr;
	}

	const ast::Type * visitType( const ast::Type * node, Type * type ) {
		// Some types do this in their constructor so add a check.
		if ( !node->attributes.empty() && type->attributes.empty() ) {
			type->attributes = get<Attribute>().acceptL( node->attributes );
		}
		this->node = type;
		return nullptr;
	}

	const ast::Type * visit( const ast::VoidType * node ) override final {
		return visitType( node, new VoidType{ cv( node ) } );
	}

	const ast::Type * visit( const ast::BasicType * node ) override final {
		auto type = new BasicType{ cv( node ), (BasicType::Kind)(unsigned)node->kind };
		// I believe this should always be a BasicType.
		if ( ast::sizeType == node ) {
			Validate::SizeType = type;
		}
		return visitType( node, type );
	}

	const ast::Type * visit( const ast::PointerType * node ) override final {
		return visitType( node, new PointerType{
			cv( node ),
			get<Type>().accept1( node->base ),
			get<Expression>().accept1( node->dimension ),
			(bool)node->isVarLen,
			(bool)node->isStatic
		} );
	}

	const ast::Type * visit( const ast::ArrayType * node ) override final {
		return visitType( node, new ArrayType{
			cv( node ),
			get<Type>().accept1( node->base ),
			get<Expression>().accept1( node->dimension ),
			(bool)node->isVarLen,
			(bool)node->isStatic
		} );
	}

	const ast::Type * visit( const ast::ReferenceType * node ) override final {
		return visitType( node, new ReferenceType{
			cv( node ),
			get<Type>().accept1( node->base )
		} );
	}

	const ast::Type * visit( const ast::QualifiedType * node ) override final {
		return visitType( node, new QualifiedType{
			cv( node ),
			get<Type>().accept1( node->parent ),
			get<Type>().accept1( node->child )
		} );
	}

	const ast::Type * visit( const ast::FunctionType * node ) override final {
		static std::string dummy_paramvar_prefix = "__param_";
		static std::string dummy_returnvar_prefix = "__retval_";

		auto ty = new FunctionType {
			cv( node ),
			(bool)node->isVarArgs
		};
		auto returns = get<Type>().acceptL(node->returns);
		auto params = get<Type>().acceptL(node->params);

		int ret_index = 0;
		for (auto t: returns) {
			// xxx - LinkageSpec shouldn't matter but needs to be something
			ObjectDecl * dummy = new ObjectDecl(dummy_returnvar_prefix + std::to_string(ret_index++), {}, LinkageSpec::C, nullptr, t, nullptr);
			ty->returnVals.push_back(dummy);
		}
		int param_index = 0;
		for (auto t: params) {
			ObjectDecl * dummy = new ObjectDecl(dummy_paramvar_prefix + std::to_string(param_index++), {}, LinkageSpec::C, nullptr, t, nullptr);
			ty->parameters.push_back(dummy);
		}

		// ty->returnVals = get<DeclarationWithType>().acceptL( node->returns );
		// ty->parameters = get<DeclarationWithType>().acceptL( node->params );

		auto types = get<TypeInstType>().acceptL( node->forall );
		for (auto t : types) {
			auto newT = new TypeDecl(*t->baseType);
			newT->name = t->name; // converted by typeString()
			for (auto asst : newT->assertions) delete asst;
			newT->assertions.clear();
			ty->forall.push_back(newT);
		}
		auto assts = get<VariableExpr>().acceptL( node->assertions );
		if (!assts.empty()) {
			assert(!types.empty());
			for (auto asst : assts) {
				auto newDecl = new ObjectDecl(*strict_dynamic_cast<ObjectDecl*>(asst->var));
				delete newDecl->type;
				newDecl->type = asst->result->clone();
				newDecl->storageClasses.is_extern = true; // hack
				ty->forall.back()->assertions.push_back(newDecl);
			}
		}

		return visitType( node, ty );
	}

	const ast::Type * postvisit( const ast::BaseInstType * old, ReferenceToType * ty ) {
		ty->parameters = get<Expression>().acceptL( old->params );
		ty->hoistType = old->hoistType;
		return visitType( old, ty );
	}

	const ast::Type * visit( const ast::StructInstType * node ) override final {
		StructInstType * ty;
		if ( node->base ) {
			ty = new StructInstType{
				cv( node ),
				get<StructDecl>().accept1( node->base ),
				get<Attribute>().acceptL( node->attributes )
			};
		} else {
			ty = new StructInstType{
				cv( node ),
				node->name,
				get<Attribute>().acceptL( node->attributes )
			};
		}
		return postvisit( node, ty );
	}

	const ast::Type * visit( const ast::UnionInstType * node ) override final {
		UnionInstType * ty;
		if ( node->base ) {
			ty = new UnionInstType{
				cv( node ),
				get<UnionDecl>().accept1( node->base ),
				get<Attribute>().acceptL( node->attributes )
			};
		} else {
			ty = new UnionInstType{
				cv( node ),
				node->name,
				get<Attribute>().acceptL( node->attributes )
			};
		}
		return postvisit( node, ty );
	}

	const ast::Type * visit( const ast::EnumInstType * node ) override final {
		EnumInstType * ty;
		if ( node->base ) {
			ty = new EnumInstType{
				cv( node ),
				get<EnumDecl>().accept1( node->base ),
				get<Attribute>().acceptL( node->attributes )
			};
		} else {
			ty = new EnumInstType{
				cv( node ),
				node->name,
				get<Attribute>().acceptL( node->attributes )
			};
		}
		return postvisit( node, ty );
	}

	const ast::Type * visit( const ast::TraitInstType * node ) override final {
		TraitInstType * ty;
		if ( node->base ) {
			ty = new TraitInstType{
				cv( node ),
				get<TraitDecl>().accept1( node->base ),
				get<Attribute>().acceptL( node->attributes )
			};
		} else {
			ty = new TraitInstType{
				cv( node ),
				node->name,
				get<Attribute>().acceptL( node->attributes )
			};
		}
		return postvisit( node, ty );
	}

	const ast::Type * visit( const ast::TypeInstType * node ) override final {
		TypeInstType * ty;
		if ( node->base ) {
			ty = new TypeInstType{
				cv( node ),
				node->typeString(),
				get<TypeDecl>().accept1( node->base ),
				get<Attribute>().acceptL( node->attributes )
			};
		} else {
			ty = new TypeInstType{
				cv( node ),
				node->typeString(),
				node->kind == ast::TypeDecl::Ftype,
				get<Attribute>().acceptL( node->attributes )
			};
		}
		return postvisit( node, ty );
	}

	const ast::Type * visit( const ast::TupleType * node ) override final {
		return visitType( node, new TupleType{
			cv( node ),
			get<Type>().acceptL( node->types )
			// members generated by TupleType c'tor
		} );
	}

	const ast::Type * visit( const ast::TypeofType * node ) override final {
		return visitType( node, new TypeofType{
			cv( node ),
			get<Expression>().accept1( node->expr ),
			(bool)node->kind
		} );
	}

	const ast::Type * visit( const ast::VTableType * node ) override final {
		return visitType( node, new VTableType{
			cv( node ),
			get<Type>().accept1( node->base )
		} );
	}

	const ast::Type * visit( const ast::VarArgsType * node ) override final {
		return visitType( node, new VarArgsType{ cv( node ) } );
	}

	const ast::Type * visit( const ast::ZeroType * node ) override final {
		return visitType( node, new ZeroType{ cv( node ) } );
	}

	const ast::Type * visit( const ast::OneType * node ) override final {
		return visitType( node, new OneType{ cv( node ) } );
	}

	const ast::Type * visit( const ast::GlobalScopeType * node ) override final {
		return visitType( node, new GlobalScopeType{} );
	}

	const ast::Designation * visit( const ast::Designation * node ) override final {
		auto designation = new Designation( get<Expression>().acceptL( node->designators ) );
		designation->location = node->location;
		this->node = designation;
		return nullptr;
	}

	const ast::Init * visit( const ast::SingleInit * node ) override final {
		auto init = new SingleInit(
			get<Expression>().accept1( node->value ),
			ast::MaybeConstruct == node->maybeConstructed
		);
		init->location = node->location;
		this->node = init;
		return nullptr;
	}

	const ast::Init * visit( const ast::ListInit * node ) override final {
		auto init = new ListInit(
			get<Initializer>().acceptL( node->initializers ),
			get<Designation>().acceptL( node->designations ),
			ast::MaybeConstruct == node->maybeConstructed
		);
		init->location = node->location;
		this->node = init;
		return nullptr;
	}

	const ast::Init * visit( const ast::ConstructorInit * node ) override final {
		auto init = new ConstructorInit(
			get<Statement>().accept1( node->ctor ),
			get<Statement>().accept1( node->dtor ),
			get<Initializer>().accept1( node->init )
		);
		init->location = node->location;
		this->node = init;
		return nullptr;
	}

	const ast::Attribute * visit( const ast::Attribute * node ) override final {
		auto attr = new Attribute(
			node->name,
			get<Expression>().acceptL(node->params)
		);
		this->node = attr;
		return nullptr;
	}

	const ast::TypeSubstitution * visit( const ast::TypeSubstitution * node ) override final {
		// Handled by convertTypeSubstitution helper instead.
		// TypeSubstitution is not a node in the old model, so the conversion result wouldn't fit in this->node.
		assert( 0 );
		(void)node;
		return nullptr;
	}
};

std::list< Declaration * > convert( const ast::TranslationUnit && translationUnit ) {
	// Copy values from the global store to the local static variables.
	ast::sizeType = translationUnit.global.sizeType;
	ast::dereferenceOperator = translationUnit.global.dereference;
	ast::dtorStruct = translationUnit.global.dtorStruct;
	ast::dtorStructDestroy = translationUnit.global.dtorDestroy;

	ConverterNewToOld c;
	std::list< Declaration * > decls;
	for(auto d : translationUnit.decls) {
		decls.emplace_back( c.decl( d ) );
	}
	return decls;
}

//================================================================================================

class ConverterOldToNew : public Visitor {
public:
	ast::Decl * decl() {
		return strict_dynamic_cast< ast::Decl * >( node );
	}
	
	ConverterOldToNew() = default;
	ConverterOldToNew(const ConverterOldToNew &) = delete;
	ConverterOldToNew(ConverterOldToNew &&) = delete;
private:
	/// conversion output
	ast::Node * node = nullptr;
	/// cache of nodes that might be referenced by readonly<> for de-duplication
	/// in case that some nodes are dropped by conversion (due to possible structural change)
	/// use smart pointers in cache value to prevent accidental invalidation.
	/// at conversion stage, all created nodes are guaranteed to be unique, therefore
	/// const_casting out of smart pointers is permitted.
	std::unordered_map< const BaseSyntaxNode *, ast::readonly<ast::Node> > cache = {};

	// Local Utilities:

	template<typename NewT, typename OldT>
	NewT * getAccept1( OldT old ) {
		if ( ! old ) return nullptr;
		old->accept(*this);
		ast::Node * ret = node;
		node = nullptr;
		return strict_dynamic_cast< NewT * >( ret );
	}

#	define GET_ACCEPT_1(child, type) \
		getAccept1< ast::type, decltype( old->child ) >( old->child )


	template<typename NewT, typename OldC>
	std::vector< ast::ptr<NewT> > getAcceptV( const OldC& old ) {
		std::vector< ast::ptr<NewT> > ret;
		ret.reserve( old.size() );
		for ( auto a : old ) {
			a->accept( *this );
			ret.emplace_back( strict_dynamic_cast< NewT * >(node) );
			node = nullptr;
		}
		return ret;
	}

#	define GET_ACCEPT_V(child, type) \
		getAcceptV< ast::type, decltype( old->child ) >( old->child )

#	define GET_ACCEPT_E(child, type) \
		getAccept1< ast::type, decltype( old->base ) >( old->base )

	template<typename NewT, typename OldC>
	std::deque< ast::ptr<NewT> > getAcceptD( const OldC& old ) {
		std::deque< ast::ptr<NewT> > ret;
		for ( auto a : old ) {
			a->accept( *this );
			ret.emplace_back( strict_dynamic_cast< NewT * >(node) );
			node = nullptr;
		}
		return ret;
	}

#	define GET_ACCEPT_D(child, type) \
		getAcceptD< ast::type, decltype( old->child ) >( old->child )

	ast::Label make_label(const Label* old) {
		CodeLocation const & location =
		    ( old->labelled ) ? old->labelled->location : CodeLocation();
		return ast::Label(
			location,
			old->name,
			GET_ACCEPT_V(attributes, Attribute)
		);
	}

	template<template <class...> class C>
	C<ast::Label> make_labels(C<Label> olds) {
		C<ast::Label> ret;
		for (auto oldn : olds) {
			ret.push_back( make_label( &oldn ) );
		}
		return ret;
	}

#	define GET_LABELS_V(labels) \
		to<std::vector>::from( make_labels( std::move( labels ) ) )

	static ast::CV::Qualifiers cv( const Type * ty ) { return { ty->tq.val }; }

	/// returns true and sets `node` if in cache
	bool inCache( const BaseSyntaxNode * old ) {
		auto it = cache.find( old );
		if ( it == cache.end() ) return false;
		node = const_cast<ast::Node *>(it->second.get());
		return true;
	}

	// Now all the visit functions:

	virtual void visit( const ObjectDecl * old ) override final {
		if ( inCache( old ) ) {
			return;
		}
		auto&& type = GET_ACCEPT_1(type, Type);
		auto&& init = GET_ACCEPT_1(init, Init);
		auto&& bfwd = GET_ACCEPT_1(bitfieldWidth, Expr);
		auto&& attr = GET_ACCEPT_V(attributes, Attribute);

		auto decl = new ast::ObjectDecl(
			old->location,
			old->name,
			type,
			init,
			{ old->get_storageClasses().val },
			{ old->linkage.val },
			bfwd,
			std::move(attr),
			{ old->get_funcSpec().val }
		);
		cache.emplace(old, decl);
		assert(cache.find( old ) != cache.end());
		decl->scopeLevel = old->scopeLevel;
		decl->mangleName = old->mangleName;
		decl->isDeleted  = old->isDeleted;
		decl->asmName    = GET_ACCEPT_1(asmName, Expr);
		decl->uniqueId   = old->uniqueId;
		decl->extension  = old->extension;

		this->node = decl;
	}

	virtual void visit( const FunctionDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto paramVars = GET_ACCEPT_V(type->parameters, DeclWithType);
		auto returnVars = GET_ACCEPT_V(type->returnVals, DeclWithType);
		auto forall = GET_ACCEPT_V(type->forall, TypeDecl);

		// function type is now derived from parameter decls instead of storing them

		/*
		auto ftype = new ast::FunctionType((ast::ArgumentFlag)old->type->isVarArgs, cv(old->type));
		ftype->params.reserve(paramVars.size());
		ftype->returns.reserve(returnVars.size());

		for (auto & v: paramVars) {
			ftype->params.emplace_back(v->get_type());
		}
		for (auto & v: returnVars) {
			ftype->returns.emplace_back(v->get_type());
		}
		ftype->forall = std::move(forall);
		*/

		// can function type have attributes? seems not to be the case.
		// visitType(old->type, ftype);

		// collect assertions and put directly in FunctionDecl
		std::vector<ast::ptr<ast::DeclWithType>> assertions;
		for (auto & param: forall) {
			for (auto & asst: param->assertions) {
				assertf(asst->unique(), "newly converted decl must be unique");
				assertions.emplace_back(asst);
			}
			auto mut = param.get_and_mutate();
			assertf(mut == param, "newly converted decl must be unique");
			mut->assertions.clear();
		}

		auto decl = new ast::FunctionDecl{
			old->location,
			old->name,
			// GET_ACCEPT_1(type, FunctionType),
			std::move(forall),
			std::move(assertions),
			std::move(paramVars),
			std::move(returnVars),
			{},
			{ old->storageClasses.val },
			{ old->linkage.val },
			GET_ACCEPT_V(attributes, Attribute),
			{ old->get_funcSpec().val },
			(old->type->isVarArgs) ? ast::VariableArgs : ast::FixedArgs
		};

		// decl->type = ftype;
		cache.emplace( old, decl );

		decl->withExprs = GET_ACCEPT_V(withExprs, Expr);
		decl->stmts = GET_ACCEPT_1(statements, CompoundStmt);
		decl->scopeLevel = old->scopeLevel;
		decl->mangleName = old->mangleName;
		decl->isDeleted  = old->isDeleted;
		decl->asmName    = GET_ACCEPT_1(asmName, Expr);
		decl->uniqueId   = old->uniqueId;
		decl->extension  = old->extension;

		this->node = decl;

		if ( Validate::dereferenceOperator == old ) {
			ast::dereferenceOperator = decl;
		}

		if ( Validate::dtorStructDestroy == old ) {
			ast::dtorStructDestroy = decl;
		}
	}

	virtual void visit( const StructDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto decl = new ast::StructDecl(
			old->location,
			old->name,
			(ast::AggregateDecl::Aggregate)old->kind,
			GET_ACCEPT_V(attributes, Attribute),
			{ old->linkage.val }
		);
		cache.emplace( old, decl );
		decl->parent = GET_ACCEPT_1(parent, AggregateDecl);
		decl->body   = old->body;
		decl->params = GET_ACCEPT_V(parameters, TypeDecl);
		decl->members    = GET_ACCEPT_V(members, Decl);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;

		if ( Validate::dtorStruct == old ) {
			ast::dtorStruct = decl;
		}
	}

	virtual void visit( const UnionDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto decl = new ast::UnionDecl(
			old->location,
			old->name,
			GET_ACCEPT_V(attributes, Attribute),
			{ old->linkage.val }
		);
		cache.emplace( old, decl );
		decl->parent = GET_ACCEPT_1(parent, AggregateDecl);
		decl->body   = old->body;
		decl->params = GET_ACCEPT_V(parameters, TypeDecl);
		decl->members    = GET_ACCEPT_V(members, Decl);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}


	virtual void visit( const EnumDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto decl = new ast::EnumDecl(
			old->location,
			old->name,
			old->isTyped,
			GET_ACCEPT_V(attributes, Attribute),
			{ old->linkage.val },
			GET_ACCEPT_1(base, Type),
			old->hide == EnumDecl::EnumHiding::Hide ? ast::EnumDecl::EnumHiding::Hide : ast::EnumDecl::EnumHiding::Visible,
			old->enumValues
		);
		cache.emplace( old, decl );
		decl->parent = GET_ACCEPT_1(parent, AggregateDecl);
		decl->body   = old->body;
		decl->params = GET_ACCEPT_V(parameters, TypeDecl);
		decl->members    = GET_ACCEPT_V(members, Decl);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };
		this->node = decl;
	}

	virtual void visit( const TraitDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto decl = new ast::TraitDecl(
			old->location,
			old->name,
			GET_ACCEPT_V(attributes, Attribute),
			{ old->linkage.val }
		);
		cache.emplace( old, decl );
		decl->parent = GET_ACCEPT_1(parent, AggregateDecl);
		decl->body   = old->body;
		decl->params = GET_ACCEPT_V(parameters, TypeDecl);
		decl->members    = GET_ACCEPT_V(members, Decl);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}

	virtual void visit( const TypeDecl * old ) override final {
		if ( inCache( old ) ) return;
		auto decl = new ast::TypeDecl{
			old->location,
			old->name,
			{ old->storageClasses.val },
			GET_ACCEPT_1(base, Type),
			(ast::TypeDecl::Kind)(unsigned)old->kind,
			old->sized,
			GET_ACCEPT_1(init, Type)
		};
		cache.emplace( old, decl );
		decl->assertions = GET_ACCEPT_V(assertions, DeclWithType);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;

		this->node = decl;
	}

	virtual void visit( const TypedefDecl * old ) override final {
		auto decl = new ast::TypedefDecl(
			old->location,
			old->name,
			{ old->storageClasses.val },
			GET_ACCEPT_1(base, Type),
			{ old->linkage.val }
		);
		decl->assertions = GET_ACCEPT_V(assertions, DeclWithType);
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}

	virtual void visit( const AsmDecl * old ) override final {
		auto decl = new ast::AsmDecl{
			old->location,
			GET_ACCEPT_1(stmt, AsmStmt)
		};
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}

	virtual void visit( const DirectiveDecl * old ) override final {
		auto decl = new ast::DirectiveDecl{
			old->location,
			GET_ACCEPT_1(stmt, DirectiveStmt)
		};
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}

	virtual void visit( const StaticAssertDecl * old ) override final {
		auto decl = new ast::StaticAssertDecl{
			old->location,
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_1(message, ConstantExpr)
		};
		decl->extension  = old->extension;
		decl->uniqueId   = old->uniqueId;
		decl->storage    = { old->storageClasses.val };

		this->node = decl;
	}

	virtual void visit( const InlineMemberDecl * old ) override final {
		if ( inCache( old ) ) {
			return;
		}
		auto&& type = GET_ACCEPT_1(type, Type);
		auto&& attr = GET_ACCEPT_V(attributes, Attribute);

		auto decl = new ast::InlineMemberDecl(
			old->location,
			old->name,
			type,
			{ old->get_storageClasses().val },
			{ old->linkage.val },
			std::move(attr),
			{ old->get_funcSpec().val }
		);
		cache.emplace(old, decl);
		assert(cache.find( old ) != cache.end());
		decl->scopeLevel = old->scopeLevel;
		decl->mangleName = old->mangleName;
		decl->isDeleted  = old->isDeleted;
		decl->asmName    = GET_ACCEPT_1(asmName, Expr);
		decl->uniqueId   = old->uniqueId;
		decl->extension  = old->extension;

		this->node = decl;
	}

	virtual void visit( const CompoundStmt * old ) override final {
		if ( inCache( old ) ) return;
		auto stmt = new ast::CompoundStmt(
			old->location,
			to<std::list>::from( GET_ACCEPT_V(kids, Stmt) ),
			GET_LABELS_V(old->labels)
		);

		this->node = stmt;
		cache.emplace( old, this->node );
	}

	virtual void visit( const ExprStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::ExprStmt(
			old->location,
			GET_ACCEPT_1(expr, Expr),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const AsmStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::AsmStmt(
			old->location,
			old->voltile,
			GET_ACCEPT_1(instruction, Expr),
			GET_ACCEPT_V(output, Expr),
			GET_ACCEPT_V(input, Expr),
			GET_ACCEPT_V(clobber, ConstantExpr),
			GET_LABELS_V(old->gotolabels),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const DirectiveStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::DirectiveStmt(
			old->location,
			old->directive,
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const IfStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::IfStmt(
			old->location,
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_1(then, Stmt),
			GET_ACCEPT_1(else_, Stmt),
			GET_ACCEPT_V(initialization, Stmt),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const SwitchStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::SwitchStmt(
			old->location,
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_V(statements, CaseClause),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const CaseStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::CaseClause(
			old->location,
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_V(stmts, Stmt)
		);
		auto labels = GET_LABELS_V(old->labels);
		assertf(labels.empty(), "Labels found on CaseStmt.");
		cache.emplace( old, this->node );
	}

	virtual void visit( const WhileDoStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::WhileDoStmt(
			old->location,
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_1(body, Stmt),
			GET_ACCEPT_1(else_, Stmt),
			GET_ACCEPT_V(initialization, Stmt),
			(old->isDoWhile) ? ast::DoWhile : ast::While,
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const ForStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::ForStmt(
			old->location,
			GET_ACCEPT_V(initialization, Stmt),
			GET_ACCEPT_1(condition, Expr),
			GET_ACCEPT_1(increment, Expr),
			GET_ACCEPT_1(body, Stmt),
			GET_ACCEPT_1(else_, Stmt),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const BranchStmt * old ) override final {
		if ( inCache( old ) ) return;
		if (old->computedTarget) {
			this->node = new ast::BranchStmt(
				old->location,
				GET_ACCEPT_1(computedTarget, Expr),
				GET_LABELS_V(old->labels)
			);
		} else {
			ast::BranchStmt::Kind kind;
			switch (old->type) {
			#define CASE(n) \
			case BranchStmt::n: \
				kind = ast::BranchStmt::n; \
				break
			CASE(Goto);
			CASE(Break);
			CASE(Continue);
			CASE(FallThrough);
			CASE(FallThroughDefault);
			#undef CASE
			default:
				assertf(false, "Invalid BranchStmt::Type %d\n", old->type);
			}

			auto stmt = new ast::BranchStmt(
				old->location,
				kind,
				make_label(&old->originalTarget),
				GET_LABELS_V(old->labels)
			);
			stmt->target = make_label(&old->target);
			this->node = stmt;
		}
		cache.emplace( old, this->node );
	}

	virtual void visit( const ReturnStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::ReturnStmt(
			old->location,
			GET_ACCEPT_1(expr, Expr),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const ThrowStmt * old ) override final {
		if ( inCache( old ) ) return;
		ast::ExceptionKind kind;
		switch (old->kind) {
		case ThrowStmt::Terminate:
			kind = ast::ExceptionKind::Terminate;
			break;
		case ThrowStmt::Resume:
			kind = ast::ExceptionKind::Resume;
			break;
		default:
			assertf(false, "Invalid ThrowStmt::Kind %d\n", old->kind);
		}

		this->node = new ast::ThrowStmt(
			old->location,
			kind,
			GET_ACCEPT_1(expr, Expr),
			GET_ACCEPT_1(target, Expr),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const TryStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::TryStmt(
			old->location,
			GET_ACCEPT_1(block, CompoundStmt),
			GET_ACCEPT_V(handlers, CatchClause),
			GET_ACCEPT_1(finallyBlock, FinallyClause),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const CatchStmt * old ) override final {
		if ( inCache( old ) ) return;
		ast::ExceptionKind kind;
		switch (old->kind) {
		case CatchStmt::Terminate:
			kind = ast::ExceptionKind::Terminate;
			break;
		case CatchStmt::Resume:
			kind = ast::ExceptionKind::Resume;
			break;
		default:
			assertf(false, "Invalid CatchStmt::Kind %d\n", old->kind);
		}

		this->node = new ast::CatchClause(
			old->location,
			kind,
			GET_ACCEPT_1(decl, Decl),
			GET_ACCEPT_1(cond, Expr),
			GET_ACCEPT_1(body, Stmt)
		);
		auto labels = GET_LABELS_V(old->labels);
		assertf(labels.empty(), "Labels found on CatchStmt.");
		cache.emplace( old, this->node );
	}

	virtual void visit( const FinallyStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::FinallyClause(
			old->location,
			GET_ACCEPT_1(block, CompoundStmt)
		);
		auto labels = GET_LABELS_V(old->labels);
		assertf(labels.empty(), "Labels found on FinallyStmt.");
		cache.emplace( old, this->node );
	}

	virtual void visit( const SuspendStmt * old ) override final {
		if ( inCache( old ) ) return;
		ast::SuspendStmt::Kind type;
		switch (old->type) {
			case SuspendStmt::Coroutine: type = ast::SuspendStmt::Coroutine; break;
			case SuspendStmt::Generator: type = ast::SuspendStmt::Generator; break;
			case SuspendStmt::None     : type = ast::SuspendStmt::None     ; break;
			default: abort();
		}
		this->node = new ast::SuspendStmt(
			old->location,
			GET_ACCEPT_1(then  , CompoundStmt),
			type,
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const WaitForStmt * old ) override final {
		if ( inCache( old ) ) return;
		ast::WaitForStmt * stmt = new ast::WaitForStmt(
			old->location,
			GET_LABELS_V(old->labels)
		);

		stmt->clauses.reserve( old->clauses.size() );
		for (size_t i = 0 ; i < old->clauses.size() ; ++i) {
			auto clause = new ast::WaitForClause( old->location );

			clause->target_func = GET_ACCEPT_1(clauses[i].target.function, Expr);
			clause->target_args = GET_ACCEPT_V(clauses[i].target.arguments, Expr);
			clause->stmt = GET_ACCEPT_1(clauses[i].statement, Stmt);
			clause->cond = GET_ACCEPT_1(clauses[i].condition, Expr);

			stmt->clauses.push_back( clause );
		}
		stmt->timeout_time = GET_ACCEPT_1(timeout.time, Expr);
		stmt->timeout_stmt = GET_ACCEPT_1(timeout.statement, Stmt);
		stmt->timeout_cond = GET_ACCEPT_1(timeout.condition, Expr);
		stmt->else_stmt = GET_ACCEPT_1(orelse.statement, Stmt);
		stmt->else_cond = GET_ACCEPT_1(orelse.condition, Expr);

		this->node = stmt;
		cache.emplace( old, this->node );
	}

	virtual void visit( const WithStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::WithStmt(
			old->location,
			GET_ACCEPT_V(exprs, Expr),
			GET_ACCEPT_1(stmt, Stmt)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const NullStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::NullStmt(
			old->location,
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const DeclStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::DeclStmt(
			old->location,
			GET_ACCEPT_1(decl, Decl),
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, this->node );
	}

	virtual void visit( const ImplicitCtorDtorStmt * old ) override final {
		if ( inCache( old ) ) return;
		auto stmt = new ast::ImplicitCtorDtorStmt(
			old->location,
			nullptr,
			GET_LABELS_V(old->labels)
		);
		cache.emplace( old, stmt );
		stmt->callStmt = GET_ACCEPT_1(callStmt, Stmt);
		this->node = stmt;
	}

	virtual void visit( const MutexStmt * old ) override final {
		if ( inCache( old ) ) return;
		this->node = new ast::MutexStmt(
			old->location,
			GET_ACCEPT_1(stmt, Stmt),
			GET_ACCEPT_V(mutexObjs, Expr)
		);
		cache.emplace( old, this->node );
	}

	// TypeSubstitution shouldn't exist yet in old.
	ast::TypeSubstitution * convertTypeSubstitution(const TypeSubstitution * old) {
		
		if (!old) return nullptr;
		if (old->empty()) return nullptr;
		assert(false);

		/*
		ast::TypeSubstitution *rslt = new ast::TypeSubstitution();

		for (decltype(old->begin()) old_i = old->begin(); old_i != old->end(); old_i++) {
			rslt->add( old_i->first,
			           getAccept1<ast::Type>(old_i->second) );
		}

		return rslt;
		*/
	}

	void convertInferUnion(ast::Expr::InferUnion               &newInferred,
						   const std::map<UniqueId,ParamEntry> &oldInferParams,
						   const std::vector<UniqueId>         &oldResnSlots) {

		assert( oldInferParams.empty() || oldResnSlots.empty() );
		// assert( newInferred.mode == ast::Expr::InferUnion::Empty );

		if ( !oldInferParams.empty() ) {
			ast::InferredParams &tgt = newInferred.inferParams();
			for (auto & old : oldInferParams) {
				tgt[old.first] = ast::ParamEntry(
					old.second.decl,
					getAccept1<ast::Decl>(old.second.declptr),
					getAccept1<ast::Type>(old.second.actualType),
					getAccept1<ast::Type>(old.second.formalType),
					getAccept1<ast::Expr>(old.second.expr)
				);
			}
		} else if ( !oldResnSlots.empty() ) {
			ast::ResnSlots &tgt = newInferred.resnSlots();
			for (auto old : oldResnSlots) {
				tgt.push_back(old);
			}
		}
	}

	ast::Expr * visitBaseExpr_SkipResultType( const Expression * old, ast::Expr * nw) {

		nw->env    = convertTypeSubstitution(old->env);

		nw->extension = old->extension;
		convertInferUnion(nw->inferred, old->inferParams, old->resnSlots);

		return nw;
	}

	ast::Expr * visitBaseExpr( const Expression * old, ast::Expr * nw) {

		nw->result = GET_ACCEPT_1(result, Type);
		return visitBaseExpr_SkipResultType(old, nw);;
	}

	virtual void visit( const ApplicationExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::ApplicationExpr(
				old->location,
				GET_ACCEPT_1(function, Expr),
				GET_ACCEPT_V(args, Expr)
			)
		);
	}

	virtual void visit( const UntypedExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::UntypedExpr(
				old->location,
				GET_ACCEPT_1(function, Expr),
				GET_ACCEPT_V(args, Expr)
			)
		);
	}

	virtual void visit( const NameExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::NameExpr(
				old->location,
				old->get_name()
			)
		);
	}

	virtual void visit( const QualifiedNameExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::QualifiedNameExpr (
				old->location,
				GET_ACCEPT_1(type_decl, Decl),
				old->name
			)
		);
	}

	virtual void visit( const CastExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::CastExpr(
				old->location,
				GET_ACCEPT_1(arg, Expr),
				old->isGenerated ? ast::GeneratedCast : ast::ExplicitCast
			)
		);
	}

	virtual void visit( const KeywordCastExpr * old ) override final {
		ast::AggregateDecl::Aggregate castTarget = (ast::AggregateDecl::Aggregate)old->target;
		assert( ast::AggregateDecl::Generator <= castTarget && castTarget <= ast::AggregateDecl::Thread );
		this->node = visitBaseExpr( old,
			new ast::KeywordCastExpr(
				old->location,
				GET_ACCEPT_1(arg, Expr),
				castTarget,
				{old->concrete_target.field, old->concrete_target.getter}
			)
		);
	}

	virtual void visit( const VirtualCastExpr * old ) override final {
		this->node = visitBaseExpr_SkipResultType( old,
			new ast::VirtualCastExpr(
				old->location,
				GET_ACCEPT_1(arg, Expr),
				GET_ACCEPT_1(result, Type)
			)
		);
	}

	virtual void visit( const AddressExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::AddressExpr(
				old->location,
				GET_ACCEPT_1(arg, Expr)
			)
		);
	}

	virtual void visit( const LabelAddressExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::LabelAddressExpr(
				old->location,
				make_label(&old->arg)
			)
		);
	}

	virtual void visit( const UntypedMemberExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::UntypedMemberExpr(
				old->location,
				GET_ACCEPT_1(member, Expr),
				GET_ACCEPT_1(aggregate, Expr)
			)
		);
	}

	virtual void visit( const MemberExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::MemberExpr(
				old->location,
				GET_ACCEPT_1(member, DeclWithType),
				GET_ACCEPT_1(aggregate, Expr),
				ast::MemberExpr::NoOpConstructionChosen
			)
		);
	}

	virtual void visit( const VariableExpr * old ) override final {
		auto expr = new ast::VariableExpr(
			old->location
		);

		expr->var = GET_ACCEPT_1(var, DeclWithType);
		visitBaseExpr( old, expr );

		this->node = expr;
	}

	virtual void visit( const ConstantExpr * old ) override final {
		ast::ConstantExpr *rslt = new ast::ConstantExpr(
			old->location,
			GET_ACCEPT_1(result, Type),
			old->constant.rep,
			old->constant.ival
		);
		rslt->underlyer = getAccept1< ast::Type, Type* >( old->constant.type );
		this->node = visitBaseExpr( old, rslt );
	}

	virtual void visit( const SizeofExpr * old ) override final {
		assert (old->expr || old->type);
		assert (! (old->expr && old->type));
		ast::SizeofExpr *rslt;
		if (old->expr) {
			assert(!old->isType);
			rslt = new ast::SizeofExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr)
			);
		}
		if (old->type) {
			assert(old->isType);
			rslt = new ast::SizeofExpr(
				old->location,
				GET_ACCEPT_1(type, Type)
			);
		}
		this->node = visitBaseExpr( old, rslt );
	}

	virtual void visit( const AlignofExpr * old ) override final {
		assert (old->expr || old->type);
		assert (! (old->expr && old->type));
		ast::AlignofExpr *rslt;
		if (old->expr) {
			assert(!old->isType);
			rslt = new ast::AlignofExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr)
			);
		}
		if (old->type) {
			assert(old->isType);
			rslt = new ast::AlignofExpr(
				old->location,
				GET_ACCEPT_1(type, Type)
			);
		}
		this->node = visitBaseExpr( old, rslt );
	}

	virtual void visit( const UntypedOffsetofExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::UntypedOffsetofExpr(
				old->location,
				GET_ACCEPT_1(type, Type),
				old->member
			)
		);
	}

	virtual void visit( const OffsetofExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::OffsetofExpr(
				old->location,
				GET_ACCEPT_1(type, Type),
				GET_ACCEPT_1(member, DeclWithType)
			)
		);
	}

	virtual void visit( const OffsetPackExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::OffsetPackExpr(
				old->location,
				GET_ACCEPT_1(type, StructInstType)
			)
		);
	}

	virtual void visit( const LogicalExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::LogicalExpr(
				old->location,
				GET_ACCEPT_1(arg1, Expr),
				GET_ACCEPT_1(arg2, Expr),
				old->get_isAnd() ?
					ast::LogicalFlag::AndExpr :
					ast::LogicalFlag::OrExpr
			)
		);
	}

	virtual void visit( const ConditionalExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::ConditionalExpr(
				old->location,
				GET_ACCEPT_1(arg1, Expr),
				GET_ACCEPT_1(arg2, Expr),
				GET_ACCEPT_1(arg3, Expr)
			)
		);
	}

	virtual void visit( const CommaExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::CommaExpr(
				old->location,
				GET_ACCEPT_1(arg1, Expr),
				GET_ACCEPT_1(arg2, Expr)
			)
		);
	}

	virtual void visit( const TypeExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::TypeExpr(
				old->location,
				GET_ACCEPT_1(type, Type)
			)
		);
	}

	virtual void visit( const DimensionExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::DimensionExpr( old->location, old->name )
		);
	}

	virtual void visit( const AsmExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::AsmExpr(
				old->location,
				old->inout,
				GET_ACCEPT_1(constraint, Expr),
				GET_ACCEPT_1(operand, Expr)
			)
		);
	}

	virtual void visit( const ImplicitCopyCtorExpr * old ) override final {
		auto rslt = new ast::ImplicitCopyCtorExpr(
			old->location,
			GET_ACCEPT_1(callExpr, ApplicationExpr)
		);

		this->node = visitBaseExpr( old, rslt );
	}

	virtual void visit( const ConstructorExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::ConstructorExpr(
				old->location,
				GET_ACCEPT_1(callExpr, Expr)
			)
		);
	}

	virtual void visit( const CompoundLiteralExpr * old ) override final {
		this->node = visitBaseExpr_SkipResultType( old,
			new ast::CompoundLiteralExpr(
				old->location,
				GET_ACCEPT_1(result, Type),
				GET_ACCEPT_1(initializer, Init)
			)
		);
	}

	virtual void visit( const RangeExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::RangeExpr(
				old->location,
				GET_ACCEPT_1(low, Expr),
				GET_ACCEPT_1(high, Expr)
			)
		);
	}

	virtual void visit( const UntypedTupleExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::UntypedTupleExpr(
				old->location,
				GET_ACCEPT_V(exprs, Expr)
			)
		);
	}

	virtual void visit( const TupleExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::TupleExpr(
				old->location,
				GET_ACCEPT_V(exprs, Expr)
			)
		);
	}

	virtual void visit( const TupleIndexExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::TupleIndexExpr(
				old->location,
				GET_ACCEPT_1(tuple, Expr),
				old->index
			)
		);
	}

	virtual void visit( const TupleAssignExpr * old ) override final {
		this->node = visitBaseExpr_SkipResultType( old,
			new ast::TupleAssignExpr(
				old->location,
				GET_ACCEPT_1(result, Type),
				GET_ACCEPT_1(stmtExpr, StmtExpr)
			)
		);
	}

	virtual void visit( const StmtExpr * old ) override final {
		auto rslt = new ast::StmtExpr(
			old->location,
			GET_ACCEPT_1(statements, CompoundStmt)
		);
		rslt->returnDecls = GET_ACCEPT_V(returnDecls, ObjectDecl);
		rslt->dtors       = GET_ACCEPT_V(dtors      , Expr);

		this->node = visitBaseExpr_SkipResultType( old, rslt );
	}

	virtual void visit( const UniqueExpr * old ) override final {
		auto rslt = new ast::UniqueExpr(
			old->location,
			GET_ACCEPT_1(expr, Expr),
			old->get_id()
		);
		rslt->object = GET_ACCEPT_1(object, ObjectDecl);
		rslt->var    = GET_ACCEPT_1(var   , VariableExpr);

		this->node = visitBaseExpr( old, rslt );
	}

	virtual void visit( const UntypedInitExpr * old ) override final {
		std::deque<ast::InitAlternative> initAlts;
		for (auto ia : old->initAlts) {
			initAlts.push_back(ast::InitAlternative(
				getAccept1< ast::Type, Type * >( ia.type ),
				getAccept1< ast::Designation, Designation * >( ia.designation )
			));
		}
		this->node = visitBaseExpr( old,
			new ast::UntypedInitExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr),
				std::move(initAlts)
			)
		);
	}

	virtual void visit( const InitExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::InitExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr),
				GET_ACCEPT_1(designation, Designation)
			)
		);
	}

	virtual void visit( const DeletedExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::DeletedExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr),
				inCache(old->deleteStmt) ?
					strict_dynamic_cast<ast::Decl*>(this->node) :
					GET_ACCEPT_1(deleteStmt, Decl)
			)
		);
	}

	virtual void visit( const DefaultArgExpr * old ) override final {
		this->node = visitBaseExpr( old,
			new ast::DefaultArgExpr(
				old->location,
				GET_ACCEPT_1(expr, Expr)
			)
		);
	}

	virtual void visit( const GenericExpr * old ) override final {
		std::vector<ast::GenericExpr::Association> associations;
		for (auto association : old->associations) {
			associations.push_back(ast::GenericExpr::Association(
				getAccept1< ast::Type, Type * >( association.type ),
				getAccept1< ast::Expr, Expression * >( association.expr )
			));
		}
		this->node = visitBaseExpr( old,
			new ast::GenericExpr(
				old->location,
				GET_ACCEPT_1(control, Expr),
				std::move(associations)
			)
		);
	}

	void visitType( const Type * old, ast::Type * type ) {
		// Some types do this in their constructor so add a check.
		if ( !old->attributes.empty() && type->attributes.empty() ) {
			type->attributes = GET_ACCEPT_V(attributes, Attribute);
		}
		this->node = type;
	}

	virtual void visit( const VoidType * old ) override final {
		visitType( old, new ast::VoidType{ cv( old ) } );
	}

	virtual void visit( const BasicType * old ) override final {
		auto type = new ast::BasicType{ (ast::BasicType::Kind)(unsigned)old->kind, cv( old ) };
		// I believe this should always be a BasicType.
		if ( Validate::SizeType == old ) {
			ast::sizeType = type;
		}
		visitType( old, type );
	}

	virtual void visit( const PointerType * old ) override final {
		visitType( old, new ast::PointerType{
			GET_ACCEPT_1( base, Type ),
			GET_ACCEPT_1( dimension, Expr ),
			(ast::LengthFlag)old->isVarLen,
			(ast::DimensionFlag)old->isStatic,
			cv( old )
		} );
	}

	virtual void visit( const ArrayType * old ) override final {
		visitType( old, new ast::ArrayType{
			GET_ACCEPT_1( base, Type ),
			GET_ACCEPT_1( dimension, Expr ),
			(ast::LengthFlag)old->isVarLen,
			(ast::DimensionFlag)old->isStatic,
			cv( old )
		} );
	}

	virtual void visit( const ReferenceType * old ) override final {
		visitType( old, new ast::ReferenceType{
			GET_ACCEPT_1( base, Type ),
			cv( old )
		} );
	}

	virtual void visit( const QualifiedType * old ) override final {
		visitType( old, new ast::QualifiedType{
			GET_ACCEPT_1( parent, Type ),
			GET_ACCEPT_1( child, Type ),
			cv( old )
		} );
	}

	virtual void visit( const FunctionType * old ) override final {
		auto ty = new ast::FunctionType {
			(ast::ArgumentFlag)old->isVarArgs,
			cv( old )
		};
		auto returnVars = GET_ACCEPT_V(returnVals, DeclWithType);
		auto paramVars = GET_ACCEPT_V(parameters, DeclWithType);
		// ty->returns = GET_ACCEPT_V( returnVals, DeclWithType );
		// ty->params = GET_ACCEPT_V( parameters, DeclWithType );
		for (auto & v: returnVars) {
			ty->returns.emplace_back(v->get_type());
		}
		for (auto & v: paramVars) {
			ty->params.emplace_back(v->get_type());
		}
		// xxx - when will this be non-null?
		// will have to create dangling (no-owner) decls to be pointed to
		auto foralls = GET_ACCEPT_V( forall, TypeDecl );

		for (auto & param : foralls) {
			ty->forall.emplace_back(new ast::TypeInstType(param));
			for (auto asst : param->assertions) {
				ty->assertions.emplace_back(
					new ast::VariableExpr(param->location, asst));
			}
		}
		visitType( old, ty );
	}

	void postvisit( const ReferenceToType * old, ast::BaseInstType * ty ) {
		ty->params = GET_ACCEPT_V( parameters, Expr );
		ty->hoistType = old->hoistType;
		visitType( old, ty );
	}

	virtual void visit( const StructInstType * old ) override final {
		ast::StructInstType * ty;
		if ( old->baseStruct ) {
			ty = new ast::StructInstType{
				GET_ACCEPT_1( baseStruct, StructDecl ),
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		} else {
			ty = new ast::StructInstType{
				old->name,
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		}
		postvisit( old, ty );
	}

	virtual void visit( const UnionInstType * old ) override final {
		ast::UnionInstType * ty;
		if ( old->baseUnion ) {
			ty = new ast::UnionInstType{
				GET_ACCEPT_1( baseUnion, UnionDecl ),
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		} else {
			ty = new ast::UnionInstType{
				old->name,
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		}
		postvisit( old, ty );
	}

	virtual void visit( const EnumInstType * old ) override final {
		ast::EnumInstType * ty; 
		if ( old->baseEnum ) {
			ty = new ast::EnumInstType{
				GET_ACCEPT_1( baseEnum, EnumDecl ),
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		} else {
			ty = new ast::EnumInstType{
				old->name,
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		}
		postvisit( old, ty );
	}

	virtual void visit( const TraitInstType * old ) override final {
		ast::TraitInstType * ty;
		if ( old->baseTrait ) {
			ty = new ast::TraitInstType{
				GET_ACCEPT_1( baseTrait, TraitDecl ),
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		} else {
			ty = new ast::TraitInstType{
				old->name,
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		}
		postvisit( old, ty );
	}

	virtual void visit( const TypeInstType * old ) override final {
		ast::TypeInstType * ty;
		if ( old->baseType ) {
			ty = new ast::TypeInstType{
				old->name,
				GET_ACCEPT_1( baseType, TypeDecl ),
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		} else {
			ty = new ast::TypeInstType{
				old->name,
				old->isFtype ? ast::TypeDecl::Ftype : ast::TypeDecl::Dtype,
				cv( old ),
				GET_ACCEPT_V( attributes, Attribute )
			};
		}
		postvisit( old, ty );
	}

	virtual void visit( const TupleType * old ) override final {
		visitType( old, new ast::TupleType{
			GET_ACCEPT_V( types, Type ),
			// members generated by TupleType c'tor
			cv( old )
		} );
	}

	virtual void visit( const TypeofType * old ) override final {
		visitType( old, new ast::TypeofType{
			GET_ACCEPT_1( expr, Expr ),
			(ast::TypeofType::Kind)old->is_basetypeof,
			cv( old )
		} );
	}

	virtual void visit( const VTableType * old ) override final {
		visitType( old, new ast::VTableType{
			GET_ACCEPT_1( base, Type ),
			cv( old )
		} );
	}

	virtual void visit( const AttrType * ) override final {
		assertf( false, "AttrType deprecated in new AST." );
	}

	virtual void visit( const VarArgsType * old ) override final {
		visitType( old, new ast::VarArgsType{ cv( old ) } );
	}

	virtual void visit( const ZeroType * old ) override final {
		visitType( old, new ast::ZeroType{ cv( old ) } );
	}

	virtual void visit( const OneType * old ) override final {
		visitType( old, new ast::OneType{ cv( old ) } );
	}

	virtual void visit( const GlobalScopeType * old ) override final {
		visitType( old, new ast::GlobalScopeType{} );
	}

	virtual void visit( const Designation * old ) override final {
		this->node = new ast::Designation(
			old->location,
			GET_ACCEPT_D(designators, Expr)
		);
	}

	virtual void visit( const SingleInit * old ) override final {
		this->node = new ast::SingleInit(
			old->location,
			GET_ACCEPT_1(value, Expr),
			(old->get_maybeConstructed()) ? ast::MaybeConstruct : ast::NoConstruct
		);
	}

	virtual void visit( const ListInit * old ) override final {
		this->node = new ast::ListInit(
			old->location,
			GET_ACCEPT_V(initializers, Init),
			GET_ACCEPT_V(designations, Designation),
			(old->get_maybeConstructed()) ? ast::MaybeConstruct : ast::NoConstruct
		);
	}

	virtual void visit( const ConstructorInit * old ) override final {
		this->node = new ast::ConstructorInit(
			old->location,
			GET_ACCEPT_1(ctor, Stmt),
			GET_ACCEPT_1(dtor, Stmt),
			GET_ACCEPT_1(init, Init)
		);
	}

	virtual void visit( const Constant * ) override final {
		// Handled in visit( ConstantEpxr * ).
		// In the new tree, Constant fields are inlined into containing ConstantExpression.
		assert( 0 );
	}

	virtual void visit( const Attribute * old ) override final {
		this->node = new ast::Attribute(
			old->name,
			GET_ACCEPT_V( parameters, Expr )
		);
	}
};

#undef GET_LABELS_V
#undef GET_ACCEPT_V
#undef GET_ACCEPT_1

ast::TranslationUnit convert( const std::list< Declaration * > && translationUnit ) {
	ConverterOldToNew c;
	ast::TranslationUnit unit;
	if (Validate::SizeType) {
		// this should be a BasicType.
		auto old = strict_dynamic_cast<BasicType *>(Validate::SizeType);
		ast::sizeType = new ast::BasicType{ (ast::BasicType::Kind)(unsigned)old->kind };
	}

	for(auto d : translationUnit) {
		d->accept( c );
		unit.decls.emplace_back( c.decl() );
	}
	deleteAll(translationUnit);

	// Load the local static varables into the global store.
	unit.global.sizeType = ast::sizeType;
	unit.global.dereference = ast::dereferenceOperator;
	unit.global.dtorStruct = ast::dtorStruct;
	unit.global.dtorDestroy = ast::dtorStructDestroy;

	return unit;
}
