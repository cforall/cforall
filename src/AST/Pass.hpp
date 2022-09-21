//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Pass.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15:37:05 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once
// IWYU pragma: private, include "AST/Pass.hpp"

#include <functional>
#include <list>
#include <stack>

#include "AST/Fwd.hpp"
#include "AST/Node.hpp"

#include "AST/Attribute.hpp"
#include "AST/Decl.hpp"
#include "AST/Expr.hpp"
#include "AST/Init.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"

#include "AST/Visitor.hpp"

#include "AST/SymbolTable.hpp"

// Private prelude header, needed for some of the magic tricks this class pulls off
#include "AST/Pass.proto.hpp"

namespace ast {
//-------------------------------------------------------------------------------------------------
// Templated visitor type
// To use declare a Pass< YOUR VISITOR TYPE >
// The visitor type should specify the previsit/postvisit for types that are desired.
// Note: previsit/postvisit must be **public** members
//
// Several additional features are available through inheritance
// | PureVisitor           - makes the visitor pure, it never modifies nodes in place and always
//                           clones nodes it needs to make changes to
// | WithConstTypeSubstitution - provides polymorphic const TypeSubstitution * typeSubs for the
//                           current expression
// | WithStmtsToAdd        - provides the ability to insert statements before or after the current
//                           statement by adding new statements into stmtsToAddBefore or
//                           stmtsToAddAfter respectively.
// | WithDeclsToAdd        - provides the ability to insert declarations before or after the
//                           current declarations by adding new DeclStmt into declsToAddBefore or
//                           declsToAddAfter respectively.
// | WithShortCircuiting   - provides the ability to skip visiting child nodes; set visit_children
//                           to false in pre{visit,visit} to skip visiting children
// | WithGuards            - provides the ability to save/restore data like a LIFO stack; to save,
//                           call GuardValue with the variable to save, the variable will
//                           automatically be restored to its previous value after the
//                           corresponding postvisit/postmutate teminates.
// | WithVisitorRef        - provides an pointer to the templated visitor wrapper
// | WithSymbolTable       - provides symbol table functionality
//
// Other Special Members:
// | result                - Either a method that takes no parameters or a field. If a method (or
//                           callable field) get_result calls it, otherwise the value is returned.
//-------------------------------------------------------------------------------------------------
template< typename core_t >
class Pass final : public ast::Visitor {
public:
	using core_type = core_t;
	using type = Pass<core_t>;

	/// Forward any arguments to the pass constructor
	/// Propagate 'this' if necessary
	template< typename... Args >
	Pass( Args &&... args)
		: core( std::forward<Args>( args )... )
	{
		// After the pass is constructed, check if it wants the have a pointer to the wrapping visitor
		type * const * visitor = __pass::visitor(core, 0);
		if(visitor) {
			*const_cast<type **>( visitor ) = this;
		}
	}

	virtual ~Pass() = default;

	/// Storage for the actual pass.
	core_t core;

	/// If the core defines a result, call it if possible, otherwise return it.
	inline auto get_result() -> decltype( __pass::get_result( core, '0' ) ) {
		return __pass::get_result( core, '0' );
	}

	/// Construct and run a pass on a translation unit.
	template< typename... Args >
	static void run( TranslationUnit & decls, Args &&... args ) {
		Pass<core_t> visitor( std::forward<Args>( args )... );
		accept_all( decls, visitor );
	}

	/// Contruct and run a pass on a pointer to extract a value.
	template< typename node_type, typename... Args >
	static auto read( node_type const * node, Args&&... args ) {
		Pass<core_t> visitor( std::forward<Args>( args )... );
		auto const * temp = node->accept( visitor );
		assert( temp == node );
		return visitor.get_result();
	}

	// Versions of the above for older compilers.
	template< typename... Args >
	static void run( TranslationUnit & decls ) {
		Pass<core_t> visitor;
		accept_all( decls, visitor );
	}

	template< typename node_type, typename... Args >
	static auto read( node_type const * node ) {
		Pass<core_t> visitor;
		auto const * temp = node->accept( visitor );
		assert( temp == node );
		return visitor.get_result();
	}

	/// Visit function declarations
	const ast::DeclWithType *     visit( const ast::ObjectDecl           * ) override final;
	const ast::DeclWithType *     visit( const ast::FunctionDecl         * ) override final;
	const ast::Decl *             visit( const ast::StructDecl           * ) override final;
	const ast::Decl *             visit( const ast::UnionDecl            * ) override final;
	const ast::Decl *             visit( const ast::EnumDecl             * ) override final;
	const ast::Decl *             visit( const ast::TraitDecl            * ) override final;
	const ast::Decl *             visit( const ast::TypeDecl             * ) override final;
	const ast::Decl *             visit( const ast::TypedefDecl          * ) override final;
	const ast::AsmDecl *          visit( const ast::AsmDecl              * ) override final;
	const ast::DirectiveDecl *    visit( const ast::DirectiveDecl        * ) override final;
	const ast::StaticAssertDecl * visit( const ast::StaticAssertDecl     * ) override final;
	const ast::CompoundStmt *     visit( const ast::CompoundStmt         * ) override final;
	const ast::Stmt *             visit( const ast::ExprStmt             * ) override final;
	const ast::Stmt *             visit( const ast::AsmStmt              * ) override final;
	const ast::Stmt *             visit( const ast::DirectiveStmt        * ) override final;
	const ast::Stmt *             visit( const ast::IfStmt               * ) override final;
	const ast::Stmt *             visit( const ast::WhileDoStmt          * ) override final;
	const ast::Stmt *             visit( const ast::ForStmt              * ) override final;
	const ast::Stmt *             visit( const ast::SwitchStmt           * ) override final;
	const ast::CaseClause *       visit( const ast::CaseClause           * ) override final;
	const ast::Stmt *             visit( const ast::BranchStmt           * ) override final;
	const ast::Stmt *             visit( const ast::ReturnStmt           * ) override final;
	const ast::Stmt *             visit( const ast::ThrowStmt            * ) override final;
	const ast::Stmt *             visit( const ast::TryStmt              * ) override final;
	const ast::CatchClause *      visit( const ast::CatchClause          * ) override final;
	const ast::FinallyClause *    visit( const ast::FinallyClause        * ) override final;
	const ast::Stmt *             visit( const ast::SuspendStmt          * ) override final;
	const ast::Stmt *             visit( const ast::WaitForStmt          * ) override final;
	const ast::WaitForClause *    visit( const ast::WaitForClause        * ) override final;
	const ast::Decl *             visit( const ast::WithStmt             * ) override final;
	const ast::NullStmt *         visit( const ast::NullStmt             * ) override final;
	const ast::Stmt *             visit( const ast::DeclStmt             * ) override final;
	const ast::Stmt *             visit( const ast::ImplicitCtorDtorStmt * ) override final;
	const ast::Stmt *             visit( const ast::MutexStmt            * ) override final;
	const ast::Expr *             visit( const ast::ApplicationExpr      * ) override final;
	const ast::Expr *             visit( const ast::UntypedExpr          * ) override final;
	const ast::Expr *             visit( const ast::NameExpr             * ) override final;
	const ast::Expr *			  visit( const ast::QualifiedNameExpr	 * ) override final;
	const ast::Expr *             visit( const ast::AddressExpr          * ) override final;
	const ast::Expr *             visit( const ast::LabelAddressExpr     * ) override final;
	const ast::Expr *             visit( const ast::CastExpr             * ) override final;
	const ast::Expr *             visit( const ast::KeywordCastExpr      * ) override final;
	const ast::Expr *             visit( const ast::VirtualCastExpr      * ) override final;
	const ast::Expr *             visit( const ast::UntypedMemberExpr    * ) override final;
	const ast::Expr *             visit( const ast::MemberExpr           * ) override final;
	const ast::Expr *             visit( const ast::VariableExpr         * ) override final;
	const ast::Expr *             visit( const ast::ConstantExpr         * ) override final;
	const ast::Expr *             visit( const ast::SizeofExpr           * ) override final;
	const ast::Expr *             visit( const ast::AlignofExpr          * ) override final;
	const ast::Expr *             visit( const ast::UntypedOffsetofExpr  * ) override final;
	const ast::Expr *             visit( const ast::OffsetofExpr         * ) override final;
	const ast::Expr *             visit( const ast::OffsetPackExpr       * ) override final;
	const ast::Expr *             visit( const ast::LogicalExpr          * ) override final;
	const ast::Expr *             visit( const ast::ConditionalExpr      * ) override final;
	const ast::Expr *             visit( const ast::CommaExpr            * ) override final;
	const ast::Expr *             visit( const ast::TypeExpr             * ) override final;
	const ast::Expr *             visit( const ast::DimensionExpr        * ) override final;
	const ast::Expr *             visit( const ast::AsmExpr              * ) override final;
	const ast::Expr *             visit( const ast::ImplicitCopyCtorExpr * ) override final;
	const ast::Expr *             visit( const ast::ConstructorExpr      * ) override final;
	const ast::Expr *             visit( const ast::CompoundLiteralExpr  * ) override final;
	const ast::Expr *             visit( const ast::RangeExpr            * ) override final;
	const ast::Expr *             visit( const ast::UntypedTupleExpr     * ) override final;
	const ast::Expr *             visit( const ast::TupleExpr            * ) override final;
	const ast::Expr *             visit( const ast::TupleIndexExpr       * ) override final;
	const ast::Expr *             visit( const ast::TupleAssignExpr      * ) override final;
	const ast::Expr *             visit( const ast::StmtExpr             * ) override final;
	const ast::Expr *             visit( const ast::UniqueExpr           * ) override final;
	const ast::Expr *             visit( const ast::UntypedInitExpr      * ) override final;
	const ast::Expr *             visit( const ast::InitExpr             * ) override final;
	const ast::Expr *             visit( const ast::DeletedExpr          * ) override final;
	const ast::Expr *             visit( const ast::DefaultArgExpr       * ) override final;
	const ast::Expr *             visit( const ast::GenericExpr          * ) override final;
	const ast::Type *             visit( const ast::VoidType             * ) override final;
	const ast::Type *             visit( const ast::BasicType            * ) override final;
	const ast::Type *             visit( const ast::PointerType          * ) override final;
	const ast::Type *             visit( const ast::ArrayType            * ) override final;
	const ast::Type *             visit( const ast::ReferenceType        * ) override final;
	const ast::Type *             visit( const ast::QualifiedType        * ) override final;
	const ast::Type *             visit( const ast::FunctionType         * ) override final;
	const ast::Type *             visit( const ast::StructInstType       * ) override final;
	const ast::Type *             visit( const ast::UnionInstType        * ) override final;
	const ast::Type *             visit( const ast::EnumInstType         * ) override final;
	const ast::Type *             visit( const ast::TraitInstType        * ) override final;
	const ast::Type *             visit( const ast::TypeInstType         * ) override final;
	const ast::Type *             visit( const ast::TupleType            * ) override final;
	const ast::Type *             visit( const ast::TypeofType           * ) override final;
	const ast::Type *             visit( const ast::VTableType           * ) override final;
	const ast::Type *             visit( const ast::VarArgsType          * ) override final;
	const ast::Type *             visit( const ast::ZeroType             * ) override final;
	const ast::Type *             visit( const ast::OneType              * ) override final;
	const ast::Type *             visit( const ast::GlobalScopeType      * ) override final;
	const ast::Designation *      visit( const ast::Designation          * ) override final;
	const ast::Init *             visit( const ast::SingleInit           * ) override final;
	const ast::Init *             visit( const ast::ListInit             * ) override final;
	const ast::Init *             visit( const ast::ConstructorInit      * ) override final;
	const ast::Attribute *        visit( const ast::Attribute            * ) override final;
	const ast::TypeSubstitution * visit( const ast::TypeSubstitution     * ) override final;

	template<typename core_type>
	friend void accept_all( std::list< ptr<Decl> > & decls, Pass<core_type>& visitor );

	bool isInFunction() const {
		return inFunction;
	}

private:

	bool __visit_children() { __pass::bool_ref * ptr = __pass::visit_children(core, 0); return ptr ? *ptr : true; }

private:

	__pass::result1<ast::Stmt> call_accept( const ast::Stmt * );
	__pass::result1<ast::Expr> call_accept( const ast::Expr * );

	/// This has a `type` member that is the return type for the
	/// generic call_accept if the generic call_accept is defined.
	template< typename node_t >
	using generic_call_accept_result =
		std::enable_if<
				!std::is_base_of<ast::Expr, node_t>::value &&
				!std::is_base_of<ast::Stmt, node_t>::value
			, __pass::result1<
				typename std::remove_pointer< typename std::result_of<
					decltype(&node_t::accept)(node_t*, type&) >::type >::type
			>
		>;

	template< typename node_t >
	auto call_accept( const node_t * node )
		-> typename generic_call_accept_result<node_t>::type;

	// requests WithStmtsToAdd directly add to this statement, as if it is a compound.
	__pass::result1<ast::Stmt> call_accept_as_compound(const ast::Stmt *);

	// requests type environment to be updated (why is it implemented like this?)
	__pass::result1<ast::Expr> call_accept_top(const ast::Expr *);

	template< template <class...> class container_t >
	__pass::resultNstmt<container_t> call_accept( const container_t< ptr<Stmt> > & );

	template< template <class...> class container_t, typename node_t >
	__pass::resultN< container_t, node_t > call_accept( const container_t< ptr<node_t> > & container );

public:
	/// Logic to call the accept and mutate the parent if needed, delegates call to accept
	template<typename node_t, typename parent_t, typename field_t>
	void maybe_accept(const node_t * &, field_t parent_t::* field);

	template<typename node_t, typename parent_t, typename field_t>
	void maybe_accept_as_compound(const node_t * &, field_t parent_t::* field);

	template<typename node_t, typename parent_t, typename field_t>
	void maybe_accept_top(const node_t * &, field_t parent_t::* field);

private:
	/// Internal RAII guard for symbol table features
	struct guard_symtab {
		guard_symtab( Pass<core_t> & pass ): pass( pass ) { __pass::symtab::enter(pass.core, 0); }
		~guard_symtab()                                   { __pass::symtab::leave(pass.core, 0); }
		Pass<core_t> & pass;
	};

	/// Internal RAII guard for scope features
	struct guard_scope {
		guard_scope( Pass<core_t> & pass ): pass( pass ) { __pass::scope::enter(pass.core, 0); }
		~guard_scope()                                   { __pass::scope::leave(pass.core, 0); }
		Pass<core_t> & pass;
	};

	/// Internal RAII guard for forall substitutions
	struct guard_forall_subs {
		guard_forall_subs( Pass<core_t> & pass, const FunctionType * type )
		: pass( pass ), type( type ) { __pass::forall::enter(pass.core, 0, type ); }
		~guard_forall_subs()         { __pass::forall::leave(pass.core, 0, type ); }
		Pass<core_t> & pass;
		const FunctionType * type;
	};

private:
	bool inFunction = false;
	bool atFunctionTop = false;
};

/// Apply a pass to an entire translation unit
template<typename core_t>
void accept_all( std::list< ast::ptr<ast::Decl> > &, ast::Pass<core_t> & visitor );

template<typename core_t>
void accept_all( ast::TranslationUnit &, ast::Pass<core_t> & visitor );

//-------------------------------------------------------------------------------------------------
// PASS ACCESSORIES
//-------------------------------------------------------------------------------------------------

/// If used the visitor will always clone nodes.
struct PureVisitor {};

struct WithCodeLocation {
	const CodeLocation * location = nullptr;
};

/// Keep track of the polymorphic const TypeSubstitution * typeSubs for the current expression.
struct WithConstTypeSubstitution {
	const TypeSubstitution * typeSubs = nullptr;
};

/// Used if visitor requires added statements before or after the current node.
/// The Pass template handles what *before* and *after* means automatically
template< template<class...> class container_t = std::list >
struct WithStmtsToAdd {
	container_t< ptr<Stmt> > stmtsToAddBefore;
	container_t< ptr<Stmt> > stmtsToAddAfter;
};

/// Used if visitor requires added declarations before or after the current node.
/// The Pass template handles what *before* and *after* means automatically
template< template<class...> class container_t = std::list >
struct WithDeclsToAdd {
	container_t< ptr<Decl> > declsToAddBefore;
	container_t< ptr<Decl> > declsToAddAfter;
};

/// Use if visitation should stop at certain levels
/// set visit_children false of all child nodes should be ignored
struct WithShortCircuiting {
	__pass::bool_ref visit_children;
};

/// Used to restore values/functions/etc. when the Pass finishes visiting this node
class WithGuards {
	__pass::at_cleanup_t at_cleanup = [](__pass::cleanup_func_t, void*) {
		std::cerr << "No cleanup function was set" << std::endl;
		abort();
	};

	template< typename core_t>
	friend auto __pass::at_cleanup( core_t & core, int ) -> decltype( &core.at_cleanup );
public:

	/// When this node is finished being visited, restore the value of a variable
	/// You may assign to the return value to set the new value in the same statement.
	template< typename T >
	T& GuardValue( T& val ) {
		at_cleanup( [ val ]( void * newVal ) {
			* static_cast< T * >( newVal ) = val;
		}, static_cast< void * >( & val ) );
		return val;
	}

	/// On the object, all beginScope now and endScope when the current node is finished being visited
	template< typename T >
	void GuardScope( T& val ) {
		val.beginScope();
		at_cleanup( []( void * val ) {
			static_cast< T * >( val )->endScope();
		}, static_cast< void * >( & val ) );
	}

	/// When this node is finished being visited, call a function
	template< typename Func >
	void GuardAction( Func func ) {
		at_cleanup( [func](void *) { func(); }, nullptr );
	}

	/// When this node is finished being visited, call a member of an object.
	template<typename T>
	void GuardMethod( T * obj, void (T::*method)() ) {
		at_cleanup( [ method ]( void * object ) {
			static_cast< T * >( object )->method();
		}, static_cast< void * >( obj ) );
	}
};

/// Used to get a pointer to the pass with its wrapped type
template<typename core_t>
struct WithVisitorRef {
	Pass<core_t> * const visitor = nullptr;

	bool isInFunction() const {
		return visitor->isInFunction();
	}
};

/// Use when the templated visitor should update the symbol table
struct WithSymbolTable {
	SymbolTable symtab;
};

/// Used to get a pointer to the wrapping TranslationUnit.
struct WithConstTranslationUnit {
	const TranslationUnit * translationUnit = nullptr;

	const TranslationUnit & transUnit() const {
		assertf( translationUnit, "WithConstTranslationUnit not set-up." );
		return *translationUnit;
	}
};

}

#include "Common/Stats.h"

namespace ast {
extern struct PassVisitorStats {
	size_t depth = 0;
	Stats::Counters::MaxCounter<double> * max;
	Stats::Counters::AverageCounter<double> * avg;
} pass_visitor_stats;
}

#include "AST/Pass.impl.hpp"
