//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ast::Pass.impl.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15::37::05 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once
// IWYU pragma: private, include "AST/Pass.hpp"

#include <type_traits>
#include <unordered_map>

#include "AST/Copy.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/TypeSubstitution.hpp"

#define VISIT_START( node ) \
	using namespace ast; \
	/* back-up the last known code location */ \
	__attribute__((unused)) auto loc_guard = ast::__pass::make_location_guard( core, node, 0 ); \
	/* back-up the visit children */ \
	__attribute__((unused)) ast::__pass::visit_children_guard guard1( ast::__pass::visit_children(core, 0) ); \
	/* setup the scope for passes that want to run code at exit */ \
	__attribute__((unused)) ast::__pass::guard_value          guard2( ast::__pass::at_cleanup    (core, 0) ); \
	/* begin tracing memory allocation if requested by this pass */ \
	__pass::beginTrace( core, 0 ); \
	/* call the implementation of the previsit of this pass */ \
	__pass::previsit( core, node, 0 );

#define VISIT_END( type, node ) \
	/* call the implementation of the postvisit of this pass */ \
	auto __return = __pass::postvisit( core, node, 0 ); \
	assertf(__return, "post visit should never return null"); \
	/* end tracing memory allocation if requested by this pass */ \
	__pass::endTrace( core, 0 ); \
	return __return;

#ifdef PEDANTIC_PASS_ASSERT
#define __pedantic_pass_assert(...) assert(__VA_ARGS__)
#define __pedantic_pass_assertf(...) assertf(__VA_ARGS__)
#else
#define __pedantic_pass_assert(...)
#define __pedantic_pass_assertf(...)
#endif

namespace ast {
	template<typename node_t>
	node_t * shallowCopy( const node_t * node );

	namespace __pass {
		// Check if this is either a null pointer or a pointer to an empty container
		template<typename T>
		static inline bool empty( T * ptr ) {
			return !ptr || ptr->empty();
		}

		template< typename core_t, typename node_t >
		static inline node_t* mutate(const node_t *node) {
			return std::is_base_of<PureVisitor, core_t>::value ? ::ast::shallowCopy(node) : ::ast::mutate(node);
		}

		//------------------------------
		template<typename it_t, template <class...> class container_t>
		static inline void take_all( it_t it, container_t<ast::ptr<ast::Decl>> * decls, bool * mutated = nullptr ) {
			if(empty(decls)) return;

			std::transform(decls->begin(), decls->end(), it, [](const ast::Decl * decl) -> auto {
					return new DeclStmt( decl->location, decl );
				});
			decls->clear();
			if(mutated) *mutated = true;
		}

		template<typename it_t, template <class...> class container_t>
		static inline void take_all( it_t it, container_t<ast::ptr<ast::Stmt>> * stmts, bool * mutated = nullptr ) {
			if(empty(stmts)) return;

			std::move(stmts->begin(), stmts->end(), it);
			stmts->clear();
			if(mutated) *mutated = true;
		}

		//------------------------------
		/// Check if should be skipped, different for pointers and containers
		template<typename node_t>
		bool skip( const ast::ptr<node_t> & val) {
			return !val;
		}

		template< template <class...> class container_t, typename node_t >
		bool skip( const container_t<ast::ptr< node_t >> & val ) {
			return val.empty();
		}

		//------------------------------
		/// Get the value to visit, different for pointers and containers
		template<typename node_t>
		auto get( const ast::ptr<node_t> & val, int ) -> decltype(val.get()) {
			return val.get();
		}

		template<typename node_t>
		const node_t & get( const node_t & val, long) {
			return val;
		}

		//------------------------------
		/// Check if value was mutated, different for pointers and containers
		template<typename lhs_t, typename rhs_t>
		bool differs( const lhs_t * old_val, const rhs_t * new_val ) {
			return old_val != new_val;
		}

		template< template <class...> class container_t, typename node_t >
		bool differs( const container_t<ast::ptr< node_t >> &, const container_t<ast::ptr< node_t >> & new_val ) {
			return !new_val.empty();
		}
	}

	template< typename core_t >
	template< typename node_t >
	auto ast::Pass< core_t >::call_accept( const node_t * node )
		-> typename ast::Pass< core_t >::template generic_call_accept_result<node_t>::type
	{
		__pedantic_pass_assert( __visit_children() );
		__pedantic_pass_assert( node );

		static_assert( !std::is_base_of<ast::Expr, node_t>::value, "ERROR");
		static_assert( !std::is_base_of<ast::Stmt, node_t>::value, "ERROR");

		auto nval = node->accept( *this );
		__pass::result1<
			typename std::remove_pointer< decltype( node->accept(*this) ) >::type
		> res;
		res.differs = nval != node;
		res.value = nval;
		return res;
	}

	template< typename core_t >
	__pass::template result1<ast::Expr> ast::Pass< core_t >::call_accept( const ast::Expr * expr ) {
		__pedantic_pass_assert( __visit_children() );
		__pedantic_pass_assert( expr );

		auto nval = expr->accept( *this );
		return { nval != expr, nval };
	}

	template< typename core_t >
	__pass::template result1<ast::Stmt> ast::Pass< core_t >::call_accept( const ast::Stmt * stmt ) {
		__pedantic_pass_assert( __visit_children() );
		__pedantic_pass_assert( stmt );

		const ast::Stmt * nval = stmt->accept( *this );
		return { nval != stmt, nval };
	}

	template< typename core_t >
	__pass::template result1<ast::Expr> ast::Pass< core_t >::call_accept_top( const ast::Expr * expr ) {
		__pedantic_pass_assert( __visit_children() );
		__pedantic_pass_assert( expr );

		const ast::TypeSubstitution ** typeSubs_ptr = __pass::typeSubs( core, 0 );
		if ( typeSubs_ptr && expr->env ) {
			*typeSubs_ptr = expr->env;
		}

		auto nval = expr->accept( *this );
		return { nval != expr, nval };
	}

	template< typename core_t >
	__pass::template result1<ast::Stmt> ast::Pass< core_t >::call_accept_as_compound( const ast::Stmt * stmt ) {
		__pedantic_pass_assert( __visit_children() );
		__pedantic_pass_assert( stmt );

		// add a few useful symbols to the scope
		using __pass::empty;

		// get the stmts/decls that will need to be spliced in
		auto stmts_before = __pass::stmtsToAddBefore( core, 0 );
		auto stmts_after  = __pass::stmtsToAddAfter ( core, 0 );
		auto decls_before = __pass::declsToAddBefore( core, 0 );
		auto decls_after  = __pass::declsToAddAfter ( core, 0 );

		// These may be modified by subnode but most be restored once we exit this statemnet.
		ValueGuardPtr< const ast::TypeSubstitution * > __old_env         ( __pass::typeSubs( core, 0 ) );
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_before) >::type > __old_decls_before( stmts_before );
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_after ) >::type > __old_decls_after ( stmts_after  );
		ValueGuardPtr< typename std::remove_pointer< decltype(decls_before) >::type > __old_stmts_before( decls_before );
		ValueGuardPtr< typename std::remove_pointer< decltype(decls_after ) >::type > __old_stmts_after ( decls_after  );

		// Now is the time to actually visit the node
		const ast::Stmt * nstmt = stmt->accept( *this );

		// If the pass doesn't want to add anything then we are done
		if( empty(stmts_before) && empty(stmts_after) && empty(decls_before) && empty(decls_after) ) {
			return { nstmt != stmt, nstmt };
		}

		// Make sure that it is either adding statements or declartions but not both
		// this is because otherwise the order would be awkward to predict
		assert(( empty( stmts_before ) && empty( stmts_after ))
		    || ( empty( decls_before ) && empty( decls_after )) );

		// Create a new Compound Statement to hold the new decls/stmts
		ast::CompoundStmt * compound = new ast::CompoundStmt( stmt->location );

		// Take all the declarations that go before
		__pass::take_all( std::back_inserter( compound->kids ), decls_before );
		__pass::take_all( std::back_inserter( compound->kids ), stmts_before );

		// Insert the original declaration
		compound->kids.emplace_back( nstmt );

		// Insert all the declarations that go before
		__pass::take_all( std::back_inserter( compound->kids ), decls_after );
		__pass::take_all( std::back_inserter( compound->kids ), stmts_after );

		return {true, compound};
	}

	template< typename core_t >
	template< template <class...> class container_t >
	__pass::template resultNstmt<container_t> ast::Pass< core_t >::call_accept( const container_t< ptr<Stmt> > & statements ) {
		__pedantic_pass_assert( __visit_children() );
		if( statements.empty() ) return {};

		// We are going to aggregate errors for all these statements
		SemanticErrorException errors;

		// add a few useful symbols to the scope
		using __pass::empty;

		// get the stmts/decls that will need to be spliced in
		auto stmts_before = __pass::stmtsToAddBefore( core, 0 );
		auto stmts_after  = __pass::stmtsToAddAfter ( core, 0 );
		auto decls_before = __pass::declsToAddBefore( core, 0 );
		auto decls_after  = __pass::declsToAddAfter ( core, 0 );

		// These may be modified by subnode but most be restored once we exit this statemnet.
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_before) >::type > __old_decls_before( stmts_before );
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_after ) >::type > __old_decls_after ( stmts_after  );
		ValueGuardPtr< typename std::remove_pointer< decltype(decls_before) >::type > __old_stmts_before( decls_before );
		ValueGuardPtr< typename std::remove_pointer< decltype(decls_after ) >::type > __old_stmts_after ( decls_after  );

		// update pass statitistics
		pass_visitor_stats.depth++;
		pass_visitor_stats.max->push(pass_visitor_stats.depth);
		pass_visitor_stats.avg->push(pass_visitor_stats.depth);

		__pass::resultNstmt<container_t> new_kids;
		for( auto value : enumerate( statements ) ) {
			try {
				size_t i = value.idx;
				const Stmt * stmt = value.val;
				__pedantic_pass_assert( stmt );
				const ast::Stmt * new_stmt = stmt->accept( *this );
				assert( new_stmt );
				if(new_stmt != stmt ) { new_kids.differs = true; }

				// Make sure that it is either adding statements or declartions but not both
				// this is because otherwise the order would be awkward to predict
				assert(( empty( stmts_before ) && empty( stmts_after ))
				    || ( empty( decls_before ) && empty( decls_after )) );

				// Take all the statements which should have gone after, N/A for first iteration
				new_kids.take_all( decls_before );
				new_kids.take_all( stmts_before );

				// Now add the statement if there is one
				if(new_stmt != stmt) {
					new_kids.values.emplace_back( new_stmt, i, false );
				} else {
					new_kids.values.emplace_back( nullptr, i, true );
				}

				// Take all the declarations that go before
				new_kids.take_all( decls_after );
				new_kids.take_all( stmts_after );
			}
			catch ( SemanticErrorException &e ) {
				errors.append( e );
			}
		}
		pass_visitor_stats.depth--;
		if ( !errors.isEmpty() ) { throw errors; }

		return new_kids;
	}

	template< typename core_t >
	template< template <class...> class container_t, typename node_t >
	__pass::template resultN<container_t, node_t> ast::Pass< core_t >::call_accept( const container_t< ast::ptr<node_t> > & container ) {
		__pedantic_pass_assert( __visit_children() );
		if( container.empty() ) return {};
		SemanticErrorException errors;

		pass_visitor_stats.depth++;
		pass_visitor_stats.max->push(pass_visitor_stats.depth);
		pass_visitor_stats.avg->push(pass_visitor_stats.depth);

		bool mutated = false;
		container_t<ptr<node_t>> new_kids;
		for ( const node_t * node : container ) {
			try {
				__pedantic_pass_assert( node );
				const node_t * new_stmt = strict_dynamic_cast< const node_t * >( node->accept( *this ) );
				if(new_stmt != node ) {
					mutated = true;
					new_kids.emplace_back( new_stmt );
				} else {
					new_kids.emplace_back( nullptr );
				}

			}
			catch( SemanticErrorException &e ) {
				errors.append( e );
			}
		}

		__pedantic_pass_assert( new_kids.size() == container.size() );
		pass_visitor_stats.depth--;
		if ( ! errors.isEmpty() ) { throw errors; }

		return ast::__pass::resultN<container_t, node_t>{ mutated, new_kids };
	}

	template< typename core_t >
	template<typename node_t, typename super_t, typename field_t>
	void ast::Pass< core_t >::maybe_accept(
		const node_t * & parent,
		field_t super_t::*field
	) {
		static_assert( std::is_base_of<super_t, node_t>::value, "Error deducing member object" );

		if(__pass::skip(parent->*field)) return;
		const auto & old_val = __pass::get(parent->*field, 0);

		static_assert( !std::is_same<const ast::Node * &, decltype(old_val)>::value, "ERROR");

		auto new_val = call_accept( old_val );

		static_assert( !std::is_same<const ast::Node *, decltype(new_val)>::value /* || std::is_same<int, decltype(old_val)>::value */, "ERROR");

		if( new_val.differs ) {
			auto new_parent = __pass::mutate<core_t>(parent);
			new_val.apply(new_parent, field);
			parent = new_parent;
		}
	}

	template< typename core_t >
	template<typename node_t, typename super_t, typename field_t>
	void ast::Pass< core_t >::maybe_accept_top(
		const node_t * & parent,
		field_t super_t::*field
	) {
		static_assert( std::is_base_of<super_t, node_t>::value, "Error deducing member object" );

		if(__pass::skip(parent->*field)) return;
		const auto & old_val = __pass::get(parent->*field, 0);

		static_assert( !std::is_same<const ast::Node * &, decltype(old_val)>::value, "ERROR");

		auto new_val = call_accept_top( old_val );

		static_assert( !std::is_same<const ast::Node *, decltype(new_val)>::value /* || std::is_same<int, decltype(old_val)>::value */, "ERROR");

		if( new_val.differs ) {
			auto new_parent = __pass::mutate<core_t>(parent);
			new_val.apply(new_parent, field);
			parent = new_parent;
		}
	}

	template< typename core_t >
	template<typename node_t, typename super_t, typename field_t>
	void ast::Pass< core_t >::maybe_accept_as_compound(
		const node_t * & parent,
		field_t super_t::*child
	) {
		static_assert( std::is_base_of<super_t, node_t>::value, "Error deducing member object" );

		if(__pass::skip(parent->*child)) return;
		const auto & old_val = __pass::get(parent->*child, 0);

		static_assert( !std::is_same<const ast::Node * &, decltype(old_val)>::value, "ERROR");

		auto new_val = call_accept_as_compound( old_val );

		static_assert( !std::is_same<const ast::Node *, decltype(new_val)>::value || std::is_same<int, decltype(old_val)>::value, "ERROR");

		if( new_val.differs ) {
			auto new_parent = __pass::mutate<core_t>(parent);
			new_val.apply( new_parent, child );
			parent = new_parent;
		}
	}

}

//------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------

template< typename core_t >
inline void ast::accept_all( std::list< ast::ptr<ast::Decl> > & decls, ast::Pass< core_t > & visitor ) {
	// We are going to aggregate errors for all these statements
	SemanticErrorException errors;

	// add a few useful symbols to the scope
	using __pass::empty;

	// get the stmts/decls that will need to be spliced in
	auto decls_before = __pass::declsToAddBefore( visitor.core, 0);
	auto decls_after  = __pass::declsToAddAfter ( visitor.core, 0);

	// update pass statitistics
	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);

	for ( std::list< ast::ptr<ast::Decl> >::iterator i = decls.begin(); ; ++i ) {
		// splice in new declarations after previous decl
		if ( !empty( decls_after ) ) { decls.splice( i, *decls_after ); }

		if ( i == decls.end() ) break;

		try {
			// run visitor on declaration
			ast::ptr<ast::Decl> & node = *i;
			assert( node );
			node = node->accept( visitor );
		}
		catch( SemanticErrorException &e ) {
			if (__pass::on_error (visitor.core, *i, 0))
				errors.append( e );
		}

		// splice in new declarations before current decl
		if ( !empty( decls_before ) ) { decls.splice( i, *decls_before ); }
	}
	pass_visitor_stats.depth--;
	if ( !errors.isEmpty() ) { throw errors; }
}

template< typename core_t >
inline void ast::accept_all( ast::TranslationUnit & unit, ast::Pass< core_t > & visitor ) {
	if ( auto ptr = __pass::translation_unit::get_cptr( visitor.core, 0 ) ) {
		ValueGuard<const TranslationUnit *> guard( *ptr );
		*ptr = &unit;
		return ast::accept_all( unit.decls, visitor );
	} else {
		return ast::accept_all( unit.decls, visitor );
	}
}

// A NOTE ON THE ORDER OF TRAVERSAL
//
// Types and typedefs have their base types visited before they are added to the type table.  This is ok, since there is
// no such thing as a recursive type or typedef.
//
//             typedef struct { T *x; } T; // never allowed
//
// for structs/unions, it is possible to have recursion, so the decl should be added as if it's incomplete to begin, the
// members are traversed, and then the complete type should be added (assuming the type is completed by this particular
// declaration).
//
//             struct T { struct T *x; }; // allowed
//
// It is important to add the complete type to the symbol table *after* the members/base has been traversed, since that
// traversal may modify the definition of the type and these modifications should be visible when the symbol table is
// queried later in this pass.

//--------------------------------------------------------------------------
// ObjectDecl
template< typename core_t >
const ast::DeclWithType * ast::Pass< core_t >::visit( const ast::ObjectDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &ObjectDecl::type );
		}
		maybe_accept( node, &ObjectDecl::init          );
		maybe_accept( node, &ObjectDecl::bitfieldWidth );
		maybe_accept( node, &ObjectDecl::attributes    );
	}

	__pass::symtab::addId( core, 0, node );

	VISIT_END( DeclWithType, node );
}

//--------------------------------------------------------------------------
// FunctionDecl
template< typename core_t >
const ast::DeclWithType * ast::Pass< core_t >::visit( const ast::FunctionDecl * node ) {
	VISIT_START( node );

	__pass::symtab::addId( core, 0, node );

	if ( __visit_children() ) {
		maybe_accept( node, &FunctionDecl::withExprs );
	}
	{
		// with clause introduces a level of scope (for the with expression members).
		// with clause exprs are added to the symbol table before parameters so that parameters
		// shadow with exprs and not the other way around.
		guard_symtab guard { *this };
		__pass::symtab::addWith( core, 0, node->withExprs, node );
		{
			guard_symtab guard { *this };
			// implicit add __func__ identifier as specified in the C manual 6.4.2.2
			// This is a C name and so has C linkage.
			static ast::ptr< ast::ObjectDecl > func{ new ast::ObjectDecl{
				CodeLocation{}, "__func__",
				new ast::ArrayType{
					new ast::BasicType{ ast::BasicType::Char, ast::CV::Const },
					nullptr, VariableLen, DynamicDim
				},
				nullptr,
				ast::Storage::Classes(),
				ast::Linkage::C,
			} };
			__pass::symtab::addId( core, 0, func );
			if ( __visit_children() ) {
				maybe_accept( node, &FunctionDecl::type_params );
				maybe_accept( node, &FunctionDecl::assertions );
				maybe_accept( node, &FunctionDecl::params );
				maybe_accept( node, &FunctionDecl::returns );
				maybe_accept( node, &FunctionDecl::type );
				maybe_accept( node, &FunctionDecl::attributes );
				// First remember that we are now within a function.
				ValueGuard< bool > oldInFunction( inFunction );
				inFunction = true;
				// The function body needs to have the same scope as parameters.
				// A CompoundStmt will not enter a new scope if atFunctionTop is true.
				ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
				atFunctionTop = true;
				maybe_accept( node, &FunctionDecl::stmts );
			}
		}
	}

	VISIT_END( DeclWithType, node );
}

//--------------------------------------------------------------------------
// StructDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::StructDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	// needs to be on the heap because addStruct saves the pointer
	__pass::symtab::addStructFwd( core, 0, node );

	if ( __visit_children() ) {
		guard_symtab guard { * this };
		maybe_accept( node, &StructDecl::params     );
		maybe_accept( node, &StructDecl::members    );
		maybe_accept( node, &StructDecl::attributes );
	}

	// this addition replaces the forward declaration
	__pass::symtab::addStruct( core, 0, node );

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// UnionDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::UnionDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	__pass::symtab::addUnionFwd( core, 0, node );

	if ( __visit_children() ) {
		guard_symtab guard { * this };
		maybe_accept( node, &UnionDecl::params     );
		maybe_accept( node, &UnionDecl::members    );
		maybe_accept( node, &UnionDecl::attributes );
	}

	__pass::symtab::addUnion( core, 0, node );

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// EnumDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::EnumDecl * node ) {
	VISIT_START( node );

	__pass::symtab::addEnum( core, 0, node );

	if ( __visit_children() ) {
		if ( node->hide == ast::EnumDecl::EnumHiding::Hide ) {
			guard_symtab guard { *this };
			maybe_accept( node, &EnumDecl::base );
			maybe_accept( node, &EnumDecl::params     );
			maybe_accept( node, &EnumDecl::members    );
			maybe_accept( node, &EnumDecl::attributes );
		} else {
			maybe_accept( node, &EnumDecl::base );
			maybe_accept( node, &EnumDecl::params     );
			maybe_accept( node, &EnumDecl::members    );
			maybe_accept( node, &EnumDecl::attributes );
		}
	}

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// TraitDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::TraitDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &TraitDecl::params     );
		maybe_accept( node, &TraitDecl::members    );
		maybe_accept( node, &TraitDecl::attributes );
	}

	__pass::symtab::addTrait( core, 0, node );

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// TypeDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::TypeDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &TypeDecl::base   );
	}

	// see A NOTE ON THE ORDER OF TRAVERSAL, above
	// note that assertions come after the type is added to the symtab, since they are not part of the type proper
	// and may depend on the type itself
	__pass::symtab::addType( core, 0, node );

	if ( __visit_children() ) {
		maybe_accept( node, &TypeDecl::assertions );

		{
			guard_symtab guard { *this };
			maybe_accept( node, &TypeDecl::init );
		}
	}

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// TypedefDecl
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::TypedefDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &TypedefDecl::base   );
	}

	__pass::symtab::addType( core, 0, node );

	if ( __visit_children() ) {
		maybe_accept( node, &TypedefDecl::assertions );
	}

	VISIT_END( Decl, node );
}

//--------------------------------------------------------------------------
// AsmDecl
template< typename core_t >
const ast::AsmDecl * ast::Pass< core_t >::visit( const ast::AsmDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &AsmDecl::stmt );
	}

	VISIT_END( AsmDecl, node );
}

//--------------------------------------------------------------------------
// DirectiveDecl
template< typename core_t >
const ast::DirectiveDecl * ast::Pass< core_t >::visit( const ast::DirectiveDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &DirectiveDecl::stmt );
	}

	VISIT_END( DirectiveDecl, node );
}

//--------------------------------------------------------------------------
// StaticAssertDecl
template< typename core_t >
const ast::StaticAssertDecl * ast::Pass< core_t >::visit( const ast::StaticAssertDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &StaticAssertDecl::cond );
		maybe_accept( node, &StaticAssertDecl::msg  );
	}

	VISIT_END( StaticAssertDecl, node );
}

//--------------------------------------------------------------------------
// InlineMemberDecl
template< typename core_t >
const ast::DeclWithType * ast::Pass< core_t >::visit( const ast::InlineMemberDecl * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &InlineMemberDecl::type );
		}
	}

	VISIT_END( DeclWithType, node );
}

//--------------------------------------------------------------------------
// CompoundStmt
template< typename core_t >
const ast::CompoundStmt * ast::Pass< core_t >::visit( const ast::CompoundStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// Do not enter (or leave) a new symbol table scope if atFunctionTop.
		// But always enter (and leave) a new general scope.
		if ( atFunctionTop ) {
			ValueGuard< bool > guard1( atFunctionTop );
			atFunctionTop = false;
			guard_scope guard2( *this );
			maybe_accept( node, &CompoundStmt::kids );
		} else {
			guard_symtab guard1( *this );
			guard_scope guard2( *this );
			maybe_accept( node, &CompoundStmt::kids );
		}
	}

	VISIT_END( CompoundStmt, node );
}

//--------------------------------------------------------------------------
// ExprStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::ExprStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &ExprStmt::expr );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// AsmStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::AsmStmt * node ) {
	VISIT_START( node )

	if ( __visit_children() ) {
		maybe_accept( node, &AsmStmt::instruction );
		maybe_accept( node, &AsmStmt::output      );
		maybe_accept( node, &AsmStmt::input       );
		maybe_accept( node, &AsmStmt::clobber     );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// DirectiveStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::DirectiveStmt * node ) {
	VISIT_START( node )

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// IfStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::IfStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// if statements introduce a level of scope (for the initialization)
		guard_symtab guard { *this };
		maybe_accept( node, &IfStmt::inits    );
		maybe_accept_top( node, &IfStmt::cond     );
		maybe_accept_as_compound( node, &IfStmt::then );
		maybe_accept_as_compound( node, &IfStmt::else_ );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// WhileDoStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::WhileDoStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// while statements introduce a level of scope (for the initialization)
		guard_symtab guard { *this };
		maybe_accept( node, &WhileDoStmt::inits );
		maybe_accept_top( node, &WhileDoStmt::cond  );
		maybe_accept_as_compound( node, &WhileDoStmt::body  );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// ForStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::ForStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// for statements introduce a level of scope (for the initialization)
		guard_symtab guard { *this };
		// xxx - old ast does not create WithStmtsToAdd scope for loop inits. should revisit this later.
		maybe_accept( node, &ForStmt::inits );
		maybe_accept_top( node, &ForStmt::cond  );
		maybe_accept_top( node, &ForStmt::inc   );
		maybe_accept_as_compound( node, &ForStmt::body  );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// SwitchStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::SwitchStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &SwitchStmt::cond  );
		maybe_accept( node, &SwitchStmt::cases );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// CaseClause
template< typename core_t >
const ast::CaseClause * ast::Pass< core_t >::visit( const ast::CaseClause * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &CaseClause::cond  );
		maybe_accept( node, &CaseClause::stmts );
	}

	VISIT_END( CaseClause, node );
}

//--------------------------------------------------------------------------
// BranchStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::BranchStmt * node ) {
	VISIT_START( node );
	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// ReturnStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::ReturnStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &ReturnStmt::expr );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// ThrowStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::ThrowStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &ThrowStmt::expr   );
		maybe_accept( node, &ThrowStmt::target );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// TryStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::TryStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &TryStmt::body     );
		maybe_accept( node, &TryStmt::handlers );
		maybe_accept( node, &TryStmt::finally  );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// CatchClause
template< typename core_t >
const ast::CatchClause * ast::Pass< core_t >::visit( const ast::CatchClause * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// catch statements introduce a level of scope (for the caught exception)
		guard_symtab guard { *this };
		maybe_accept( node, &CatchClause::decl );
		maybe_accept_top( node, &CatchClause::cond );
		maybe_accept_as_compound( node, &CatchClause::body );
	}

	VISIT_END( CatchClause, node );
}

//--------------------------------------------------------------------------
// FinallyClause
template< typename core_t >
const ast::FinallyClause * ast::Pass< core_t >::visit( const ast::FinallyClause * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &FinallyClause::body );
	}

	VISIT_END( FinallyClause, node );
}

//--------------------------------------------------------------------------
// FinallyStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::SuspendStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &SuspendStmt::then   );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// WhenClause
template< typename core_t >
const ast::WhenClause * ast::Pass< core_t >::visit( const ast::WhenClause * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &WhenClause::target );
		maybe_accept( node, &WhenClause::stmt );
		maybe_accept( node, &WhenClause::when_cond );
	}

	VISIT_END( WhenClause, node );
}

//--------------------------------------------------------------------------
// WaitForStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::WaitForStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &WaitForStmt::clauses );
		maybe_accept( node, &WaitForStmt::timeout_time );
		maybe_accept( node, &WaitForStmt::timeout_stmt );
		maybe_accept( node, &WaitForStmt::timeout_cond );
		maybe_accept( node, &WaitForStmt::else_stmt );
		maybe_accept( node, &WaitForStmt::else_cond );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// WaitForClause
template< typename core_t >
const ast::WaitForClause * ast::Pass< core_t >::visit( const ast::WaitForClause * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &WaitForClause::target );
		maybe_accept( node, &WaitForClause::target_args );
		maybe_accept( node, &WaitForClause::stmt );
		maybe_accept( node, &WaitForClause::when_cond );
	}

	VISIT_END( WaitForClause, node );
}

//--------------------------------------------------------------------------
// WaitUntilStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::WaitUntilStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &WaitUntilStmt::clauses );
		maybe_accept( node, &WaitUntilStmt::timeout_time );
		maybe_accept( node, &WaitUntilStmt::timeout_stmt );
		maybe_accept( node, &WaitUntilStmt::timeout_cond );
		maybe_accept( node, &WaitUntilStmt::else_stmt );
		maybe_accept( node, &WaitUntilStmt::else_cond );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// WithStmt
template< typename core_t >
const ast::Decl * ast::Pass< core_t >::visit( const ast::WithStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &WithStmt::exprs );
		{
			// catch statements introduce a level of scope (for the caught exception)
			guard_symtab guard { *this };
			__pass::symtab::addWith( core, 0, node->exprs, node );
			maybe_accept( node, &WithStmt::stmt );
		}
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// NullStmt
template< typename core_t >
const ast::NullStmt * ast::Pass< core_t >::visit( const ast::NullStmt * node ) {
	VISIT_START( node );
	VISIT_END( NullStmt, node );
}

//--------------------------------------------------------------------------
// DeclStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::DeclStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &DeclStmt::decl );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// ImplicitCtorDtorStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::ImplicitCtorDtorStmt * node ) {
	VISIT_START( node );

	// For now this isn't visited, it is unclear if this causes problem
	// if all tests are known to pass, remove this code
	if ( __visit_children() ) {
		maybe_accept( node, &ImplicitCtorDtorStmt::callStmt );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// MutexStmt
template< typename core_t >
const ast::Stmt * ast::Pass< core_t >::visit( const ast::MutexStmt * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// mutex statements introduce a level of scope (for the initialization)
		guard_symtab guard { *this };
		maybe_accept( node, &MutexStmt::stmt );
		maybe_accept( node, &MutexStmt::mutexObjs );
	}

	VISIT_END( Stmt, node );
}

//--------------------------------------------------------------------------
// ApplicationExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::ApplicationExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &ApplicationExpr::result );
		}
		maybe_accept( node, &ApplicationExpr::func );
		maybe_accept( node, &ApplicationExpr::args );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UntypedExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UntypedExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UntypedExpr::result );
		}

		maybe_accept( node, &UntypedExpr::args );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// NameExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::NameExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &NameExpr::result );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// QualifiedNameExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::QualifiedNameExpr * node ) {
	VISIT_START( node );
	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &QualifiedNameExpr::type_decl );
	}
	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// CastExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::CastExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &CastExpr::result );
		}
		maybe_accept( node, &CastExpr::arg );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// KeywordCastExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::KeywordCastExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &KeywordCastExpr::result );
		}
		maybe_accept( node, &KeywordCastExpr::arg );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// VirtualCastExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::VirtualCastExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &VirtualCastExpr::result );
		}
		maybe_accept( node, &VirtualCastExpr::arg );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// AddressExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::AddressExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &AddressExpr::result );
		}
		maybe_accept( node, &AddressExpr::arg );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// LabelAddressExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::LabelAddressExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &LabelAddressExpr::result );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UntypedMemberExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UntypedMemberExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UntypedMemberExpr::result );
		}
		maybe_accept( node, &UntypedMemberExpr::aggregate );
		maybe_accept( node, &UntypedMemberExpr::member    );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// MemberExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::MemberExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &MemberExpr::result );
		}
		maybe_accept( node, &MemberExpr::aggregate );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// VariableExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::VariableExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &VariableExpr::result );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// ConstantExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::ConstantExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &ConstantExpr::result );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// SizeofExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::SizeofExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &SizeofExpr::result );
		}
		if ( node->type ) {
			maybe_accept( node, &SizeofExpr::type );
		} else {
			maybe_accept( node, &SizeofExpr::expr );
		}
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// AlignofExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::AlignofExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &AlignofExpr::result );
		}
		if ( node->type ) {
			maybe_accept( node, &AlignofExpr::type );
		} else {
			maybe_accept( node, &AlignofExpr::expr );
		}
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UntypedOffsetofExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UntypedOffsetofExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UntypedOffsetofExpr::result );
		}
		maybe_accept( node, &UntypedOffsetofExpr::type   );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// OffsetofExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::OffsetofExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &OffsetofExpr::result );
		}
		maybe_accept( node, &OffsetofExpr::type   );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// OffsetPackExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::OffsetPackExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &OffsetPackExpr::result );
		}
		maybe_accept( node, &OffsetPackExpr::type   );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// LogicalExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::LogicalExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &LogicalExpr::result );
		}
		maybe_accept( node, &LogicalExpr::arg1 );
		maybe_accept( node, &LogicalExpr::arg2 );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// ConditionalExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::ConditionalExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &ConditionalExpr::result );
		}
		maybe_accept( node, &ConditionalExpr::arg1 );
		maybe_accept( node, &ConditionalExpr::arg2 );
		maybe_accept( node, &ConditionalExpr::arg3 );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// CommaExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::CommaExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &CommaExpr::result );
		}
		maybe_accept( node, &CommaExpr::arg1 );
		maybe_accept( node, &CommaExpr::arg2 );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// TypeExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::TypeExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &TypeExpr::result );
		}
		maybe_accept( node, &TypeExpr::type );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// DimensionExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::DimensionExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &DimensionExpr::result );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// AsmExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::AsmExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &AsmExpr::result );
		}
		maybe_accept( node, &AsmExpr::constraint );
		maybe_accept( node, &AsmExpr::operand    );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// ImplicitCopyCtorExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::ImplicitCopyCtorExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &ImplicitCopyCtorExpr::result );
		}
		maybe_accept( node, &ImplicitCopyCtorExpr::callExpr    );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// ConstructorExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::ConstructorExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &ConstructorExpr::result );
		}
		maybe_accept( node, &ConstructorExpr::callExpr );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// CompoundLiteralExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::CompoundLiteralExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &CompoundLiteralExpr::result );
		}
		maybe_accept( node, &CompoundLiteralExpr::init );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// RangeExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::RangeExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &RangeExpr::result );
		}
		maybe_accept( node, &RangeExpr::low    );
		maybe_accept( node, &RangeExpr::high   );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UntypedTupleExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UntypedTupleExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UntypedTupleExpr::result );
		}
		maybe_accept( node, &UntypedTupleExpr::exprs  );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// TupleExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::TupleExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &TupleExpr::result );
		}
		maybe_accept( node, &TupleExpr::exprs  );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// TupleIndexExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::TupleIndexExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &TupleIndexExpr::result );
		}
		maybe_accept( node, &TupleIndexExpr::tuple  );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// TupleAssignExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::TupleAssignExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &TupleAssignExpr::result );
		}
		maybe_accept( node, &TupleAssignExpr::stmtExpr );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// StmtExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::StmtExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// don't want statements from outer CompoundStmts to be added to this StmtExpr
		// get the stmts that will need to be spliced in
		auto stmts_before = __pass::stmtsToAddBefore( core, 0);
		auto stmts_after  = __pass::stmtsToAddAfter ( core, 0);

		// These may be modified by subnode but most be restored once we exit this statemnet.
		ValueGuardPtr< const ast::TypeSubstitution * > __old_env( __pass::typeSubs( core, 0 ) );
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_before) >::type > __old_decls_before( stmts_before );
		ValueGuardPtr< typename std::remove_pointer< decltype(stmts_after ) >::type > __old_decls_after ( stmts_after  );

		{
			guard_symtab guard { *this };
			maybe_accept( node, &StmtExpr::result );
		}
		maybe_accept( node, &StmtExpr::stmts       );
		maybe_accept( node, &StmtExpr::returnDecls );
		maybe_accept( node, &StmtExpr::dtors       );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UniqueExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UniqueExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UniqueExpr::result );
		}
		maybe_accept( node, &UniqueExpr::expr   );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// UntypedInitExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::UntypedInitExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &UntypedInitExpr::result );
		}
		maybe_accept( node, &UntypedInitExpr::expr   );
		// not currently visiting initAlts, but this doesn't matter since this node is only used in the resolver.
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// InitExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::InitExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &InitExpr::result );
		}
		maybe_accept( node, &InitExpr::expr   );
		maybe_accept( node, &InitExpr::designation );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// DeletedExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::DeletedExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &DeletedExpr::result );
		}
		maybe_accept( node, &DeletedExpr::expr );
		// don't visit deleteStmt, because it is a pointer to somewhere else in the tree.
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// DefaultArgExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::DefaultArgExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &DefaultArgExpr::result );
		}
		maybe_accept( node, &DefaultArgExpr::expr );
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// GenericExpr
template< typename core_t >
const ast::Expr * ast::Pass< core_t >::visit( const ast::GenericExpr * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			guard_symtab guard { *this };
			maybe_accept( node, &GenericExpr::result );
		}
		maybe_accept( node, &GenericExpr::control );

		std::vector<GenericExpr::Association> new_kids;
		new_kids.reserve(node->associations.size());
		bool mutated = false;
		for( const auto & assoc : node->associations ) {
			const Type * type = nullptr;
			if( assoc.type ) {
				guard_symtab guard { *this };
				type = assoc.type->accept( *this );
				if( type != assoc.type ) mutated = true;
			}
			const Expr * expr = nullptr;
			if( assoc.expr ) {
				expr = assoc.expr->accept( *this );
				if( expr != assoc.expr ) mutated = true;
			}
			new_kids.emplace_back( type, expr );
		}

		if(mutated) {
			auto n = __pass::mutate<core_t>(node);
			n->associations = std::move( new_kids );
			node = n;
		}
	}

	VISIT_END( Expr, node );
}

//--------------------------------------------------------------------------
// VoidType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::VoidType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// BasicType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::BasicType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// PointerType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::PointerType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &PointerType::dimension );
		maybe_accept( node, &PointerType::base );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// ArrayType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::ArrayType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &ArrayType::dimension );
		maybe_accept( node, &ArrayType::base );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// ReferenceType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::ReferenceType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &ReferenceType::base );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// QualifiedType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::QualifiedType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &QualifiedType::parent );
		maybe_accept( node, &QualifiedType::child );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// FunctionType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::FunctionType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		// guard_forall_subs forall_guard { *this, node };
		// mutate_forall( node );
		maybe_accept( node, &FunctionType::assertions );
		maybe_accept( node, &FunctionType::returns );
		maybe_accept( node, &FunctionType::params  );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// StructInstType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::StructInstType * node ) {
	VISIT_START( node );

	__pass::symtab::addStruct( core, 0, node->name );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &StructInstType::params );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// UnionInstType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::UnionInstType * node ) {
	VISIT_START( node );

	__pass::symtab::addUnion( core, 0, node->name );

	if ( __visit_children() ) {
		guard_symtab guard { *this };
		maybe_accept( node, &UnionInstType::params );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// EnumInstType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::EnumInstType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &EnumInstType::params );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// TraitInstType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::TraitInstType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &TraitInstType::params );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// TypeInstType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::TypeInstType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		{
			maybe_accept( node, &TypeInstType::params );
		}
		// ensure that base re-bound if doing substitution
		__pass::forall::replace( core, 0, node );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// TupleType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::TupleType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &TupleType::types );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// TypeofType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::TypeofType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &TypeofType::expr );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// VTableType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::VTableType * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &VTableType::base );
	}

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// VarArgsType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::VarArgsType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// ZeroType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::ZeroType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// OneType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::OneType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}

//--------------------------------------------------------------------------
// GlobalScopeType
template< typename core_t >
const ast::Type * ast::Pass< core_t >::visit( const ast::GlobalScopeType * node ) {
	VISIT_START( node );

	VISIT_END( Type, node );
}


//--------------------------------------------------------------------------
// Designation
template< typename core_t >
const ast::Designation * ast::Pass< core_t >::visit( const ast::Designation * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &Designation::designators );
	}

	VISIT_END( Designation, node );
}

//--------------------------------------------------------------------------
// SingleInit
template< typename core_t >
const ast::Init * ast::Pass< core_t >::visit( const ast::SingleInit * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept_top( node, &SingleInit::value );
	}

	VISIT_END( Init, node );
}

//--------------------------------------------------------------------------
// ListInit
template< typename core_t >
const ast::Init * ast::Pass< core_t >::visit( const ast::ListInit * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &ListInit::designations );
		maybe_accept( node, &ListInit::initializers );
	}

	VISIT_END( Init, node );
}

//--------------------------------------------------------------------------
// ConstructorInit
template< typename core_t >
const ast::Init * ast::Pass< core_t >::visit( const ast::ConstructorInit * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &ConstructorInit::ctor );
		maybe_accept( node, &ConstructorInit::dtor );
		maybe_accept( node, &ConstructorInit::init );
	}

	VISIT_END( Init, node );
}

//--------------------------------------------------------------------------
// Attribute
template< typename core_t >
const ast::Attribute * ast::Pass< core_t >::visit( const ast::Attribute * node  )  {
	VISIT_START( node );

	if ( __visit_children() ) {
		maybe_accept( node, &Attribute::params );
	}

	VISIT_END( Attribute, node );
}

//--------------------------------------------------------------------------
// TypeSubstitution
template< typename core_t >
const ast::TypeSubstitution * ast::Pass< core_t >::visit( const ast::TypeSubstitution * node ) {
	VISIT_START( node );

	if ( __visit_children() ) {
		bool mutated = false;
		ast::TypeSubstitution::TypeMap new_map;
		for ( const auto & p : node->typeMap ) {
			guard_symtab guard { *this };
			auto new_node = p.second->accept( *this );
			if (new_node != p.second) mutated = true;
			new_map.insert({ p.first, new_node });
		}
		if (mutated) {
			auto new_node = __pass::mutate<core_t>( node );
			new_node->typeMap.swap( new_map );
			node = new_node;
		}
	}

	VISIT_END( TypeSubstitution, node );
}

#undef __pedantic_pass_assertf
#undef __pedantic_pass_assert
#undef VISIT_START
#undef VISIT_END
