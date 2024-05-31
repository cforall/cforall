//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Pass.proto.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15::37::05 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once
// IWYU pragma: private, include "Pass.hpp"

#include "Common/Iterate.hpp"
#include "Common/Stats/Heap.hpp"
#include "Common/Utility.hpp"
namespace ast {
	template<typename core_t> class Pass;
	class TranslationUnit;
	struct PureVisitor;
	template<typename node_t> node_t * deepCopy( const node_t * );
}

#ifdef PEDANTIC_PASS_ASSERT
#define __pedantic_pass_assert(...) assert(__VA_ARGS__)
#define __pedantic_pass_assertf(...) assertf(__VA_ARGS__)
#else
#define __pedantic_pass_assert(...)
#define __pedantic_pass_assertf(...)
#endif

namespace ast::__pass {

typedef std::function<void( void * )> cleanup_func_t;
typedef std::function<void( cleanup_func_t, void * )> at_cleanup_t;

/// Replaces guards we don't want to use.
struct empty_guard {
};

/// Implementation of the value guard. Created inside the visit scope.
class value_guard {
public:
	/// Push onto the cleanup
	value_guard( at_cleanup_t & at_cleanup ) {
		at_cleanup = [this]( cleanup_func_t && func, void* val ) {
			push( std::move( func ), val );
		};
	}

	~value_guard() {
		while( !cleanups.empty() ) {
			auto& cleanup = cleanups.top();
			cleanup.func( cleanup.val );
			cleanups.pop();
		}
	}

	void push( cleanup_func_t && func, void* val ) {
		cleanups.emplace( std::move(func), val );
	}

private:
	struct cleanup_t {
		cleanup_func_t func;
		void * val;

		cleanup_t( cleanup_func_t&& func, void * val ) : func(func), val(val) {}
	};

	std::stack< cleanup_t, std::vector<cleanup_t> > cleanups;
};

/// The result is a single node.
template< typename node_t >
struct result1 {
	bool differs = false;
	const node_t * value = nullptr;

	template< typename object_t, typename super_t, typename field_t >
	void apply( object_t * object, field_t super_t::* field ) {
		object->*field = value;
	}
};

/// The result is a container of statements.
template< template<class...> class container_t >
struct resultNstmt {
	/// The delta/change on a single node.
	struct delta {
		ptr<Stmt> new_val;
		ssize_t old_idx;
		bool is_old;

		explicit delta(const Stmt * s) : new_val(s), old_idx(-1), is_old(false) {}
		explicit delta(ssize_t i) : new_val(nullptr), old_idx(i), is_old(true) {}
	};

	bool differs = false;
	container_t< delta > values;

	template< typename object_t, typename super_t, typename field_t >
	void apply( object_t * object, field_t super_t::* field ) {
		field_t & container = object->*field;
		__pedantic_pass_assert( container.size() <= values.size() );

		auto cit = enumerate(container).begin();

		container_t<ptr<Stmt>> nvals;
		for ( delta & d : values ) {
			if ( d.is_old ) {
				__pedantic_pass_assert( cit.idx <= d.old_idx );
				std::advance( cit, d.old_idx - cit.idx );
				nvals.push_back( std::move( (*cit).val ) );
			} else {
				nvals.push_back( std::move( d.new_val ) );
			}
		}

		container = std::move(nvals);
	}

	template< template<class...> class incontainer_t >
	void take_all( incontainer_t<ptr<Stmt>> * stmts ) {
		if ( !stmts || stmts->empty() ) return;

		std::transform( stmts->begin(), stmts->end(), std::back_inserter( values ),
			[](ast::ptr<ast::Stmt>& stmt) -> delta {
				return delta( stmt.release() );
			});
		stmts->clear();
		differs = true;
	}

	template< template<class...> class incontainer_t >
	void take_all( incontainer_t<ptr<Decl>> * decls ) {
		if ( !decls || decls->empty() ) return;

		std::transform( decls->begin(), decls->end(), std::back_inserter( values ),
			[](ast::ptr<ast::Decl>& decl) -> delta {
				ast::Decl const * d = decl.release();
				return delta( new DeclStmt( d->location, d ) );
			});
		decls->clear();
		differs = true;
	}
};

/// The result is a container of nodes.
template< template<class...> class container_t, typename node_t >
struct resultN {
	bool differs = false;
	container_t<ptr<node_t>> values;

	template< typename object_t, typename super_t, typename field_t >
	void apply( object_t * object, field_t super_t::* field ) {
		field_t & container = object->*field;
		__pedantic_pass_assert( container.size() == values.size() );

		for ( size_t i = 0; i < container.size(); ++i ) {
			// Take all the elements that are different in 'values'
			// and swap them into 'container'
			if ( values[i] != nullptr ) swap(container[i], values[i]);
		}
		// Now the original containers should still have the unchanged values
		// but also contain the new values.
	}
};

/// "Short hand" to check if this is a valid previsit function
/// Mostly used to make the static_assert look (and print) prettier
template<typename core_t, typename node_t>
struct is_valid_previsit {
	using ret_t = decltype( std::declval<core_t*>()->previsit( std::declval<const node_t *>() ) );

	static constexpr bool value = std::is_void< ret_t >::value ||
		std::is_base_of<const node_t, typename std::remove_pointer<ret_t>::type >::value;
};

//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Deep magic (a.k.a template meta programming) to make the templated visitor work
// Basically the goal is to make 2 previsit
// 1 - Use when a pass implements a valid previsit. This uses overloading which means the any overload of
//     'pass.previsit( node )' that compiles will be used for that node for that type
//     This requires that this option only compile for passes that actually define an appropriate visit.
//     SFINAE will make sure the compilation errors in this function don't halt the build.
//     See http://en.cppreference.com/w/cpp/language/sfinae for details on SFINAE
// 2 - Since the first implementation might not be specilizable, the second implementation exists and does nothing.
//     This is needed only to eliminate the need for passes to specify any kind of handlers.
//     The second implementation only works because it has a lower priority. This is due to the bogus last parameter.
//     The second implementation takes a long while the first takes an int. Since the caller always passes an literal 0
//     the first implementation takes priority in regards to overloading.
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// PreVisit : may mutate the pointer passed in if the node is mutated in the previsit call
template<typename core_t, typename node_t>
static inline auto previsit( core_t & core, const node_t * & node, int ) -> decltype( core.previsit( node ), void() ) {
	static_assert(
		is_valid_previsit<core_t, node_t>::value,
		"Previsit may not change the type of the node. It must return its paremeter or void."
	);

	// We need to reassign the result to 'node', unless the function
	// returns void, then we just leave 'node' unchanged
	if constexpr ( std::is_void_v<decltype( core.previsit( node ) )> ) {
		core.previsit( node );
	} else {
		node = core.previsit( node );
		assertf( node, "Previsit must not return nullptr." );
	}
}

template<typename core_t, typename node_t>
static inline auto previsit( core_t &, const node_t *, long ) {}

// PostVisit : never mutates the passed pointer but may return a different node
template<typename core_t, typename node_t>
static inline auto postvisit( core_t & core, const node_t * node, int ) ->
	decltype( core.postvisit( node ), node->accept( *(Visitor*)nullptr ) )
{
	// We need to return the result unless the function
	// returns void, then we just return the original node
	if constexpr ( std::is_void_v<decltype( core.postvisit( node ) )> ) {
		core.postvisit( node );
		return node;
	} else {
		return core.postvisit( node );
	}
}

template<typename core_t, typename node_t>
static inline const node_t * postvisit( core_t &, const node_t * node, long ) { return node; }

//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Deep magic (a.k.a template meta programming) continued
// To make the templated visitor be more expressive, we allow 'accessories' : classes/structs the implementation can inherit
// from in order to get extra functionallity for example
// class ErrorChecker : WithShortCircuiting { ... };
// Pass<ErrorChecker> checker;
// this would define a pass that uses the templated visitor with the additionnal feature that it has short circuiting
// Note that in all cases the accessories are not required but guarantee the requirements of the feature is matched
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// For several accessories, the feature is enabled by detecting that a specific field is present
// Use a macro the encapsulate the logic of detecting a particular field
// The type is not strictly enforced but does match the accessory
#define FIELD_PTR( name, default_type ) \
template< typename core_t > \
static inline auto name( core_t & core, int ) -> decltype( &core.name ) { return &core.name; } \
\
template< typename core_t > \
static inline default_type * name( core_t &, long ) { return nullptr; }

// List of fields and their expected types
FIELD_PTR( typeSubs, const ast::TypeSubstitution * )
FIELD_PTR( stmtsToAddBefore, std::list< ast::ptr< ast::Stmt > > )
FIELD_PTR( stmtsToAddAfter , std::list< ast::ptr< ast::Stmt > > )
FIELD_PTR( declsToAddBefore, std::list< ast::ptr< ast::Decl > > )
FIELD_PTR( declsToAddAfter , std::list< ast::ptr< ast::Decl > > )
FIELD_PTR( visit_children, bool )
FIELD_PTR( visitor, ast::Pass<core_t> * const )
FIELD_PTR( translationUnit, const TranslationUnit * )

// Remove the macro to make sure we don't clash
#undef FIELD_PTR

template< typename core_t >
static inline auto beginTrace(core_t &, int) -> decltype( core_t::traceId, void() ) {
	// Stats::Heap::stacktrace_push(core_t::traceId);
}

template< typename core_t >
static inline auto endTrace(core_t &, int) -> decltype( core_t::traceId, void() ) {
	// Stats::Heap::stacktrace_pop();
}

template< typename core_t >
static void beginTrace(core_t &, long) {}

template< typename core_t >
static void endTrace(core_t &, long) {}

// Allows visitor to handle an error on top-level declarations, and possibly suppress the error.
// If on_error() returns false, the error will be ignored. By default, it returns true.

template< typename core_t >
static bool on_error (core_t &, ptr<Decl> &, long) { return true; }

template< typename core_t >
static auto on_error (core_t & core, ptr<Decl> & decl, int) -> decltype(core.on_error(decl)) {
	return core.on_error(decl);
}

// These are the guards declared at the beginning of a visit.
// They are replaced with empty objects when not used.

template< typename core_t, typename node_t >
static inline auto make_location_guard( core_t & core, node_t * node, int )
		-> decltype( node->location, ValueGuardPtr<const CodeLocation *>( &core.location ) ) {
	ValueGuardPtr<const CodeLocation *> guard( &core.location );
	core.location = &node->location;
	return guard;
}

template< typename core_t, typename node_t >
static inline empty_guard make_location_guard( core_t &, node_t *, long ) {
	return empty_guard();
}

template< typename core_t >
static inline auto make_visit_children_guard( core_t & core, int )
		-> decltype( ValueGuardPtr<bool>( &core.visit_children ) ) {
	ValueGuardPtr<bool> guard( &core.visit_children );
	core.visit_children = true;
	return guard;
}

template< typename core_t >
static inline empty_guard make_visit_children_guard( core_t &, long ) {
	return empty_guard();
}

template< typename core_t >
static inline auto make_value_guard( core_t & core, int )
		-> decltype( __pass::value_guard( core.at_cleanup ) ) {
	// Requires guaranteed copy elision:
	return value_guard( core.at_cleanup );
}

template< typename core_t >
static inline empty_guard make_value_guard( core_t &, long ) {
	return empty_guard();
}

// Another feature of the templated visitor is that it calls beginScope()/endScope() for compound statement.
// All passes which have such functions are assumed desire this behaviour
// detect it using the same strategy
namespace scope {
	template<typename core_t>
	static inline auto enter( core_t & core, int ) -> decltype( core.beginScope(), void() ) {
		core.beginScope();
	}

	template<typename core_t>
	static inline void enter( core_t &, long ) {}

	template<typename core_t>
	static inline auto leave( core_t & core, int ) -> decltype( core.endScope(), void() ) {
		core.endScope();
	}

	template<typename core_t>
	static inline void leave( core_t &, long ) {}
} // namespace scope

// Certain passes desire an up to date symbol table automatically
// detect the presence of a member name `symtab` and call all the members appropriately
namespace symtab {
	// Some simple scoping rules
	template<typename core_t>
	static inline auto enter( core_t & core, int ) -> decltype( core.symtab, void() ) {
		core.symtab.enterScope();
	}

	template<typename core_t>
	static inline auto enter( core_t &, long ) {}

	template<typename core_t>
	static inline auto leave( core_t & core, int ) -> decltype( core.symtab, void() ) {
		core.symtab.leaveScope();
	}

	template<typename core_t>
	static inline auto leave( core_t &, long ) {}

	// The symbol table has 2 kind of functions mostly, 1 argument and 2 arguments
	// Create macro to condense these common patterns
	#define SYMTAB_FUNC1( func, type ) \
	template<typename core_t> \
	static inline auto func( core_t & core, int, type arg ) -> decltype( core.symtab.func( arg ), void() ) {\
		core.symtab.func( arg ); \
	} \
	\
	template<typename core_t> \
	static inline void func( core_t &, long, type ) {}

	#define SYMTAB_FUNC2( func, type1, type2 ) \
	template<typename core_t> \
	static inline auto func( core_t & core, int, type1 arg1, type2 arg2 ) -> decltype( core.symtab.func( arg1, arg2 ), void () ) {\
		core.symtab.func( arg1, arg2 ); \
	} \
	\
	template<typename core_t> \
	static inline void func( core_t &, long, type1, type2 ) {}

	SYMTAB_FUNC1( addId     , const DeclWithType *  );
	SYMTAB_FUNC1( addType   , const NamedTypeDecl * );
	SYMTAB_FUNC1( addStruct , const StructDecl *    );
	SYMTAB_FUNC1( addEnum   , const EnumDecl *      );
	SYMTAB_FUNC1( addUnion  , const UnionDecl *     );
	SYMTAB_FUNC1( addTrait  , const TraitDecl *     );
	SYMTAB_FUNC2( addWith   , const std::vector< ptr<Expr> > &, const Decl * );

	// A few extra functions have more complicated behaviour, they are hand written
	template<typename core_t>
	static inline auto addStructFwd( core_t & core, int, const ast::StructDecl * decl ) -> decltype( core.symtab.addStruct( decl ), void() ) {
		ast::StructDecl * fwd = new ast::StructDecl( decl->location, decl->name );
		for ( const auto & param : decl->params ) {
			fwd->params.push_back( deepCopy( param.get() ) );
		}
		core.symtab.addStruct( fwd );
	}

	template<typename core_t>
	static inline void addStructFwd( core_t &, long, const ast::StructDecl * ) {}

	template<typename core_t>
	static inline auto addUnionFwd( core_t & core, int, const ast::UnionDecl * decl ) -> decltype( core.symtab.addUnion( decl ), void() ) {
		ast::UnionDecl * fwd = new ast::UnionDecl( decl->location, decl->name );
		for ( const auto & param : decl->params ) {
			fwd->params.push_back( deepCopy( param.get() ) );
		}
		core.symtab.addUnion( fwd );
	}

	template<typename core_t>
	static inline void addUnionFwd( core_t &, long, const ast::UnionDecl * ) {}

	template<typename core_t>
	static inline auto addStructId( core_t & core, int, const std::string & str ) -> decltype( core.symtab.addStructId( str ), void() ) {
		if ( ! core.symtab.lookupStruct( str ) ) {
			core.symtab.addStructId( str );
		}
	}

	template<typename core_t>
	static inline void addStructId( core_t &, long, const std::string & ) {}

	template<typename core_t>
	static inline auto addUnionId( core_t & core, int, const std::string & str ) -> decltype( core.symtab.addUnionId( str ), void() ) {
		if ( ! core.symtab.lookupUnion( str ) ) {
			core.symtab.addUnionId( str );
		}
	}

	template<typename core_t>
	static inline void addUnionId( core_t &, long, const std::string & ) {}

	#undef SYMTAB_FUNC1
	#undef SYMTAB_FUNC2
} // namespace symtab

// Some passes need to mutate TypeDecl and properly update their pointing TypeInstType.
// Detect the presence of a member name `subs` and call all members appropriately
namespace forall {
	// Some simple scoping rules
	template<typename core_t>
	static inline auto enter( core_t & core, int, const ast::FunctionType * type )
			-> decltype( core.subs, void() ) {
		if ( ! type->forall.empty() ) core.subs.beginScope();
	}

	template<typename core_t>
	static inline auto enter( core_t &, long, const ast::FunctionType * ) {}

	template<typename core_t>
	static inline auto leave( core_t & core, int, const ast::FunctionType * type )
			-> decltype( core.subs, void() ) {
		if ( ! type->forall.empty() ) { core.subs.endScope(); }
	}

	template<typename core_t>
	static inline auto leave( core_t &, long, const ast::FunctionType * ) {}

	// Replaces a TypeInstType's base TypeDecl according to the table
	template<typename core_t>
	static inline auto replace( core_t & core, int, const ast::TypeInstType *& inst )
			-> decltype( core.subs, void() ) {
		inst = ast::mutate_field(
			inst, &ast::TypeInstType::base, core.subs.replace( inst->base ) );
	}

	template<typename core_t>
	static inline auto replace( core_t &, long, const ast::TypeInstType *& ) {}
} // namespace forall

// For passes, usually utility passes, that have a result.
namespace result {
	template<typename core_t>
	static inline auto get( core_t & core, char ) -> decltype( core.result() ) {
		return core.result();
	}

	template<typename core_t>
	static inline auto get( core_t & core, int ) -> decltype( core.result ) {
		return core.result;
	}

	template<typename core_t>
	static inline void get( core_t &, long ) {}
}

} // namespace ast::__pass

#undef __pedantic_pass_assertf
#undef __pedantic_pass_assert
