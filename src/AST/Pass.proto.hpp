//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Pass.impl.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May 09 15::37::05 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once
// IWYU pragma: private, include "Pass.hpp"

#include "Common/Stats/Heap.h"

namespace ast {
template<typename core_t>
class Pass;

struct TranslationUnit;

struct PureVisitor;

namespace __pass {
	typedef std::function<void( void * )> cleanup_func_t;
	typedef std::function<void( cleanup_func_t, void * )> at_cleanup_t;


	// boolean reference that may be null
	// either refers to a boolean value or is null and returns true
	class bool_ref {
	public:
		bool_ref() = default;
		~bool_ref() = default;

		operator bool() { return m_ref ? *m_ref : true; }
		bool operator=( bool val ) { assert(m_ref); return *m_ref = val; }

	private:

		friend class visit_children_guard;

		bool * set( bool * val ) {
			bool * prev = m_ref;
			m_ref = val;
			return prev;
		}

		bool * m_ref = nullptr;
	};

	// Implementation of the guard value
	// Created inside the visit scope
	class guard_value {
	public:
		/// Push onto the cleanup
		guard_value( at_cleanup_t * at_cleanup ) {
			if( at_cleanup ) {
				*at_cleanup = [this]( cleanup_func_t && func, void* val ) {
					push( std::move( func ), val );
				};
			}
		}

		~guard_value() {
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

	// Guard structure implementation for whether or not children should be visited
	class visit_children_guard {
	public:

		visit_children_guard( bool_ref * ref )
			: m_val ( true )
			, m_prev( ref ? ref->set( &m_val ) : nullptr )
			, m_ref ( ref )
		{}

		~visit_children_guard() {
			if( m_ref ) {
				m_ref->set( m_prev );
			}
		}

		operator bool() { return m_val; }

	private:
		bool       m_val;
		bool     * m_prev;
		bool_ref * m_ref;
	};

	/// "Short hand" to check if this is a valid previsit function
	/// Mostly used to make the static_assert look (and print) prettier
	template<typename core_t, typename node_t>
	struct is_valid_previsit {
		using ret_t = decltype( std::declval<core_t*>()->previsit( std::declval<const node_t *>() ) );

		static constexpr bool value = std::is_void< ret_t >::value ||
			std::is_base_of<const node_t, typename std::remove_pointer<ret_t>::type >::value;
	};

	/// Used by previsit implementation
	/// We need to reassign the result to 'node', unless the function
	/// returns void, then we just leave 'node' unchanged
	template<bool is_void>
	struct __assign;

	template<>
	struct __assign<true> {
		template<typename core_t, typename node_t>
		static inline void result( core_t & core, const node_t * & node ) {
			core.previsit( node );
		}
	};

	template<>
	struct __assign<false> {
		template<typename core_t, typename node_t>
		static inline void result( core_t & core, const node_t * & node ) {
			node = core.previsit( node );
			assertf(node, "Previsit must not return NULL");
		}
	};

	/// Used by postvisit implementation
	/// We need to return the result unless the function
	/// returns void, then we just return the original node
	template<bool is_void>
	struct __return;

	template<>
	struct __return<true> {
		template<typename core_t, typename node_t>
		static inline const node_t * result( core_t & core, const node_t * & node ) {
			core.postvisit( node );
			return node;
		}
	};

	template<>
	struct __return<false> {
		template<typename core_t, typename node_t>
		static inline auto result( core_t & core, const node_t * & node ) {
			return core.postvisit( node );
		}
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

		__assign<
			std::is_void<
				decltype( core.previsit( node ) )
			>::value
		>::result( core, node );
	}

	template<typename core_t, typename node_t>
	static inline auto previsit( core_t &, const node_t *, long ) {}

	// PostVisit : never mutates the passed pointer but may return a different node
	template<typename core_t, typename node_t>
	static inline auto postvisit( core_t & core, const node_t * node, int ) ->
		decltype( core.postvisit( node ), node->accept( *(Visitor*)nullptr ) )
	{
		return __return<
			std::is_void<
				decltype( core.postvisit( node ) )
			>::value
		>::result( core, node );
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
	FIELD_PTR( visit_children, __pass::bool_ref )
	FIELD_PTR( at_cleanup, __pass::at_cleanup_t )
	FIELD_PTR( visitor, ast::Pass<core_t> * const )

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
	// If onError() returns false, the error will be ignored. By default, it returns true.

	template< typename core_t >
	static bool on_error (core_t &, ptr<Decl> &, long) { return true; }

	template< typename core_t >
	static auto on_error (core_t & core, ptr<Decl> & decl, int) -> decltype(core.on_error(decl)) {
		return core.on_error(decl);
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
			fwd->params = decl->params;
			core.symtab.addStruct( fwd );
		}

		template<typename core_t>
		static inline void addStructFwd( core_t &, long, const ast::StructDecl * ) {}

		template<typename core_t>
		static inline auto addUnionFwd( core_t & core, int, const ast::UnionDecl * decl ) -> decltype( core.symtab.addUnion( decl ), void() ) {
			UnionDecl * fwd = new UnionDecl( decl->location, decl->name );
			fwd->params = decl->params;
			core.symtab.addUnion( fwd );
		}

		template<typename core_t>
		static inline void addUnionFwd( core_t &, long, const ast::UnionDecl * ) {}

		template<typename core_t>
		static inline auto addStruct( core_t & core, int, const std::string & str ) -> decltype( core.symtab.addStruct( str ), void() ) {
			if ( ! core.symtab.lookupStruct( str ) ) {
				core.symtab.addStruct( str );
			}
		}

		template<typename core_t>
		static inline void addStruct( core_t &, long, const std::string & ) {}

		template<typename core_t>
		static inline auto addUnion( core_t & core, int, const std::string & str ) -> decltype( core.symtab.addUnion( str ), void() ) {
			if ( ! core.symtab.lookupUnion( str ) ) {
				core.symtab.addUnion( str );
			}
		}

		template<typename core_t>
		static inline void addUnion( core_t &, long, const std::string & ) {}

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

	template<typename core_t>
	static inline auto get_result( core_t & core, char ) -> decltype( core.result() ) {
		return core.result();
	}

	template<typename core_t>
	static inline auto get_result( core_t & core, int ) -> decltype( core.result ) {
		return core.result;
	}

	template<typename core_t>
	static inline void get_result( core_t &, long ) {}
} // namespace __pass
} // namespace ast
