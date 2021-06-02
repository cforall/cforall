#pragma once
// IWYU pragma: private, include "PassVisitor.h"

template<typename pass_type>
class PassVisitor;

typedef std::function<void( void * )> cleanup_func_t;
typedef std::function< void( cleanup_func_t, void * ) > at_cleanup_t;

class guard_value_impl {
public:
	guard_value_impl( at_cleanup_t * at_cleanup ) {
		if( at_cleanup ) {
			*at_cleanup = [this]( cleanup_func_t && func, void* val ) {
				push( std::move( func ), val );
			};
		}
	}

	~guard_value_impl() {
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

	std::stack< cleanup_t, std::vector< cleanup_t > > cleanups;
};

class bool_ref {
public:
	bool_ref() = default;
	~bool_ref() = default;

	operator bool() { return m_ref ? *m_ref : true; }
	bool operator=( bool val ) { assert(m_ref); return *m_ref = val; }

private:

	friend class ChildrenGuard;

	bool * set( bool * val ) {
		bool * prev = m_ref;
		m_ref = val;
		return prev;
	}

	bool * m_ref = nullptr;
};

class ChildrenGuard {
public:

	ChildrenGuard( bool_ref * ref )
		: m_val ( true )
		, m_prev( ref ? ref->set( &m_val ) : nullptr )
		, m_ref ( ref )
	{}

	~ChildrenGuard() {
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

//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Deep magic (a.k.a template meta programming) to make the templated visitor work
// Basically the goal is to make 2 previsit_impl
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
// Mutator functions work along the same principal
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Visit
template<typename pass_type, typename node_type>
static inline auto previsit_impl( pass_type& pass, node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.previsit( node ), void() ) {
	pass.previsit( node );
}

template<typename pass_type, typename node_type>
static inline void previsit_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) node_type * node, __attribute__((unused)) long unused ) {}


template<typename pass_type, typename node_type>
static inline auto postvisit_impl( pass_type& pass, node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.postvisit( node ), void() ) {
	pass.postvisit( node );
}

template<typename pass_type, typename node_type>
static inline void postvisit_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) node_type * node, __attribute__((unused)) long unused ) {}

template<typename pass_type, typename node_type>
static inline auto previsit_impl( pass_type& pass, const node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.previsit( node ), void() ) {
	pass.previsit( node );
}

template<typename pass_type, typename node_type>
static inline void previsit_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) const node_type * node, __attribute__((unused)) long unused ) {}


template<typename pass_type, typename node_type>
static inline auto postvisit_impl( pass_type& pass, const node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.postvisit( node ), void() ) {
	pass.postvisit( node );
}

template<typename pass_type, typename node_type>
static inline void postvisit_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) const node_type * node, __attribute__((unused)) long unused ) {}

//---------------------------------------------------------
// Mutate
template<typename pass_type, typename node_type>
static inline auto premutate_impl( pass_type& pass, node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.premutate( node ), void() ) {
	return pass.premutate( node );
}

template<typename pass_type, typename node_type>
static inline void premutate_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) node_type * node, __attribute__((unused)) long unused ) {}


template<typename return_type, typename pass_type, typename node_type>
static inline auto postmutate_impl( pass_type& pass, node_type * node, __attribute__((unused)) int unused ) -> decltype( pass.postmutate( node ) ) {
	return pass.postmutate( node );
}

template<typename return_type, typename pass_type, typename node_type>
static inline return_type postmutate_impl( __attribute__((unused)) pass_type& pass, node_type * node, __attribute__((unused)) long unused ) { return node; }

//---------------------------------------------------------
// Begin/End scope
template<typename pass_type>
static inline auto begin_scope_impl( pass_type& pass, __attribute__((unused)) int unused ) -> decltype( pass.beginScope(), void() ) {
	pass.beginScope();
}

template<typename pass_type>
static inline void begin_scope_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) long unused ) {}


template<typename pass_type>
static inline auto end_scope_impl( pass_type& pass, __attribute__((unused)) int unused ) -> decltype( pass.endScope(), void() ) {
	pass.endScope();
}

template<typename pass_type>
static inline void end_scope_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) long unused ) {}

//---------------------------------------------------------
// Fields
#define FIELD_PTR( type, name )                                                                                                        \
template<typename pass_type>                                                                                                           \
static inline auto name##_impl( pass_type& pass, __attribute__((unused)) int unused ) -> decltype( &pass.name ) { return &pass.name; } \
                                                                                                                                       \
template<typename pass_type>                                                                                                           \
static inline type * name##_impl( __attribute__((unused)) pass_type& pass, __attribute__((unused)) long unused ) { return nullptr;}    \

FIELD_PTR( const TypeSubstitution *, env )
FIELD_PTR( std::list< Statement* >, stmtsToAddBefore )
FIELD_PTR( std::list< Statement* >, stmtsToAddAfter  )
FIELD_PTR( std::list< Declaration* >, declsToAddBefore )
FIELD_PTR( std::list< Declaration* >, declsToAddAfter  )
FIELD_PTR( bool_ref, visit_children )
FIELD_PTR( at_cleanup_t, at_cleanup )
FIELD_PTR( PassVisitor<pass_type> * const, visitor )

#undef FIELD_PTR

//---------------------------------------------------------
// Indexer
template<typename pass_type>
static inline auto indexer_impl_enterScope( pass_type & pass, int ) -> decltype( pass.indexer.enterScope(), void() ) {
	pass.indexer.enterScope();
}

template<typename pass_type>
static inline auto indexer_impl_enterScope( pass_type &, long ) {}

template<typename pass_type>
static inline auto indexer_impl_leaveScope( pass_type & pass, int ) -> decltype( pass.indexer.leaveScope(), void() ) {
	pass.indexer.leaveScope();
}

template<typename pass_type>
static inline auto indexer_impl_leaveScope( pass_type &, long ) {}


#define INDEXER_FUNC1( func, type )                                                                                             \
template<typename pass_type>                                                                                                   \
static inline auto indexer_impl_##func ( pass_type & pass, int, type arg ) -> decltype( pass.indexer.func( arg ), void() ) {   \
	pass.indexer.func( arg );                                                                                                \
}                                                                                                                              \
template<typename pass_type>                                                                                                   \
static inline void indexer_impl_##func ( pass_type &, long, type ) { }

#define INDEXER_FUNC2( func, type1, type2 )                                                                                             \
template<typename pass_type>                                                                                                   \
static inline auto indexer_impl_##func ( pass_type & pass, int, type1 arg1, type2 arg2 ) -> decltype( pass.indexer.func( arg1, arg2 ), void() ) {   \
	pass.indexer.func( arg1, arg2 );                                                                                                \
}                                                                                                                              \
template<typename pass_type>                                                                                                   \
static inline void indexer_impl_##func ( pass_type &, long, type1, type2 ) { }


INDEXER_FUNC1( addId     , const DeclarationWithType *       );
INDEXER_FUNC1( addType   , const NamedTypeDecl *             );
INDEXER_FUNC1( addStruct , const StructDecl *                );
INDEXER_FUNC1( addEnum   , const EnumDecl *                  );
INDEXER_FUNC1( addUnion  , const UnionDecl *                 );
INDEXER_FUNC1( addTrait  , const TraitDecl *                 );
INDEXER_FUNC2( addWith   , const std::list< Expression * > &, const Declaration * );

#undef INDEXER_FUNC1
#undef INDEXER_FUNC2

template<typename pass_type>
static inline auto indexer_impl_addStructFwd( pass_type & pass, int, const StructDecl * decl ) -> decltype( pass.indexer.addStruct( decl ), void() ) {
	StructDecl * fwd = new StructDecl( decl->name );
	cloneAll( decl->parameters, fwd->parameters );
	pass.indexer.addStruct( fwd );
}

template<typename pass_type>
static inline auto indexer_impl_addStructFwd( pass_type &, long, const StructDecl * ) {}

template<typename pass_type>
static inline auto indexer_impl_addUnionFwd( pass_type & pass, int, const UnionDecl * decl ) -> decltype( pass.indexer.addUnion( decl ), void() ) {
	UnionDecl * fwd = new UnionDecl( decl->name );
	cloneAll( decl->parameters, fwd->parameters );
	pass.indexer.addUnion( fwd );
}

template<typename pass_type>
static inline auto indexer_impl_addUnionFwd( pass_type &, long, const UnionDecl * ) {}

template<typename pass_type>
static inline auto indexer_impl_addStruct( pass_type & pass, int, const std::string & str ) -> decltype( pass.indexer.addStruct( str ), void() ) {
	if ( ! pass.indexer.lookupStruct( str ) ) {
		pass.indexer.addStruct( str );
	}
}

template<typename pass_type>
static inline auto indexer_impl_addStruct( pass_type &, long, const std::string & ) {}

template<typename pass_type>
static inline auto indexer_impl_addUnion( pass_type & pass, int, const std::string & str ) -> decltype( pass.indexer.addUnion( str ), void() ) {
	if ( ! pass.indexer.lookupUnion( str ) ) {
		pass.indexer.addUnion( str );
	}
}

template<typename pass_type>
static inline auto indexer_impl_addUnion( pass_type &, long, const std::string & ) {}
