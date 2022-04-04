//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Node.hpp --
//
// Author           : Thierry Delisle
// Created On       : Wed May 8 10:27:04 2019
// Last Modified By : Andrew Beach
// Last Modified On : Fri Mar 25 10:33:00 2022
// Update Count     : 7
//

#pragma once

#include <cassert>
#include <cstddef>     // for nullptr_t
#include <iosfwd>
#include <type_traits> // for remove_reference

#include "Common/ErrorObjects.h"  // for SemanticErrorException

namespace ast {

class Visitor;

/// Base class for all AST nodes.
/// Keeps both strong and weak reference counts.
class Node {
public:
	// override defaults to ensure assignment doesn't
	// change/share reference counts
	Node() = default;
	Node(const Node&) : strong_count(0), weak_count(0) {}
	Node(Node&&) : strong_count(0), weak_count(0) {}
	Node& operator= (const Node&) = delete;
	Node& operator= (Node&&) = delete;
	virtual ~Node() {}

	virtual const Node * accept( Visitor & v ) const = 0;

	/// Types of node references
	enum class ref_type {
		strong,
		weak
	};

	bool unique() const { return strong_count == 1; }
	bool isManaged() const {return strong_count > 0; }

private:
	/// Make a copy of this node; should be overridden in subclass with more precise return type
	virtual Node * clone() const = 0;

	/// Must be copied in ALL derived classes
	template<typename node_t>
	friend node_t * mutate(const node_t * node);
	template<typename node_t>
	friend node_t * shallowCopy(const node_t * node);

	mutable size_t strong_count = 0;
	mutable size_t weak_count = 0;
	mutable bool was_ever_strong = false;

	void increment(ref_type ref) const {
		switch (ref) {
			case ref_type::strong: strong_count++; was_ever_strong = true; break;
			case ref_type::weak  : weak_count  ++; break;
		}
	}

	void decrement(ast::Node::ref_type ref, bool do_delete = true) const {
		switch (ref) {
			case ref_type::strong: strong_count--; break;
			case ref_type::weak  : weak_count  --; break;
		}

		if( do_delete && !strong_count && !weak_count) {
			delete this;
		}
	}

	template< typename node_t, enum Node::ref_type ref_t >
	friend class ptr_base;
};

/// Mutate a node, non-member function to avoid static type
/// problems and be able to use auto return
template<typename node_t>
node_t * mutate( const node_t * node ) {
	if (node->strong_count <= 1) {
		return const_cast<node_t *>(node);
	}

	assertf(
		node->weak_count == 0,
		"Error: mutating node with weak references to it will invalidate some references"
	);
	return node->clone();
}

/// Mutate a node field (only clones if not equal to existing value)
template<typename node_t, typename super_t, typename field_t, typename assn_t>
const node_t * mutate_field( const node_t * node, field_t super_t::* field, assn_t && val ) {
	// skip mutate if equivalent
	if ( node->*field == val ) return node;

	// mutate and return
	node_t * ret = mutate( node );
	ret->*field = std::forward< assn_t >( val );
	return ret;
}

/// Mutate a single index of a node field (only clones if not equal to existing value)
template<typename node_t, typename super_t, typename coll_t, typename ind_t, typename field_t>
const node_t * mutate_field_index(
	const node_t * node, coll_t super_t::* field, ind_t i, field_t && val
) {
	// skip mutate if equivalent
	if  ( (node->*field)[i] == val ) return node;

	// mutate and return
	node_t * ret = mutate( node );
	(ret->*field)[i] = std::forward< field_t >( val );
	return ret;
}

/// Mutate an entire indexed collection by cloning to accepted value
template<typename node_t, typename super_t, typename coll_t>
const node_t * mutate_each( const node_t * node, coll_t super_t::* field, Visitor & v ) {
	for ( unsigned i = 0; i < (node->*field).size(); ++i ) {
		node = mutate_field_index( node, field, i, (node->*field)[i]->accept( v ) );
	}
	return node;
}

std::ostream& operator<< ( std::ostream& out, const Node * node );

/// Call a visitor on a possibly-null node
template<typename node_t>
auto maybe_accept( const node_t * n, Visitor & v ) -> decltype( n->accept(v) ) {
	return n ? n->accept( v ) : nullptr;
}

/// Call a visitor on a collection of nodes, throwing any exceptions when completed
template< typename Container >
void accept_each( const Container & c, Visitor & v ) {
	SemanticErrorException errors;
	for ( const auto & i : c ) {
		try {
			if ( i ) {
				i->accept( v );
			}
		} catch ( SemanticErrorException & e ) {
			errors.append( e );
		}
	}
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

/// Base class for the smart pointer types
/// should never really be used.
template< typename node_t, enum Node::ref_type ref_t >
class ptr_base {
public:
	ptr_base() : node(nullptr) {}
	ptr_base( const node_t * n ) : node(n) { if( node ) _inc(node); }
	~ptr_base() { if( node ) { auto tmp = node; node = nullptr; _dec(tmp); } }

	ptr_base( const ptr_base & o ) : node(o.node) {
		if( node ) _inc(node);
	}

	ptr_base( ptr_base && o ) : node(o.node) { o.node = nullptr; }

	template< enum Node::ref_type o_ref_t >
	ptr_base( const ptr_base<node_t, o_ref_t> & o ) : node(o.get()) {
		if( node ) _inc(node);
	}

	template< enum Node::ref_type o_ref_t >
	ptr_base( ptr_base<node_t, o_ref_t> && o ) : node(o.get()) {
		if( node ) _inc(node);
	}

	ptr_base & operator=( const node_t * node ) {
		assign( node );
		return *this;
	}

	template<typename o_node_t>
	ptr_base & operator=( const o_node_t * node ) {
		assign( strict_dynamic_cast<const node_t *, nullptr>(node) );
		return *this;
	}

	ptr_base & operator=( std::nullptr_t ) {
		if ( node ) _dec(node);
		node = nullptr;
		return *this;
	}

	ptr_base & operator=( const ptr_base & o ) {
		assign(o.node);
		return *this;
	}

	ptr_base & operator=( ptr_base && o ) {
		if ( node == o.node ) return *this;
		if ( node ) _dec(node);
		node = o.node;
		o.node = nullptr;
		return *this;
	}

	template< enum Node::ref_type o_ref_t >
	ptr_base & operator=( const ptr_base<node_t, o_ref_t> & o ) {
		assign(o.get());
		return *this;
	}

	template< enum Node::ref_type o_ref_t >
	ptr_base & operator=( ptr_base<node_t, o_ref_t> && o ) {
		assign(o.get());
		return *this;
	}

	/// Swaps the nodes contained within two pointers.
	void swap( ptr_base & other ) noexcept;

	const node_t * get() const { _check(); return  node; }
	const node_t * operator->() const { _check(); return  node; }
	const node_t & operator* () const { _check(); return *node; }
	explicit operator bool() const { _check(); return node; }
	operator const node_t * () const & { _check(); return node; }
	operator const node_t * () && = delete;

	const node_t * release() {
		const node_t * ret = node;
		if ( node ) {
			_dec(node, false);
			node = nullptr;
		}
		return ret;
	}

	/// wrapper for convenient access to dynamic_cast
	template<typename o_node_t>
	const o_node_t * as() const { _check(); return dynamic_cast<const o_node_t *>(node); }

	/// Wrapper that makes sure dynamic_cast returns non-null.
	template<typename o_node_t>
	const o_node_t * strict_as() const {
		if (const o_node_t * ret = as<o_node_t>()) return ret;
		_strict_fail();
	}

	/// Wrapper that makes sure dynamic_cast does not fail.
	template<typename o_node_t, decltype(nullptr) null>
	const o_node_t * strict_as() const { return node ? strict_as<o_node_t>() : nullptr; }

	/// Returns a mutable version of the pointer in this node.
	node_t * get_and_mutate();

	/// Sets this pointer to a mutated version of a pointer (possibly) owned elsehere.
	/// Returns a mutable version of the pointer in this node.
	node_t * set_and_mutate( const node_t * n );

	using ptr = const node_t *;

private:
	void assign( const node_t * other ) {
		if( other ) _inc(other);
		if( node  ) _dec(node );
		node = other;
	}

	void _inc( const node_t * other );
	void _dec( const node_t * other, bool do_delete = true );
	void _check() const;
	void _strict_fail() const __attribute__((noreturn));

	const node_t * node;
};

/// Owning pointer to node
template< typename node_t >
using ptr = ptr_base< node_t, Node::ref_type::strong >;

/// Observing pointer to node
template< typename node_t >
using readonly = ptr_base< node_t, Node::ref_type::weak >;

/// Non-member swap that an participate in overload resolution.
template< typename node_t, enum Node::ref_type ref_t >
void swap( ptr_base< node_t, ref_t > & l, ptr_base< node_t, ref_t > & r ) {
	l.swap( r );
}

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
