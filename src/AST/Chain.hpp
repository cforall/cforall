//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Chain.hpp --
//
// Author           : Thierry Delisle
// Created On       : Wed Jun 05 14:11:52 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "Node.hpp"

namespace ast {

template<typename T>
struct _chain_mutator;

template<typename node_t, Node::ref_type ref_t>
struct _chain_mutator<ptr_base<node_t, ref_t>>;

template<template <class...> class container_t, typename node_t, Node::ref_type ref_t>
struct _chain_mutator<container_t<ptr_base<node_t, ref_t>>>;

template<typename node_t, Node::ref_type ref_t>
struct _chain_mutator<ptr_base<node_t, ref_t>> {
	ptr_base<node_t, ref_t> & base;

	template<typename actual_node_t, typename child_t>
	auto operator()( child_t actual_node_t::*child ) {
		auto n = mutate(base.get());
		actual_node_t * node = strict_dynamic_cast<actual_node_t *>(n);
		base = node;
		return _chain_mutator< typename std::remove_reference< decltype(node->*child) >::type >{node->*child};
	}

	node_t * operator->() {
		auto n = mutate(base.get());
		base = n;
		return n;
	}
};

template<template <class...> class container_t, typename node_t, Node::ref_type ref_t>
struct _chain_mutator<container_t<ptr_base<node_t, ref_t>>> {
	container_t<ptr_base<node_t, ref_t>> & base;

	auto operator[]( size_t i ) {
		return _chain_mutator<ptr_base<node_t, ref_t>>{base[i]};
	}
};


template< typename node_t, Node::ref_type ref_t >
auto chain_mutate( ptr_base<node_t, ref_t> & base ) {
	return _chain_mutator<ptr_base<node_t, ref_t>>{ base };
}

}