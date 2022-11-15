//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ScopedMap.h --
//
// Author           : Aaron B. Moss
// Created On       : Wed Dec 2 11:37:00 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:23:24 2017
// Update Count     : 2
//

#pragma once

#include <cassert>
#include <iterator>
#include <map>
#include <utility>
#include <vector>

namespace GenPoly {

/// A map where the items are placed into nested scopes.
/// Inserted items are placed into the innermost scope, lookup looks from the
/// innermost scope outward. Erasing a key means that find() will no longer
/// report any instance of the key in a scope further out, but the erasure
/// itself is scoped. Key erasure works by inserting a sentinal value into
/// the value field, and thus only works for Value types where a meaningful
/// sentinal can be chosen.
template<typename Key, typename Value>
class ErasableScopedMap {
	typedef std::map< Key, Value > Scope;
	typedef std::vector< Scope > ScopeList;

	/// Scoped list of maps.
	ScopeList scopes;
	/// Sentinal value for erased keys.
	Value erased;
public:
	typedef typename Scope::key_type key_type;
	typedef typename Scope::mapped_type mapped_type;
	typedef typename Scope::value_type value_type;
	typedef typename ScopeList::size_type size_type;
	typedef typename ScopeList::difference_type difference_type;
	typedef typename Scope::reference reference;
	typedef typename Scope::const_reference const_reference;
	typedef typename Scope::pointer pointer;
	typedef typename Scope::const_pointer const_pointer;

	// Both iterator types are complete bidirection iterators, defined below.
	class iterator;
	class const_iterator;

	/// Starts a new scope
	void beginScope() {
		Scope scope;
		scopes.push_back(scope);
	}

	/// Ends a scope; invalidates any iterators pointing to elements of that scope
	void endScope() {
		scopes.pop_back();
		assert( ! scopes.empty() );
	}

	/// Default constructor initializes with one scope
	ErasableScopedMap( const Value &erased_ ) : erased( erased_ ) { beginScope(); }

	iterator begin() { return iterator(*this, scopes.back().begin(), scopes.size()-1).next_valid(); }
	const_iterator begin() const { return const_iterator(*this, scopes.back().begin(), scopes.size()-1).next_valid(); }
	const_iterator cbegin() const { return const_iterator(*this, scopes.back().begin(), scopes.size()-1).next_valid(); }
	iterator end() { return iterator(*this, scopes[0].end(), 0); }
	const_iterator end() const { return const_iterator(*this, scopes[0].end(), 0); }
	const_iterator cend() const { return const_iterator(*this, scopes[0].end(), 0); }

	/// Gets the index of the current scope (counted from 1)
	size_type currentScope() const { return scopes.size(); }

	/// Finds the given key in the outermost scope it occurs; returns end() for none such
	iterator find( const Key &key ) {
		for ( size_type i = scopes.size() - 1; ; --i ) {
			typename Scope::iterator val = scopes[i].find( key );
			if ( val != scopes[i].end() ) {
				return val->second == erased ? end() : iterator( *this, val, i );
			}
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator find( const Key &key ) const {
		return const_iterator( const_cast< ErasableScopedMap< Key, Value >* >(this)->find( key ) );
	}

	/// Finds the given key in the outermost scope inside the given scope where it occurs
	iterator findNext( const_iterator &it, const Key &key ) {
		if ( it.i == 0 ) return end();
		for ( size_type i = it.i - 1; ; --i ) {
			typename Scope::iterator val = scopes[i].find( key );
			if ( val != scopes[i].end() ) {
				return val->second == erased ? end() : iterator( *this, val, i );
			}
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator findNext( const_iterator &it, const Key &key ) const {
		return const_iterator( const_cast< ErasableScopedMap< Key, Value >* >(this)->findNext( it, key ) );
	}

	/// Inserts the given key-value pair into the outermost scope
	std::pair< iterator, bool > insert( const value_type &value ) {
		std::pair< typename Scope::iterator, bool > res = scopes.back().insert( value );
		return std::make_pair( iterator(*this, res.first, scopes.size()-1), res.second );
	}
	std::pair< iterator, bool > insert( const Key &key, const Value &value ) { return insert( std::make_pair( key, value ) ); }

	/// Marks the given element as erased from this scope inward; returns 1 for erased an element, 0 otherwise
	size_type erase( const Key &key ) {
		typename Scope::iterator val = scopes.back().find( key );
		if ( val != scopes.back().end() ) {
			val->second = erased;
			return 1;
		} else {
			scopes.back().insert( val, std::make_pair( key, erased ) );
			return 0;
		}
	}

	Value& operator[] ( const Key &key ) {
		iterator slot = find( key );
		if ( slot != end() ) return slot->second;
		return insert( key, Value() ).first->second;
	}
};

template<typename Key, typename Value>
class ErasableScopedMap<Key, Value>::iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ErasableScopedMap;
	typedef typename std::map< Key, Value >::iterator wrapped_iterator;
	typedef typename std::vector< std::map< Key, Value > > scope_list;
	typedef typename scope_list::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != map->scopes[i].end() && it->second != map->erased;
	}

	/// Increments on invalid
	iterator& next_valid() {
		if ( ! is_valid() ) { ++(*this); }
		return *this;
	}

	/// Decrements on invalid
	iterator& prev_valid() {
		if ( ! is_valid() ) { --(*this); }
		return *this;
	}

	iterator(ErasableScopedMap< Key, Value > const &_map, const wrapped_iterator &_it, size_type _i)
			: map(&_map), it(_it), i(_i) {}

public:
	iterator(const iterator &that) : map(that.map), it(that.it), i(that.i) {}
	iterator& operator= (const iterator &that) {
		map = that.map; i = that.i; it = that.it;
		return *this;
	}

	reference operator* () { return *it; }
	pointer operator-> () { return it.operator->(); }

	iterator& operator++ () {
		if ( it == map->scopes[i].end() ) {
			if ( i == 0 ) return *this;
			--i;
			it = map->scopes[i].begin();
		} else {
			++it;
		}
		return next_valid();
	}

	iterator& operator++ (int) { iterator tmp = *this; ++(*this); return tmp; }

	iterator& operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == map->scopes[i].begin() ) {
			++i;
			it = map->scopes[i].end();
		}
		--it;
		return prev_valid();
	}
	iterator& operator-- (int) { iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const iterator &that) {
		return map == that.map && i == that.i && it == that.it;
	}
	bool operator!= (const iterator &that) { return !( *this == that ); }

private:
	ErasableScopedMap< Key, Value > const *map;
	wrapped_iterator it;
	size_type i;
};

template<typename Key, typename Value>
class ErasableScopedMap<Key, Value>::const_iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ErasableScopedMap;
	typedef typename std::map< Key, Value >::iterator wrapped_iterator;
	typedef typename std::map< Key, Value >::const_iterator wrapped_const_iterator;
	typedef typename std::vector< std::map< Key, Value > > scope_list;
	typedef typename scope_list::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != map->scopes[i].end() && it->second != map->erased;
	}

	/// Increments on invalid
	const_iterator& next_valid() {
		if ( ! is_valid() ) { ++(*this); }
		return *this;
	}

	/// Decrements on invalid
	const_iterator& prev_valid() {
		if ( ! is_valid() ) { --(*this); }
		return *this;
	}

	const_iterator(ErasableScopedMap< Key, Value > const &_map, const wrapped_const_iterator &_it, size_type _i)
			: map(&_map), it(_it), i(_i) {}
public:
	const_iterator(const iterator &that) : map(that.map), it(that.it), i(that.i) {}
	const_iterator(const const_iterator &that) : map(that.map), it(that.it), i(that.i) {}
	const_iterator& operator= (const iterator &that) {
		map = that.map; i = that.i; it = that.it;
		return *this;
	}
	const_iterator& operator= (const const_iterator &that) {
		map = that.map; i = that.i; it = that.it;
		return *this;
	}

	const_reference operator* () { return *it; }
	const_pointer operator-> () { return it.operator->(); }

	const_iterator& operator++ () {
		if ( it == map->scopes[i].end() ) {
			if ( i == 0 ) return *this;
			--i;
			it = map->scopes[i].begin();
		} else {
			++it;
		}
		return next_valid();
	}
	const_iterator& operator++ (int) { const_iterator tmp = *this; ++(*this); return tmp; }

	const_iterator& operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == map->scopes[i].begin() ) {
			++i;
			it = map->scopes[i].end();
		}
		--it;
		return prev_valid();
	}
	const_iterator& operator-- (int) { const_iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const const_iterator &that) {
		return map == that.map && i == that.i && it == that.it;
	}
	bool operator!= (const const_iterator &that) { return !( *this == that ); }

private:
	ErasableScopedMap< Key, Value > const *map;
	wrapped_const_iterator it;
	size_type i;
};

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
