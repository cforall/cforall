//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ScopedSet.h --
//
// Author           : Aaron B. Moss
// Created On       : Thu Dec 3 11:51:00 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Jul 22 09:22:17 2017
// Update Count     : 2
//

#pragma once

#include <iterator>
#include <set>
#include <vector>

namespace GenPoly {

/// A set where the items are placed into nested scopes;
/// inserted items are placed into the innermost scope, lookup looks from the innermost scope outward
template<typename Value>
class ScopedSet {
	typedef std::set< Value > Scope;
	typedef std::vector< Scope > ScopeList;

	/// Scoped list of sets.
	ScopeList scopes;
public:
	typedef typename Scope::key_type key_type;
	typedef typename Scope::value_type value_type;
	typedef typename ScopeList::size_type size_type;
	typedef typename ScopeList::difference_type difference_type;
	typedef typename Scope::reference reference;
	typedef typename Scope::const_reference const_reference;
	typedef typename Scope::pointer pointer;
	typedef typename Scope::const_pointer const_pointer;

	// Both iterator types are complete bidirectional iterators, see below.
	class iterator;
	class const_iterator;

	/// Starts a new scope
	void beginScope() {
		scopes.emplace_back();
	}

	/// Ends a scope; invalidates any iterators pointing to elements of that scope
	void endScope() {
		scopes.pop_back();
	}

	/// Default constructor initializes with one scope
	ScopedSet() { beginScope(); }

	iterator begin() { return iterator(scopes, scopes.back().begin(), scopes.size()-1).next_valid(); }
	const_iterator begin() const { return const_iterator(scopes, scopes.back().begin(), scopes.size()-1).next_valid(); }
	const_iterator cbegin() const { return const_iterator(scopes, scopes.back().begin(), scopes.size()-1).next_valid(); }
	iterator end() { return iterator(scopes, scopes[0].end(), 0); }
	const_iterator end() const { return const_iterator(scopes, scopes[0].end(), 0); }
	const_iterator cend() const { return const_iterator(scopes, scopes[0].end(), 0); }

	/// Gets the index of the current scope (counted from 1)
	size_type currentScope() const { return scopes.size(); }

	/// Finds the given key in the outermost scope it occurs; returns end() for none such
	iterator find( const Value &key ) {
		for ( size_type i = scopes.size() - 1; ; --i ) {
			typename Scope::iterator val = scopes[i].find( key );
			if ( val != scopes[i].end() ) return iterator( scopes, val, i );
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator find( const Value &key ) const {
		return const_iterator( const_cast< ScopedSet< Value >* >(this)->find( key ) );
	}

	/// Finds the given key in the outermost scope inside the given scope where it occurs
	iterator findNext( const_iterator &it, const Value &key ) {
		if ( it.i == 0 ) return end();
		for ( size_type i = it.i - 1; ; --i ) {
			typename Scope::iterator val = scopes[i].find( key );
			if ( val != scopes[i].end() ) return iterator( scopes, val, i );
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator findNext( const_iterator &it, const Value &key ) const {
		return const_iterator( const_cast< ScopedSet< Value >* >(this)->findNext( it, key ) );
	}

	/// Inserts the given value into the outermost scope
	std::pair< iterator, bool > insert( const value_type &value ) {
		std::pair< typename Scope::iterator, bool > res = scopes.back().insert( value );
		return std::make_pair( iterator(scopes, res.first, scopes.size()-1), res.second );
	}

	bool contains( const Value & key ) const {
		return find( key ) != cend();
	}
};

template<typename Value>
class ScopedSet<Value>::iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ScopedSet;
	friend class const_iterator;
	typedef typename Scope::iterator wrapped_iterator;
	typedef typename ScopeList::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != (*scopes)[i].end();
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

	iterator(ScopeList const &_scopes, const wrapped_iterator &_it, size_type _i)
		: scopes(&_scopes), it(_it), i(_i) {}
public:
	iterator(const iterator &that) : scopes(that.scopes), it(that.it), i(that.i) {}
	iterator& operator= (const iterator &that) {
		scopes = that.scopes; i = that.i; it = that.it;
		return *this;
	}

	reference operator* () { return *it; }
	pointer operator-> () { return it.operator->(); }

	iterator& operator++ () {
		if ( it == (*scopes)[i].end() ) {
			if ( i == 0 ) return *this;
			--i;
			it = (*scopes)[i].begin();
		} else {
			++it;
		}
		return next_valid();
	}
	iterator operator++ (int) { iterator tmp = *this; ++(*this); return tmp; }

	iterator& operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == (*scopes)[i].begin() ) {
			++i;
			it = (*scopes)[i].end();
		}
		--it;
		return prev_valid();
	}
	iterator operator-- (int) { iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const iterator &that) {
		return scopes == that.scopes && i == that.i && it == that.it;
	}
	bool operator!= (const iterator &that) { return !( *this == that ); }

	size_type get_level() const { return i; }

private:
	ScopeList const *scopes;
	wrapped_iterator it;
	size_type i;
};

template<typename Value>
class ScopedSet<Value>::const_iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ScopedSet;
	typedef typename Scope::iterator wrapped_iterator;
	typedef typename Scope::const_iterator wrapped_const_iterator;
	typedef typename ScopeList::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != (*scopes)[i].end();
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

	const_iterator(ScopeList const &_scopes, const wrapped_const_iterator &_it, size_type _i)
		: scopes(&_scopes), it(_it), i(_i) {}
public:
	const_iterator(const iterator &that) : scopes(that.scopes), it(that.it), i(that.i) {}
	const_iterator(const const_iterator &that) : scopes(that.scopes), it(that.it), i(that.i) {}
	const_iterator& operator= (const iterator &that) {
		scopes = that.scopes; i = that.i; it = that.it;
		return *this;
	}
	const_iterator& operator= (const const_iterator &that) {
		scopes = that.scopes; i = that.i; it = that.it;
		return *this;
	}

	const_reference operator* () { return *it; }
	const_pointer operator-> () { return it.operator->(); }

	const_iterator& operator++ () {
		if ( it == (*scopes)[i].end() ) {
			if ( i == 0 ) return *this;
			--i;
			it = (*scopes)[i].begin();
		} else {
			++it;
		}
		return next_valid();
	}
	const_iterator operator++ (int) { const_iterator tmp = *this; ++(*this); return tmp; }

	const_iterator& operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == (*scopes)[i].begin() ) {
			++i;
			it = (*scopes)[i].end();
		}
		--it;
		return prev_valid();
	}
	const_iterator operator-- (int) { const_iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const const_iterator &that) {
		return scopes == that.scopes && i == that.i && it == that.it;
	}
	bool operator!= (const const_iterator &that) { return !( *this == that ); }

	size_type get_level() const { return i; }

private:
	ScopeList const *scopes;
	wrapped_const_iterator it;
	size_type i;
};

} // namespace GenPoly

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
