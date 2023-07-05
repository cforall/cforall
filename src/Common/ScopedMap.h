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
// Last Modified On : Tue Feb 15 08:41:28 2022
// Update Count     : 5
//

#pragma once

#include <cassert>
#include <iterator>
#include <map>
#include <utility>
#include <vector>

/// Default (empty) ScopedMap note type
struct EmptyNote {};

/// A map where the items are placed into nested scopes;
/// inserted items are placed into the innermost scope, lookup looks from the innermost scope outward.
/// Scopes may be annotated with a value; the annotation defaults to empty
template<typename Key, typename Value, typename Note = EmptyNote>
class ScopedMap {
	typedef std::map< Key, Value > MapType;
	struct Scope {
		MapType map;
		Note note;

		template<typename N>
		Scope(N && n) : map(), note(std::forward<N>(n)) {}

		Scope() = default;
		Scope(const Scope &) = default;
		Scope(Scope &&) = default;
		Scope & operator= (const Scope &) = default;
		Scope & operator= (Scope &&) = default;
	};
	typedef std::vector< Scope > ScopeList;

	/// Scoped list of maps.
	ScopeList scopes;
public:
	typedef typename MapType::key_type key_type;
	typedef typename MapType::mapped_type mapped_type;
	typedef typename MapType::value_type value_type;
	typedef typename ScopeList::size_type size_type;
	typedef typename ScopeList::difference_type difference_type;
	typedef typename MapType::reference reference;
	typedef typename MapType::const_reference const_reference;
	typedef typename MapType::pointer pointer;
	typedef typename MapType::const_pointer const_pointer;

	// Both iterator types are complete bidrectional iterators, see below.
	class iterator;
	class const_iterator;

	/// Starts a new scope
	void beginScope() {
		scopes.emplace_back();
	}

	// Starts a new scope with the given note
	template<typename N>
	void beginScope( N && n ) {
		scopes.emplace_back( std::forward<N>(n) );
	}

	/// Ends a scope; invalidates any iterators pointing to elements of that scope
	void endScope() {
		scopes.pop_back();
		assert( ! scopes.empty() );
	}

	/// Default constructor initializes with one scope
	ScopedMap() : scopes() { beginScope(); }

	/// Constructs with a given note on the outermost scope
	template<typename N>
	ScopedMap( N && n ) : scopes() { beginScope(std::forward<N>(n)); }

	iterator begin() { return iterator(scopes, scopes.back().map.begin(), currentScope()).next_valid(); }
	const_iterator begin() const { return const_iterator(scopes, scopes.back().map.begin(), currentScope()).next_valid(); }
	const_iterator cbegin() const { return const_iterator(scopes, scopes.back().map.begin(), currentScope()).next_valid(); }
	iterator end() { return iterator(scopes, scopes[0].map.end(), 0); }
	const_iterator end() const { return const_iterator(scopes, scopes[0].map.end(), 0); }
	const_iterator cend() const { return const_iterator(scopes, scopes[0].map.end(), 0); }

	/// Gets the index of the current scope (counted from 1)
	size_type currentScope() const { return scopes.size() - 1; }

	/// Gets the note at the given scope
	Note & getNote() { return scopes.back().note; }
	const Note & getNote() const { return scopes.back().note; }
	Note & getNote( size_type i ) { return scopes[i].note; }
	const Note & getNote( size_type i ) const { return scopes[i].note; }

	/// Finds the given key in the outermost scope it occurs; returns end() for none such
	iterator find( const Key & key ) {
		for ( size_type i = scopes.size() - 1; ; --i ) {
			typename MapType::iterator val = scopes[i].map.find( key );
			if ( val != scopes[i].map.end() ) return iterator( scopes, val, i );
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator find( const Key & key ) const {
			return const_iterator( const_cast< ScopedMap< Key, Value, Note >* >(this)->find( key ) );
	}

	/// Finds the given key in the provided scope; returns end() for none such
	iterator findAt( size_type scope, const Key & key ) {
		typename MapType::iterator val = scopes[scope].map.find( key );
		if ( val != scopes[scope].map.end() ) return iterator( scopes, val, scope );
		return end();
	}
	const_iterator findAt( size_type scope, const Key & key ) const {
		return const_iterator( const_cast< ScopedMap< Key, Value, Note >* >(this)->findAt( scope, key ) );
	}

	/// Finds the given key in the outermost scope inside the given scope where it occurs
	iterator findNext( const_iterator & it, const Key & key ) {
		if ( it.level == 0 ) return end();
		for ( size_type i = it.level - 1; ; --i ) {
			typename MapType::iterator val = scopes[i].map.find( key );
			if ( val != scopes[i].map.end() ) return iterator( scopes, val, i );
			if ( i == 0 ) break;
		}
		return end();
	}
	const_iterator findNext( const_iterator & it, const Key & key ) const {
			return const_iterator( const_cast< ScopedMap< Key, Value, Note >* >(this)->findNext( it, key ) );
	}

	/// Inserts the given key-value pair into the outermost scope
	template< typename value_type_t >
	std::pair< iterator, bool > insert( value_type_t && value ) {
		std::pair< typename MapType::iterator, bool > res = scopes.back().map.insert( std::forward<value_type_t>( value ) );
		return std::make_pair( iterator(scopes, std::move( res.first ), scopes.size()-1), std::move( res.second ) );
	}

	template< typename value_t >
	std::pair< iterator, bool > insert( const Key & key, value_t && value ) { return insert( std::make_pair( key, std::forward<value_t>( value ) ) ); }

	template< typename value_type_t >
	std::pair< iterator, bool > insertAt( size_type scope, value_type_t && value ) {
		std::pair< typename MapType::iterator, bool > res = scopes.at(scope).map.insert( std::forward<value_type_t>( value ) );
		return std::make_pair( iterator(scopes, std::move( res.first ), scope), std::move( res.second ) );
	}

	template< typename value_t >
	std::pair< iterator, bool > insertAt( size_type scope, const Key & key, value_t && value ) {
		return insertAt( scope, std::make_pair( key, std::forward<value_t>( value ) ) );
	}

	Value & operator[] ( const Key & key ) {
		iterator slot = find( key );
		if ( slot != end() ) return slot->second;
		return insert( key, Value() ).first->second;
	}

	/// Erases element with key in the innermost scope that has it.
	size_type erase( const Key & key ) {
		for ( auto it = scopes.rbegin() ; it != scopes.rend() ; ++it ) {
			size_type i = it->map.erase( key );
			if ( 0 != i ) return i;
		}
		return 0;
	}

	size_type count( const Key & key ) const {
		size_type c = 0;
		auto it = find( key );
		auto end = cend();

		while(it != end) {
			c++;
			it = findNext(it, key);
		}

		return c;
	}

	bool contains( const Key & key ) const {
		return find( key ) != cend();
	}
};

template<typename Key, typename Value, typename Note>
class ScopedMap<Key, Value, Note>::iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ScopedMap;
	friend class const_iterator;
	typedef typename MapType::iterator wrapped_iterator;
	typedef typename ScopeList::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != (*scopes)[level].map.end();
	}

	/// Increments on invalid
	iterator & next_valid() {
		if ( ! is_valid() ) { ++(*this); }
		return *this;
	}

	/// Decrements on invalid
	iterator & prev_valid() {
		if ( ! is_valid() ) { --(*this); }
		return *this;
	}

	iterator(ScopeList & _scopes, const wrapped_iterator & _it, size_type inLevel)
		: scopes(&_scopes), it(_it), level(inLevel) {}
public:
	iterator(const iterator & that) : scopes(that.scopes), it(that.it), level(that.level) {}
	iterator & operator= (const iterator & that) {
		scopes = that.scopes; level = that.level; it = that.it;
		return *this;
	}

	reference operator* () { return *it; }
	pointer operator-> () const { return it.operator->(); }

	iterator & operator++ () {
		if ( it == (*scopes)[level].map.end() ) {
			if ( level == 0 ) return *this;
			--level;
			it = (*scopes)[level].map.begin();
		} else {
			++it;
		}
		return next_valid();
	}
	iterator operator++ (int) { iterator tmp = *this; ++(*this); return tmp; }

	iterator & operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == (*scopes)[level].map.begin() ) {
			++level;
			it = (*scopes)[level].map.end();
		}
		--it;
		return prev_valid();
	}
	iterator operator-- (int) { iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const iterator & that) const {
		return scopes == that.scopes && level == that.level && it == that.it;
	}
	bool operator!= (const iterator & that) const { return !( *this == that ); }

	size_type get_level() const { return level; }

	Note & get_note() { return (*scopes)[level].note; }
	const Note & get_note() const { return (*scopes)[level].note; }

private:
	ScopeList *scopes;
	wrapped_iterator it;
	size_type level;
};

template<typename Key, typename Value, typename Note>
class ScopedMap<Key, Value, Note>::const_iterator :
		public std::iterator< std::bidirectional_iterator_tag, value_type > {
	friend class ScopedMap;
	typedef typename ScopedMap::MapType::iterator wrapped_iterator;
	typedef typename ScopedMap::MapType::const_iterator wrapped_const_iterator;
	typedef typename ScopedMap::ScopeList scope_list;
	typedef typename scope_list::size_type size_type;

	/// Checks if this iterator points to a valid item
	bool is_valid() const {
		return it != (*scopes)[level].map.end();
	}

	/// Increments on invalid
	const_iterator & next_valid() {
		if ( ! is_valid() ) { ++(*this); }
		return *this;
	}

	/// Decrements on invalid
	const_iterator & prev_valid() {
		if ( ! is_valid() ) { --(*this); }
		return *this;
	}

	const_iterator(scope_list const & _scopes, const wrapped_const_iterator & _it, size_type inLevel)
		: scopes(&_scopes), it(_it), level(inLevel) {}
public:
	const_iterator(const iterator & that) : scopes(that.scopes), it(that.it), level(that.level) {}
	const_iterator(const const_iterator & that) : scopes(that.scopes), it(that.it), level(that.level) {}
	const_iterator & operator= (const iterator & that) {
		scopes = that.scopes; level = that.level; it = that.it;
		return *this;
	}
	const_iterator & operator= (const const_iterator & that) {
		scopes = that.scopes; level = that.level; it = that.it;
		return *this;
	}

	const_reference operator* () { return *it; }
	const_pointer operator-> () { return it.operator->(); }

	const_iterator & operator++ () {
		if ( it == (*scopes)[level].map.end() ) {
			if ( level == 0 ) return *this;
			--level;
			it = (*scopes)[level].map.begin();
		} else {
			++it;
		}
		return next_valid();
	}
	const_iterator operator++ (int) { const_iterator tmp = *this; ++(*this); return tmp; }

	const_iterator & operator-- () {
		// may fail if this is the begin iterator; allowed by STL spec
		if ( it == (*scopes)[level].map.begin() ) {
			++level;
			it = (*scopes)[level].map.end();
		}
		--it;
		return prev_valid();
	}
	const_iterator operator-- (int) { const_iterator tmp = *this; --(*this); return tmp; }

	bool operator== (const const_iterator & that) const {
		return scopes == that.scopes && level == that.level && it == that.it;
	}
	bool operator!= (const const_iterator & that) const { return !( *this == that ); }

	size_type get_level() const { return level; }

	const Note & get_note() const { return (*scopes)[level].note; }

private:
	scope_list const *scopes;
	wrapped_const_iterator it;
	size_type level;
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
