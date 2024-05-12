//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// PersistentMap.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Thu Mar  7 15:50:00 2019
// Last Modified By : Aaron B. Moss
// Last Modified On : Thu Mar  7 15:50:00 2019
// Update Count     : 1
//

#pragma once

#include <cassert>        // for assertf
#include <cstddef>        // for size_t
#include <functional>     // for hash, equal_to
#include <memory>         // for shared_ptr, enable_shared_from_this, make_shared
#include <unordered_map>  // for unordered_map
#include <utility>        // for forward, move

/// Wraps a hash table in a persistent data structure, using a technique based
/// on the persistent array in Conchon & Filliatre "A Persistent Union-Find
/// Data Structure"

template<typename Key, typename Val,
		typename Hash = std::hash<Key>, typename Eq = std::equal_to<Key>>
class PersistentMap
	: public std::enable_shared_from_this<PersistentMap<Key, Val, Hash, Eq>> {
public:
	/// Type of this class
	using Self = PersistentMap<Key, Val, Hash, Eq>;
	/// Type of pointer to this class
	using Ptr = std::shared_ptr<Self>;

	/// Types of version nodes
	enum Mode {
		BASE,  ///< Root node of version tree
		REM,   ///< Key removal node
		INS,   ///< Key update node
		UPD    ///< Key update node
	};

private:
	using Base = std::unordered_map<Key, Val, Hash, Eq>;

	/// Insertion/update node
	struct Ins {
		Ptr base;  ///< Modified map
		Key key;   ///< Key inserted
		Val val;   ///< Value stored

		template<typename P, typename K, typename V>
		Ins(P&& p, K&& k, V&& v)
		: base(std::forward<P>(p)), key(std::forward<K>(k)), val(std::forward<V>(v)) {}
	};

	/// Removal node
	struct Rem {
		Ptr base;  ///< Modified map
		Key key;   ///< Key removed

		template<typename P, typename K>
		Rem(P&& p, K&& k) : base(std::forward<P>(p)), key(std::forward<K>(k)) {}
	};

	/// Underlying storage
	union Data {
		char def;
		Base base;
		Ins ins;
		Rem rem;

		Data() : def('\0') {}
		~Data() {}
	} data;

	/// Type of node
	mutable Mode mode;

	/// get mutable reference as T
	template<typename T>
	T& as() { return reinterpret_cast<T&>(data); }

	/// get const reference as T
	template<typename T>
	const T& as() const { return reinterpret_cast<const T&>(data); }

	/// get rvalue reference as T
	template<typename T>
	T&& take_as() { return std::move(as<T>()); }

	/// initialize as T
	template<typename T, typename... Args>
	void init( Args&&... args ) {
		new( &as<T>() ) T { std::forward<Args>(args)... };
	}

	/// reset as current mode
	void reset() {
		switch( mode ) {
			case BASE:          as<Base>().~Base(); break;
			case REM:           as<Rem>().~Rem();   break;
			case INS: case UPD: as<Ins>().~Ins();   break;
		}
	}

	/// reset as base
	void reset_as_base() {
		as<Base>().~Base();
	}

public:
	using key_type = typename Base::key_type;
	using mapped_type = typename Base::mapped_type;
	using value_type = typename Base::value_type;
	using size_type = typename Base::size_type;
	using difference_type = typename Base::difference_type;
	using iterator = typename Base::const_iterator;

	PersistentMap() : data(), mode(BASE) { init<Base>(); }

	PersistentMap( Base&& b ) : data(), mode(BASE) { init<Base>(std::move(b)); }

	PersistentMap( const Self& o ) = delete;

	Self& operator= ( const Self& o ) = delete;

	~PersistentMap() { reset(); }

	/// Create a pointer to a new, empty persistent map
	static Ptr new_ptr() { return std::make_shared<Self>(); }

	/// reroot persistent map at current node
	void reroot() const {
		// recursive base case
		if ( mode == BASE ) return;

		// reroot base
		Self* mut_this = const_cast<Self*>(this);
		Ptr base = ( mode == REM ) ? mut_this->as<Rem>().base : mut_this->as<Ins>().base;
		base->reroot();

		// remove map from base
		Base base_map = base->template take_as<Base>();
		base->reset_as_base();

		// switch base to inverse of self and mutate base map
		switch ( mode ) {
			case REM: {
				Rem& self = mut_this->as<Rem>();
				auto it = base_map.find( self.key );

				base->template init<Ins>(
						mut_this->shared_from_this(), std::move(self.key), std::move(it->second) );
				base->mode = INS;

				base_map.erase( it );
				break;
			}
			case INS: {
				Ins& self = mut_this->as<Ins>();

				base->template init<Rem>( mut_this->shared_from_this(), self.key );
				base->mode = REM;

				base_map.emplace( std::move(self.key), std::move(self.val) );
				break;
			}
			case UPD: {
				Ins& self = mut_this->as<Ins>();
				auto it = base_map.find( self.key );

				base->template init<Ins>(
						mut_this->shared_from_this(), std::move(self.key), std::move(it->second) );
				base->mode = UPD;

				it->second = std::move(self.val);
				break;
			}
			case BASE: assertf(false, "unreachable"); break;
		}

		// set base map into self
		mut_this->reset();
		mut_this->init<Base>( std::move(base_map) );
		mode = BASE;
	}

private:
	/// the base after rerooting at the current node
	const Base& rerooted() const {
		reroot();
		return as<Base>();
	}

public:
	/// true iff the map is empty
	bool empty() const { return rerooted().empty(); }

	/// number of entries in map
	size_type size() const { return rerooted().size(); }

	/// begin iterator for map; may be invalidated by calls to non-iteration functions
	/// or functions on other maps in the same tree
	iterator begin() const { return rerooted().begin(); }

	/// end iterator for map; may be invalidated by calls to non-iteration functions
	/// or functions on other maps in the same tree
	iterator end() const { return rerooted().end(); }

	/// underlying map iterator for value
	iterator find(const Key& k) const { return rerooted().find( k ); }

	/// check if value is present
	size_type count(const Key& k) const { return rerooted().count( k ); }

	/// get value; undefined behaviour if not present
	const Val& get(const Key& k) const {
		const Base& self = rerooted();
		auto it = self.find( k );
		return it->second;
	}

	/// get value; returns default if not present
	template<typename V>
	Val get_or_default(const Key& k, V&& d) const {
		const Base& self = rerooted();
		auto it = self.find( k );
		if ( it == self.end() ) return d;
		else return it->second;
	}

	/// set value, storing new map in output variable
	template<typename K, typename V>
	Ptr set(K&& k, V&& v) {
		reroot();

		// transfer map to new node
		Ptr ret = std::make_shared<Self>( take_as<Base>() );
		reset_as_base();
		Base& base_map = ret->template as<Base>();

		// check if this is update or insert
		auto it = base_map.find( k );
		if ( it == base_map.end() ) {
			// set self to REM node and insert into base
			init<Rem>( ret, k );
			mode = REM;

			base_map.emplace_hint( it, std::forward<K>(k), std::forward<V>(v) );
		} else {
			// set self to UPD node and modify base
			init<Ins>( ret, std::forward<K>(k), std::move(it->second) );
			mode = UPD;

			it->second = std::forward<V>(v);
		}

		return ret;
	}

	/// remove value, storing new map in output variable; does nothing if key not in map
	Ptr erase(const Key& k) {
		reroot();

		// exit early if key does not exist in map
		if ( ! as<Base>().count( k ) ) return this->shared_from_this();

		// transfer map to new node
		Ptr ret = std::make_shared<Self>( take_as<Base>() );
		reset_as_base();
		Base& base_map = ret->template as<Base>();

		// set self to INS node and remove from base
		init<Ins>( ret, k, base_map[k] );
		mode = INS;

		base_map.erase( k );

		return ret;
	}
};

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
