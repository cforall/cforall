//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// VectorMap.h --
//
// Author           : Aaron B. Moss
// Created On       : Wed Feb  1 16:55:00 2017
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Jul 21 22:19:29 2017
// Update Count     : 2
//

#pragma once

#include <iterator>
#include <utility>
#include <vector>

/// Maps integers from a contiguous range to T's
template<typename T>
class VectorMap {
	std::vector<T> data;

public:
	typedef typename std::vector<T>::size_type size_type;
	typedef size_type key_type;
	typedef T mapped_type;
	typedef std::pair<size_type, T&> value_type;
	typedef std::pair<size_type, const T&> const_value_type;
	typedef typename std::vector<T>::difference_type difference_type;
	typedef const value_type& reference;
	typedef const const_value_type& const_reference;
	typedef const value_type* pointer;
	typedef const const_value_type* const_pointer;
	
	class iterator : public std::iterator< std::random_access_iterator_tag,
	                                       value_type,
										   difference_type,
										   pointer,
										   reference > {
	friend class VectorMap;
	friend class const_iterator;
	
		value_type data;

		iterator(size_type i, std::vector<T>& v) : data(i, v[i]) {}
	public:
		iterator(const iterator& that) : data(that.data) {}
		iterator& operator= (const iterator& that) {
			new(&data) value_type{ that.data };
			return *this;
		}

		reference operator* () { return data; }
		pointer operator-> () { return &data; }

		iterator& operator++ () {
			// SHENANIGANS: relies on pair<unsigned, T&> having a trivial destructor and
			// vector<T> having contiguous layout
			new(&data) value_type{ (data.first + 1), *(&data.second + 1) };
			return *this;
		}
		iterator operator++ (int) { iterator tmp = *this; ++(*this); return tmp; }

		iterator& operator-- () {
			// SHENANIGANS: same reasons as operator++
			new(&data) value_type{ (data.first - 1), *(&data.second - 1) };
			return *this;
		}
		iterator operator-- (int) { iterator tmp = *this; --(*this); return tmp; }

		iterator& operator+= (difference_type i) {
			// SHENANIGANS: same reasons as operator++
			new(&data) value_type{ (data.first + i), *(&data.second + i) };
			return *this;
		}

		iterator operator+ (difference_type i) const { iterator tmp = *this; return tmp += i; }

		iterator& operator-= (difference_type i) {
			// SHENANIGANS: same reasons as operator++
			new(&data) value_type{ (data.first - i), *(&data.second - i) };
			return *this;
		}

		iterator operator- (difference_type i) const { iterator tmp = *this; return tmp -= i; }

		difference_type operator- (const iterator& o) const { return data.first - o.data.first; }

		value_type operator[] (difference_type i) const {
			// SHENANIGANS: same reasons as operator++
			return value_type{ (data.first + i), *(&data.second + i) };
		}

		bool operator== (const iterator& o) const {
			return data.first == o.data.first && &data.second == &o.data.second;
		}
		
		bool operator!= (const iterator& that) const { return !(*this == that); }

		bool operator< (const iterator& o) const { return data.first < o.data.first; }

		bool operator> (const iterator& o) const { return data.first > o.data.first; }

		bool operator<= (const iterator& o) const { return data.first <= o.data.first; }

		bool operator>= (const iterator& o) const { return data.first >= o.data.first; }
	};

	class const_iterator : public std::iterator< std::bidirectional_iterator_tag,
	                                             const_value_type,
												  difference_type,
												  const_pointer,
												  const_reference  > {
	friend class VectorMap;
		const_value_type data;

		const_iterator(size_type i, const std::vector<T>& v) : data(i, v[i]) {}
	public:
		const_iterator(const iterator& that) : data(that.data) {}
		const_iterator(const const_iterator& that) : data(that.data) {}
		const_iterator& operator= (const iterator& that) {
			new(&data) const_value_type{ that.data };
			return *this;
		}
		const_iterator& operator= (const const_iterator& that) {
			new(&data) const_value_type{ that.data };
			return *this;
		}

		const_reference operator* () { return data; }
		const_pointer operator-> () { return &data; }

		const_iterator& operator++ () {
			// SHENANIGANS: same reasons as iterator::operator++
			new(&data) const_value_type{ (data.first + 1), *(&data.second + 1) };
			return *this;
		}
		const_iterator operator++ (int) { const_iterator tmp = *this; ++(*this); return tmp; }

		const_iterator& operator-- () {
			// SHENANIGANS: same reasons as iterator::operator++
			new(&data) const_value_type{ (data.first - 1), *(&data.second - 1) };
			return *this;
		}
		const_iterator operator-- (int) { const_iterator tmp = *this; --(*this); return tmp; }

		const_iterator& operator+= (difference_type i) {
			// SHENANIGANS: same reasons as iterator::operator++
			new(&data) const_value_type{ (data.first + i), *(&data.second + i) };
			return *this;
		}

		const_iterator operator+ (difference_type i) const {
			const_iterator tmp = *this; return tmp += i;
		}

		const_iterator& operator-= (difference_type i) {
			// SHENANIGANS: same reasons as iterator::operator++
			new(&data) const_value_type{ (data.first - i), *(&data.second - i) };
			return *this;
		}

		const_iterator operator- (difference_type i) const {
			const_iterator tmp = *this; return tmp -= i;
		}

		difference_type operator- (const const_iterator& o) const {
			return data.first - o.data.first;
		}

		const_value_type operator[] (difference_type i) const {
			// SHENANIGANS: same reasons as iterator::operator++
			return const_value_type{ (data.first + i), *(&data.second + i) };
		}

		bool operator== (const const_iterator& o) const {
			return data.first == o.data.first && &data.second == &o.data.second;
		}
		
		bool operator!= (const const_iterator& that) const { return !(*this == that); }

		bool operator< (const const_iterator& o) const { return data.first < o.data.first; }

		bool operator> (const const_iterator& o) const { return data.first > o.data.first; }

		bool operator<= (const const_iterator& o) const { return data.first <= o.data.first; }

		bool operator>= (const const_iterator& o) const { return data.first >= o.data.first; }
	};

	/// Reserve space for n elements
	void reserve(size_type n) {
		if ( n > data.size() ) { data.insert( data.end(), n - data.size(), T{} ); }
	}

	/// Unsafe access; no bounds checking
	T& operator[] (size_type i) { return data[i]; }
	const T& operator[] (size_type i) const { return data[i]; }

	/// Safe access; will insert new values if needed
	T& at(size_type i) {
		reserve(i+1);
		return data[i];
	}

	/// Number of stored values
	unsigned size() const { return data.size(); }

	/// No stored values
	bool empty() const { return data.empty(); }

	/// Empties the map
	void clear() { data.clear(); }

	/// Returns 1 if element in map, 0 otherwise
	size_type count( size_type i ) const { return i < size() ? 1 : 0; }

	iterator begin() { return iterator{ 0, data }; }
	const_iterator begin() const { return const_iterator{ 0, data }; }
	const_iterator cbegin() const { return const_iterator{ 0, data }; }

	iterator end() { return iterator{ data.size(), data }; }
	const_iterator end() const { return const_iterator{ data.size(), data }; }
	const_iterator cend() const { return const_iterator{ data.size(), data }; }

	iterator find( size_type i ) { return i < size() ? iterator{ i, data } : end(); }
	const_iterator find( size_type i ) const { return i < size() ? const_iterator{ i, data } : end(); }
};

template<typename T>
typename VectorMap<T>::iterator operator+ (typename VectorMap<T>::difference_type i, 
                                           const typename VectorMap<T>::iterator& it) {
	return it + i;
}

template<typename T>
typename VectorMap<T>::const_iterator operator+ (typename VectorMap<T>::difference_type i, 
                                                 const typename VectorMap<T>::const_iterator& it) {
	return it + i;
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
