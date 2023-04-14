//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Iterate.hpp --
//
// Author           : Andrew Beach
// Created On       : Fri Feb 17 13:32:00 2023
// Last Modified By : Andrew Beach
// Last Modified On : Fri Feb 17 13:32:00 2023
// Update Count     : 0
//

#pragma once

#include <algorithm>
#include <functional>
#include <iterator>

// Returns an iterator that is it advanced n times.
template< class InputIt, class Distance >
InputIt operator+( InputIt it, Distance n ) {
	advance(it, n);
	return it;
}

// -----------------------------------------------------------------------------
// Helper struct and function to support
// for ( val : reverseIterate( container ) ) {}
// syntax to have a for each that iterates backwards

template< typename T >
struct reverse_iterate_t {
	T& ref;

	reverse_iterate_t( T & ref ) : ref(ref) {}

	// this does NOT work on const T!!!
	// typedef typename T::reverse_iterator iterator;
	auto begin() { return ref.rbegin(); }
	auto end() { return ref.rend(); }
};

template< typename T >
reverse_iterate_t< T > reverseIterate( T & ref ) {
	return reverse_iterate_t< T >( ref );
}

// -----------------------------------------------------------------------------
// Helper struct and function to support
// for ( val_and_index : enumerate( container ) ) {}
// which iterates through the container and tracks the index as well.

template< typename T >
struct enumerate_t {
	template<typename val_t>
	struct value_t {
		val_t & val;
		size_t idx;
	};

	template< typename iter_t, typename val_t >
	struct iterator_t {
		iter_t it;
		size_t idx;

		iterator_t( iter_t _it, size_t _idx ) : it(_it), idx(_idx) {}

		value_t<val_t> operator*() const { return value_t<val_t>{ *it, idx }; }

		bool operator==(const iterator_t & o) const { return o.it == it; }
		bool operator!=(const iterator_t & o) const { return o.it != it; }

		iterator_t & operator++() {
			it++;
			idx++;
			return *this;
		}

		using difference_type   = typename std::iterator_traits< iter_t >::difference_type;
		using value_type        = value_t<val_t>;
		using pointer           = value_t<val_t> *;
		using reference         = value_t<val_t> &;
		using iterator_category = std::forward_iterator_tag;
	};

	T & ref;

	using iterator = iterator_t< typename T::iterator, typename T::value_type >;
	using const_iterator = iterator_t< typename T::const_iterator, const typename T::value_type >;

	iterator begin() { return iterator( ref.begin(), 0 ); }
	iterator end()   { return iterator( ref.end(), ref.size() ); }

	const_iterator begin() const { return const_iterator( ref.cbegin(), 0 ); }
	const_iterator end()   const { return const_iterator( ref.cend(), ref.size() ); }

	const_iterator cbegin() const { return const_iterator( ref.cbegin(), 0 ); }
	const_iterator cend()   const { return const_iterator( ref.cend(), ref.size() ); }
};

template< typename T >
enumerate_t<T> enumerate( T & ref ) {
	return enumerate_t< T >{ ref };
}

template< typename T >
const enumerate_t< const T > enumerate( const T & ref ) {
	return enumerate_t< const T >{ ref };
}

// -----------------------------------------------------------------------------
// Helper function to transform one iterable container into another.

template< typename OutType, typename Range, typename Functor >
OutType map_range( const Range& range, Functor&& functor ) {
	OutType out;

	std::transform(
		begin( range ),
		end( range ),
		std::back_inserter( out ),
		std::forward< Functor >( functor )
	);

	return out;
}

// -----------------------------------------------------------------------------
// Helper struct and function to support:
// for ( auto val : group_iterate( container1, container2, ... ) ) { ... }
// This iteraters through multiple containers of the same size.

template<typename... Args>
class group_iterate_t {
	using Iterables = std::tuple<Args...>;
	Iterables iterables;

	// Getting the iterator and value types this way preserves const.
	template<size_t I> using Iter = decltype(std::get<I>(iterables).begin());
	template<size_t I> using Data = decltype(*std::get<I>(iterables).begin());
	template<typename> struct base_iterator;

	// This inner template puts the sequence of `0, 1, ... sizeof...(Args)-1`
	// into a pack. These are the indexes into the tuples, so unpacking can
	// go over each element of the tuple.
	// The std::integer_sequence is just used to build that sequence.
	// A library reference will probably explain it better than I can.
	template<std::size_t... Indices>
	struct base_iterator<std::integer_sequence<std::size_t, Indices...>> {
		using value_type = std::tuple< Data<Indices>... >;
		std::tuple<Iter<Indices>...> iterators;

		base_iterator( Iter<Indices>... is ) : iterators( is... ) {}
		base_iterator operator++() {
			return base_iterator( ++std::get<Indices>( iterators )... );
		}
		bool operator!=( const base_iterator& other ) const {
			return iterators != other.iterators;
		}
		value_type operator*() const {
			return std::tie( *std::get<Indices>( iterators )... );
		}

		static base_iterator make_begin( Iterables & data ) {
			return base_iterator( std::get<Indices>( data ).begin()... );
		}
		static base_iterator make_end( Iterables & data ) {
			return base_iterator( std::get<Indices>( data ).end()... );
		}
	};

public:
	group_iterate_t( const Args &... args ) : iterables( args... ) {}

	using iterator = base_iterator<decltype(
		std::make_integer_sequence<std::size_t, sizeof...(Args)>())>;

	iterator begin() { return iterator::make_begin( iterables ); }
	iterator end() { return iterator::make_end( iterables ); }
};

// Helpers for the bounds checks (the non-varatic part of group_iterate):
static inline void runGroupBoundsCheck(size_t size0, size_t size1) {
	assertf( size0 == size1,
		"group iteration requires containers of the same size: <%zd, %zd>.",
		size0, size1 );
}

static inline void runGroupBoundsCheck(size_t size0, size_t size1, size_t size2) {
	assertf( size0 == size1 && size1 == size2,
		"group iteration requires containers of the same size: <%zd, %zd, %zd>.",
		size0, size1, size2 );
}

/// Performs bounds check to ensure that all arguments are of the same length.
template< typename... Args >
group_iterate_t<Args...> group_iterate( Args &&... args ) {
	runGroupBoundsCheck( args.size()... );
	return group_iterate_t<Args...>( std::forward<Args>( args )... );
}

/// Does not perform a bounds check - requires user to ensure that iteration terminates when appropriate.
template< typename... Args >
group_iterate_t<Args...> unsafe_group_iterate( Args &&... args ) {
	return group_iterate_t<Args...>( std::forward<Args>( args )... );
}

// -----------------------------------------------------------------------------
// Helper struct and function to support
// for ( val : lazy_map( container1, f ) ) {}
// syntax to have a for each that iterates a container,
// mapping each element by applying f.

template< typename T, typename Func >
struct lambda_iterate_t {
	const T & ref;
	std::function<Func> f;

	struct iterator {
		typedef decltype(begin(ref)) Iter;
		Iter it;
		std::function<Func> f;
		iterator( Iter it, std::function<Func> f ) : it(it), f(f) {}
		iterator & operator++() {
			++it; return *this;
		}
		bool operator!=( const iterator &other ) const { return it != other.it; }
		auto operator*() const -> decltype(f(*it)) { return f(*it); }
	};

	lambda_iterate_t( const T & ref, std::function<Func> f ) : ref(ref), f(f) {}

	auto begin() const -> decltype(iterator(std::begin(ref), f)) { return iterator(std::begin(ref), f); }
	auto end() const   -> decltype(iterator(std::end(ref), f)) { return iterator(std::end(ref), f); }
};

template< typename... Args >
lambda_iterate_t<Args...> lazy_map( const Args &... args ) {
	return lambda_iterate_t<Args...>( args...);
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
