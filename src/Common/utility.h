//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// utility.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Mon Apr 25 14:26:00 2022
// Update Count     : 51
//

#pragma once

#include <cassert>
#include <cctype>
#include <algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <cstring>										// memcmp

#include "Common/Indenter.h"

class Expression;

/// bring std::move into global scope
using std::move;

/// partner to move that copies any copyable type
template<typename T>
T copy( const T & x ) { return x; }

template< typename T >
static inline T * maybeClone( const T *orig ) {
	if ( orig ) {
		return orig->clone();
	} else {
		return 0;
	} // if
}

template< typename T, typename U >
struct maybeBuild_t {
	static T * doit( const U *orig ) {
		if ( orig ) {
			return orig->build();
		} else {
			return 0;
		} // if
	}
};

template< typename T, typename U >
static inline T * maybeBuild( const U *orig ) {
	return maybeBuild_t<T,U>::doit(orig);
}

template< typename T, typename U >
static inline T * maybeMoveBuild( const U *orig ) {
	T* ret = maybeBuild<T>(orig);
	delete orig;
	return ret;
}

template< typename Input_iterator >
void printEnums( Input_iterator begin, Input_iterator end, const char * const *name_array, std::ostream &os ) {
	for ( Input_iterator i = begin; i != end; ++i ) {
		os << name_array[ *i ] << ' ';
	} // for
}

template< typename Container >
void deleteAll( const Container &container ) {
	for ( const auto &i : container ) {
		delete i;
	} // for
}

template< typename Container >
void printAll( const Container &container, std::ostream &os, Indenter indent = {} ) {
	for ( typename Container::const_iterator i = container.begin(); i != container.end(); ++i ) {
		if ( *i ) {
			os << indent;
			(*i)->print( os, indent );
			// need an endl after each element because it's not easy to know when each individual item should end
			os << std::endl;
		} // if
	} // for
}

template< typename SrcContainer, typename DestContainer >
void cloneAll( const SrcContainer &src, DestContainer &dest ) {
	typename SrcContainer::const_iterator in = src.begin();
	std::back_insert_iterator< DestContainer > out( dest );
	while ( in != src.end() ) {
		*out++ = (*in++)->clone();
	} // while
}

template< typename SrcContainer, typename DestContainer, typename Predicate >
void cloneAll_if( const SrcContainer &src, DestContainer &dest, Predicate pred ) {
	std::back_insert_iterator< DestContainer > out( dest );
	for ( auto x : src ) {
		if ( pred(x) ) {
			*out++ = x->clone();
		}
	} // while
}

template< typename Container >
void assertAll( const Container &container ) {
	int count = 0;
	for ( typename Container::const_iterator i = container.begin(); i != container.end(); ++i ) {
		if ( !(*i) ) {
			std::cerr << count << " is null" << std::endl;
		} // if
	} // for
}

template < typename T >
std::list<T> tail( std::list<T> l ) {
	if ( ! l.empty() ) {
		std::list<T> ret(++(l.begin()), l.end());
		return ret;
	} // if
}

template < typename T >
std::list<T> flatten( std::list < std::list<T> > l) {
	typedef std::list <T> Ts;

	Ts ret;

	switch ( l.size() ) {
	  case 0:
		return ret;
	  case 1:
		return l.front();
	  default:
		ret = flatten(tail(l));
		ret.insert(ret.begin(), l.front().begin(), l.front().end());
		return ret;
	} // switch
}

/// Splice src onto the end of dst, clearing src
template< typename T >
void splice( std::vector< T > & dst, std::vector< T > & src ) {
	dst.reserve( dst.size() + src.size() );
	for ( T & x : src ) { dst.emplace_back( std::move( x ) ); }
	src.clear();
}

/// Splice src onto the begining of dst, clearing src
template< typename T >
void spliceBegin( std::vector< T > & dst, std::vector< T > & src ) {
	splice( src, dst );
	dst.swap( src );
}

template < typename T >
void toString_single( std::ostream & os, const T & value ) {
	os << value;
}

template < typename T, typename... Params >
void toString_single( std::ostream & os, const T & value, const Params & ... params ) {
	os << value;
	toString_single( os, params ... );
}

template < typename ... Params >
std::string toString( const Params & ... params ) {
	std::ostringstream os;
	toString_single( os, params... );
	return os.str();
}

#define toCString( ... ) toString( __VA_ARGS__ ).c_str()

// replace element of list with all elements of another list
template< typename T >
void replace( std::list< T > &org, typename std::list< T >::iterator pos, std::list< T > &with ) {
	typename std::list< T >::iterator next = pos; advance( next, 1 );

	//if ( next != org.end() ) {
	org.erase( pos );
	org.splice( next, with );
	//}

	return;
}

// replace range of a list with a single element
template< typename T >
void replace( std::list< T > &org, typename std::list< T >::iterator begin, typename std::list< T >::iterator end, const T & with ) {
	org.insert( begin, with );
	org.erase( begin, end );
}

template< typename... Args >
auto filter(Args&&... args) -> decltype(std::copy_if(std::forward<Args>(args)...)) {
  return std::copy_if(std::forward<Args>(args)...);
}

template <typename E, typename UnaryPredicate, template< typename, typename...> class Container, typename... Args >
void filter( Container< E *, Args... > & container, UnaryPredicate pred, bool doDelete ) {
	auto i = begin( container );
	while ( i != end( container ) ) {
		auto it = next( i );
		if ( pred( *i ) ) {
			if ( doDelete ) {
				delete *i;
			} // if
			container.erase( i );
		} // if
		i = it;
	} // while
}

template<typename Container, typename Pred>
void erase_if( Container & cont, Pred && pred ) {
	auto keep_end = std::remove_if( cont.begin(), cont.end(), pred );
	cont.erase( keep_end, cont.end() );
}

template< typename... Args >
auto zip(Args&&... args) -> decltype(zipWith(std::forward<Args>(args)..., std::make_pair)) {
  return zipWith(std::forward<Args>(args)..., std::make_pair);
}

template< class InputIterator1, class InputIterator2, class OutputIterator, class BinFunction >
void zipWith( InputIterator1 b1, InputIterator1 e1, InputIterator2 b2, InputIterator2 e2, OutputIterator out, BinFunction func ) {
	while ( b1 != e1 && b2 != e2 )
		*out++ = func(*b1++, *b2++);
}

// it's nice to actually be able to increment iterators by an arbitrary amount
template< class InputIt, class Distance >
InputIt operator+( InputIt it, Distance n ) {
	advance(it, n);
	return it;
}

template< typename T >
void warn_single( const T & arg ) {
	std::cerr << arg << std::endl;
}

template< typename T, typename... Params >
void warn_single(const T & arg, const Params & ... params ) {
	std::cerr << arg;
	warn_single( params... );
}

template< typename... Params >
void warn( const Params & ... params ) {
	std::cerr << "Warning: ";
	warn_single( params... );
}

// determines if pref is a prefix of str
static inline bool isPrefix( const std::string & str, const std::string & pref, unsigned int start = 0 ) {
	if ( pref.size() > str.size() ) return false;
    return 0 == memcmp( str.c_str() + start, pref.c_str(), pref.size() );
	// return prefix == full.substr(0, prefix.size()); // for future, requires c++17
}

// -----------------------------------------------------------------------------
// Ref Counted Singleton class
// Objects that inherit from this class will have at most one reference to it
// but if all references die, the object will be deleted.

template< typename ThisType >
class RefCountSingleton {
  public:
	static std::shared_ptr<ThisType> get() {
		if( global_instance.expired() ) {
			std::shared_ptr<ThisType> new_instance = std::make_shared<ThisType>();
			global_instance = new_instance;
			return std::move(new_instance);
		}
		return global_instance.lock();
	}
  private:
	static std::weak_ptr<ThisType> global_instance;
};

template< typename ThisType >
std::weak_ptr<ThisType> RefCountSingleton<ThisType>::global_instance;

// -----------------------------------------------------------------------------
// RAII object to regulate "save and restore" behaviour, e.g.
// void Foo::bar() {
//   ValueGuard<int> guard(var); // var is a member of type Foo
//   var = ...;
// } // var's original value is restored
template< typename T >
struct ValueGuard {
	T old;
	T& ref;

	ValueGuard(T& inRef) : old(inRef), ref(inRef) {}
	~ValueGuard() { ref = old; }
};

template< typename T >
struct ValueGuardPtr {
	T old;
	T* ref;

	ValueGuardPtr(T * inRef) : old( inRef ? *inRef : T() ), ref(inRef) {}
	ValueGuardPtr(const ValueGuardPtr& other) = delete;
	ValueGuardPtr(ValueGuardPtr&& other) : old(other.old), ref(other.ref) { other.ref = nullptr; }
	~ValueGuardPtr() { if( ref ) *ref = old; }
};

template< typename aT >
struct FuncGuard {
	aT m_after;

	template< typename bT >
	FuncGuard( bT before, aT after ) : m_after( after ) {
		before();
	}

	~FuncGuard() {
		m_after();
	}
};

template< typename bT, typename aT >
FuncGuard<aT> makeFuncGuard( bT && before, aT && after ) {
	return FuncGuard<aT>( std::forward<bT>(before), std::forward<aT>(after) );
}

template< typename T >
struct ValueGuardPtr< std::list< T > > {
	std::list< T > old;
	std::list< T >* ref;

	ValueGuardPtr( std::list< T > * inRef) : old(), ref(inRef) {
		if( ref ) { swap( *ref, old ); }
	}
	~ValueGuardPtr() { if( ref ) { swap( *ref, old ); } }
};

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
// syntax to have a for each that iterates a container, mapping each element by applying f
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

// -----------------------------------------------------------------------------
// O(1) polymorphic integer ilog2, using clz, which returns the number of leading 0-bits, starting at the most
// significant bit (single instruction on x86)

template<typename T>
inline
#if defined(__GNUC__) && __GNUC__ > 4
constexpr
#endif
T ilog2(const T & t) {
	if(std::is_integral<T>::value) {
		const constexpr int r = sizeof(t) * __CHAR_BIT__ - 1;
		if( sizeof(T) == sizeof(unsigned       int) ) return r - __builtin_clz  ( t );
		if( sizeof(T) == sizeof(unsigned      long) ) return r - __builtin_clzl ( t );
		if( sizeof(T) == sizeof(unsigned long long) ) return r - __builtin_clzll( t );
	}
	assert(false);
	return -1;
} // ilog2

// -----------------------------------------------------------------------------
/// evaluates expr as a long long int. If second is false, expr could not be evaluated
std::pair<long long int, bool> eval(const Expression * expr);

namespace ast {
	class Expr;
}

std::pair<long long int, bool> eval(const ast::Expr * expr);

// -----------------------------------------------------------------------------
/// Reorders the input range in-place so that the minimal-value elements according to the
/// comparator are in front;
/// returns the iterator after the last minimal-value element.
template<typename Iter, typename Compare>
Iter sort_mins( Iter begin, Iter end, Compare& lt ) {
	if ( begin == end ) return end;

	Iter min_pos = begin;
	for ( Iter i = begin + 1; i != end; ++i ) {
		if ( lt( *i, *min_pos ) ) {
			// new minimum cost; swap into first position
			min_pos = begin;
			std::iter_swap( min_pos, i );
		} else if ( ! lt( *min_pos, *i ) ) {
			// duplicate minimum cost; swap into next minimum position
			++min_pos;
			std::iter_swap( min_pos, i );
		}
	}
	return ++min_pos;
}

template<typename Iter, typename Compare>
inline Iter sort_mins( Iter begin, Iter end, Compare&& lt ) {
	return sort_mins( begin, end, lt );
}

/// sort_mins defaulted to use std::less
template<typename Iter>
inline Iter sort_mins( Iter begin, Iter end ) {
	return sort_mins( begin, end, std::less<typename std::iterator_traits<Iter>::value_type>{} );
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
