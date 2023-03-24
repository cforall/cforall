//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// utility.h -- General utilities used across the compiler.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Feb 17 15:25:00 2023
// Update Count     : 53
//

#pragma once

#include <cassert>
#include <cctype>
#include <algorithm>
#include <iostream>
#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <type_traits>
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

// determines if pref is a prefix of str
static inline bool isPrefix( const std::string & str, const std::string & pref, unsigned int start = 0 ) {
	if ( pref.size() > str.size() ) return false;
	return pref == str.substr(start, pref.size());
}

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

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
