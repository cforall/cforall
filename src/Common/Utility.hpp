//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Utility.hpp -- General utilities used across the compiler.
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jan 17 14:40:00 2024
// Update Count     : 54
//

#pragma once

#include <cassert>
#include <algorithm>
#include <list>
#include <string>
#include <type_traits>
#include <vector>

/// partner to move that copies any copyable type
template<typename T>
T copy( const T & x ) { return x; }

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

/// Remove elements that match pred from the container.
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

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
