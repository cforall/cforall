//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FilterCombos.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Mon Jul 23 16:05:00 2018
// Last Modified By : Aaron B. Moss
// Last Modified On : Mon Jul 23 16:05:00 2018
// Update Count     : 1
//

#pragma once

#include <vector>

/// Type of index vector for combinations
typedef std::vector<unsigned> Indices;

/// Combo iterator that simply collects values into a vector, marking all values as valid.
/// Prefer combos in Typeops.hpp to use of IntoVectorComboIter with filterCombos
/// @param T	The element type of the vector.
template<typename T>
class IntoVectorComboIter {
	std::vector<T> crnt_combo;
public:
	/// Outputs a vector of T
	using OutType = std::vector<T>;

	/// Adds the element to the current combination, always returning true for valid.
	bool append( const T& x ) {
		crnt_combo.push_back( x );
		return true;
	}

	/// Removes the last element of the current combination.
	void backtrack() { crnt_combo.pop_back(); }

	/// Returns a copy of the current combination.
	OutType finalize() { return crnt_combo; }
};

/// Filters combinations from qs by the given combo iterator. If the iterator rejects some prefix 
/// of a combination, it will not accept any combination with that prefix.
/// qs should not be empty
template<typename Q, typename ComboIter>
auto filterCombos( const std::vector<Q>& qs, ComboIter&& iter )
		-> std::vector<typename ComboIter::OutType> {
    unsigned n = qs.size();  // number of queues to combine

	std::vector<typename ComboIter::OutType> out;  // filtered output
	for ( auto& q : qs ) if ( q.empty() ) return out;  // return empty if any empty queue

	Indices inds;
	inds.reserve( n );
	inds.push_back( 0 );

	while (true) {
		unsigned i = inds.size() - 1;

		// iterate or keep successful match
		if ( iter.append( qs[i][inds.back()] ) ) {
			if ( i + 1 == n ) {
				// keep successful match of entire combination and continue iterating final place
				out.push_back( iter.finalize() );
				iter.backtrack();
			} else {
				// try to extend successful prefix
				inds.push_back( 0 );
				continue;
			}
		}

		// move to next combo
		++inds.back();
		if ( inds.back() < qs[i].size() ) continue;
		
		// Otherwise done with the current row.
		// At this point, an invalid prefix is stored in inds and iter is in a state looking at 
		// all but the final value in inds. Now backtrack to next prefix:
		inds.pop_back();
		while ( ! inds.empty() ) {
			// try the next value at the previous index
			++inds.back();
			iter.backtrack();
			if ( inds.back() < qs[inds.size()-1].size() ) break;
			// if the previous index is finished, backtrack out
			inds.pop_back();
		}

		// Done if backtracked the entire array
		if ( inds.empty() ) return out;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
