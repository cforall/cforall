//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Stats.hpp --
//
// Author           : Thierry Delisle
// Created On       : Thu Feb 28 11::27:10 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

// Entry point for compiler analytics.
/*
The compiler currently supports 3 times of analytics:
	 - generic counters
	 - heap statistics
	 - timiing statistics

These can be enabled using the --stats option, to which a comma seperated list of options can be passed.
For more information, see Stats/Stats.cpp

Counters:
	The counters are a generic tree of counters that print in a 2-column output format.
	They can count maximums, averages, totals, etc.

	Currently all counters are under the same enable block, this could be changed if needed.

Heap:
	Measures the total calls malloc and free as the peak number of allocations per pass

Timing:
	Comming soon
*/


#include "Common/Stats/Counter.hpp"
#include "Common/Stats/Heap.hpp"
#include "Common/Stats/Time.hpp"

namespace Stats {
	void parse_params(const char * const params);
	void print();
}
