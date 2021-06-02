//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Heap.h --
//
// Author           : Thierry Delisle
// Created On       : Thu May  3 16:16:10 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri May  4 14:34:08 2018
// Update Count     : 3
//

#pragma once

namespace Stats {
	namespace Heap {
		void newPass( const char * const name );
		void print();

		size_t new_stacktrace_id(const char * const name);
		void stacktrace_push(size_t id);
		void stacktrace_pop();
	}
}
