//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveTime.cpp --
//
// Author           : Thierry Delisle
// Created On       : Wed Sep 16 15:45:51 2020
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "ResolveTime.hpp"

#include <fstream>
#include <iomanip>

#include "AST/Fwd.hpp"
#include "AST/Expr.hpp"
#include "AST/Print.hpp"
#include "AST/Type.hpp"

namespace Stats {
	namespace ResolveTime {
		static inline long long rdtscl(void) {
			#if defined( __i386 ) || defined( __x86_64 )
				unsigned int lo, hi;
				__asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
				return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );
			#elif defined( __aarch64__ )
				int64_t value;
				asm volatile("mrs %0, cntvct_el0" : "=r"(value));
				return value;
			#else
				#error unknown hardware architecture
			#endif
		}

		extern bool enabled;
		bool started = false;
		long long before;
		std::ostream & out = std::cout;

		void start( const ast::Expr * expr ) {
			if(enabled) {
				assert(!started);
				started = true;

				out << expr->location << " : ";

				before = rdtscl();
			}
		}
		void stop() {
			if(enabled) {
				assert(started);
				auto after = rdtscl();
				out << (after - before) << std::endl;

				started = false;
			}
		}
	};
};
