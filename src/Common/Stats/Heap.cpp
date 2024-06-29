//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Heap.cpp --
//
// Author           : Thierry Delisle
// Created On       : Thu May  3 16:16:10 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri May  4 17:27:31 2018
// Update Count     : 28
//

#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstring>
#include <iomanip>
#include <iostream>

// Most of the other statistics features are deactivated only by defining
// NO_STATISTICS (or their NO_%_STATISTICS macro). However the heap has some
// other compatability concerns and will disable itself in some cases.
//
// I do not claim to understand these cases. But TCMALLOC is often defined by
// default and you can pass --disable-gprofiler to configure to remove it.

#if defined(NO_STATISTICS) || defined(TCMALLOC) || defined(__SANITIZE_ADDRESS__)
	#define NO_HEAP_STATISTICS
#elif defined(__has_feature)
	#if __has_feature(address_sanitizer)
		#define NO_HEAP_STATISTICS
	#endif
#endif

namespace Stats {
	namespace Heap {
#if defined( NO_HEAP_STATISTICS )
		void newPass( const char * const ) {}

		void print() {}
#else
		extern bool enabled;

		struct StatBlock {
			const char * name  = nullptr;	///< Name of this pass
			size_t mallocs     = 0;			///< Allocations in this pass
			size_t frees       = 0;			///< Frees in this pass
			size_t n_allocs    = 0;			///< Current number of live allocations
			size_t peak_allocs = 0;			///< Peak number of live allocations this pass
		};

		StatBlock    passes[100] = {{ "Pre-Parse", 0, 0, 0, 0 }};
		const size_t passes_size = sizeof(passes) / sizeof(passes[0]);
		size_t       passes_cnt = 1;

		StatBlock    stacktrace_stats[100];
		size_t       stacktrace_stats_count = 0;
		bool         stacktrace_stats_enabled = true;

		size_t       trace[1000];
		const size_t stacktrace_max_depth = sizeof(trace) / sizeof(size_t);
		size_t       stacktrace_depth;

		size_t new_stacktrace_id(const char * const name) {
			stacktrace_stats[stacktrace_stats_count].name = name;
			return stacktrace_stats_count++;
		}

		void stacktrace_push(size_t id) {
			++stacktrace_depth;
			assertf(stacktrace_depth < stacktrace_max_depth, "Stack trace too deep: increase size of array in Heap.cpp");
			trace[stacktrace_depth] = id;
		}

		void stacktrace_pop() {
			assertf(stacktrace_depth > 0, "Invalid stack tracing operation: trace is empty");
			--stacktrace_depth;
		}

		void newPass( const char * const name ) {
			passes[passes_cnt].name    = name;
			passes[passes_cnt].mallocs = 0;
			passes[passes_cnt].frees   = 0;
			passes[passes_cnt].n_allocs
				= passes[passes_cnt].peak_allocs
				= passes[passes_cnt-1].n_allocs;
			passes_cnt++;

			assertf(passes_cnt < passes_size, "Too many passes for Stats::Heap, increase the size of the array in Heap.cpp");
		}

		void print(size_t value, size_t total) {
			std::cerr << std::setw(12) << value;
			std::cerr << "(" << std::setw(3);
			std::cerr << (value == 0 ? 0 : value * 100 / total);
			std::cerr << "%) | ";
		}

		void print(const StatBlock& stat, size_t nc, size_t total_mallocs, size_t total_frees, size_t overall_peak) {
			std::cerr << std::setw(nc) << stat.name;
			std::cerr << " | ";

			print(stat.mallocs,     total_mallocs);
			print(stat.frees,       total_frees  );
			print(stat.peak_allocs, overall_peak );
			std::cerr << "\n";
		}

		void print(char c, size_t nc) {
			for(size_t i = 0; i < nc; i++) {
				std::cerr << c;
			}
			std::cerr << '\n';
		}

		void print() {
			if(!enabled) return;

			size_t nc = 0;
			size_t total_mallocs = 0;
			size_t total_frees   = 0;
			size_t overall_peak  = 0;
			for(size_t i = 0; i < passes_cnt; i++) {
				nc = std::max(nc, std::strlen(passes[i].name));
				total_mallocs += passes[i].mallocs;
				total_frees   += passes[i].frees;
				overall_peak = std::max(overall_peak, passes[i].peak_allocs);
			}
			size_t nct = nc + 65;

			const char * const title = "Heap Usage Statistic";
			print('=', nct);
			for(size_t i = 0; i < (nct - std::strlen(title)) / 2; i++) std::cerr << ' ';
			std::cerr << title << std::endl;
			print('-', nct);
			std::cerr << std::setw(nc) << "Pass";
			std::cerr << " |       Malloc Count |         Free Count |        Peak Allocs |" << std::endl;

			print('-', nct);
			for(size_t i = 0; i < passes_cnt; i++) {
				print(passes[i], nc, total_mallocs, total_frees, overall_peak);
			}

			print('-', nct);
			std::cerr << std::setw(nc) << "Trace";
			std::cerr << " |       Malloc Count |         Free Count |        Peak Allocs |" << std::endl;

			print('-', nct);
			for (size_t i = 0; i < stacktrace_stats_count; i++) {
				print(stacktrace_stats[i], nc, total_mallocs, total_frees, overall_peak);
			}
			print('-', nct);
			print({"Sum", total_mallocs, total_frees, 0, overall_peak},
				nc, total_mallocs, total_frees, overall_peak);

		}

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
		extern "C" {
#include <dlfcn.h>
#include <execinfo.h>
		}

	//=============================================================================================
	// Interposing helpers
	//=============================================================================================

		typedef void (* generic_fptr_t)(void);
		generic_fptr_t interpose_symbol( const char * symbol, const char * version ) {
			const char * error;

			static void * library;
			if ( ! library ) {
#				if defined( RTLD_NEXT )
					library = RTLD_NEXT;
#				else
					// missing RTLD_NEXT => must hard-code library name, assuming libstdc++
					library = dlopen( "libc.so.6", RTLD_LAZY );
					error = dlerror();
					if ( error ) {
						std::cerr << "interpose_symbol : failed to open libc, " << error << std::endl;
						abort();
					}
#				endif // RTLD_NEXT
			} // if

			generic_fptr_t fptr;

#			if defined( _GNU_SOURCE )
				if ( version ) {
					fptr = (generic_fptr_t)dlvsym( library, symbol, version );
				} else {
					fptr = (generic_fptr_t)dlsym( library, symbol );
				}
#			else
				fptr = (generic_fptr_t)dlsym( library, symbol );
#			endif // _GNU_SOURCE

			error = dlerror();
			if ( error ) {
				std::cerr << "interpose_symbol : internal error, " << error << std::endl;
				abort();
			}

			return fptr;
		}

		extern "C" {
			void * malloc( size_t size ) __attribute__((malloc));
			void * malloc( size_t size ) {
				static auto __malloc = reinterpret_cast<void * (*)(size_t)>(interpose_symbol( "malloc", nullptr ));
				if( enabled && passes_cnt > 0 ) {
					passes[passes_cnt - 1].mallocs++;
					passes[passes_cnt - 1].n_allocs++;
					passes[passes_cnt - 1].peak_allocs
						= std::max(passes[passes_cnt - 1].peak_allocs, passes[passes_cnt - 1].n_allocs);
				}

				if ( stacktrace_stats_enabled && stacktrace_depth > 0) {
					stacktrace_stats[trace[stacktrace_depth]].mallocs++;
				}
				return __malloc( size );
			}

			void free( void * ptr ) {
				static auto __free = reinterpret_cast<void   (*)(void *)>(interpose_symbol( "free", nullptr ));
				if( enabled && passes_cnt > 0 ) {
					passes[passes_cnt - 1].frees++;
					passes[passes_cnt - 1].n_allocs--;
				}
				if ( stacktrace_stats_enabled && stacktrace_depth > 0) {
					stacktrace_stats[trace[stacktrace_depth]].frees++;
				}
				return __free( ptr );
			}

			void * calloc( size_t nelem, size_t size ) {
				static auto __calloc = reinterpret_cast<void * (*)(size_t, size_t)>(interpose_symbol( "calloc", nullptr ));
				if( enabled && passes_cnt > 0 ) {
					passes[passes_cnt - 1].mallocs++;
					passes[passes_cnt - 1].n_allocs++;
					passes[passes_cnt - 1].peak_allocs
						= std::max(passes[passes_cnt - 1].peak_allocs, passes[passes_cnt - 1].n_allocs);
				}
				if ( stacktrace_stats_enabled && stacktrace_depth > 0) {
					stacktrace_stats[trace[stacktrace_depth]].mallocs++;
				}
				return __calloc( nelem, size );
			}

			void * realloc( void * ptr, size_t size ) {
				static auto __realloc = reinterpret_cast<void * (*)(void *, size_t)>(interpose_symbol( "realloc", nullptr ));
				void * s = __realloc( ptr, size );
				if ( enabled && s != ptr && passes_cnt > 0 ) {			// did realloc get new storage ?
					passes[passes_cnt - 1].mallocs++;
					passes[passes_cnt - 1].frees++;
				} // if
				if ( stacktrace_stats_enabled && stacktrace_depth > 0) {
					stacktrace_stats[trace[stacktrace_depth]].mallocs++;
					stacktrace_stats[trace[stacktrace_depth]].frees++;
				}
				return s;
			}
		}
#endif
	}
}
